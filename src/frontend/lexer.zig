const std = @import("std");
const mem = std.mem;

const Token = @import("./token.zig").Token;

pub const LexerError = error{ InvalidNumberFormat, IllegalChar, IllegalUtf8Char, MissingChar };

pub const Source = struct {
    buffer: []const u8,
    file_name: []const u8,
};

pub const Lexer = struct {
    const Self = @This();

    source: Source,
    position: usize = 0,
    prev_ch: u8 = 0,
    ch: u8 = 0,

    pub fn init(source: Source) Self {
        return Self{ .source = source, .ch = source.buffer[0] };
    }

    pub fn has_tokens(self: *Self) bool {
        return self.ch != 0;
    }

    pub fn next_token(self: *Self) LexerError!Token {
        self.skip_whitespace();

        const tok: LexerError!Token = switch (self.ch) {
            0 => .eof,
            '@' => {
                const symbol = self.read_symbol();
                return .{ .symbol = symbol };
            },
            'a'...'z', 'A'...'Z', '_', '$' => {
                const ident = self.read_identifier();
                if (Token.keywords.get(ident)) |token| {
                    return token;
                }
                return .{ .ident = ident };
            },
            '0'...'9' => {
                const number = try self.read_number();
                return .{ .number = number };
            },
            '"' => {
                const string = self.read_string();
                return .{ .string = string };
            },
            '\'' => {
                const char = try self.read_char();
                return .{ .char = char };
            },
            else => blk: {
                var i = Token.operators.kvs.len;
                while (i > 0) : (i -= 1) {
                    const operator = Token.operators.kvs[i - 1].key;
                    if (mem.startsWith(u8, self.source.buffer[self.position..], operator)) {
                        self.forward_n(operator.len);
                        return Token.operators.kvs[i - 1].value;
                    }
                }
                break :blk LexerError.IllegalChar;
            },
        };

        self.forward();
        return tok;
    }

    fn peek_char(self: *Self, n: usize) u8 {
        if (self.position + n >= self.source.buffer.len) {
            return 0;
        } else {
            return self.source.buffer[self.position + n];
        }
    }

    inline fn forward(self: *Self) void {
        self.forward_n(1);
    }

    fn forward_n(self: *Self, n: usize) void {
        self.position = self.position + n;

        if (self.position >= self.source.buffer.len) {
            self.ch = 0;
        } else {
            self.ch = self.source.buffer[self.position];
        }

        if (self.position - 1 >= self.source.buffer.len) {
            self.prev_ch = 0;
        } else {
            self.prev_ch = self.source.buffer[self.position - 1];
        }
    }

    fn read_symbol(self: *Self) []const u8 {
        const position = self.position;

        self.forward();
        if (self.ch == '@') self.forward();

        while (std.ascii.isAlphanumeric(self.ch) or self.ch == '_' or self.ch == '$') {
            self.forward();
        }

        return self.source.buffer[position..self.position];
    }

    fn read_string(self: *Self) []const u8 {
        const position = self.position;

        self.forward();
        while (self.ch > 0 and !(self.ch == '"' and self.prev_ch != '\\')) {
            self.forward();
        }
        self.forward();

        return self.source.buffer[position + 1 .. self.position - 1];
    }

    fn read_char(self: *Self) LexerError![]const u8 {
        self.forward();
        // utf-8 char
        const bytes_len: u3 = blk: {
            if (self.ch & 0b10000000 == 0)
                break :blk 1;
            if (self.ch & 0b11111000 == 0b11110000)
                break :blk 4;
            if (self.ch & 0b11110000 == 0b11100000)
                break :blk 3;
            if (self.ch & 0b11100000 == 0b11000000)
                break :blk 2;
            // invalid utf-8 char
            return LexerError.IllegalUtf8Char;
        };

        const char = self.source.buffer[self.position .. self.position + bytes_len];
        self.forward_n(bytes_len);

        if (self.ch != '\'') return LexerError.MissingChar;
        self.forward();

        return char;
    }

    fn read_identifier(self: *Self) []const u8 {
        const position = self.position;

        while (std.ascii.isAlphanumeric(self.ch) or self.ch == '_' or self.ch == '$') {
            self.forward();
        }

        return self.source.buffer[position..self.position];
    }

    fn read_number(self: *Self) LexerError![]const u8 {
        const position = self.position;

        if (self.ch == '0') {
            const char = self.peek_char(1);
            switch (char) {
                'x' => {
                    self.forward_n(2);
                    while (std.ascii.isHex(self.ch)) {
                        self.forward();
                    }
                },
                'o' => {
                    self.forward_n(2);
                    while (self.ch >= '0' and self.ch <= '7') {
                        self.forward();
                    }
                },
                'b' => {
                    self.forward_n(2);
                    while (self.ch == '0' or self.ch == '1') {
                        self.forward();
                    }
                },
                else => if (std.ascii.isAlphabetic(char))
                    return LexerError.InvalidNumberFormat,
            }
        }

        if (position == self.position) {
            while (std.ascii.isDigit(self.ch)) {
                self.forward();
            }

            if (self.ch == '.' and !std.ascii.isAlphabetic(self.peek_char(1))) {
                self.forward();
                while (std.ascii.isDigit(self.ch)) {
                    self.forward();
                }
            }
        }

        return self.source.buffer[position..self.position];
    }

    fn skip_whitespace(self: *Self) void {
        while (std.ascii.isWhitespace(self.ch)) {
            self.forward();
        }
    }
};

const expectEqualDeep = std.testing.expectEqualDeep;
test "lexer parsing operators" {
    const input = "= += -= *= /= %= **= <<= >>= >>>= &= |= ^= ~= &&= ||= ??= ?= == != >= <= > < + - * / % ** << >> >>> & | ^ ~ && || ?? ...";
    var lex = Lexer.init(Source{ .buffer = input, .file_name = "test_file" });

    var tokens = [_]Token{
        .eq,
        .plus_eq,
        .minus_eq,
        .star_eq,
        .div_eq,
        .mod_eq,
        .pow_eq,
        .lshift_eq,
        .rshift_eq,
        .urshift_eq,
        .bit_and_eq,
        .bit_or_eq,
        .bit_xor_eq,
        .bit_not_eq,
        .and_eq,
        .or_eq,
        .nullish_eq,
        .conditional_eq,

        .eqs,
        .neq,
        .geq,
        .leq,
        .gt,
        .lt,

        .plus,
        .minus,
        .star,
        .div,
        .mod,
        .pow,
        .lshift,
        .rshift,
        .urshift,
        .bit_and,
        .bit_or,
        .bit_xor,
        .bit_not,
        .and_op,
        .or_op,
        .nullish,
        .dotdotdot,
        .eof,
    };

    for (tokens) |token| {
        const tok = try lex.next_token();

        try expectEqualDeep(token, tok);
    }
}

test "lexer parsing ident and keywords" {
    const input = "test1 let const func return if else export null true false";
    var lex = Lexer.init(Source{ .buffer = input, .file_name = "test_file" });

    var tokens = [_]Token{ .{ .ident = "test1" }, .kw_let, .kw_const, .kw_function, .kw_return, .kw_if, .kw_else, .kw_export, .kw_null, .kw_true, .kw_false, .eof };

    for (tokens) |token| {
        const tok = try lex.next_token();

        try expectEqualDeep(token, tok);
    }
}

test "lexer parsing number" {
    const input = "0 10 1.2 1. 0x1Af 0b101 0o7";
    var lex = Lexer.init(Source{ .buffer = input, .file_name = "test_file" });

    var tokens = [_]Token{ .{ .number = "0" }, .{ .number = "10" }, .{ .number = "1.2" }, .{ .number = "1." }, .{ .number = "0x1Af" }, .{ .number = "0b101" }, .{ .number = "0o7" }, .eof };

    for (tokens) |token| {
        const tok = try lex.next_token();

        try expectEqualDeep(token, tok);
    }
}

test "lexer parsing string and char" {
    const input = "\"test\" 'a' 'é' '女'";
    var lex = Lexer.init(Source{ .buffer = input, .file_name = "test_file" });

    var tokens = [_]Token{ .{ .string = "test" }, .{ .char = "a" }, .{ .char = "é" }, .{ .char = "女" }, .eof };

    for (tokens) |token| {
        const tok = try lex.next_token();

        try expectEqualDeep(token, tok);
    }
}
