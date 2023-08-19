const std = @import("std");
const lexer = @import("./lexer.zig");

const Statement = union(enum) {
    exported: *const Statement,
    expression: Expr,
    variable_declaration: struct {
        constant: bool,
        name: []const u8,
        value: Expr,
    },
    function_declaration: struct {
        name: []const u8,
        args: std.ArrayList([]const u8),
        statements: std.ArrayList(Statement),
    },
};

const Expr = union(enum) {
    block: std.ArrayList(Statement),
    parent: *const Expr,

    unary_operation: struct { right: *const Expr, kind: UnaryOp },
    binary_operation: struct { left: *const Expr, right: *const Expr, kind: BinaryOp },

    ident: []const u8,
    char_litteral: []const u8,
    string_litteral: []const u8,
    symbol_litteral: struct { name: []const u8, strictly_unique: bool },
    float_litteral: []const u8,
    int_litteral: []const u8,
    bool_litteral: bool,
    null_litteral,
};

const UnaryOp = enum {
    none,

    plus,
    negate,

    const Self = @This();
    fn from_token(token: lexer.Token) Self {
        return switch (token) {
            .plus => .plus,
            .minus => .negate,
            else => .none,
        };
    }
};

const BinaryOp = enum {
    none,

    assign,
    plus_assign,
    sub_assign,
    mult_assign,
    div_assign,
    mod_assign,
    pow_assign,
    lshift_assign,
    rshift_assign,
    urshift_assign,
    bit_and_assign,
    bit_or_assign,
    bit_xor_assign,
    bit_not_assign,
    and_assign,
    or_assign,
    nullish_assign,
    conditional_assign,

    eqs,
    neq,
    geq,
    leq,
    gt,
    lt,

    add,
    sub,
    mult,
    div,
    mod,
    pow,
    lshift,
    rshift,
    urshift,
    bit_and,
    bit_or,
    bit_xor,
    bit_not,
    and_op,
    or_op,
    nullish,

    const Self = @This();
    fn from_token(token: lexer.Token) Self {
        return switch (token) {
            .eq => .assign,
            .plus_eq => .plus_assign,
            .minus_eq => .sub_assign,
            .star_eq => .mult_assign,
            .div_eq => .div_assign,
            .mod_eq => .mod_assign,
            .pow_eq => .pow_assign,
            .lshift_eq => .lshift_assign,
            .rshift_eq => .rshift_assign,
            .urshift_eq => .urshift_assign,
            .bit_and_eq => .bit_and_assign,
            .bit_or_eq => .bit_or_assign,
            .bit_xor_eq => .bit_xor_assign,
            .bit_not_eq => .bit_not_assign,
            .and_eq => .and_assign,
            .or_eq => .or_assign,
            .nullish_eq => .nullish_assign,
            .conditional_eq => .conditional_assign,

            .eqs => .eqs,
            .neq => .neq,
            .geq => .geq,
            .leq => .leq,
            .gt => .gt,
            .lt => .lt,

            .plus => .add,
            .minus => .sub,
            .star => .mult,
            .div => .div,
            .mod => .mod,
            .pow => .pow,
            .lshift => .lshift,
            .rshift => .rshift,
            .urshift => .urshift,
            .bit_and => .bit_and,
            .bit_or => .bit_or,
            .bit_xor => .bit_xor,
            .bit_not => .bit_not,
            .and_op => .and_op,
            .or_op => .or_op,
            .nullish => .nullish,
            else => .none,
        };
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: *const std.ArrayList(lexer.Token),

    current_token: lexer.Token,
    current_token_index: usize = 0,

    const Self = @This();
    pub fn parse(allocator: std.mem.Allocator, tokens: *const std.ArrayList(lexer.Token)) !std.ArrayList(Statement) {
        var parser = Parser{ .allocator = allocator, .tokens = tokens, .current_token = tokens.items[0] };

        return parser.parse_statements();
    }

    /// Test if the current token if it's possible
    inline fn check(self: *Self, token: lexer.Token) bool {
        return @intFromEnum(self.current_token) == @intFromEnum(token);
    }

    /// Skip the current token if it's possible
    fn skip(self: *Self) void {
        self.current_token_index += 1;
        if (self.current_token_index >= self.tokens.items.len)
            return;

        self.current_token = self.tokens.items[self.current_token_index];
    }

    /// Check whether the `token` has the type of the current token and skip it if it's possible.
    /// If it doesn't, it throws an error.
    fn eat(self: *Self, token: lexer.Token) void {
        if (@as(i32, @intFromEnum(self.current_token)) == @as(i32, @intFromEnum(token))) {
            self.current_token_index += 1;
            if (self.current_token_index < self.tokens.items.len)
                self.current_token = self.tokens.items[self.current_token_index];
        } else {
            std.log.err("invalid token expected \"{}\" but receive \"{}\"", .{ token, self.current_token });
            std.process.exit(1);
        }
    }

    fn parse_statements(self: *Self) !std.ArrayList(Statement) {
        var statements = std.ArrayList(Statement).init(self.allocator);
        while (!self.check(.eof)) {
            if (try self.parse_statement()) |stmt|
                try statements.append(stmt);
        }
        return statements;
    }

    /// This function dispatches statement parsing
    fn parse_statement(self: *Self) !?Statement {
        return switch (self.current_token) {
            .kw_const, .kw_let => self.parse_var_declaration(),
            .kw_function => try self.parse_function(),
            .kw_export => blk: {
                self.eat(.kw_export);
                if (try self.parse_statement()) |stmt| {
                    break :blk .{ .exported = &stmt };
                } else break :blk error.InvalidExport;
            },
            .semi_colon => blk: {
                self.eat(.semi_colon);
                break :blk null;
            },
            else => blk: {
                const expr = self.parse_expression();
                self.eat(.semi_colon);
                break :blk .{ .expression = expr };
            },
        };
    }

    /// Parse statement as follows `func fonction_name(args) {code;}`
    fn parse_function(self: *Self) anyerror!Statement {
        self.skip();
        const name = switch (self.current_token) {
            .ident => |name| name,
            else => {
                std.log.err("invalid token expected an identifier but receive \"{}\"", .{self.current_token});
                std.process.exit(1);
            },
        };
        self.skip();
        self.eat(.lparent);
        var args = std.ArrayList([]const u8).init(self.allocator);
        while (!self.check(.rparent)) {
            const arg = switch (self.current_token) {
                .ident => |arg_name| arg_name,
                else => {
                    std.log.err("invalid token expected an identifier but receive \"{}\"", .{self.current_token});
                    std.process.exit(1);
                },
            };
            try args.append(arg);
            self.skip();
            if (!self.check(.comma))
                break self.eat(.comma);
        }
        self.eat(.rparent);

        self.eat(.lbrace);
        var statements = std.ArrayList(Statement).init(self.allocator);
        while (!self.check(.rparent)) {
            if (try self.parse_statement()) |stmt| {
                try statements.append(stmt);
            } else break;
        }
        self.eat(.rbrace);

        return .{ .function_declaration = .{
            .name = name,
            .args = args,
            .statements = statements,
        } };
    }

    /// Parse statement as follows `const|let var_name = value;`
    fn parse_var_declaration(self: *Self) Statement {
        const is_constant = self.check(.kw_const);
        self.skip();
        const name = switch (self.current_token) {
            .ident => |name| name,
            else => {
                std.log.err("invalid token expected an identifier but receive \"{}\"", .{self.current_token});
                std.process.exit(1);
            },
        };
        self.skip();
        self.eat(.eq);
        const value = self.parse_expression();
        self.eat(.semi_colon);

        return .{ .variable_declaration = .{ .name = name, .value = value, .constant = is_constant } };
    }

    fn parse_expression(self: *Self) Expr {
        return self.parse_assign();
    }

    /// Parse primitve value like number, string or more
    fn parse_primitive(self: *Self) Expr {
        return switch (self.current_token) {
            .ident => |ident| blk: {
                self.skip();
                break :blk .{ .ident = ident };
            },
            .string => |string| blk: {
                self.skip();
                break :blk .{ .string_litteral = string };
            },
            .symbol => |symbol| blk: {
                self.skip();
                const is_unique = std.mem.startsWith(u8, symbol, "@@");
                break :blk .{ .symbol_litteral = .{ .name = symbol[(if (is_unique) 2 else 1)..], .strictly_unique = is_unique } };
            },
            .char => |char| blk: {
                self.skip();
                break :blk .{ .char_litteral = char };
            },
            .kw_true, .kw_false => blk: {
                const is_true = self.check(.kw_true);
                self.skip();
                break :blk .{ .bool_litteral = is_true };
            },
            .number => |number| blk: {
                self.skip();
                for (number) |c| {
                    if (c == '.') break :blk .{ .float_litteral = number };
                }
                break :blk .{ .int_litteral = number };
            },
            .kw_null => blk: {
                self.skip();
                break :blk .null_litteral;
            },
            .lparent => blk: {
                self.skip();
                const expr = self.parse_expression();
                self.eat(.rparent);
                break :blk .{ .parent = &expr };
            },
            else => {
                unreachable;
            },
        };
    }

    /// Parse expression like `+1` or `-1`
    fn parse_unary(self: *Self) Expr {
        if (self.check(lexer.Token.plus) or self.check(lexer.Token.minus)) {
            const op = UnaryOp.from_token(self.current_token);
            self.skip();
            const right = self.parse_unary();

            return .{ .unary_operation = .{ .right = &right, .kind = op } };
        }

        return self.parse_primitive();
    }

    /// Parse expression like `2 * 3`
    fn parse_factor(self: *Self) Expr {
        var left = self.parse_unary();
        while (self.check(lexer.Token.star) or self.check(lexer.Token.div) or self.check(lexer.Token.pow)) {
            const op = BinaryOp.from_token(self.current_token);
            self.skip();
            const right = self.parse_unary();
            left = .{ .binary_operation = .{ .left = &left, .right = &right, .kind = op } };
        }
        return left;
    }

    /// Parse expression like `2 + 3`
    fn parse_term(self: *Self) Expr {
        var left = self.parse_factor();
        while (self.check(lexer.Token.plus) or self.check(lexer.Token.minus)) {
            const op = BinaryOp.from_token(self.current_token);
            self.skip();
            const right = self.parse_factor();
            left = .{ .binary_operation = .{ .left = &left, .right = &right, .kind = op } };
        }
        return left;
    }

    fn parse_condition(self: *Self) Expr {
        var left = self.parse_term();
        const token_int = @intFromEnum(self.current_token);
        while (token_int >= @intFromEnum(lexer.Token.eqs) and token_int <= @intFromEnum(lexer.Token.lt)) {
            const op = BinaryOp.from_token(self.current_token);
            self.skip();
            const right = self.parse_term();
            left = .{ .binary_operation = .{ .left = &left, .right = &right, .kind = op } };
        }
        return left;
    }

    /// Parse expression like `a = 0` or `a += b`
    fn parse_assign(self: *Self) Expr {
        var left = self.parse_condition();
        const token_int = @intFromEnum(self.current_token);
        while (token_int >= @intFromEnum(lexer.Token.eq) and token_int <= @intFromEnum(lexer.Token.conditional_eq)) {
            const op = BinaryOp.from_token(self.current_token);
            self.skip();
            const right = self.parse_condition();
            left = .{ .binary_operation = .{ .left = &left, .right = &right, .kind = op } };
        }
        return left;
    }
};

fn parse(source: lexer.Source, allocator: std.mem.Allocator) !std.ArrayList(Statement) {
    var lex = lexer.Lexer.init(source);
    var tokens = std.ArrayList(lexer.Token).init(allocator);
    defer tokens.deinit();

    while (lex.has_tokens()) {
        try tokens.append(try lex.next_token());
    }
    try tokens.append(.eof);

    return Parser.parse(allocator, &tokens);
}

const expectEqualDeep = std.testing.expectEqualDeep;
test "parser parse variable declaration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const input = "let a = 0;";
    const result = try parse(.{ .buffer = input, .file_name = "test_file" }, allocator);
    defer result.deinit();

    try expectEqualDeep(.{.{ .variable_declaration = .{ .constant = false, .name = "a", .value = .{ .int_litteral = "0" } } }}, result.items);
}

test "parser parse constant declaration" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const input = "const a = 0;";
    const result = try parse(.{ .buffer = input, .file_name = "test_file" }, allocator);
    defer result.deinit();

    try expectEqualDeep(.{.{ .variable_declaration = .{ .constant = true, .name = "a", .value = .{ .int_litteral = "0" } } }}, result.items);
}

test "parser parse expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const input = "1 + 1 * -1;";
    const result = try parse(.{ .buffer = input, .file_name = "test_file" }, allocator);
    defer result.deinit();

    try expectEqualDeep(.{.{ .expression = .{ .binary_operation = .{ .kind = .add, .left = &.{ .int_litteral = "1" }, .right = &.{ .binary_operation = .{ .kind = .mult, .left = &.{ .int_litteral = "1" }, .right = .{ .unary_operation = .{ .right = &.{ .int_litteral = "1" }, .kind = .negate } } } } } } }}, result.items);
}

test "parser parse null" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const input = "null;";
    const result = try parse(.{ .buffer = input, .file_name = "test_file" }, allocator);
    defer result.deinit();

    try expectEqualDeep(.{.{ .expression = .null_litteral }}, result.items);
}

// FIXME error: expected type 'struct{struct{function_declaration: struct{comptime name: *const [1:0]u8 = "a", args: array_list.ArrayListAligned([]const u8,null), statements: array_list.ArrayListAligned(parser.Statement,null)}}}', found '[]parser.Statement'
// test "parser parse function declaration" {
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     const allocator = arena.allocator();
//     defer arena.deinit();

//     const input = "func a(){}";
//     const result = try parse(.{ .buffer = input, .file_name = "test_file" }, allocator);
//     defer result.deinit();
//     try expectEqualDeep(.{.{
//         .function_declaration = .{
//             .name = "a",
//             .args = std.ArrayList([]const u8).fromOwnedSlice(allocator, &.{}),
//             .statements = std.ArrayList(Statement).fromOwnedSlice(allocator, &.{}),
//         },
//     }}, result.items);
// }
