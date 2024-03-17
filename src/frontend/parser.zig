const std = @import("std");
const Bin = @import("../utils/bin.zig").Bin;

const ast = @import("./ast.zig");
const Token = @import("./token.zig").Token;
const lexer = @import("./lexer.zig");

const Error = @import("../errors.zig").Error;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: *const std.ArrayList(Token),

    current_token: Token,
    current_token_index: usize = 0,

    const Self = @This();
    pub fn parse(allocator: std.mem.Allocator, tokens: *const std.ArrayList(Token)) Error!std.ArrayList(ast.Statement) {
        var parser = Parser{ .allocator = allocator, .tokens = tokens, .current_token = tokens.items[0] };

        return parser.parse_statements();
    }

    /// Test if the current token if it's possible
    inline fn check(self: *Self, token: Token) bool {
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
    fn eat(self: *Self, token: Token) void {
        if (@as(i32, @intFromEnum(self.current_token)) == @as(i32, @intFromEnum(token))) {
            self.current_token_index += 1;
            if (self.current_token_index < self.tokens.items.len)
                self.current_token = self.tokens.items[self.current_token_index];
        } else {
            std.log.err("invalid token expected \"{}\" but receive \"{}\"", .{ token, self.current_token });
            std.process.exit(1);
        }
    }

    fn parse_statements(self: *Self) Error!std.ArrayList(ast.Statement) {
        var statements = std.ArrayList(ast.Statement).init(self.allocator);
        while (!self.check(.eof)) {
            if (try self.parse_statement(null)) |stmt|
                statements.append(stmt) catch return Error.AllocationOutOfMemory;
        }
        return statements;
    }

    /// This function dispatches statement parsing
    fn parse_statement(self: *Self, previous_attributes: ?std.ArrayList(ast.Statement.Attribute)) Error!?ast.Statement {
        const attributes = previous_attributes orelse try self.parse_attributes();

        return switch (self.current_token) {
            .kw_const, .kw_let => try self.parse_var_declaration(attributes),
            .kw_function => try self.parse_function(attributes),
            .kw_type => try self.parse_type(attributes),
            .kw_export => blk: {
                self.eat(.kw_export);
                if (try self.parse_statement(attributes)) |stmt| {
                    break :blk .{ .exported = try Bin(ast.Statement).init(self.allocator, stmt) };
                } else break :blk Error.InvalidExport;
            },
            .semi_colon => blk: {
                self.eat(.semi_colon);
                break :blk null;
            },
            else => blk: {
                const expr = try self.parse_expression();
                self.eat(.semi_colon);
                break :blk .{ .expression = expr };
            },
        };
    }

    /// Parse statement as follows `func fonction_name(args: type): type {code;}`
    fn parse_function(self: *Self, attributes: std.ArrayList(ast.Statement.Attribute)) Error!ast.Statement {
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
        var args = std.ArrayList(ast.Statement.Arg).init(self.allocator);
        while (!self.check(.rparent)) {
            const arg_name = try self.parse_primitive();
            var type_expr: ?ast.Expr = null;
            if (self.check(.colon)) {
                self.eat(.colon);
                type_expr = try self.parse_expression();
            }
            args.append(.{ .name = arg_name, .type = type_expr }) catch return Error.AllocationOutOfMemory;

            if (!self.check(.comma))
                break self.eat(.comma);
        }
        self.eat(.rparent);

        var type_expr: ?ast.Expr = null;
        if (self.check(.colon)) {
            self.eat(.colon);
            type_expr = try self.parse_expression();
        }

        self.eat(.lbrace);
        var statements = std.ArrayList(ast.Statement).init(self.allocator);
        while (!self.check(.rbrace)) {
            if (try self.parse_statement(null)) |stmt| {
                statements.append(stmt) catch return Error.AllocationOutOfMemory;
            } else break;
        }
        self.eat(.rbrace);

        return .{ .function_declaration = .{
            .attributes = attributes,
            .name = name,
            .type = type_expr,
            .args = args,
            .statements = statements,
        } };
    }

    /// Parse statement as follows `const|let var_name: type = value;`
    fn parse_var_declaration(self: *Self, attributes: std.ArrayList(ast.Statement.Attribute)) Error!ast.Statement {
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
        var type_expr: ?ast.Expr = null;
        if (self.check(.colon)) {
            self.eat(.colon);
            type_expr = try self.parse_expression();
        }
        self.eat(.eq);
        const value = try self.parse_expression();
        self.eat(.semi_colon);

        return .{ .variable_declaration = .{
            .attributes = attributes,
            .name = name,
            .type = type_expr,
            .value = value,
            .constant = is_constant,
        } };
    }

    /// Parse statement as follows `type type_name = value;`
    fn parse_type(self: *Self, attributes: std.ArrayList(ast.Statement.Attribute)) Error!ast.Statement {
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
        const value = try self.parse_expression();
        self.eat(.semi_colon);

        return .{ .type_definition = .{
            .attributes = attributes,
            .name = name,
            .value = value,
        } };
    }

    fn parse_attributes(self: *Self) Error!std.ArrayList(ast.Statement.Attribute) {
        var attributes = std.ArrayList(ast.Statement.Attribute).init(self.allocator);

        while (self.check(.hash_bracket)) {
            self.eat(.hash_bracket);
            const name = switch (self.current_token) {
                .ident => |name| name,
                else => {
                    std.log.err("invalid token expected an identifier but receive \"{}\"", .{self.current_token});
                    std.process.exit(1);
                },
            };
            attributes.append(.{ .name = name }) catch return Error.AllocationOutOfMemory;
            self.eat(.rbracket);
        }

        return attributes;
    }

    fn parse_expression(self: *Self) Error!ast.Expr {
        return self.parse_assign();
    }

    /// Parse primitve value like number, string or more
    fn parse_primitive(self: *Self) Error!ast.Expr {
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
                break :blk .{ .symbol_litteral = .{ .name = symbol, .strictly_unique = is_unique } };
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
                const expr = try self.parse_expression();
                self.eat(.rparent);
                break :blk .{ .parent = try Bin(ast.Expr).init(self.allocator, expr) };
            },
            .question => blk: {
                self.skip();
                break :blk self.parse_expression();
            },
            else => unreachable,
        };
    }

    /// Parse expression like `+1` or `-1`
    fn parse_unary(self: *Self) Error!ast.Expr {
        if (self.check(Token.plus) or self.check(Token.minus)) {
            const op = ast.UnaryOp.from_token(self.current_token);
            self.skip();
            const right = try self.parse_unary();

            return .{ .unary_operation = .{ .right = try Bin(ast.Expr).init(self.allocator, right), .kind = op } };
        }

        return self.parse_primitive();
    }

    /// Parse expression like `2 * 3`
    fn parse_factor(self: *Self) Error!ast.Expr {
        var left = try self.parse_unary();
        while (self.check(Token.star) or self.check(Token.div) or self.check(Token.pow)) {
            const op = ast.BinaryOp.from_token(self.current_token);
            self.skip();
            const right = try self.parse_unary();
            left = .{ .binary_operation = .{
                .left = try Bin(ast.Expr).init(self.allocator, left),
                .right = try Bin(ast.Expr).init(self.allocator, right),
                .kind = op,
            } };
        }
        return left;
    }

    /// Parse expression like `2 + 3`
    fn parse_term(self: *Self) Error!ast.Expr {
        var left = try self.parse_factor();
        while (self.check(Token.plus) or self.check(Token.minus)) {
            const op = ast.BinaryOp.from_token(self.current_token);
            self.skip();
            const right = try self.parse_factor();
            left = .{ .binary_operation = .{
                .left = try Bin(ast.Expr).init(self.allocator, left),
                .right = try Bin(ast.Expr).init(self.allocator, right),
                .kind = op,
            } };
        }
        return left;
    }

    fn parse_condition(self: *Self) Error!ast.Expr {
        var left = try self.parse_term();
        const token_int = @intFromEnum(self.current_token);
        while (token_int >= @intFromEnum(Token.eqs) and token_int <= @intFromEnum(Token.lt)) {
            const op = ast.BinaryOp.from_token(self.current_token);
            self.skip();
            const right = try self.parse_term();
            left = .{ .binary_operation = .{
                .left = try Bin(ast.Expr).init(self.allocator, left),
                .right = try Bin(ast.Expr).init(self.allocator, right),
                .kind = op,
            } };
        }
        return left;
    }

    /// Parse expression like `a = 0` or `a += b`
    fn parse_assign(self: *Self) Error!ast.Expr {
        var left = try self.parse_condition();
        const token_int = @intFromEnum(self.current_token);
        while (token_int >= @intFromEnum(Token.eq) and token_int <= @intFromEnum(Token.conditional_eq)) {
            const op = ast.BinaryOp.from_token(self.current_token);
            self.skip();
            const right = try self.parse_condition();
            left = .{ .binary_operation = .{
                .left = try Bin(ast.Expr).init(self.allocator, left),
                .right = try Bin(ast.Expr).init(self.allocator, right),
                .kind = op,
            } };
        }
        return left;
    }
};

// FIXME: an error from the std (for the moment I'm checking that the tests don't leak memory)
// $HOME/zig/lib/std/testing.zig:745:52: error: unable to resolve inferred error set
//                         else => try expectEqualDeep(expected.*, actual.*),

fn parse(source: lexer.Source, allocator: std.mem.Allocator) !std.ArrayList(ast.Statement) {
    var lex = lexer.Lexer.init(source);
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    while (lex.has_tokens()) {
        try tokens.append(try lex.next_token());
    }
    try tokens.append(.eof);

    return Parser.parse(allocator, &tokens);
}

const expectEqualDeep = std.testing.expectEqualDeep;
test "parser parse variable declaration" {
    const allocator = std.testing.allocator;
    const input = "let a = 0;";
    const result = try parse(.{ .buffer = input, .file_name = "test_file" }, allocator);
    defer {
        for (result.items) |s| {
            s.deinit();
        }
        result.deinit();
    }

    // const expected: ast.Statement = .{
    //     .variable_declaration = .{
    //         .constant = false,
    //         .name = "a",
    //         .type = null,
    //         .value = .{ .int_litteral = "0" }
    //     }
    // };
    // try expectEqualDeep(expected, result.items[0]);
}

test "parser parse constant declaration" {
    const allocator = std.testing.allocator;
    const input = "const a = 0;";
    const result = try parse(.{ .buffer = input, .file_name = "test_file" }, allocator);
    defer {
        for (result.items) |s| {
            s.deinit();
        }
        result.deinit();
    }

    // const expected: ast.Statement = .{
    //     .variable_declaration = .{
    //         .constant = true,
    //         .name = "a",
    //         .type = null,
    //         .value = .{ .int_litteral = "0" }
    //     }
    // };
    // try expectEqualDeep(expected, result.items[0]);
}

test "parser parse expression" {
    const allocator = std.testing.allocator;
    const input = "1 + 1 * -1;";
    const result = try parse(.{ .buffer = input, .file_name = "test_file" }, allocator);
    defer {
        for (result.items) |s| {
            s.deinit();
        }
        result.deinit();
    }

    // const expected: ast.Statement = .{
    //     .expression = .{
    //         .binary_operation = .{
    //             .kind = .add,
    //             .left = try Bin(ast.Expr).init(allocator, .{ .int_litteral = "1" }),
    //             .right = try Bin(ast.Expr).init(allocator, .{
    //                 .binary_operation = .{
    //                     .kind = .mult,
    //                     .left = try Bin(ast.Expr).init(allocator, .{ .int_litteral = "1" }),
    //                     .right = try Bin(ast.Expr).init(allocator, .{
    //                         .unary_operation = .{
    //                             .right = try Bin(ast.Expr).init(allocator, .{ .int_litteral = "1" }),
    //                             .kind = .negate
    //                         }
    //                     })
    //                 }
    //             })
    //         }
    //     }
    // };
    // try expectEqualDeep(expected, result.items[0]);
}

test "parser parse null" {
    const allocator = std.testing.allocator;
    const input = "null;";
    var result = try parse(.{ .buffer = input, .file_name = "test_file" }, allocator);
    defer {
        for (result.items) |s| {
            s.deinit();
        }
        result.deinit();
    }

    // const expected: ast.Statement = .{ .expression = .null_litteral };
    // try expectEqualDeep(expected, result.items[0]);
}

test "parser parse function declaration" {
    const allocator = std.testing.allocator;
    const input = "func a(){}";
    const result = try parse(.{ .buffer = input, .file_name = "test_file" }, allocator);
    defer {
        for (result.items) |s| {
            s.deinit();
        }
        result.deinit();
    }

    // const expected: ast.Statement = .{
    //     .function_declaration = .{
    //         .name = "a",
    //         .args = std.ArrayList(ast.Statement.Arg).fromOwnedSlice(allocator, &.{}),
    //         .type = null,
    //         .statements = std.ArrayList(ast.Statement).fromOwnedSlice(allocator, &.{}),
    //     },
    // };
    // try expectEqualDeep(expected, result.items[0]);
}
