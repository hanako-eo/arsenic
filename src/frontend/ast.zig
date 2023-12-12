const std = @import("std");
const InvariantBin = @import("../utils/bin.zig").InvariantBin;

const Token = @import("./token.zig").Token;
const lexer = @import("./lexer.zig");

pub const Statement = union(enum) {
    pub const VarDeclaration = struct {
        constant: bool,
        name: Expr,
        type: ?Expr,
        value: Expr,
    };
    pub const FnDeclaration = struct {
        name: []const u8,
        args: std.ArrayList(Arg),
        type: ?Expr,
        statements: std.ArrayList(Statement),
    };
    pub const Arg = struct {
        name: Expr,
        type: ?Expr
    };

    exported: InvariantBin(Statement),
    expression: Expr,
    variable_declaration: VarDeclaration,
    function_declaration: FnDeclaration,
};

pub const Expr = union(enum) {
    pub const Symbol = struct { name: []const u8, strictly_unique: bool };

    block: std.ArrayList(Statement),
    parent: InvariantBin(Expr),

    optional: InvariantBin(Expr),

    unary_operation: struct { right: InvariantBin(Expr), kind: UnaryOp },
    binary_operation: struct { left: InvariantBin(Expr), right: InvariantBin(Expr), kind: BinaryOp },

    ident: []const u8,
    char_litteral: []const u8,
    string_litteral: []const u8,
    symbol_litteral: Symbol,
    float_litteral: []const u8,
    int_litteral: []const u8,
    bool_litteral: bool,
    null_litteral,
};

pub const UnaryOp = enum {
    none,

    plus,
    negate,

    const Self = @This();
    pub fn from_token(token: Token) Self {
        return switch (token) {
            .plus => .plus,
            .minus => .negate,
            else => .none,
        };
    }
};

pub const BinaryOp = enum {
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
    pub fn from_token(token: Token) Self {
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
