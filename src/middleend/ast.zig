const std = @import("std");
const Bin = @import("../utils/bin.zig").Bin;

const ast = @import("../frontend/ast.zig");
const context = @import("../context.zig");

pub const Statement = union(enum) {
    pub const VarDeclaration = struct {
        constant: bool,
        name: []const u8,
        type: Type,
        value: Expr,
        exported: bool,
    };
    pub const FnDeclaration = struct {
        name: []const u8,
        args: std.ArrayList(Arg),
        type: Type,
        statements: std.ArrayList(Statement),
        context: context.Context,
        exported: bool,
    };
    pub const Arg = struct { name: Expr, type: Type };

    expression: Expr,
    variable_declaration: VarDeclaration,
    function_declaration: FnDeclaration,

    const Self = @This();
    pub fn deinit(self: Self) void {
        switch (self) {
            .function_declaration => |f| {
                for (f.args.items) |arg| {
                    arg.name.deinit();
                    arg.type.deinit();
                }
                f.type.deinit();
                for (f.statements.items) |s| {
                    s.deinit();
                }
            },
            .variable_declaration => |v| {
                v.type.deinit();
                v.value.deinit();
            },
            .exported => |s| s.deinit(),
            .expression => |e| e.deinit(),
        }
    }
};

pub const Expr = union(enum) {
    // TODO: add context
    block: std.ArrayList(Statement),
    parent: Bin(Expr),

    unary_operation: struct { right: Bin(Expr), kind: ast.UnaryOp },
    binary_operation: struct { left: Bin(Expr), right: Bin(Expr), kind: ast.BinaryOp },

    ident: []const u8,
    char_litteral: []const u8,
    string_litteral: []const u8,
    symbol_litteral: ast.Expr.Symbol,
    float_litteral: []const u8,
    int_litteral: []const u8,
    bool_litteral: bool,
    null_litteral,

    const Self = @This();
    pub fn deinit(self: Self) void {
        switch (self) {
            .unary_operation => |u| {
                u.right.deinit();
            },
            .binary_operation => |b| {
                b.left.deinit();
                b.right.deinit();
            },
            .block => |b| {
                for (b.items) |s| {
                    s.deinit();
                }
            },
            .parent => |e| e.deinit(),
            else => {},
        }
    }
};

pub const Type = union(enum) {
    pub const FnDefinition = struct {
        name: []const u8,
        args: std.ArrayList(Statement.Arg),
        type: Bin(Type),
    };

    optional: Bin(Type),
    ident: []const u8,
    func: FnDefinition,
    void_litteral,
    none,

    // TODO litterals, generics, tuples and more

    const Self = @This();
    pub fn deinit(self: Self) void {
        switch (self) {
            .func => |f| {
                for (f.args.items) |arg| {
                    arg.name.deinit();
                    arg.type.deinit();
                }
                f.type.deinit();
            },
            .optional => |e| e.deinit(),
            else => {},
        }
    }
};
