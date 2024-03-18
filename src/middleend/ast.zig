const std = @import("std");
const Bin = @import("../utils/bin.zig").Bin;

const ast = @import("../frontend/ast.zig");
const context = @import("../context.zig");

pub const Attribute = enum {
    global,
};

pub const Statement = union(enum) {
    pub const VarDeclaration = struct {
        attributes: std.ArrayList(Attribute),
        constant: bool,
        name: []const u8,
        type: Type,
        value: Expr,
        exported: bool,
    };
    pub const FnDeclaration = struct {
        attributes: std.ArrayList(Attribute),
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
    type_definition: TypeDefinition,

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

    unary_operation: struct { right: Bin(Expr), kind: ast.UnaryOp, type: Type },
    binary_operation: struct { left: Bin(Expr), right: Bin(Expr), kind: ast.BinaryOp, type: Type },

    ident: []const u8,
    char_litteral: struct { value: []const u8, type: Type },
    string_litteral: struct { value: []const u8, type: Type },
    symbol_litteral: struct { value: ast.Expr.Symbol, type: Type },
    float_litteral: struct { value: []const u8, type: Type },
    int_litteral: struct { value: []const u8, type: Type },
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

pub const TypeDefinition = struct {
    attributes: std.ArrayList(Attribute),
    name: []const u8,
    value: Type,
    exported: bool,
};
pub const Type = union(enum) {
    pub const FnDefinition = struct {
        args: std.ArrayList(Statement.Arg),
        type: Bin(Type),
    };

    optional: Bin(Type),
    ident: []const u8,
    func: FnDefinition,

    int,
    float,
    char,
    string,
    symbol,
    boolean,

    void_litteral,
    null_litteral,
    unknown,
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
