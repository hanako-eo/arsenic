const std = @import("std");
const Bin = @import("../utils/bin.zig").Bin;
const Rc = @import("../utils/rc.zig").Rc;

const ast = @import("../frontend/ast.zig");
const Context = @import("../context.zig");
const Error = @import("../errors.zig").Error;

pub const AttribuableStatements = enum(u8) {
    variable = 0b1,
    function = 0b10,
    type = 0b100,
};

pub const Attribute = enum {
    global,
    primitive,
};

pub const defined_attributes = std.ComptimeStringMap(struct { @"0": AttribuableStatements, @"1": Attribute }, .{
    .{ "global", .{ .type, .global } },
    .{ "primitive", .{ .type, .primitive } },
});

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
        context: Context,
        exported: bool,
    };
    pub const Arg = struct { name: Expr, type: Type };

    expression: Expr,
    variable_declaration: Rc(VarDeclaration),
    function_declaration: Rc(FnDeclaration),
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

    ident: struct { value: []const u8, type: Type },
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
    pub fn to_primitive(name: []const u8) ?Type {
        return if (std.mem.eql(u8, name, "void")) .void_litteral
            else if (std.mem.eql(u8, name, "null")) .null_litteral
            else if (std.mem.eql(u8, name, "unknown")) .unknown
            else if (std.mem.eql(u8, name, "int")) .int
            else if (std.mem.eql(u8, name, "float")) .float
            else if (std.mem.eql(u8, name, "char")) .char
            else if (std.mem.eql(u8, name, "string")) .string
            else if (std.mem.eql(u8, name, "symbol")) .symbol
            else if (std.mem.eql(u8, name, "bool")) .boolean
            else null;
    }

    pub fn from_primitive(self: Self) ?[]const u8 {
        return switch (self) {
            .void_litteral => "void",
            .null_litteral => "null",
            .unknown => "unknown",
            .int => "int",
            .float => "float",
            .char => "char",
            .string => "string",
            .symbol => "symbol",
            .boolean => "bool",
            else => null,
        };
    }

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

    pub fn is_surtype_of(self: *const Self, context: *Context, subtype: *const Self) Error!bool {
        return switch (self.*) {
            .ident => |type_name| blk: {
                const type_ = try context.get_type(type_name);
                break :blk switch (subtype.*) {
                    .ident => |subtype_name| type_.is_surtype_of(context, &try context.get_type(subtype_name)),
                    else => type_.is_surtype_of(context, subtype),
                };
            },
            .optional => |option| switch (subtype.*) {
                .optional => |subtype_optional| option.ptr.is_surtype_of(context, subtype_optional.ptr),
                .null_litteral => true,
                else => option.ptr.is_surtype_of(context, subtype),
            },
            .func => |func| switch (subtype.*) {
                .func => |subtype_func| blk: {
                    if (func.args.items.len != subtype_func.args.items.len)
                        break :blk false;

                    for (func.args.items, subtype_func.args.items) |arg, subtype_arg| {
                        if (!try arg.type.is_surtype_of(context, &subtype_arg.type))
                            break :blk false;
                    }

                    break :blk true;
                },
                else => false,
            },
            .none => false,
            else => switch (subtype.*) {
                .optional, .func, .none => false,
                .ident => |subtype_name| subtype.is_surtype_of(context, &try context.get_type(subtype_name)),
                else => @intFromEnum(self.*) == @intFromEnum(subtype.*),
            },
        };
    }
};
