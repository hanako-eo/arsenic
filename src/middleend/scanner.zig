const std = @import("std");
const Bin = @import("../utils/bin.zig").Bin;
const Rc = @import("../utils/rc.zig").Rc;
const front_ast = @import("../frontend/ast.zig");

const ast = @import("./ast.zig");
const Context = @import("../context.zig");
const Error = @import("../errors.zig").Error;

allocator: std.mem.Allocator,
context: *Context,

const Self = @This();
pub fn scan(allocator: std.mem.Allocator, context: *Context, statements: *const std.ArrayList(front_ast.Statement)) Error!std.ArrayList(ast.Statement) {
    var scanner = Self{
        .allocator = allocator,
        .context = context,
    };

    return scanner.scan_statements(statements);
}

fn scan_statements(self: *Self, statements: *const std.ArrayList(front_ast.Statement)) Error!std.ArrayList(ast.Statement) {
    var instructions = std.ArrayList(ast.Statement).initCapacity(self.allocator, statements.items.len) catch return Error.AllocationOutOfMemory;
    errdefer instructions.deinit();

    for (statements.items) |*statement| {
        instructions.appendAssumeCapacity(try self.scan_statement(statement, false));
    }

    return instructions;
}

fn scan_statement(self: *Self, statement: *const front_ast.Statement, exported: bool) Error!ast.Statement {
    return switch (statement.*) {
        .exported => |*exported_stmt| blk: {
            if (!self.context.compare_kind(.module) and !self.context.compare_kind(.global))
                break :blk Error.NonModuleExport;

            break :blk switch (exported_stmt.ptr.*) {
                .exported, .expression => Error.InvalidExport,
                else => try self.scan_statement(exported_stmt.ptr, true),
            };
        },
        .expression => |*expr| .{ .expression = try self.scan_expression(expr) },
        .variable_declaration => |*variable| blk: {
            const scanned_var = try Rc(ast.Statement.VarDeclaration).init(self.allocator, try self.scan_variable(variable, exported));
            try self.context.declare_runtime(.{ .variable = scanned_var });

            break :blk .{ .variable_declaration = scanned_var };
        },
        .function_declaration => |*function| blk: {
            const scanned_fn = try self.scan_function(function, exported);
            try self.context.declare_type(ast.TypeDefinition{
                .attributes = scanned_fn.attributes.clone() catch return Error.AllocationOutOfMemory,
                .exported = false,
                .name = scanned_fn.name,
                .value = .{ .func = .{
                    .args = scanned_fn.args.clone() catch return Error.AllocationOutOfMemory,
                    .type = try Bin(ast.Type).init(self.allocator, scanned_fn.type),
                } },
            });

            const function_declaration = try Rc(ast.Statement.FnDeclaration).init(self.allocator, scanned_fn);
            try self.context.declare_runtime(.{ .function = function_declaration });

            break :blk .{ .function_declaration = function_declaration };
        },
        .type_definition => |*type_| blk: {
            const scanned_definition = try self.scan_type_definition(type_, exported);
            try self.context.declare_type(scanned_definition);

            break :blk .{ .type_definition = scanned_definition };
        },
    };
}

fn scan_variable(self: *Self, variable: *const front_ast.Statement.VarDeclaration, exported: bool) Error!ast.Statement.VarDeclaration {
    return .{
        .attributes = try self.scan_attributes(.variable, &variable.attributes),
        .constant = variable.constant,
        .name = variable.name,
        .type = if (variable.type) |return_type|
            try self.scan_type(&return_type)
        else
            .none,
        .value = try self.scan_expression(&variable.value),
        .exported = exported,
    };
}

fn scan_function(self: *Self, function: *const front_ast.Statement.FnDeclaration, exported: bool) Error!ast.Statement.FnDeclaration {
    const parent_context = self.context;
    defer self.context = parent_context;

    var function_context = Context.init(self.allocator, .function, parent_context, parent_context.global);
    self.context = &function_context;

    return .{
        .attributes = try self.scan_attributes(.function, &function.attributes),
        .name = function.name,
        .args = try self.scan_fn_args(&function.args),
        .type = if (function.type) |*return_type| try self.scan_type(return_type) else .void_litteral,
        .statements = try self.scan_statements(&function.statements),
        .context = function_context,
        .exported = exported,
    };
}

fn scan_fn_args(self: *Self, args: *const std.ArrayList(front_ast.Statement.Arg)) Error!std.ArrayList(ast.Statement.Arg) {
    var scanned_args = std.ArrayList(ast.Statement.Arg).initCapacity(self.allocator, args.items.len) catch return Error.AllocationOutOfMemory;
    errdefer scanned_args.deinit();

    for (args.items) |*arg| {
        scanned_args.appendAssumeCapacity(.{
            .name = try self.scan_expression(&arg.name),
            .type = if (arg.type) |return_type|
                try self.scan_type(&return_type)
            else
                .none,
        });
    }

    return scanned_args;
}

fn scan_type_definition(self: *Self, type_: *const front_ast.Statement.TypeDefinition, exported: bool) Error!ast.TypeDefinition {
    return .{
        .attributes = try self.scan_attributes(.type, &type_.attributes),
        .name = type_.name,
        .value = try self.scan_type(&type_.value),
        .exported = exported,
    };
}

fn scan_attributes(self: *Self, attr_kind: ast.AttribuableStatements, attrs: *const std.ArrayList(front_ast.Statement.Attribute)) Error!std.ArrayList(ast.Attribute) {
    var attributes: std.ArrayList(ast.Attribute) = std.ArrayList(ast.Attribute).initCapacity(self.allocator, attrs.capacity) catch return Error.AllocationOutOfMemory;
    errdefer attributes.deinit();

    for (attrs.items) |attribute| {
        if (ast.defined_attributes.get(attribute.name)) |autorised_kind| {
            if (@intFromEnum(autorised_kind.@"0") & @intFromEnum(attr_kind) == 0)
                return Error.IncorrectAttribute;

            attributes.appendAssumeCapacity(autorised_kind.@"1");
        } else return Error.AttributeDoesNotExist;
    }

    return attributes;
}

fn scan_expression(self: *Self, expr: *const front_ast.Expr) Error!ast.Expr {
    return switch (expr.*) {
        .optional => Error.InvalidOptionExpression,
        .block => |statements| .{ .block = try self.scan_statements(&statements) },
        .parent => |p_expr| .{
            .parent = try Bin(ast.Expr).init(self.allocator, try self.scan_expression(p_expr.ptr)),
        },
        .unary_operation => |unary| .{ .unary_operation = .{
            .right = try Bin(ast.Expr).init(self.allocator, try self.scan_expression(unary.right.ptr)),
            .kind = unary.kind,
            .type = .none,
        } },
        .binary_operation => |binary| .{ .binary_operation = .{
            .left = try Bin(ast.Expr).init(self.allocator, try self.scan_expression(binary.left.ptr)),
            .right = try Bin(ast.Expr).init(self.allocator, try self.scan_expression(binary.right.ptr)),
            .kind = binary.kind,
            .type = .none,
        } },

        .ident => |value| blk: {
            if (!self.context.has_at_runtime(value))
                return Error.UndefinedVariable;

            break :blk .{ .ident = .{ .value = value, .type = .none } };
        },
        .char_litteral => |value| .{ .char_litteral = value },
        .string_litteral => |value| .{ .string_litteral = value },
        .symbol_litteral => |value| .{ .symbol_litteral = value },
        .float_litteral => |value| .{ .float_litteral = value },
        .int_litteral => |value| .{ .int_litteral = value },
        .bool_litteral => |value| .{ .bool_litteral = value },
        .null_litteral => .null_litteral,
    };
}

fn scan_type(self: *Self, expr: *const front_ast.Expr) Error!ast.Type {
    return switch (expr.*) {
        .optional => |optional_expr| switch (optional_expr.ptr.*) {
            .optional => Error.OptionalChaining,
            else => .{ .optional = try Bin(ast.Type).init(self.allocator, try self.scan_type(optional_expr.ptr)) },
        },
        .parent => |p_expr| try self.scan_type(p_expr.ptr),
        .ident => |name| blk: {
            if (ast.Type.to_primitive(name)) |type_| {
                break :blk type_;
            } else {
                if (!self.context.has_type(name))
                    return Error.UnknowType;

                break :blk .{ .ident = name };
            }
        },
        .null_litteral => .null_litteral,

        .unary_operation, .binary_operation => Error.UnauthorisedOperation,

        else => Error.UnauthorisedLitteral,
    };
}
