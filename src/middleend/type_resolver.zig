const std = @import("std");
const Rc = @import("../utils/rc.zig").Rc;

const ast = @import("./ast.zig");
const Context = @import("../context.zig");
const Error = @import("../errors.zig").Error;

allocator: std.mem.Allocator,
context: *Context,

const Self = @This();
pub fn resolve(allocator: std.mem.Allocator, context: *Context, statements: *std.ArrayList(ast.Statement)) Error!void {
    var resolver = Self{
        .allocator = allocator,
        .context = context,
    };

    return resolver.resolve_statements(statements);
}

pub fn resolve_statements(self: *Self, statements: *std.ArrayList(ast.Statement)) Error!void {
    for (statements.items) |*statement| {
        try self.resolve_statement(statement);
    }
}

pub fn resolve_statement(self: *Self, statement: *ast.Statement) Error!void {
    switch (statement.*) {
        .expression => |*expr| _ = try self.resolve_expression(expr),
        .variable_declaration => |rc_var_def| {
            const var_def = rc_var_def.deref();
            const resolved_type = try self.resolve_expression(&var_def.value);
            switch (var_def.type) {
                .none => var_def.type = resolved_type,
                else => {
                    if (try var_def.type.is_surtype_of(self.context, &resolved_type))
                        return;

                    // TODO: casting
                    return Error.InvalidType;
                },
            }
        },
        else => {},
    }
}

pub fn resolve_expression(self: *Self, expression: *ast.Expr) Error!ast.Type {
    return switch (expression.*) {
        .char_litteral => .char,
        .string_litteral => .string,
        .symbol_litteral => .symbol,
        .float_litteral => .float,
        .int_litteral => .int,
        .bool_litteral => .boolean,
        .null_litteral => .null_litteral,
        .ident => |*ident| blk: {
            const runtime_decl = try self.context.get_runtime(ident.value);
            ident.type = switch (runtime_decl) {
                .function => |func_decl| try self.context.get_type(func_decl.deref().name),
                .variable => |var_decl| var_decl.deref().type,
            };
            break :blk ident.type;
        },
        else => Error.UnknowType,
    };
}
