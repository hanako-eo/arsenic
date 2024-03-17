const std = @import("std");
const front_ast = @import("./frontend/ast.zig");
const ast = @import("./middleend/ast.zig");

const Error = @import("./errors.zig").Error;

pub const ContextKind = enum { global, module, function, local };

pub const Context = struct {
    global: *Context,
    parent: ?*const Context,
    declarations: std.StringHashMap(*const ast.Statement),
    types: std.StringHashMap(*const ast.TypeDefinition),
    kind: ContextKind,
    allocator: std.mem.Allocator,

    const Self = @This();
    pub fn init(allocator: std.mem.Allocator, kind: ContextKind, parent: ?*const Context, global: *Context) Self {
        return Self{
            .kind = kind,
            .parent = parent,
            .global = global,
            .types = std.StringHashMap(*const ast.TypeDefinition).init(allocator),
            .declarations = std.StringHashMap(*const ast.Statement).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.declarations.deinit();
        self.types.deinit();
    }

    pub fn compare_kind(self: *const Self, other: ContextKind) bool {
        return self.kind == other;
    }

    pub fn declare_type(self: *Self, definition: *const ast.TypeDefinition) Error!void {
        const is_global = for (definition.attributes.items) |attr| {
            if (attr == .global)
                break true;
        } else false;

        if (self.kind != .global and is_global)
            return self.global.declare_type(definition);

        if (self.types.contains(definition.name))
            return Error.AlreadyDeclareType;

        self.types.put(definition.name, definition) catch return Error.AllocationOutOfMemory;
    }

    pub fn get_type(self: *Self, name: []const u8) Error!ast.Type {
        return if (self.types.get(name)) |type_def| {
            type_def;
        } else switch (self.kind) {
            .global => Error.UnknowType,
            else => self.parent.?.get_type(name),
        };
    }

    pub fn has_type(self: *const Self, name: []const u8) bool {
        return self.types.contains(name) or switch (self.kind) {
            .global => false,
            else => self.parent.?.has_at_runtime(name),
        };
    }

    pub fn declare_runtime(self: *Self, declaration: *const ast.Statement) Error!void {
        const name = switch (declaration.*) {
            .variable_declaration => |variable_declaration| variable_declaration.name,
            .function_declaration => |function_declaration| function_declaration.name,
            else => return Error.NonDeclarationGiven,
        };

        if (self.declarations.contains(name))
            return Error.AlreadyDeclareAtRuntime;

        self.declarations.put(name, declaration) catch return Error.AllocationOutOfMemory;
    }

    pub fn has_at_runtime(self: *const Self, name: []const u8) bool {
        return self.declarations.contains(name) or switch (self.kind) {
            .global, .module => false,
            else => self.parent.?.has_at_runtime(name),
        };
    }
};
