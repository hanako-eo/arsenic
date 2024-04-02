const std = @import("std");
const Rc = @import("./utils/rc.zig").Rc;

const front_ast = @import("./frontend/ast.zig");
const ast = @import("./middleend/ast.zig");

const Error = @import("./errors.zig").Error;

pub const ContextKind = enum { global, module, function, local };

pub const Runtime = union(enum) {
    variable: Rc(ast.Statement.VarDeclaration),
    function: Rc(ast.Statement.FnDeclaration),
};

const Self = @This();

global: *Self,
parent: ?*const Self,
runtimes: std.StringHashMap(Runtime),
types: std.StringHashMap(ast.TypeDefinition),
kind: ContextKind,
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator, kind: ContextKind, parent: ?*const Self, global: *Self) Self {
    return Self{
        .kind = kind,
        .parent = parent,
        .global = global,
        .types = std.StringHashMap(ast.TypeDefinition).init(allocator),
        .runtimes = std.StringHashMap(Runtime).init(allocator),
        .allocator = allocator,
    };
}

pub fn deinit(self: *Self) void {
    self.runtimes.deinit();
    self.types.deinit();
}

pub fn compare_kind(self: *const Self, other: ContextKind) bool {
    return self.kind == other;
}

pub fn declare_type(self: *Self, definition: ast.TypeDefinition) Error!void {
    if (self.kind != .global) {
        const is_global = for (definition.attributes.items) |attr| {
            if (attr == .global)
                break true;
        } else false;

        if (is_global)
            return self.global.declare_type(definition);
    }

    if (self.types.contains(definition.name))
        return Error.AlreadyDeclareType;

    self.types.put(definition.name, definition) catch return Error.AllocationOutOfMemory;
}

pub fn has_type(self: *const Self, name: []const u8) bool {
    return self.types.contains(name) or switch (self.kind) {
        .global => false,
        else => self.parent.?.has_at_runtime(name),
    };
}

pub fn get_type(self: *const Self, name: []const u8) Error!ast.Type {
    return if (self.types.get(name)) |*t| blk: {
        break :blk t.value;
    } else switch (self.kind) {
        .global => Error.UnknowType,
        else => self.parent.?.get_type(name),
    };
}

pub fn declare_runtime(self: *Self, runtime: Runtime) Error!void {
    const name = switch (runtime) {
        .variable => |variable| variable.deref().name,
        .function => |function| function.deref().name,
    };

    if (self.runtimes.contains(name))
        return Error.AlreadyDeclareAtRuntime;

    self.runtimes.put(name, runtime) catch return Error.AllocationOutOfMemory;
}

pub fn has_at_runtime(self: *const Self, name: []const u8) bool {
    return self.runtimes.contains(name) or switch (self.kind) {
        .global, .module => false,
        else => self.parent.?.has_at_runtime(name),
    };
}

pub fn get_runtime(self: *const Self, name: []const u8) Error!Runtime {
    return self.runtimes.get(name) orelse switch (self.kind) {
        .global => Error.UndefinedVariable,
        else => self.parent.?.get_runtime(name),
    };
}
