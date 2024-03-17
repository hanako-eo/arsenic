const std = @import("std");
const ast = @import("./middleend/ast.zig");

const Error = @import("./errors.zig").Error;

pub const ContextKind = enum { global, module, function, local };

pub const Context = struct {
    const FunctionDefinition = struct { args: std.ArrayList(*const ast.Type), return_type: *const ast.Type };
    pub const TypeDefinition = struct { attributs: std.StringHashMap(*const ast.Type), methods: std.StringHashMap(FunctionDefinition) };

    global: *const Context,
    parent: ?*const Context,
    declarations: std.StringHashMap(*const ast.Statement),
    types: std.StringHashMap(ast.Type),
    kind: ContextKind,
    allocator: std.mem.Allocator,

    const Self = @This();
    pub fn init(allocator: std.mem.Allocator, kind: ContextKind, parent: ?*const Context) Self {
        var context = Self{
            .kind = kind,
            .parent = parent,
            .global = undefined,
            .types = std.StringHashMap(ast.Type).init(allocator),
            .declarations = std.StringHashMap(*const ast.Statement).init(allocator),
            .allocator = allocator,
        };

        if (parent) |p| {
            context.global = p.global;
        } else {
            context.global = &context;
        }

        return context;
    }

    pub fn deinit(self: *Self) void {
        self.declarations.deinit();

        var types_iter = self.types.valueIterator();
        while (types_iter.next()) |type_def| {
            switch (type_def.*) {
                .func => |func_definition| func_definition.args.deinit(),
                else => {},
            }
        }

        self.types.deinit();
    }

    pub fn compare_kind(self: *const Self, other: ContextKind) bool {
        return self.kind == other;
    }

    pub fn declare_type(self: *Self, name: []const u8, type_: ast.Type) Error!void {
        if (self.types.get(name) != null)
            return Error.AlreadyDeclareType;
        self.types.put(name, type_) catch return Error.AllocationOutOfMemory;
    }

    pub fn get_type(self: *Self, name: []const u8) Error!ast.Type {
        return if (self.types.get(name)) |type_def| {
            type_def;
        } else switch (self.kind) {
            .global => Error.UnknowType,
            else => self.parent.?.get_type(name),
        };
    }

    pub fn has_at_runtime(self: *const Self, name: []const u8) bool {
        return self.declarations.contains(name) or switch (self.kind) {
            .global, .module => false,
            else => self.parent.?.has_at_runtime(name),
        };
    }

    pub fn declare_runtime(self: *Self, declaration: *const ast.Statement) Error!void {
        const name = switch (declaration.*) {
            .variable_declaration => |variable_declaration| variable_declaration.name,
            .function_declaration => |function_declaration| function_declaration.name,
            else => return Error.NonDeclarationGiven,
        };

        if (self.declarations.get(name) != null)
            return Error.AlreadyDeclareAtRuntime;

        self.declarations.put(name, declaration) catch return Error.AllocationOutOfMemory;
    }
};
