const std = @import("std");
const ast = @import("./frontend/ast.zig");

pub const ContextKind = enum {
    global,
    function,
    local
};

pub const Context = struct {
    // TODO: replace []const u8 to a real struct to add optional, tuple and more 
    const FunctionDefinition = struct {
        args: std.ArrayList([]const u8),
        return_type: []const u8
    };
    pub const TypeDefinition = struct {
        attributs: std.StringHashMap([]const u8),
        methods: std.StringHashMap(FunctionDefinition)
    };

    global: *const Context,
    parent: ?*const Context,
    declarations: std.StringHashMap(*const ast.Statement),
    types: std.StringHashMap(TypeDefinition),
    kind: ContextKind,
    allocator: std.mem.Allocator,

    const Self = @This();
    pub fn init(allocator: std.mem.Allocator, kind: ContextKind, parent: ?*const Context) Self {
        var context = Self { 
            .kind = kind,
            .parent = parent,
            .global = undefined,
            .types = std.StringHashMap(TypeDefinition).init(allocator),
            .declarations = std.StringHashMap(*const ast.Statement).init(allocator),
            .allocator = allocator,
        };

        if (parent) |p| { context.global = p.global; }
        else context.global = &context;

        return context;
    }

    pub fn deinit(self: *Self) void {
        self.declarations.deinit();

        var types_iter = self.types.valueIterator();
        while (types_iter.next()) |type_def| {
            type_def.attributs.deinit();
            var iter = type_def.methods.valueIterator();
            while (iter.next()) |func_def| {
                func_def.args.deinit();
            }
        }

        self.types.deinit();
    }

    pub fn compare_kind(self: *Self, other: Self) bool {
        return self.kind == other.kind;
    }

    pub fn declare_type(self: *Self, name: []const u8, type_: TypeDefinition) !void {
        if (self.types.get(name) != null)
            return error.AlreadyDeclareType;
        self.types.put(name, type_);
    }

    pub fn get_type(self: *Self, name: []const u8) !TypeDefinition {
        return if (self.types.get(name)) |type_def| { type_def; }
            else switch (self.kind) {
                .global => error.UnknowType,
                else => self.parent.?.get_type(name)
            };

    }

    pub fn declare_runtime(self: *Self, declaration: *const ast.Statement) !void {
        const name = switch (declaration) {
            .exported, .expression => return error.NonDeclarationGiven,
            .variable_declaration => |variable_declaration| variable_declaration.name,
            .function_declaration => |function_declaration| function_declaration.name,
        };

        if (self.declarations.get(name) != null)
            return switch (declaration) {
                .variable_declaration => error.AlreadyDeclareVariable,
                .function_declaration => error.AlreadyDeclareFunction,
                else => unreachable
            };

        try self.declarations.put(name, declaration);
    }
};