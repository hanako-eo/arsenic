const std = @import("std");
const Allocator = std.mem.Allocator;

const Error = @import("../errors.zig").Error;

pub fn Bin(comptime T: type) type {
    return struct {
        ptr: *T,
        allocator: Allocator,

        const Self = @This();
        pub fn init(allocator: Allocator, value: T) !Self {
            const ptr = allocator.create(T) catch return Error.AllocationOutOfMemory;
            ptr.* = value;

            return Self{ .allocator = allocator, .ptr = ptr };
        }

        pub fn write(self: *Self, value: T) void {
            self.ptr.* = value;
        }

        pub fn deinit(self: Self) void {
            if (@hasDecl(T, "deinit")) {
                self.ptr.deinit();
            }
            self.allocator.destroy(self.ptr);
        }
    };
}
