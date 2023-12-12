const std = @import("std");
const Allocator = std.mem.Allocator;

const Error = @import("../errors.zig").Error;

pub fn InvariantBin(comptime T: type) type {
    return struct {
        ptr: *const T,
        allocator: Allocator,

        const Self = @This();
        pub fn init (allocator: Allocator, value: T) Error!Self {
            var ptr = allocator.create(T) catch return Error.AllocationOutOfMemory;
            ptr.* = value;

            return Self {
                .allocator = allocator,
                .ptr = ptr
            };
        }

        pub fn deref(self: *Self) T {
            return self.ptr.*;
        }

        pub fn deinit(self: *Self) void {
            self.allocator.destroy(self.ptr);
        }
    };
}

pub fn Bin(comptime T: type) type {
    return struct {
        ptr: *T,
        allocator: Allocator,

        const Self = @This();
        pub fn init (allocator: Allocator, value: T) !Self {
            var ptr = try allocator.create(T);
            ptr.* = value;

            return Self {
                .allocator = allocator,
                .ptr = ptr
            };
        }

        pub fn write(self: *Self, value: T) void {
            self.ptr.* = value;
        }

        pub fn deref(self: *Self) T {
            return self.ptr.*;
        }

        pub fn deinit(self: *Self) void {
            self.allocator.destroy(self.ptr);
        }
    };
}