const std = @import("std");
const Allocator = std.mem.Allocator;

const Error = @import("../errors.zig").Error;

pub fn Rc(comptime T: type) type {
    const Inner = struct {
        counter: usize,
        value: T,
        allocator: Allocator,
    };

    return struct {
        ptr: *Inner,

        const Self = @This();
        pub fn init(allocator: Allocator, value: T) !Self {
            const ptr = allocator.create(Inner) catch return Error.AllocationOutOfMemory;
            ptr.* = .{
                .counter = 1,
                .value = value,
                .allocator = allocator,
            };

            return Self{ .ptr = ptr };
        }

        pub fn deref(self: Self) *T {
            return &self.ptr.value;
        }

        pub fn clone(self: Self) Self {
            self.ptr.counter += 1;

            return self;
        }

        pub fn deinit(self: Self) void {
            self.ptr.counter -= 1;
            if (self.ptr.counter == 0) {
                if (@hasDecl(T, "deinit")) {
                    self.ptr.value.deinit();
                }
                const allocator = self.ptr.allocator;
                allocator.destroy(self.ptr);
            }
        }
    };
}
