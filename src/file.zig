const std = @import("std");

pub const File = struct {
    allocator: std.mem.Allocator,
    buffer: ?[]u8 = null,
    file: std.fs.File,

    const Self = @This();
    pub fn open(allocator: std.mem.Allocator, path: []const u8) !File {
        const file = try std.fs.cwd().openFile(path, .{});

        return File{ .allocator = allocator, .file = file };
    }

    pub fn read(self: *Self) ![]u8 {
        if (self.buffer == null) {
            self.buffer = try self.allocator.alloc(u8, try self.file.getEndPos());
            _ = try self.file.readAll(self.buffer.?);
        }
        return self.buffer.?;
    }

    pub fn close(self: *Self) void {
        if (self.buffer) |buffer|
            self.allocator.free(buffer);
    }
};
