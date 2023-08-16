const std = @import("std");
const lexer = @import("./lexer.zig");
const File = @import("./file.zig").File;

const log = std.log;
const process = std.process;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    const args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    if (args.len == 1) {
        log.err("Usage: {s} <file>.ars", .{args[0]});
        process.exit(1);
    }

    var file = try File.open(allocator, args[1]);
    defer file.close();

    const buffer = try file.read();

    var lex = lexer.Lexer.init(.{ .file_name = args[1], .buffer = buffer });

    while (lex.has_tokens()) {
        const token = try lex.next_token();
        std.debug.print("{}\n", .{token});
    }
}
