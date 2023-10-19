const std = @import("std");
const lexer = @import("./frontend/lexer.zig");
const parser = @import("./frontend/parser.zig");

const ast_t = @import("./middleend/ast.zig");

const context = @import("./context.zig");
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

    var global_context = context.Context.init(allocator, context.ContextKind.global, null);
    defer global_context.deinit();

    var lex = lexer.Lexer.init(.{ .file_name = args[1], .buffer = buffer });
    var tokens = std.ArrayList(lexer.Token).init(allocator);

    while (lex.has_tokens()) {
        try tokens.append(try lex.next_token());
    }
    try tokens.append(.eof);

    var ast = try parser.Parser.parse(allocator, &tokens);
    _ = ast;
    tokens.deinit();
}
