const std = @import("std");
const token = @import("./frontend/token.zig");
const Lexer = @import("./frontend/lexer.zig");
const Parser = @import("./frontend/parser.zig");

const Scanner = @import("./middleend/scanner.zig");
const TypeResolver = @import("./middleend/type_resolver.zig");

const Context = @import("./context.zig");
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

    var global_context = Context.init(allocator, Context.ContextKind.global, null, undefined);
    defer global_context.deinit();

    var module_context = Context.init(allocator, Context.ContextKind.module, &global_context, &global_context);
    defer module_context.deinit();

    var lex = Lexer.init(.{ .file_name = args[1], .buffer = buffer });
    var tokens = std.ArrayList(token.Token).init(allocator);

    while (lex.has_tokens()) {
        try tokens.append(try lex.next_token());
    }
    try tokens.append(.eof);

    var ast = try Parser.parse(allocator, &tokens);
    tokens.deinit();

    var scanned_ast = try Scanner.scan(allocator, &module_context, &ast);
    for (ast.items) |s| {
        s.deinit();
    }
    ast.deinit();

    try TypeResolver.resolve(allocator, &module_context, &scanned_ast);

    var a = module_context.runtimes.keyIterator();
    while (a.next()) |b| {
        std.debug.print("{s} has the type {}\n", .{ b.*, module_context.runtimes.get(b.*).?.variable.deref().type });
    }
}

test "import other tests" {
    _ = @import("./frontend/lexer.zig");
    _ = @import("./frontend/parser.zig");
    _ = @import("./middleend/scanner.zig");
}
