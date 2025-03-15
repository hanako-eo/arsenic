# Arsenic

An experimental language designed to create robust, easy-to-maintain source code and produce the most efficient JS possible thanks to a compiler and reimagined syntax.

## Motivation

Due to years of accumulated debt in the JavaScript language itself and a standard that didn't reimagine a more robust scripting language or fix the inconsistencies inherent in JavaScript itself, it seemed logical to kick the can down the road and build a brand new language built differently to TypeScript or any other language that simply revolves around JS. In the same way as with TypeScript, Arsenic will not be an untyped or weakly typed language, but will offer strong typing to force developers to adopt habits and forge good practices.

Arsenic is a language designed as an alternative or at least a successor to TypeScript, which is stricter and more rigorous. It will correct all the little imperfections in TypeScript that make it possible to produce behaviour that is inconsistent with typing. Arsenic takes a go-like approach and has strong opinions about code written with Arsenic.

It will try also to add new "false" feature to JavaScript like operator overloading or defer.

## Roadmap

### Core Roadmap
 - [ ] Lexer/Parser (converte file into [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree))
 - [ ] Scanner (check and optimise the AST)
 - [ ] Type Checking
 - [ ] Compile to JS
 - [ ] Optimise the JS output

### Functionality Roadmap
 - [ ] Import (with `const a = #import("./a.ars");`)
 - [ ] Operator overloading (with compile only symbol `@@op_add`, `@@op_eq` and many more)
 - [ ] Interface
 - [ ] Match expression (switch like)
 - [ ] Tuples (and trying to optimise memory with them)
 - [ ] Enum and Union (sum type)
 - [ ] Optimize `Array<int>` with [`TypedArray`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray)

### (Really) Long-term Roadmap
 - [ ] Try to generate the V8 bytecode for Nodejs
 - [ ] Try to generate native binary from any source
