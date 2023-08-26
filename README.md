# Arsenic

Arsenic is a language designed as an alternative to TypeScript, stricter and more rigorous, it will transpile towards JS by offering code that is much easier to maintain. It will correct all the little imperfections of TypeScript.

It will try also to add new "false" feature to JavaScript like operator overloading or defer.

## Roadmap

### Core Roadmap
 - [x] Lexer/Parser (converte file into [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree))
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
 - [ ] Enum and Union
 - [ ] Optimize `Array<int>` with [`TypedArray`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray)

### (Really) Long-term Roadmap
 - [ ] Try to generate the V8 bytecode for Nodejs
