
+ TODO: rewrite this to conform with the new file system hierarchy.

# CCompiler Compiling Infrastructure

This document is an attempt to describe the process ccompiler
goes through when executed, from source file tokenization to
code optimization and generation.

## Source hierarchy

+ `Options::parse_arguments(argc, argv)`, from `program.hpp`.

Parses ccompiler command-line arguments. This is the very first
thing ccompiler has to before anything. Every argument is
parsed and the program options are updated.

Some compiler/language options default to some value in case
no flag/argument is passed for such. E.g. the option `-Werror`
modifies `Options::warning_as_error`, which is defaulted to `false`.

+ `ProgramContext(options)`, from `program.hpp`.

This is where information useful to the user and program
is handled. Diagnostics (such as warnings, errors, where to output
etc), and program state (diagnostics counting, program/language options
etc) are all managed by an instance of `ProgramContext`.

Every step in the compiler makes use of that instance, which has
to be unique across the entire compilation process.

+ `SourceManager::from_path(filename)`, from `source_manager.hpp`.

Source code management. `SourceManager` provides ways to access
the source code from a given file, including its text (input stream),
iterators inside the text, line and column number information for iterators,
and (to be done) logical lines for preprocessor.

This is used primarily for compile diagnostics.

+ `TokenStream::parse(program, source_manager)`, from `lexer.hpp`.

Translates an input text stream into a sequence of tokens.
Effectively produces a vector of `TokenData` elements.

`TokenStream` also offers ways to produce information from tokens
useful for diagnosing.

+ `SyntaxTree::parse(program, token_stream)`, from `parser.hpp`.

Produces an abstract syntax tree out of a sequence of tokens.
This is the second of three steps into translating C source code
into a useful AST. A `SyntaxTree` instance is a simple, generic
node into a C syntax. It holds information about tokens and also
other user data, such as annotations. It also describes the
expressions associativity, precedence and hierarchy. This step
doesn't handle wrong semantics, but it does fail on wrong syntax
when the tokens don't respect grammar, and procudes compile
errors accordingly.

A successfully parsed `SyntaxTree` instance doesn't imply correct
or meaningful code. Those checks are done later on, when static/semantic
analyses kick in.

