# CCI Compiling Infrastructure

This document is an attempt to describe the process CCI goes through when executed, from source file scanning to code optimization and generation, as well as API and project design.

Summary:

+ TODO: Introduction
+ TODO: Source code mapping
+ TODO: Diagnostics system and compiler instance
+ TODO: Lexical analysis
+ TODO: Syntactic and semantic analysis
+ TODO: Intermediate language
+ TODO: Optimizations
+ TODO: Code generation

## Introduction

TODO

## Source code mapping

Upon synctatic or semantic errors, the compiler collects enough information from the source code in order to diagnose them to the user.
The error, therefore, has to be mapped to the source code somehow.
The way CCI does it is by keeping a collection of file maps as one big source map.
A `FileMap` maps a string of some source code into a byte offset (`ByteLoc`) range.
A `SourceMap` keeps a collection of `FileMaps`, where the ending byte offset of some `FileMap` X is the starting byte offset of some `FileMap` Y that immediately succeeds X.
Absolute byte offsets are used to access an specific position in the source code.
This absolute byte offset implicitly encodes which `FileMap` it is part of.
Then, it is converted into a relative byte offset in order to index the related `FileMap`.

Information like line and column number, file name, source snippets etc are retrievable from a `SourceMap`.
It is mainly used to map the abstract syntax tree to the original source code, which helps to give better diagnostics to the user.
