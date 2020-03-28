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

Upon synctatic or semantic errors, the compiler collects enough information from the source-code in order to produce diagnostics.
Therefore, diagnostics hold information about the source-code, showing the exact lines that originate them.

The way CCI maps diagnostics to source-code is by keeping a collection of file maps as one big source map:

* A `FileMap` maps a string of some source-code into a range of byte-offsets represented by a pair of `ByteLoc`s.
* A `SourceMap` keeps a collection of `FileMap`s, where the ending byte-offset of some `FileMap` **A** is the starting byte-offset of some `FileMap` **B** that immediately succeeds **A**.

Absolute byte-offsets are used to access a specific position in the source-code.
An absolute byte-offset implicitly encodes which `FileMap` it is part of.
Then, the absolute byte-offset is converted into a relative byte-offset in order to index its `FileMap`.

Information like line and column numbers, file name, source snippets etc., are retrievable from a `SourceMap`.
This is primarily used to map the abstract-syntax-tree to the original source-code, which helps to produce content rich diagnostics.

Currently, all diagnostics are considered as errors.
This means that a compilation step that produces diagnostics, after it has finished executing, won't let the next step start.
E.g., if there are syntax related diagnostics, then the semantics analysis step won't start.
