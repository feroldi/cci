# CCI Compiling Infrastructure

This document is an attempt to describe the process cci goes through
when executed, from source file tokenization to code optimization and
generation, as well as API and project design.

Summary:

+ General
  + Why infrastructure
  + Directory skeleton
  + Why C11 only
+ Front-end
  + Source code management
  + Tokenization
  + Parsing and analyses

TODO:

+ Back-end
+ API

# General

There are a few non-obvious choices and terminologies used in this
project, so this section is intended to explain them.

## Why infrastructure

CCI stands for *C11 Compiler Infrastructure*. That means this is not
just a tool you can use to compile C code. CCI has an API, which you
can use to manipulate C code, such as tokenizing it, generating a parse
tree, doing static and semantic analyses, generating an IR, producing
an executable, writing a back-end, and so on.

## Directory skeleton

+ `include/`: This directory exposes the CCI's API you can use to write
  your own applications. There are functions for tokenizing, parsing,
  diagnosing, analysing, IR, back-ends etc.
+ `lib/`: This is where most of CCI's code base is located at. All APIs
  are implemented here, following the same names as in `include/`. E.g.
  if there's an `include/cci/lexer/lex.hpp`, then there's also a
  `lib/lexer/lex.cpp` (but not the other way around sometimes).
+ `src/`: This is where some CCI tools are implemented, where each
  directory is a separate project. For example, the CCI compiler tool
  is implemented in `src/cci/`.
+ `deps/`: All third party dependencies go here.
+ `unittest/`: This directory contains unit tests for the API. Regression
  tests are in another directory.
+ `docs/`:  All documentation or manuals go here.
+ `cmake/`: This contains some modules used across the build system.

Almost all directories have a *README.md* explaining their structure
and purpose, what they do and solve etc.

There's also a `git_revision.cpp.in` file in the root directory: that's
meant to be configured by CMake when you build a tool. You can use this
to make your application's build version.

## Why C11 only

Writing a C90 compiler is easy, there are plenty of those out there.
But a conforming C11 compiler? I only know a few. Besides, such a project
is a journey for myself, one can learn so much by writing a compiler, and
C11 seems perfect for that.
