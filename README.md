# CCI: C11 Compiler Infrastructure

[![build status](https://travis-ci.org/feroldi/cci.svg?branch=master)](https://travis-ci.org/feroldi/cci)

This is an experimental project, using C++17 to write a C11 compiler.
It doesn't provide an implementation of the Standard Library.
The main purpose of this project is to teach me compiler data structures,
language design and optimization techniques, as well as unlock an
achievement: wrote a C compiler.

## Building

Use `cmake` to build and run the project:

```
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build . --target cci
```

It's recommended to export `CC` and `CXX` to a preferable compiler before
you continue with the build process. This project is tested with Clang,
and GCC occasionally. If you're going with Clang, you can make use of
the CMake's toolchain file that comes with this project. For example:

```
cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/toolchains/clang.cmake ..
cmake --build . --target cci
```

**Note**: When compiling with Clang, if fmtlib or GTest are not compiled
with libc++, the above commands might vomit lots of linker errors.

## Usage

**Note**: This is still a work in progress project, so don't except the following
to just work.

```
cci [OPTIONS] files …
```

For example, assuming a `main.c` source file, you can compile it like so:

```
$ cat main.c
int main() { return 42; }

$ cci -o a.out main.c
$ ./a.out; echo $?
42
```

## Legacy branch

Initially, this project had a complete parser for C11 (found at `legacy`
branch), but due to maintainance issues, it was decided to rewrite that part.

## Running tests

This project depends on [GoogleTest](https://github.com/google/googletest)
in order to run unit tests. There's no need to install it in order to
compile the project, it's only required if you want to run the tests.

**Note**: When generating CMake configuration files, make sure to disable
the CMake option `BUILD_TESTING` **if** you don't want to compile and run tests.

To run unit tests, type:

    GTEST_COLOR=yes ctest --output-on-failure

Use `cci --help` to get a list of useful parameters and options.

## License

This project is licensed under the MIT license. See LICENSE.

---

# CCI design

This document is an attempt to describe the API and project design.

Summary:

+ General
  + Why infrastructure
  + Directory skeleton
  + Why C11 only
  + TODO: API design

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
+ `doc/`:  All documentation or manuals go here.
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

## API design

TODO
