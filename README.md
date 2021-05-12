# CCI: C11 Compiler Infrastructure

[![Build Status](https://travis-ci.org/feroldi/cci.svg?branch=master)](https://travis-ci.org/feroldi/cci)
[![Codecov](https://codecov.io/gh/feroldi/cci/branch/master/graph/badge.svg)](https://codecov.io/gh/feroldi/cci)

This is an experimental project of a C compiler written in C++20.
The implementation follows the ISO/IEC 9899:2011 standard, i.e., C11.
The main purpose of this project is to teach myself compiler data
structures, language design and optimization techniques.

## Building

Use `cmake` to build the project.  The following sequence of commands
builds the library, tools and unit tests:

```
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
```

You may also specify a toolchain when generating build files by
defining `CMAKE_TOOLCHAIN_FILE` to one of the supported toolchains in
`cmake/toolchains/`.  Both Clang and GCC are able to compile this project.
So, for example, if you're going to build with GCC, you may specify the
GCC toolchain like so:

```
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=../cmake/toolchains/gcc.cmake
cmake --build .
```

The same goes for Clang.
Just replace the `gcc` in
`-DCMAKE_TOOLCHAIN_FILE=../cmake/toolchains/gcc.cmake` with `clang`.

## Usage

This is still a work in progress project. Usage is to be done.

## Running tests

This project makes use of
[GoogleTest](https://github.com/google/googletest) for unit tests,
so you'll need to install it beforehand.  After installing GoogleTest,
go to the `build/` directory we created, and run `ctest`.
For example:

```
cd build
ctest --output-on-failure
```

If you're not going to run unit tests, it's possible to disable them by
specifying `BUILD_TESTING=NO` at the build generation step like so:

```
mkdir build && cd build
cmake -DBUILD_TESTING=NO -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
```

## Compiler design

This document is an attempt to describe the API and project design.

Summary:

+ General
  + Meaning of "Infrastructure" in CCI
  + Project's directory structure

## General

There are a few non-obvious choices and terminologies used in this
project, so this section is intended to explain them.

### Meaning of "Infrastructure" in CCI

TODO:

CCI stands for *C11 Compiler Infrastructure*.  That means this is not
just a tool you can use to compile C code.  CCI has an API, which you can
use to manipulate C code.  The goal is for it to allow you to scan code,
generate and traverse a parse tree, generate an IR, produce an executable,
write a back-end for it, and so on.

### Project's directory structure

+ `include/`: Exposes the CCI's API you can use to write your own
    applications. There are functions for scanning, parsing, diagnosing,
    analysing, generating IRs etc.
+ `lib/`: This is where most of CCI's code base lives. APIs are implemented here.
+ `src/`: This is where some CCI tools live, where each directory is a separate project.
  - For example, the CCI compiler tool lives under `src/cci/`.
+ `unittest/`: Contains unit tests.
+ `doc/`:  Documentation and manuals.
+ `cmake/`: Contains some modules used across the build system.

Almost all directories have a *README.md* file explaining their structure
and purpose, what they do and solve etc.

### Why C11?

C11 is a great, challenging language to make a compiler for.
It's also true that one can learn a lot by writing a compiler.
That being so, C11 seems to be an option that gets the most out of the experience.

## License

This project is licensed under the MIT license. See LICENSE.
