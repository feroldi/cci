# CCI: C11 Compiler Infrastructure

[![Build Status](https://travis-ci.org/feroldi/cci.svg?branch=master)](https://travis-ci.org/feroldi/cci)
[![Codecov](https://codecov.io/gh/feroldi/cci/branch/master/graph/badge.svg)](https://codecov.io/gh/feroldi/cci)

This is an experimental project of a C compiler written in C++20.
The implementation follows the ISO/IEC 9899:2011 standard (i.e., C11).
The main purpose of this project is to teach me compiler data structures, language design and optimization techniques.

## Building

Use `cmake` to build the project:

```
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build . --target cci
```

Both Clang and GCC are able to compile this project.

## Usage

This is still a work in progress project. Usage is to be done.

## Running tests

You'll need to install [GoogleTest](https://github.com/google/googletest), as CCI uses it for the unit tests.

After installing GoogleTest, run unit tests with the following command:

    ctest --output-on-failure

If you don't want to run unit tests, you may disable them by specifying `BUILD_TESTING=NO` when generating the build files with CMake:

```
cmake -DBUILD_TESTING=NO -DCMAKE_BUILD_TYPE=Release ..
```

## Compiler design

This document is an attempt to describe the API and project design.

Summary:

+ General
  + What does it mean by infrastructure?
  + The project's directory skeleton

## General

There are a few non-obvious choices and terminologies used in this project, so this section is intended to explain them.

### What does it mean by infrastructure?

CCI stands for *C11 Compiler Infrastructure*.
That means this is not just a tool you can use to compile C code.
CCI has an API, which you can use to manipulate C code.
It allows you to scan it, generate and traverse a parse tree, generate an IR, produce an executable, write a back-end for, and so on.

### The project's directory skeleton

+ `include/`: Exposes the CCI's API you can use to write your own applications.
    There are functions for scanning, parsing, diagnosing, analysing, IRs etc.
+ `lib/`: This is where most of CCI's code base lives. APIs are implemented here.
+ `src/`: This is where some CCI tools live, where each directory is a separate project.
  - For example, the CCI compiler tool lives under `src/cci/`.
+ `unittest/`: Contains unit tests for the API.
+ `doc/`:  Documentation or manuals go here.
+ `cmake/`: Contains some modules used across the build system.

Almost all directories have a *README.md* file explaining their structure and purpose, what they do and solve etc.

### Why C11 only

C11 is a great, challenging language to make a compiler for.
It's also true that one can learn a lot by writing a compiler.
That being so, C11 seems to be an option that gets the most out of the experience.

## License

This project is licensed under the MIT license. See LICENSE.
