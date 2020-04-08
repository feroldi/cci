# CCI: C11 Compiler Infrastructure

[![Build Status](https://travis-ci.org/feroldi/cci.svg?branch=master)](https://travis-ci.org/feroldi/cci)
[![Codecov](https://codecov.io/gh/feroldi/cci/branch/master/graph/badge.svg)](https://codecov.io/gh/feroldi/cci)

This is an experimental project, using C++17 to write a C compiler (ISO/IEC
9899:2011, to be precise).  The main purpose of this project is to teach me
compiler data structures, language design and optimization techniques (also to
unlock an achievement: make a C compiler!).

## Building

Use `cmake` to build the project:

```
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build . --target cci
```

This project is tested with Clang and GCC.

## Usage

This is still a work in progress project. Usage is to be done.

## Running tests

Unit tests are written with the help of
[GoogleTest](https://github.com/google/googletest). There isn't any need to
install it in order to compile the project, it's only required if you actually
want to run the unit tests.

**Note**: When generating the CMake configuration files, make sure to disable
the CMake option `BUILD_TESTING` in case you don't want to compile and run unit
tests.

The following command runs the unit tests:

    GTEST_COLOR=yes ctest --output-on-failure

This will run all unit tests and show a compact status summary. In case
anything goes wrong, `ctest` will output a detailed message for all failing tests.

## License

This project is licensed under the MIT license. See LICENSE.

---

# CCI design

This document is an attempt to describe the API and project design.

Summary:

+ General
  + Why infrastructure
  + Directory skeleton
  + TODO: API design

# General

There are a few non obvious choices and terminologies used in this
project, so this section is intended to explain them.

## Why infrastructure

CCI stands for *C11 Compiler Infrastructure*. That means this is not just a
tool you can use to compile C code. CCI has an API, which you can use to
manipulate C code. It allows you to scan it, generate and traverse a parse
tree, generate an IR, produce an executable, write a back-end, and so on.

## Directory skeleton

+ `include/`: Exposes the CCI's API you can use to write your own applications.
    There are functions for scanning, parsing, diagnosing, analysing, IRs etc.
+ `lib/`: This is where most of CCI's code base lives. APIs are implemented here.
+ `src/`: This is where some CCI tools live, where each directory is a separate project.
  - For example, the CCI compiler tool lives under `src/cci/`.
+ `unittest/`: Contains unit tests for the API.
+ `doc/`:  Documentation or manuals go here.
+ `cmake/`: Contains some modules used across the build system.

Almost all directories have a *README.md* explaining their structure
and purpose, what they do and solve etc.

## Why C11 only

C11 is a great, challenging language to make a compiler for.
It's also true that one can learn a lot by writing a compiler.
That being so, C11 seems to be an option that gets the most out of the experience.

## API design

TODO
