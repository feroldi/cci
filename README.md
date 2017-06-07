# An experimental C compiler

## About

This is an experimental project, using C++17 to write a C compiler.
It doesn't provide an implementation of the Standard Library.

The main purpose of this is to teach me compiler data structures,
design and optimization algorithms.

## Building

Use `cmake` to build and run the project:

```
mkdir build && cd build
cmake ..
cmake --build .
```

It's recommended to export `CC` and `CXX` to a preferable compiler
before you continue with the build process. The project is tested
with Clang only.

```
export CC=clang
export CXX=clang++
```

## Usage

```
ccompiler [OPTIONS] files …
```

For example, assuming a `main.c` source file:

```
$ cat main.c
int main() { return 42; }

$ ccompiler -o out main.c
$ ./out; echo $? # shows program exit value
42
```

Use `ccompiler --help` to get a list of useful parameters and options.
