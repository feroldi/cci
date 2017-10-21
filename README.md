# An experimental C compiler

## About

This is an experimental project, using C++17 to write a C11 compiler.
It doesn't provide an implementation of the Standard Library.

The main purpose of this is to teach me compiler data structures,
language design and optimization techniques.

## Building

Use `cmake` to build and run the project:

```
mkdir build && cd build
cmake ..
cmake --build . --target ccompiler
```

It's recommended to export `CC` and `CXX` to a preferable compiler
before you continue with the build process. This project is tested
with Clang, and GCC occasionally. For example:

```
export CC=clang
export CXX=clang++
cmake --build . --target ccompiler
```

## Usage

```
ccompiler [OPTIONS] files …
```

For example, assuming a `main.c` source file, you can compile it like so:

```
$ cat main.c
int main() { return 42; }

$ ccompiler -o a.out main.c
$ ./a.out; echo $?
42
```

Use `ccompiler --help` to get a list of useful parameters and options.
