# CCI: C11 Compiler Infrastructure

This is an experimental project, using C++17 to write a C11 compiler.
It doesn't provide an implementation of the Standard Library.
The main purpose of this project is to teach me compiler data structures,
language design and optimization techniques, as well as unlock an
achievement: wrote a C compiler.

## Building

Use `cmake` to build and run the project:

```
mkdir build && cd build
cmake ..
cmake --build . --target cci
```

It's recommended to export `CC` and `CXX` to a preferable compiler
before you continue with the build process. This project is tested
with Clang, and GCC occasionally. For example:

```
export CC=clang
export CXX=clang++
cmake --build . --target cci
```

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

## Running tests

This project depends on [GoogleTest](https://github.com/google/googletest)
in order to run the unit tests. There's no need to install it in order to
compile the project, it's only required if you want to run the tests.

**Note**: When generating CMake configuration files, make sure to enable
the CMake option `CCI_ENABLE_TESTS` **if** you want to compile and run tests.

Use `cci --help` to get a list of useful parameters and options.

## License

This project is licensed under the MIT license. See LICENSE.
