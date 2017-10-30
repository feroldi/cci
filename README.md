# Sunfyre: an experimental C11 compiler

This is an experimental project, using C++17 to write a C11 compiler.
It doesn't provide an implementation of the Standard Library.
The main purpose of this project is to teach me compiler data structures,
language design and optimization techniques, as well as unlock an
achievement: wrote a C compiler.

## Meaning of Sunfyre

This quote from Games of Thrones explains it:

> Rhaenyra Targaryen was murdered by her brother, or rather, his
> dragon. It ate her while her son watched. What's left of her is buried in
> the crypts right down there.
*―King Joffrey Baratheon tells Margaery Tyrell of Sunfyre devouring Rhaenyra.*

Changing a few names and the context is set:

> C was murdered by her brother, or rather, his
> compiler. It ate her while her linker watched. What's left of her is buried in
> the binary right down there.
*―King C++ tells about Sunfyre compiling C.*

## Building

Use `cmake` to build and run the project:

```
mkdir build && cd build
cmake ..
cmake --build . --target sunfyre
```

It's recommended to export `CC` and `CXX` to a preferable compiler
before you continue with the build process. This project is tested
with Clang, and GCC occasionally. For example:

```
export CC=clang
export CXX=clang++
cmake --build . --target sunfyre
```

## Usage

**Note**: This is still a work in progress project, so don't except the following
to just work.

```
sunfyre [OPTIONS] files …
```

For example, assuming a `main.c` source file, you can compile it like so:

```
$ cat main.c
int main() { return 42; }

$ sunfyre -o a.out main.c
$ ./a.out; echo $?
42
```

Use `sunfyre --help` to get a list of useful parameters and options.
