// MemoryBuffer - read-only block of memory.
//
// The idea is to have an interface for buffering, so it becomes easier
// to work with source files. The MemoryBuffer is a simple set of two raw pointers
// (start and end of the buffer), and a bunch of helper functions to load
// data from any object. In addition, this interface guarantees a null-terminated,
// contiguous block of memory (makes it easy to work with C-like strings).
//
// This is inspired by the llvm::MemoryBuffer, but tries to be as simple and short-scope
// as possible.
//
// Different memory buffer implementations are hidden, so you only get to see/use
// the MemoryBuffer class.
//
// For instance, you can buffer a file with MemoryBuffer::from_file, and this gets
// you a NamedMemoryBuffer (with a special identifier: the file name):
//
//     auto buf = MemoryBuffer::from_file("test.txt");
//     // buf->name() == "test.txt"
//     // buf->buffer() is an std::string_view into the buffer.
//     // for (char C : *buf) ...
//
// There's also a MemoryBufferRef, which is a set of two raw pointers and an
// std::string_view (copied from a MemoryBuffer). It's useful when you don't want
// to directly use the buffer through a pointer (this economizes virtual calls).
//
//    MemoryBufferRef MBR = buffer;
//    // MBR.name() == ...
//    // for (auto C : MBR) ...

#pragma once

#include "ccompiler/Util/Contracts.hpp"
#include <string_view>
#include <memory>

namespace ccompiler::util {

// MemoryBuffer - Represents a read-only block of memory read from a file,
// a stream etc.
//
// This interface guarantees a contiguous memory allocation, as well as a
// null-terminated byte string (end()[0] == '\0'). It is acceptable to point to
// an element past the end because of the null character guarantee.
class MemoryBuffer
{
  // Start of the memory block.
  const char *buffer_begin;

  // End of the memory block. This always points to a null character.
  const char *buffer_end;

protected:
  // Used by implementations to initialize the memory buffer.
  void init(const char *buf_beg, const char *buf_end) noexcept
  {
    Expects(buf_beg != nullptr);
    Expects(buf_end != nullptr);
    Expects(buf_end[0] == '\0');

    buffer_begin = buf_beg;
    buffer_end = buf_end;
  }

public:
  MemoryBuffer() = default;

  // Copies of MemoryBuffer might cause double-free.
  MemoryBuffer(const MemoryBuffer &) = delete;
  MemoryBuffer &operator= (const MemoryBuffer &) = delete;

  virtual ~MemoryBuffer() = default;

  // Reads a file from the file system.
  // \returns a named memory buffer allocation.
  // \throws `std::system_error` is filename doens't exist.
  static auto from_file(std::string_view filename) -> std::unique_ptr<MemoryBuffer>;

  auto begin() const noexcept -> const char * { return buffer_begin; }
  auto end() const noexcept -> const char * { return buffer_end; }

  // Returns the buffer's size in bytes.
  auto size() const noexcept -> size_t { return static_cast<size_t>(buffer_end - buffer_begin); }

  // Returns a string_view into the internal buffer.
  auto buffer() const -> std::string_view { return {begin(), size()}; }

  // Returns a pointer to the start of the internal buffer.
  auto raw_buffer() const -> const char * { return buffer_begin; }

  // Gets the buffer identifier (e.g. a file name etc).
  virtual auto name() const -> std::string_view { return "Unknown buffer"; }
};

// MemoryBufferRef - This is a copy of the main pieces of a MemoryBuffer.
//
// This is used when you don't own the memory buffer, but needs to talk to
// it anyway.
class MemoryBufferRef
{
  const char *B;       //< Start of the buffer.
  const char *E;       //< End of the buffer.
  std::string_view ID; //< Buffer's name.

public:
  MemoryBufferRef(const MemoryBuffer &MB) noexcept
    : B(MB.begin()), E(MB.end()), ID(MB.name()) {}

  auto begin() const -> const char * { return B; }
  auto end() const -> const char * { return E; }
  auto name() const -> std::string_view { return ID; }
};

} // namespace ccompiler::util
