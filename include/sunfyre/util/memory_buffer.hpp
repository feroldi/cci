// MemoryBuffer - read-only block of memory.
//
// The idea is to have a tool for buffering, so it becomes easier
// to work with source files. The MemoryBuffer is a simple set of raw data and a name,
// and a bunch of helper functions to load data from any device. In addition, this tool
// guarantees a null-terminated, contiguous block of memory (makes it easy to
// work with C-like strings).
//
// TODO: Change it back to an InputBuffer and make it guarantee a new line in the end
// of the input.

#pragma once

#include "sunfyre/util/contracts.hpp"
#include <string_view>
#include <optional>
#include <system_error>
#include <memory>

namespace sunfyre {

// MemoryBuffer - Represents a read-only block of memory read from a file,
// a stream etc.
//
// This stream guarantees a contiguous memory allocation, as well as a
// null-terminated byte string (end()[0] == '\0'). It is acceptable to point to
// an element past the end because of the null character guarantee.
class MemoryBuffer
{
  // Buffer's allocated memory.
  std::unique_ptr<char[]> raw_data;

  // Buffer's memory size in bytes.
  size_t buffer_size;

  // Buffer's identifier (e.g. file name, stream name etc).
  std::string_view name_;

  // Initializes the buffer.
  MemoryBuffer(std::unique_ptr<char[]> data, size_t size, std::string_view name)
    : raw_data(std::move(data)), buffer_size(size), name_(name)
  {
    Expects(raw_data != nullptr);
    Expects(raw_data[buffer_size] == '\0');
  }

public:
  MemoryBuffer() = default;

  // Reads a file from the file system.
  // \returns a named memory buffer allocation, or an std::error_code.
  static auto from_file(std::string_view filename) -> std::optional<MemoryBuffer>;

  const char *begin() const { return raw_data.get(); }
  const char *end() const { return raw_data.get() + buffer_size; }

  // Returns the buffer's size in bytes.
  size_t size() const { return buffer_size; }

  // Returns a string_view into the internal buffer.
  std::string_view text() const { return {begin(), size()}; }

  // Returns a pointer to the start of the internal buffer.
  const char *data() const { return raw_data.get(); }

  // Gets the buffer identifier (e.g. a file name etc).
  std::string_view name() const { return name_; }
};

} // namespace sunfyre
