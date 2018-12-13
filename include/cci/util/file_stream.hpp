#pragma once

#include "cci/util/filesystem.hpp"
#include <cstddef>
#include <cstdio>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace cci {

// Writes some data into a file stream.
//
// Returns true if all data were written successfully. False otherwise.
auto write_stream(const fs::path &source_path, const std::byte *data,
                  size_t length) -> bool;

// Writes some data into a file stream.
//
// Returns true if all data were written successfully. False otherwise.
auto write_stream(std::vector<std::byte> &stream, const std::byte *data,
                  size_t length) -> bool;

// Reads the content from a UTF-8 file.
auto read_stream_utf8(const fs::path &file_path) -> std::optional<std::string>;

// Reads the content from a binary file.
auto read_stream_binary(const fs::path &file_path)
    -> std::optional<std::vector<std::byte>>;

} // namespace cci
