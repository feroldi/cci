#pragma once

#include <cstdio>
#include <string>
#include <string_view>
#include <vector>
#include <optional>

namespace ccompiler {

// Writes some data into a file stream.
//
// Returns true if all data were written successfully. False otherwise.
auto write_to_stream(std::string_view source_path, const std::byte *data, size_t length) -> bool;

// Writes some data into a file stream.
//
// Returns true if all data were written successfully. False otherwise.
auto write_to_stream(std::vector<std::byte> &stream, const std::byte *data, size_t length) -> bool;

// FIXME: change `file_path` parameters to `fs::path`.

// Reads the content from a UTF-8 file.
auto read_from_stream_utf8(std::string_view file_path) -> std::optional<std::string>;

// Reads the content from a binary file.
auto read_from_stream_binary(std::string_view file_path) -> std::optional<std::vector<std::byte>>;

} // namespace ccompiler
