#include "ccompiler/basic/file_stream.hpp"
#include "ccompiler/util/scope_guard.hpp"
#include "fmt/format.h"
#include <cstring>
#include <memory>
#include <optional>

namespace ccompiler {

auto write_to_stream(std::string_view file_path, const std::byte *data, size_t length) -> bool
{
  if (std::FILE *stream = std::fopen(file_path.data(), "wb"); stream != nullptr)
  {
    ScopeGuard file_guard([&] { std::fclose(stream); });
    return std::fwrite(data, length, 1, stream) == length;
  }
  else
    return false;
}

auto write_to_stream(std::vector<std::byte> &stream, const std::byte *data, size_t length) -> bool
{
  stream.resize(length);
  std::memcpy(&stream[0], data, length);
  return true;
}

auto read_from_stream_utf8(std::string_view file_path) -> std::optional<std::string>
{
  if (std::FILE *stream = std::fopen(file_path.data(), "rb"); stream != nullptr)
  {
    ScopeGuard stream_guard([&] { std::fclose(stream); });
    long file_size = 0L;

    if (std::fseek(stream, 0L, SEEK_END) == 0 && (file_size = std::ftell(stream)) != -1L &&
        std::fseek(stream, 0L, SEEK_SET) == 0)
    {
      auto size = static_cast<size_t>(file_size);
      std::string buffer;
      buffer.resize(size);

      if (std::fread(buffer.data(), sizeof(buffer[0]), size, stream) == size)
        return buffer;
    }
  }

  return std::nullopt;
}

auto read_from_stream_binary(std::string_view file_path) -> std::optional<std::vector<std::byte>>
{
  if (std::FILE *stream = std::fopen(file_path.data(), "rb"); stream != nullptr)
  {
    ScopeGuard stream_guard([&] { std::fclose(stream); });
    long file_size = 0L;

    if (std::fseek(stream, 0L, SEEK_END) == 0 && (file_size = std::ftell(stream)) != -1L &&
        std::fseek(stream, 0L, SEEK_SET) == 0)
    {
      auto size = static_cast<size_t>(file_size);
      std::vector<std::byte> buffer;
      buffer.resize(size);

      if (std::fread(buffer.data(), sizeof(buffer[0]), size, stream) == size)
        return buffer;
    }
  }

  return std::nullopt;
}

} // namespace ccompiler
