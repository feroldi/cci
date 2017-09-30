#include <memory>
#include <cstdio>
#include "stream.hpp"

namespace
{

struct CloseStream
{
  void operator() (FILE* stream) const noexcept
  {
    std::fclose(stream);
  }
};

using UniqueStream = std::unique_ptr<FILE, CloseStream>;

auto open_stream(string_view path) -> UniqueStream
{
  return UniqueStream{std::fopen(path.data(), "rb")};
}

auto stream_size(const UniqueStream& stream) -> size_t
{
  std::fseek(stream.get(), 0, SEEK_END);
  long length = std::ftell(stream.get());
  std::rewind(stream.get());

  return static_cast<size_t>(length);
}

} // namespace

namespace utils
{

auto read_stream(string_view path) -> optional<std::string>
{
  if (auto stream = open_stream(path))
  {
    const size_t length = stream_size(stream);
    std::string data{};

    data.resize(length);
    std::fread(std::addressof(data[0]), 1, length, stream.get());

    return data;
  }
  else
  {
    return nullopt;
  }
}

} // namespace utils

