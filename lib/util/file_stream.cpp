#include "cci/util/file_stream.hpp"
#include "cci/util/filesystem.hpp"
#include "cci/util/scope_guard.hpp"
#include <cstring>
#include <memory>
#include <optional>

namespace cci
{

auto write_stream(const fs::path &file_path, const std::byte *data,
                  size_t length) -> bool
{
    if (std::FILE *stream = std::fopen(file_path.c_str(), "wb");
        stream != nullptr)
    {
        ScopeGuard file_guard([&] { std::fclose(stream); });
        return std::fwrite(data, length, 1, stream) == length;
    }
    else
        return false;
}

auto write_stream(std::vector<std::byte> &stream, const std::byte *data,
                  size_t length) -> bool
{
    stream.resize(length);
    return std::copy(data, data + length, stream.begin()) == stream.end();
}

template <typename Container>
static auto read_stream(const fs::path &file_path) -> std::optional<Container>
{
    std::optional<Container> data;

    if (std::FILE *stream = std::fopen(file_path.c_str(), "rb");
        stream != nullptr)
    {
        ScopeGuard stream_guard([&] { std::fclose(stream); });
        const uintmax_t size = fs::file_size(file_path);
        Container buffer;
        buffer.resize(size);
        if (std::fread(buffer.data(), sizeof(buffer[0]), size, stream) == size)
            data = std::move(buffer);
    }

    return data;
}

auto read_stream_utf8(const fs::path &file_path) -> std::optional<std::string>
{
    return read_stream<std::string>(file_path);
}

auto read_stream_binary(const fs::path &file_path)
    -> std::optional<std::vector<std::byte>>
{
    return read_stream<std::vector<std::byte>>(file_path);
}

} // namespace cci
