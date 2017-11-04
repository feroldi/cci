#include "cci/util/memory_buffer.hpp"
#include "cci/util/scope_guard.hpp"
#include <string_view>
#include <memory>
#include <system_error>
#include <optional>
#include <cassert>

namespace cci {

// TODO: change the filename to an fs::path.
auto MemoryBuffer::from_file(std::string_view filename) -> std::optional<MemoryBuffer>
{
  std::FILE *file = std::fopen(filename.data(), "rb");

  if (file == nullptr)
    return std::nullopt;

  ScopeGuard file_guard([&] { std::fclose(file); });

  // Calculates file size in bytes.
  std::fseek(file, 0, SEEK_END);
  auto file_size = static_cast<size_t>(std::ftell(file));
  std::rewind(file);

  // Adds an extra byte for the null character.
  auto file_content = std::make_unique<char[]>(file_size + 1U);

  std::fread(file_content.get(), 1U, file_size, file);
  file_content[file_size] = '\0';

  return MemoryBuffer(std::move(file_content), file_size, filename);
}

} // namespace cci
