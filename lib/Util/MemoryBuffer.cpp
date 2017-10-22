#include "ccompiler/Util/MemoryBuffer.hpp"
#include "ccompiler/Util/Contracts.hpp"
#include <string_view>
#include <memory>
#include <system_error>
#include <cassert>

namespace ccompiler::util {

class NamedMemoryBuffer : public MemoryBuffer
{
  std::unique_ptr<char[]> raw_data; //< Buffer's allocated memory.
  std::string_view name_;           //< File name.

public:
  NamedMemoryBuffer(std::unique_ptr<char[]> data, size_t data_size, std::string_view name) noexcept
    : raw_data(std::move(data)), name_(name)
  {
    MemoryBuffer::init(raw_data.get(), raw_data.get() + data_size);
  }

  // Returns the file name from which this buffer is read.
  auto name() const -> std::string_view override { return name_; }

  virtual ~NamedMemoryBuffer() override = default;
};

// CloseFileRAII - Closes a file description at the end of an enclosing scope.
struct CloseFileRAII
{
  std::FILE *FD;
  CloseFileRAII(FILE *FD) : FD(FD) {}
  ~CloseFileRAII() { std::fclose(FD); }
};

auto MemoryBuffer::from_file(std::string_view filename) -> std::unique_ptr<MemoryBuffer>
{
  std::FILE *FD = std::fopen(filename.data(), "rb");

  if (FD == nullptr)
  {
    // TODO: change the filename to an fs::path.
    throw std::system_error(std::make_error_code(std::errc::no_such_file_or_directory),
                            filename.data());
  }

  CloseFileRAII FD_guard(FD);

  // Calculates file size in bytes.
  std::fseek(FD, 0, SEEK_END);
  auto FD_len_in_bytes = static_cast<size_t>(std::ftell(FD));
  std::rewind(FD);

  // Adds an extra byte for the null character.
  auto raw_data = std::make_unique<char[]>(FD_len_in_bytes + 1U);

  std::fread(raw_data.get(), 1U, FD_len_in_bytes, FD);
  raw_data[FD_len_in_bytes] = '\0';

  return std::unique_ptr<MemoryBuffer>(new NamedMemoryBuffer(std::move(raw_data),
                                                             FD_len_in_bytes,
                                                             filename));
}

} // namespace ccompiler::util
