#pragma once

#include "cci/basic/source_location.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/util/filesystem.hpp"
#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace cci {
// SourceManager - Manages a set of source files.
class SourceManager
{
  CompilerDiagnostics &diag;

  // Stores starting positions of every line in the source.
  std::vector<SourceLocation> line_offsets;

  // Source file's text content. May be ASCII or UTF-8.
  std::string buffer;

  // Source file's path, if loaded from a file.
  std::optional<fs::path> buffer_filepath;

  SourceManager(CompilerDiagnostics &cd,
                std::vector<SourceLocation> line_offsets,
                std::string buffer) noexcept
    : diag(cd), line_offsets(std::move(line_offsets)), buffer(std::move(buffer))
  {
    diag.set_source_manager(this);
  }

public:
  SourceManager(SourceManager &&) = default;
  SourceManager &operator=(SourceManager &&) = default;

  // Constructs a SourceManager from a source file.
  //
  // \returns `std::nullopt` if `filename` doesn't exist or is a directory.
  static auto from_file(CompilerDiagnostics &, const fs::path &filename)
    -> std::optional<SourceManager>;

  // Constructs a SourceManager from a UTF-8 string buffer.
  static auto from_buffer(CompilerDiagnostics &, std::string buffer)
    -> SourceManager;

  auto get_diagnostics() const -> CompilerDiagnostics & { return diag; }

  // Returns the text slice represented by a SourceRange.
  auto text_slice(SourceRange) const -> std::string_view;

  // Returns the whole source's text.
  auto full_text() const -> std::string_view { return buffer; }

  // Returns the text line a SourceLocation is in.
  auto text_line(SourceLocation) const -> std::string_view;

  // Returns the line and column number for a SourceLocation.
  auto translate_to_linecolumn(SourceLocation) const
    -> std::pair<size_t, size_t>;

  // Checks whether this SourceManager was loaded from a file.
  auto loaded_from_file() const -> bool;

  // Returns the source's file path.
  auto file_path() const -> const fs::path &;

  // Translates a SourceLocation into its corresponding buffer pointer.
  auto decode_to_raw_ptr(SourceLocation loc) const -> const char *
  {
    return std::next(buffer.data(), loc.offset);
  }
};

// FullSourceLoc - This represents a `SourceLocation` with its respective
// `SourceManager` owner.
struct FullSourceLoc
{
private:
  const SourceManager &src_mgr;
  SourceLocation loc;

public:
  FullSourceLoc() = default;
  FullSourceLoc(const SourceManager &ctx, SourceLocation loc) noexcept
    : src_mgr(ctx), loc(loc)
  {}

  auto get_manager() const -> const SourceManager & { return src_mgr; }
  auto get_location() const -> SourceLocation { return loc; }
  auto full_text() const -> std::string_view { return src_mgr.full_text(); }
  auto text_line() const -> std::string_view { return src_mgr.text_line(loc); }
  auto translate_to_linecolumn() const -> std::pair<size_t, size_t>
  {
    return src_mgr.translate_to_linecolumn(loc);
  }
  auto loaded_from_file() const -> bool { return src_mgr.loaded_from_file(); }
  auto file_path() const -> const fs::path & { return src_mgr.file_path(); }
};

} // namespace cci
