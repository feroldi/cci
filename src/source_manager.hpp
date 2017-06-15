#pragma once

#include <string>
#include <vector>
#include "cpp/contracts.hpp"
#include "cpp/string_view.hpp"

namespace ccompiler
{

using SourceLocation = const char*;

struct SourceRange
{
  explicit SourceRange() = default;

  SourceRange(SourceLocation begin, SourceLocation end)
    : first{begin}, last{end}
  {
    Ensures(this->begin() <= this->end());
  }

  explicit SourceRange(SourceLocation loc) noexcept
    : first{loc}, last{loc}
  {}

  explicit SourceRange(string_view content) noexcept
    : first{content.begin()}, last{content.end()}
  {}

  constexpr auto begin() const noexcept -> SourceLocation
  {
    return this->first;
  }

  constexpr auto end() const noexcept -> SourceLocation
  {
    return this->last;
  }

  constexpr auto size() const noexcept -> std::size_t
  {
    return static_cast<std::size_t>(this->end() - this->begin());
  }

  constexpr auto operator[](size_t index) const noexcept -> char
  {
    return *(this->begin() + index);
  }

  constexpr auto contains(SourceLocation loc) const -> bool
  {
    return this->begin() <= loc && this->end() >= loc;
  }

  constexpr auto is_sub_of(SourceRange range) const -> bool
  {
    return this->begin() >= range.begin() && this->end() <= range.end();
  }

  operator string_view() const
  {
    return string_view{this->begin(), this->size()};
  }

  auto to_string() const -> std::string
  {
    return std::string(this->begin(), this->end());
  }

private:
  SourceLocation first;
  SourceLocation last;
};

struct SourceManager
{
  struct LineColumn
  {
    size_t line_no;
    size_t column_no;
  };

  struct SourceLineCache
  {
    std::vector<SourceRange> offsets;

    explicit SourceLineCache() = default;
    explicit SourceLineCache(SourceRange range);
  };

  const std::string filename;

  // TODO: change source_name's type to fs::path
  explicit SourceManager(std::string filename, std::string content)
    : filename{std::move(filename)}, content{std::move(content)},
      line_cache{SourceRange(this->content)}
  {}

  SourceManager(const SourceManager&) = delete;
  SourceManager& operator= (const SourceManager&) = delete;

  SourceManager(SourceManager&&) = default;
  SourceManager& operator= (SourceManager&&) = default;

  auto linecol_from_location(SourceLocation) const -> LineColumn;
  auto line_range_from_location(SourceLocation) const -> SourceRange;
  auto line_range_at(size_t line_no) const -> SourceRange;

  // TODO: change filepath's type to fs::path
  // Reads content from a file and returns a valid SourceManager.
  // Throws std::runtime_error if given filepath is invalid.
  static auto from_path(std::string filepath) -> SourceManager;

  auto begin() const -> SourceLocation
  {
    string_view v = this->content;
    return v.begin();
  }

  auto end() const -> SourceLocation
  {
    string_view v = this->content;
    return v.end();
  }

  auto range() const -> SourceRange
  {
    return SourceRange(this->begin(), this->end());
  }

private:
  const std::string content;
  const SourceLineCache line_cache;
};

} // namespace ccompiler
