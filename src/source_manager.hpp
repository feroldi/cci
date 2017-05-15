#pragma once

#include <string>
#include <vector>
#include "cpp/contracts.hpp"
#include "cpp/string_view.hpp"

using SourceLocation = const char*;

struct SourceRange
{
private:
  SourceLocation first;
  SourceLocation last;

public:
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

  auto dump_to_string() const -> std::string
  {
    return std::string(this->begin(), this->end());
  }
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
  private:
    std::vector<SourceRange> line_offsets;

  public:
    explicit SourceLineCache() = default;
    explicit SourceLineCache(SourceRange range);

    auto linecol_from_location(SourceLocation) const -> LineColumn;
    auto line_from_location(SourceLocation) const -> SourceRange;
    auto line_at(size_t line_no) const -> SourceRange;
  };

private:
  const std::string source_name;
  const std::string source_content;
  const SourceLineCache line_cache;

public:
  // TODO: change source_name's type to fs::path
  explicit SourceManager(std::string source_name, std::string content)
    : source_name{std::move(source_name)}, source_content{std::move(content)},
      line_cache{SourceRange(this->source_content)}
  {}

  SourceManager(const SourceManager&) = delete;
  SourceManager& operator= (const SourceManager&) = delete;

  SourceManager(SourceManager&&) = default;
  SourceManager& operator= (SourceManager&&) = default;

  // Reads content from a file and returns a valid SourceManager.
  // Throws std::runtime_error if given filepath is invalid.
  static auto from_path(std::string filepath) -> SourceManager;

  // Accesses lines cache.
  auto lines() const -> const SourceLineCache& { return this->line_cache; }
  auto content() const -> string_view { return this->source_content; }
};

