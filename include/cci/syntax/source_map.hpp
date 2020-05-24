#pragma once

#include "cci/util/contracts.hpp"
#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

namespace cci::syntax {

struct FileMap;

/// A byte offset that represents a byte location of a source code.
enum class ByteLoc : uint32_t
{
};

inline constexpr ByteLoc operator+(ByteLoc lhs, ByteLoc rhs) noexcept
{
    using UnderType = std::underlying_type_t<ByteLoc>;
    return ByteLoc(static_cast<UnderType>(lhs) + static_cast<UnderType>(rhs));
}

inline constexpr ByteLoc operator-(ByteLoc lhs, ByteLoc rhs) noexcept
{
    using UnderType = std::underlying_type_t<ByteLoc>;
    return ByteLoc(static_cast<UnderType>(lhs) - static_cast<UnderType>(rhs));
}

/// A UTF-8 character offset.
//
/// A character offset is not equivalent to a byte offset, because UTF-8
/// characters are multibyte.
enum class CharPos : uint32_t
{
};

inline constexpr CharPos operator+(CharPos lhs, CharPos rhs) noexcept
{
    using UnderType = std::underlying_type_t<CharPos>;
    return CharPos(static_cast<UnderType>(lhs) + static_cast<UnderType>(rhs));
}

inline constexpr CharPos operator-(CharPos lhs, CharPos rhs) noexcept
{
    using UnderType = std::underlying_type_t<CharPos>;
    return CharPos(static_cast<UnderType>(lhs) - static_cast<UnderType>(rhs));
}

/// A representation of a byte-based source code span.
struct ByteSpan
{
    ByteLoc start = ByteLoc(0);
    ByteLoc end = ByteLoc(0);

    ByteSpan() = default;
    ByteSpan(ByteLoc start, ByteLoc end) noexcept : start(start), end(end) {}

    bool operator==(const ByteSpan &other) const
    {
        return start == other.start && end == other.end;
    }
};

/// One-based line number.
enum LineNum : size_t
{
};

/// Information about a source location for diagnostics.
struct SourceLoc
{
    // TODO: Remove this "file" and use a `ByteLoc` instead.
    const FileMap &file; ///< Original source.
    LineNum line;
    CharPos column;
};

static_assert(std::is_trivially_copyable_v<ByteLoc>);
static_assert(std::is_trivially_copyable_v<CharPos>);
static_assert(std::is_trivially_copyable_v<ByteSpan>);

/// A single source in the SourceMap.
//
/// A file map represents a source code, e.g. header files, source files etc.
/// It's used by the `SourceMap` to organize the mapping between the AST and the
/// source code.
struct FileMap
{
    // FIXME: Storing the source here is slow and temporary. Use a buffer for
    // this in the `SourceMap`.
    std::string name;
    std::string src;

    /// The absolute start byte location of this file in a `SourceMap`.
    ByteLoc start_loc;

    /// The absolute end byte location of this file in a `SourceMap`.
    ByteLoc end_loc;

    /// Byte locations of the start of all lines in the source code.
    std::vector<ByteLoc> lines;

    /// Byte locations and sizes of all multibyte characters.
    std::vector<std::pair<ByteLoc, size_t>> multibyte_chars;

    /// Constructs a `FileMap` to be processed by a `SourceMap`.
    //
    /// Line and multibyte character locations are computed here.
    ///
    /// \param name The file name.
    /// \param src The source code content.
    /// \param start_loc The starting byte location for this file map.
    FileMap(std::string name, std::string src, ByteLoc start_loc);

    FileMap(const FileMap &) = delete;
    FileMap &operator=(const FileMap &) = delete;

    FileMap(FileMap &&) = default;
    FileMap &operator=(FileMap &&) = default;

    /// Returns a line from the list of precomputed line-beginnings.
    auto get_line(size_t line_index) const -> std::string_view;

    /// Returns the line index corresponding to `loc`.
    auto lookup_line_idx(ByteLoc loc) const -> size_t;

    /// Returns whether a byte location is contained in this FileMap.
    auto contains(ByteLoc loc) const -> bool
    {
        return loc >= start_loc && loc <= end_loc;
    }

    /// Returns a string view of the source content.
    auto src_view() const { return std::string_view(src); }

    /// Returns an iterator to the beginning of the source content.
    auto src_begin() const { return src.data(); }

    /// Returns an iterator to the end of the source content.
    auto src_end() const { return src.data() + src.size(); }
};

/// A set of FileMaps.
//
/// A source map is a set of `FileMap`s that maps byte and character locations
/// of these in order. Consequently, byte locations implicitly encode to which
/// file map it corresponds. This property facilitates the mapping between
/// tokens and AST nodes, and source code representation.
struct SourceMap
{
private:
    std::vector<std::unique_ptr<FileMap>> file_maps;

public:
    SourceMap() = default;

    SourceMap(const SourceMap &) = delete;
    SourceMap &operator=(const SourceMap &) = delete;

    SourceMap(SourceMap &&) = default;
    SourceMap &operator=(SourceMap &&) = default;

    /// Constructs a new internal `FileMap` with `name` and `src`. Start and end
    /// locations are calculated based on previous existing FileMaps.
    auto create_owned_filemap(std::string name, std::string src)
        -> const FileMap &;

    /// Lookups the FileMap index based on a global ByteLoc.
    auto lookup_filemap_idx(ByteLoc loc) const -> size_t;

    /// Lookups a FileMap based on a global ByteLoc.
    auto lookup_filemap(ByteLoc loc) const -> const FileMap &;

    /// Returns the starting location for the next new FileMap.
    auto next_start_loc() const -> ByteLoc;

    /// For a global position, returns the line it's contained in.
    auto lookup_line(ByteLoc loc) const -> std::pair<const FileMap &, size_t>;

    /// For a global position, computes the local offset within the containing
    /// FileMap.
    auto lookup_byte_offset(ByteLoc loc) const
        -> std::pair<const FileMap &, ByteLoc>;

    /// For a global position, lookups the source location.
    auto lookup_source_location(ByteLoc loc) const -> SourceLoc;

    /// Converts a ByteSpan of code into a string view.
    auto span_to_snippet(ByteSpan r) const -> std::string_view;

    /// Converts a byte position to a character position.
    auto byteloc_to_filemap_charloc(ByteLoc loc) const
        -> std::pair<const FileMap &, CharPos>;

    /// Converts a pointer into the source to a global byte location.
    auto ptr_to_byteloc(ByteLoc file_start_loc, const char *ptr) const
        -> ByteLoc
    {
        const FileMap &file = lookup_filemap(file_start_loc);
        cci_expects(file.start_loc == file_start_loc);
        cci_expects(ptr >= file.src_begin() && ptr <= file.src_end());
        return file.start_loc + ByteLoc(ptr - file.src_begin());
    }
};

} // namespace cci::syntax
