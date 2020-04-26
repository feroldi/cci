#include "cci/syntax/source_map.hpp"
#include "cci/syntax/char_info.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/unicode.hpp"
#include <algorithm>
#include <cstddef>
#include <memory>
#include <string>
#include <string_view>

namespace cci::srcmap {

FileMap::FileMap(std::string n, std::string s, ByteLoc sl)
    : name(std::move(n)), src(std::move(s)), start_loc(sl)
{
    const auto utf8_bom = "\uFEFF";
    if (this->src_view().substr(0, 3) == utf8_bom)
        this->src.erase(0, 3);

    if (!this->src.empty() && this->src.back() != '\n')
        this->src.push_back('\n');

    this->end_loc = this->start_loc + ByteLoc(this->src.size());

    // Computes the new line and multibyte locations of this source.
    this->lines.push_back(this->start_loc);

    auto it = src_view().begin();
    while (it != src_view().end())
    {
        const char ch = *it;
        const ByteLoc cur_loc =
            this->start_loc + ByteLoc(it - src_view().begin());
        cci_expects(this->contains(cur_loc));
        if (is_ascii(ch))
        {
            if (ch == '\n')
                this->lines.push_back(cur_loc + ByteLoc(1));
            ++it;
        }
        else
        {
            const size_t bytes =
                uni::num_bytes_for_utf8(static_cast<uni::UTF8>(ch));
            this->multibyte_chars.emplace_back(cur_loc, bytes);
            it += bytes;
        }
    }

    cci_ensures(src_view().end() == it);
    cci_ensures(this->end_loc ==
                (this->start_loc + ByteLoc(it - src_view().begin())));
}

auto FileMap::get_line(size_t line_index) const -> std::string_view
{
    cci_expects(line_index < this->lines.size());
    const ByteLoc line_loc = this->lines[line_index] - this->start_loc;
    const ByteLoc line_end = line_index == (this->lines.size() - 1)
                                 ? this->end_loc
                                 : this->lines[line_index + 1];
    const auto count = static_cast<size_t>(line_end - line_loc);
    return this->src_view().substr(static_cast<size_t>(line_loc), count);
}

auto FileMap::lookup_line_idx(ByteLoc loc) const -> size_t
{
    cci_expects(this->contains(loc));
    auto line = std::upper_bound(this->lines.begin(), this->lines.end(), loc);
    cci_ensures(line != this->lines.begin());
    return static_cast<size_t>(std::prev(line) - this->lines.begin());
}

auto SourceMap::create_owned_filemap(std::string name, std::string src)
    -> const FileMap &
{
    auto fm = std::make_unique<FileMap>(std::move(name), std::move(src),
                                        next_start_loc());
    return *this->file_maps.emplace_back(std::move(fm));
}

auto SourceMap::lookup_filemap_idx(ByteLoc loc) const -> size_t
{
    cci_expects(!this->file_maps.empty());
    cci_expects(loc < this->file_maps.back()->end_loc);
    const auto file =
        std::find_if(this->file_maps.begin(), this->file_maps.end(),
                     [&](const auto &fm) { return fm->contains(loc); });
    cci_ensures(file != this->file_maps.end());
    return static_cast<size_t>(file - this->file_maps.begin());
}

auto SourceMap::lookup_filemap(ByteLoc loc) const -> const FileMap &
{
    return *this->file_maps[lookup_filemap_idx(loc)];
}

auto SourceMap::next_start_loc() const -> ByteLoc
{
    // Adding 1 to the next starting location facilitates the distinction
    // between empty file maps.
    return this->file_maps.empty()
               ? ByteLoc(0)
               : this->file_maps.back()->end_loc + ByteLoc(1);
}

auto SourceMap::lookup_line(ByteLoc loc) const
    -> std::pair<const FileMap &, size_t>
{
    const auto &fm = *this->file_maps[lookup_filemap_idx(loc)];
    return {fm, fm.lookup_line_idx(loc)};
}

auto SourceMap::lookup_byte_offset(ByteLoc loc) const
    -> std::pair<const FileMap &, ByteLoc>
{
    const auto &fm = *this->file_maps[lookup_filemap_idx(loc)];
    const auto offset = loc - fm.start_loc;
    return {fm, offset};
}

auto SourceMap::lookup_source_location(ByteLoc loc) const -> SourceLoc
{
    const auto [fm, chloc] = byteloc_to_filemap_charloc(loc);
    const auto line_idx = fm.lookup_line_idx(loc);
    // FIXME: Refinding the file map is unnecessary; make it a FileMap operation.
    const auto line_chloc =
        byteloc_to_filemap_charloc(fm.lines[line_idx]).second;

    const auto line = LineNum(line_idx + 1);
    const auto col = chloc - line_chloc;

    cci_expects(chloc >= line_chloc);

    return SourceLoc{
        .file = fm,
        .line = line,
        .column = col,
    };
}

auto SourceMap::span_to_snippet(ByteSpan range) const -> std::string_view
{
    cci_expects(range.start <= range.end);

    const auto [start_fm, start_offset] = lookup_byte_offset(range.start);
    const auto [end_fm, end_offset] = lookup_byte_offset(range.end);

    cci_expects(&start_fm == &end_fm);

    const FileMap &fm = start_fm;
    return fm.src_view().substr(static_cast<size_t>(start_offset),
                                static_cast<size_t>(end_offset - start_offset));
}

auto SourceMap::byteloc_to_filemap_charloc(ByteLoc loc) const
    -> std::pair<const FileMap &, CharPos>
{
    const FileMap &filemap = *this->file_maps[lookup_filemap_idx(loc)];
    size_t extra_bytes = 0;

    for (const auto &[mbc_loc, mbc_bytes] : filemap.multibyte_chars)
    {
        if (mbc_loc < loc)
        {
            // Count only the extra bytes.
            extra_bytes += mbc_bytes - 1;
            cci_expects(loc >= mbc_loc + ByteLoc(mbc_bytes));
        }
        else
            break;
    }

    cci_ensures(filemap.start_loc + ByteLoc(extra_bytes) <= loc);
    return {filemap, CharPos(loc - filemap.start_loc - ByteLoc(extra_bytes))};
}
} // namespace cci::srcmap
