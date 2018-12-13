#pragma once

#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/syntax/token.hpp"
#include "cci/util/contracts.hpp"
#include <string>
#include <string_view>

namespace cci {

/// The scanner transforms a character input stream into a token stream.
struct Scanner
{
private:
    const srcmap::SourceMap &src_map; ///< Source map containing the file map
                                      ///< being scanned.
    srcmap::ByteLoc file_loc; ///< Start location of the file map being scanned.
    diag::Handler &diag;

    const char *buffer_begin; ///< Iterator into the start of the buffer.
    const char *buffer_end; ///< Iterator into the end of the buffer.
    const char *buffer_ptr; ///< Current position into the buffer to be analyzed.

public:
    /// Constructs a scanner for a `FileMap` given by `file_loc`, which will
    /// scan only the area delimited by `buf_begin` and `buf_end` iterators.
    //
    /// This is useful in cases you need to start scanning from a specific
    /// location in the file map.
    ///
    /// \param file_loc Starting byte location of the file map.
    /// \param buf_begin Iterator to the start of the area to be scanned.
    /// \param buf_end Iterator to the end of the are to be scanned.
    /// \param diag The diagnostics handler that will be used to report any
    /// errors.
    Scanner(srcmap::ByteLoc file_loc, const char *buf_begin,
            const char *buf_end, diag::Handler &diag)
        : src_map(diag.source_map())
        , file_loc(file_loc)
        , diag(diag)
        , buffer_begin(buf_begin)
        , buffer_end(buf_end)
        , buffer_ptr(buf_begin)
    {
        // Having a null character at the end of the input makes scanning a lot
        // easier.
        cci_expects(buffer_end[0] == '\0');
    }

    /// Constructs a scanner for a `FileMap`.
    //
    /// \param file The file map to be scanned.
    /// \param diag The diagnostics handler that will be used to report any
    /// errors.
    Scanner(const srcmap::FileMap &file, diag::Handler &diag)
        : Scanner(file.start_loc, file.src_begin(), file.src_end(), diag)
    {}

    /// Scans the next token from the character stream.
    //
    /// Tokens are recognized by applying lexical analysis to the first incoming
    /// characters from the stream. Once a token is fully formed, it is returned
    /// to the caller. The next call to this function will do the same from
    /// where it stopped last time it was called. Thus, the scanner is a stream
    /// of tokens, and can be used concurrently.
    ///
    /// When character stream's end of input is reached, this returns a token
    /// whose category is `Category::eof`, and range is empty.
    ///
    /// Lexical errors aren't fatal, and when they occur, this returns a token
    /// whose category is `Category::invalid`. The next call to this function
    /// will continue after the lexically invalid area.
    ///
    /// \return The next token in the stream.
    auto next_token() -> Token;

    /// Gets the source map associated with this scanner.
    auto source_map() const -> const srcmap::SourceMap &
    {
        return this->src_map;
    }

    /// Gets the diagnostics handler associated with this scanner.
    auto diagnostics() const -> diag::Handler & { return this->diag; }

    /// Translates a file map's source content iterator into an absolute ByteLoc.
    auto location_for_ptr(const char *ptr) const -> srcmap::ByteLoc
    {
        return this->source_map().ptr_to_byteloc(this->file_loc, ptr);
    }

    /// Translates an absolute byte location in the middle of a token's
    /// lexeme/spelling into the source content iterator that corresponds to
    /// that location.
    //
    /// This is useful when the lexeme of a token doesn't match the canonical
    /// representation in source code, and one needs to know the actual ByteLoc
    /// of some given character in the lexeme.
    ///
    /// For instance, trigraphs and escaped new-lines are converted in the
    /// scanning process, therefore their source representation doesn't match
    /// the lexeme. To get the correct byte location of a character in the
    /// lexeme, you need to know the inverse process of this conversion in order
    /// to find the matching position before conversion. This function helps
    /// with that.
    ///
    /// \param tok_loc The absolute byte location of a token.
    /// \param lexeme_begin The beginning of the token's parsed lexeme.
    /// \param char_pos The character position in the lexeme to be translated.
    ///
    /// \return An abosulte ByteLoc corresponding to the source representation
    /// of
    ///         that character position in the lexeme.
    auto character_location(srcmap::ByteLoc tok_loc, const char *lexeme_begin,
                            const char *char_pos) const -> srcmap::ByteLoc;

    /// Converts the source range of a token to its correspoding lexeme.
    //
    /// This process removes any trigraphs or escaped new-lines that may appear
    /// in the token's source range. UCNs are kept unchanged.
    ///
    /// \param tok The token from which to get the lexeme.
    /// \param[out] spelling_buf A pointer to a buffer in which the lexeme will
    /// be
    ///             stored.
    /// \param map The source map that contains the source representation
    ///            of `tok`.
    ///
    /// \return The length of the converted lexeme.
    static auto get_spelling_to_buffer(const Token &tok, char *spelling_buf,
                                       const srcmap::SourceMap &map) -> size_t;

    /// Converts the source range of a token to its correspoding lexeme.
    //
    /// This is similar to `get_spelling_to_buffer`, with the only addition that
    /// the output buffer is a small vector.
    ///
    /// \param tok The token from which to get the lexeme.
    /// \param[out] out The output buffer.
    ///
    /// \return A string view into the lexeme stored in the output buffer.
    auto get_spelling(const Token &tok, small_vector_impl<char> &out) const
        -> std::string_view
    {
        out.resize(tok.size());
        size_t spell_length = Scanner::get_spelling_to_buffer(
            tok, out.data(), this->source_map());
        return {out.data(), spell_length};
    }

private:
    auto try_read_ucn(const char *&start_ptr, const char *slash_ptr,
                      Token *tok = nullptr) -> uint32_t;
    auto try_advance_identifier_utf8(const char *&cur_ptr) -> bool;
    auto try_advance_identifier_ucn(const char *&cur_ptr, int64_t size,
                                    Token &result) -> bool;

    auto skip_line_comment(const char *cur_ptr) -> const char *;
    auto skip_block_comment(const char *cur_ptr) -> const char *;

    auto lex_identifier(const char *cur_ptr, Token &result) -> bool;
    auto lex_numeric_constant(const char *cur_ptr, Token &result) -> bool;
    auto lex_character_constant(const char *cur_ptr, Token &result,
                                Category char_kind) -> bool;
    auto lex_string_literal(const char *cur_ptr, Token &result,
                            Category str_kind) -> bool;
    auto lex_unicode(const char *cur_ptr, uint32_t code_point, Token &result)
        -> bool;
    auto lex_token(const char *cur_ptr, Token &result) -> bool;

    /// Fitly finalizes the scanning of a token, advancing the buffer pointer
    /// past it. This is used only by the internals of the lexical analysis.
    void form_token(Token &tok, const char *tok_end, Category category)
    {
        tok.category_ = category;
        tok.range = {location_for_ptr(buffer_ptr), location_for_ptr(tok_end)};
        buffer_ptr = tok_end;
    }

    auto report(const char *loc_ptr, diag::Diag msg) const
        -> diag::DiagnosticBuilder
    {
        return this->diag.report(
            this->source_map().ptr_to_byteloc(this->file_loc, loc_ptr), msg);
    }
};

constexpr inline auto hexdigit_value(char C) -> uint32_t
{
    if (C >= '0' && C <= '9')
        return static_cast<uint32_t>(C - '0');
    if (C >= 'a' && C <= 'f')
        return static_cast<uint32_t>(C - 'a' + 10);
    if (C >= 'A' && C <= 'F')
        return static_cast<uint32_t>(C - 'A' + 10);
    return -1U;
}

} // namespace cci
