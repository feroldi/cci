#pragma once

#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/util/contracts.hpp"
#include "cci/lex/token.hpp"
#include <string>
#include <string_view>

namespace cci {

// Lexer - The C11 lexer.
//
// The lexer works by imperatively tokenizing the input stream (the contents of
// a SourceManager in this case), instead of parsing every token and producing
// a vector all at once. This is preferable, as a Token isn't space efficient,
// and the parser works with one token at a time, which makes this approach a
// lot more appealing.
struct Lexer
{
private:
  const SourceManager &source_mgr; //< Input stream.
  CompilerDiagnostics &diags;

  const char *buffer_begin; //< Iterator into the start of the buffer.
  const char *buffer_end; //< Iterator into the end of the buffer.
  const char *buffer_ptr; //< Current position into the buffer to be analyzed.

public:
  explicit Lexer(const SourceManager &src_mgr)
    : source_mgr(src_mgr)
    , diags(src_mgr.diagnostics())
    , buffer_begin(src_mgr.full_text().begin())
    , buffer_end(src_mgr.full_text().end())
    , buffer_ptr(buffer_begin)
  {
    // Having a null character at the end of the input makes lexing a lot
    // easier.
    cci_expects(buffer_end[0] == '\0');
  }

  auto source_manager() const -> const SourceManager & { return source_mgr; }
  auto diagnostics() const -> CompilerDiagnostics & { return diags; }

  // Parses the next token in the input stream.
  //
  // This is where the whole process of tokenization happens. The lexer tries to
  // interpret whatever `buffer_ptr` is currently pointing to, and if it
  // recognizes something it knows about (such as an identifier's head, a
  // numeric constant etc), then it does the appropriate lexical analysis of
  // that token's grammar, and and returns the parsed token.
  // When end of input is reached, this simply returns std::nullopt.
  //
  // \return The parsed token on success, and std::nullopt otherwise.
  auto next_token() -> std::optional<Token>;

  // Translates a buffer pointer into a SourceLocation.
  auto location_for_ptr(const char *ptr) const -> SourceLocation
  {
    return SourceLocation(static_cast<size_t>(ptr - buffer_begin));
  }

  // Tranlates a position in the spelling of a token into the buffer pointer
  // that corresponds to the actual position.
  //
  // This is useful when the resulting spelling of a token doesn't equal the
  // canonical representation in source code, and one needs to know the actual
  // SourceLocation of some given character in the spelling.
  auto character_location(SourceLocation tok_loc, const char *spelling_begin,
                          const char *char_pos) const -> SourceLocation;

  // Computes the spelling of a token, and writes it to the caller's buffer
  // `spelling_buf`. Returns the size in bytes of written data.
  static auto get_spelling_to_buffer(const Token &, char *spelling_buf,
                                     const SourceManager &) -> size_t;

  auto get_spelling(const Token &tok, small_vector_impl<char> &out) const
    -> std::string_view
  {
    out.resize(tok.size());
    size_t spell_length =
      Lexer::get_spelling_to_buffer(tok, out.data(), source_mgr);
    return {out.data(), spell_length};
  }


private:
  auto try_read_ucn(const char *&start_ptr, const char *slash_ptr, Token *tok = nullptr) -> uint32_t;
  auto try_advance_identifier_utf8(const char *&cur_ptr) -> bool;
  auto try_advance_identifier_ucn(const char *&cur_ptr, int64_t size, Token &result) -> bool;

  auto skip_line_comment(const char *cur_ptr) -> const char *;
  auto skip_block_comment(const char *cur_ptr) -> const char *;

  auto lex_identifier(const char *cur_ptr, Token &result) -> bool;
  auto lex_numeric_constant(const char *cur_ptr, Token &result) -> bool;
  auto lex_character_constant(const char *cur_ptr, Token &result, TokenKind char_kind) -> bool;
  auto lex_string_literal(const char *cur_ptr, Token &result, TokenKind str_kind) -> bool;
  auto lex_unicode(const char *cur_ptr, uint32_t code_point, Token &result) -> bool;
  auto lex_token(const char *cur_ptr, Token &result) -> bool;

  // Fitly finalizes the lexing of a token, advancing the buffer pointer past
  // the new token. This is used only by the internals of the lexical analysis.
  void form_token(Token &tok, const char *tok_end, TokenKind kind)
  {
    tok.kind = kind;
    tok.range = {location_for_ptr(buffer_ptr), location_for_ptr(tok_end)};
    buffer_ptr = tok_end;
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
