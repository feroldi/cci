#pragma once

#include "cci/basic/source_manager.hpp"
#include "cci/basic/diagnostics.hpp"
#include <string_view>
#include <memory>


namespace cci {

// TokenKind - This represents the kind of a token, e.g. identifier,
// keyword etc.
enum class TokenKind
{
  // Keywords
  kw_auto,
  kw_break,
  kw_case,
  kw_char,
  kw_const,
  kw_continue,
  kw_default,
  kw_do,
  kw_double,
  kw_else,
  kw_enum,
  kw_extern,
  kw_float,
  kw_for,
  kw_goto,
  kw_if,
  kw_inline,
  kw_int,
  kw_long,
  kw_register,
  kw_restrict,
  kw_return,
  kw_short,
  kw_signed,
  kw_sizeof,
  kw_static,
  kw_struct,
  kw_switch,
  kw_typedef,
  kw_union,
  kw_unsigned,
  kw_void,
  kw_volatile,
  kw_while,
  kw__Alignas,
  kw__Alignof,
  kw__Atomic,
  kw__Bool,
  kw__Complex,
  kw__Generic,
  kw__Imaginary,
  kw__Noreturn,
  kw__Static_assert,
  kw__Thread_local,

  // 6.4.2 Identifiers.
  identifier,

  // 6.4.4 Constants.
  numeric_constant,

  // 6.4.4.4 Character constants.
  char_constant,
  utf8_char_constant,
  utf16_char_constant,
  utf32_char_constant,
  wide_char_constant,

  // 6.4.5 String literals.
  string_literal,
  utf8_string_literal,
  utf16_string_literal,
  utf32_string_literal,
  wide_string_literal,

  // 6.4.6 Punctuators.
  l_bracket,
  r_bracket,
  l_paren,
  r_paren,
  l_brace,
  r_brace,
  period,
  arrow,
  plusplus,
  minusminus,
  ampersand,
  star,
  plus,
  minus,
  tilde,
  exclama,
  slash,
  percent,
  lessless,
  greatergreater,
  less,
  greater,
  lesslessequal,
  greatergreaterequal,
  equalequal,
  exclamaequal,
  caret,
  pipe,
  ampamp,
  pipepipe,
  question,
  colon,
  semi,
  ellipsis,
  equal,
  starequal,
  slashequal,
  percentequal,
  plusequal,
  minusequal,
  lessequal,
  greaterequal,
  ampequal,
  caretequal,
  pipeequal,
  comma,
  hash,
  hashhash,

  // TODO String-literal.

  // Some stray character.
  unknown,

  // End of input.
  eof,
};

// Returns a string representation of a TokenKind.
//
// For instance, the name of TokenKind::kw_auto is "auto",
// TokenKind::identifier's name is "identifier", TokenKind::plusplus's
// name is "++" etc.
auto to_string(TokenKind) -> std::string_view;

// Token - A representation of a token as described in the C11 standard.
struct Token
{
  enum TokenFlags
  {
    None = 0,
    HasUCN = 1 << 0, //< Contains universal character names.
    IsDirty = 1 << 1, //< Contains escaped new lines or trigraphs.
    IsLiteral = 1 << 2, //< Is a string/char literal, or numeric constant.
  };

  // Token's syntactic category, e.g. kw_return, identifier etc.
  TokenKind kind;

  // Token's start and end locations on the source file.
  SourceRange range;

  uint8_t flags = TokenFlags::None;

  Token() = default;
  Token(TokenKind k, SourceRange r) noexcept : kind(k), range(r) {}

  // Checks whether this token is of kind `k`.
  bool is(TokenKind k) const { return kind == k; }

  // Checks wether this token is of any kind in `Ks`.
  template <typename... Kinds>
  bool is_one_of(const Kinds... ks) const
  {
    static_assert((std::is_same_v<TokenKind, Kinds> && ...));
    return (is(ks) || ...);
  }

  // Returns the source location at which this token starts.
  auto location() const -> SourceLocation { return range.start; }

  // Returns a `SourceRange` for the token's text (spelling).
  auto source_range() const -> SourceRange { return range; }

  auto spelling(const SourceManager &src_mgr) const -> std::string_view
  {
    return src_mgr.text_slice(this->source_range());
  }

  void set_flags(TokenFlags fs) { flags |= fs; }
  void clear_flags(TokenFlags fs) { flags &= ~fs; }

  bool has_UCN() const { return flags & TokenFlags::HasUCN; }
  bool is_dirty() const { return flags & TokenFlags::IsDirty; }
};

struct Lexer
{
  const SourceManager &source_mgr; //< Input to be tokenized.
  CompilerDiagnostics &diag;

  const char *buffer_begin; //< Iterator to the start of the buffer.
  const char *buffer_end; //< Iterator to the end of the buffer.
  const char *buffer_ptr; //< Current character to be analyzed.

  Lexer(const SourceManager &src_mgr) noexcept
    : source_mgr(src_mgr)
    , diag(src_mgr.get_diagnostics())
    , buffer_begin(src_mgr.full_text().begin())
    , buffer_end(src_mgr.full_text().end())
    , buffer_ptr(buffer_begin)
  {
    cci_expects(buffer_end[0] == '\0');
  }

  void form_token(Token &tok, const char *tok_end, TokenKind kind)
  {
    tok.kind = kind;
    tok.range = range_for_ptrs(buffer_ptr, tok_end);
    buffer_ptr = tok_end;
  }

  // Parses the next token in the input stream.
  auto lex(Token &) -> bool;

  auto location_for_ptr(const char *ptr) const -> SourceLocation
  {
    return SourceLocation::from_ptrs(buffer_begin, ptr);
  }

  auto range_for_ptrs(const char *start, const char *end) const -> SourceRange
  {
    return {location_for_ptr(start), location_for_ptr(end)};
  }
};


class TokenStream
{
  Lexer lexer;
  std::optional<Token> cur_tok;

  explicit TokenStream(Lexer lex) noexcept : lexer(std::move(lex)), cur_tok() {}

public:
  static auto tokenize(const SourceManager &) -> TokenStream;

  auto peek() -> Token;
  auto consume() -> Token;
  auto empty() -> bool;
};


} // namespace cci
