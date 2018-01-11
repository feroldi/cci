#pragma once

#include "cci/basic/source_manager.hpp"
#include "cci/basic/diagnostics.hpp"
#include <string_view>
#include <vector>

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

  // C11 6.4.2 Identifiers
  identifier,

  // TODO Constant
  // TODO String-literal
  // TODO Punctuator

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
  // Token's syntactic category, e.g. Kw_return, identifier etc.
  TokenKind kind;

  // Token's start and end locations on the source file.
  SourceRange range;

  Token(TokenKind k, SourceRange r) noexcept : kind(k), range(r) {}

  // Checks whether this token is of kind `k`.
  bool is(TokenKind k) const { return kind == k; }

  // Checks wether this token is of any kind in `Ks`.
  template <typename... Kinds>
  bool is_one_of(Kinds... ks) const
  {
    static_assert((std::is_same_v<TokenKind, Kinds> && ...));
    return (is(ks) || ...);
  }

  // Returns the source location at which this token starts.
  auto source_location() const -> SourceLocation { return range.start; }

  // Returns a `SourceRange` for the token's text (spelling).
  auto source_range() const -> SourceRange { return range; }
};

// TokenStream - This implements a tokenizer for the C language. It's
// an iterable sequence of `Token`s.
class TokenStream
{
  // Sequence of tokens corresponding to a source file.
  std::vector<Token> token_buffer;

  // Index of token returned by `consume()`.
  size_t cur_tok{0};

  TokenStream(std::vector<Token> tokens) noexcept : token_buffer(std::move(tokens)) {}

public:
  // Tokenizes the content of a buffer.
  //
  // \returns A TokenStream containing all the tokens from a buffer.
  static auto tokenize(CompilerDiagnostics &, const char *begin,
                       const char *end) -> TokenStream;

  // Returns and consumes the next token in the stream.
  //
  // \returns eof token when there isn't any token left.
  auto consume() -> Token;

  // Returns (but doesn't consume) the `ahead`th token.
  //
  // Behavior is undefined if stream is empty, or `ahead >= size()`.
  auto peek(size_t ahead = 0u) const -> Token;

  // Returns the amount of remaining tokens in the stream.
  auto size() const -> size_t;

  // Checks whether the stream is empty.
  auto empty() const -> bool { return size() == 0u; }
};

} // namespace cci
