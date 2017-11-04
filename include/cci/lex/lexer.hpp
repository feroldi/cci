#pragma once

#include "cci/basic/source_manager.hpp"
#include <vector>
#include <string_view>

namespace cci::lex {

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
auto to_string(TokenKind K) -> std::string_view;

// Token - A representation of a token as described in the C11 standard.
// TODO: reference token definition in the standard.
class Token
{
  // Token's kind, e.g. Kw_return, identifier etc.
  TokenKind kind;

  // Token's start and end locations on the source file.
  SourceRange range;

public:
  Token(TokenKind k, SourceRange r) : kind(k), range(r) {}

  // Checks whether this token is of kind `K`.
  bool is(TokenKind K) const { return kind == K; }

  // Checks whether this token is not of kind `K`.
  bool is_not(TokenKind K) const { return kind != K; }

  // Checks wether this token is of any kind in `Ks`.
  template <typename... Kinds>
  bool is_one_of(Kinds... Ks) const
  {
    static_assert((std::is_same_v<TokenKind, Kinds> && ...));
    return (is(Ks) || ...);
  }

  // Returns the source location where this token starts.
  auto source_loc() const -> SourceLocation { return range.start; }

  // Returns the source range where this token is in the buffer.
  auto source_range() const -> SourceRange { return range; }

  // Returns the name of this token's kind.
  auto spelling() const -> std::string_view { return to_string(kind); }
};

// TokenStream - This implements a tokenizer for the C language. It's
// an iterable sequence of `Token`s.
class TokenStream
{
  // Sequence of tokens corresponding to a source file.
  std::vector<Token> tokens;

  TokenStream(std::vector<Token> tokens) : tokens(std::move(tokens)) {}

public:
  using iterator = std::vector<Token>::iterator;
  using const_iterator = std::vector<Token>::const_iterator;

  // Tokenizes the content of a buffer.
  //
  // \returns A TokenStream containing all the tokens from a buffer.
  static auto tokenize(const char *begin, const char *end) -> TokenStream;

  auto begin() const -> const_iterator { return tokens.begin(); }
  auto end() const -> const_iterator { return tokens.end(); }
};

} // namespace cci::lex
