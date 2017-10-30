#pragma once

#include <vector>

namespace sunfyre {
class SourceManager;
class SourceLocation;
class SourceRange;
}

namespace sunfyre::lex {

// TokenKind - This represents the kind of a token, e.g. identifier,
// keyword etc.
enum class TokenKind
{
  // Keywords
  Kw_auto,
  Kw_break,
  Kw_case,
  Kw_char,
  Kw_const,
  Kw_continue,
  Kw_default,
  Kw_do,
  Kw_double,
  Kw_else,
  Kw_enum,
  Kw_extern,
  Kw_float,
  Kw_for,
  Kw_goto,
  Kw_if,
  Kw_inline,
  Kw_int,
  Kw_long,
  Kw_register,
  Kw_restrict,
  Kw_return,
  Kw_short,
  Kw_signed,
  Kw_sizeof,
  Kw_static,
  Kw_struct,
  Kw_switch,
  Kw_typedef,
  Kw_union,
  Kw_unsigned,
  Kw_void,
  Kw_volatile,
  Kw_while,
  Kw__Alignas,
  Kw__Alignof,
  Kw__Atomic,
  Kw__Bool,
  Kw__Complex,
  Kw__Generic,
  Kw__Imaginary,
  Kw__Noreturn,
  Kw__Static_assert,
  Kw__Thread_local,

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
// For example, the name of TokenKind::Kw_auto is "auto",
// TokenKind::identifier's name is "identifier", TokenKind::plusplus's
// name is "++" etc.
auto to_string(TokenKind K) -> const char *;

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
  bool is_any_of(Kinds... Ks) const
  {
    static_assert((std::is_same_v<TokenKind, Kinds> && ...));
    return (is(Ks) || ...);
  }

  // Checks wether this token is not of any kind in `Ks`.
  template <typename... Kinds>
  bool is_not_any_of(Kinds... Ks) const
  {
    static_assert((std::is_same_v<TokenKind, Kinds> && ...));
    return (is_not(Ks) && ...);
  }

  // Returns the source location where this token starts.
  auto start_loc() const -> SourceLocation { return range.begin() };

  // Returns the text from where this token was parsed.
  auto text(const SourceManager &) const -> std::string_view;

  // Returns the name of this token's kind.
  auto spelling() const -> const char * { return to_string(kind); }
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

  // Tokenizes the content held by a SourceManager.
  //
  // \returns A TokenStream containing all the tokens from a source file.
  static auto tokenize(const SourceManager &SM) -> TokenStream;

  auto begin() const -> iterator { return tokens.begin(); }
  auto end() const -> iterator { return tokens.end(); }

  auto cbegin() const -> const_iterator { return tokens.cbegin(); }
  auto cend() const -> const_iterator { return tokens.cend(); }
};

} // namespace sunfyre::lex
