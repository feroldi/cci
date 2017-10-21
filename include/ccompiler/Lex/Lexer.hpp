#pragma once

namespace ccompiler {
class SourceManager;
}

namespace ccompiler::lex {

// Token - A representation of a token as described in the C11 standard.
// TODO: reference token definition in the standard.
struct Token
{
  // Token kind (e.g. Kw_return, identifier etc).
  TokenKind kind;

  // Token's location on the source file.
  SourceRange loc;

  explicit Token(TokenKind k, SourceRange r) : kind(k), loc(r) {}

  // Checks whether this token if of a specific kind.
  bool is(TokenKind K) const { return kind == K; }

  template <typename Kinds...>
  bool is_any_of(Kinds... Ks) const { return (is(Ks) || ...); }
};

// TokenStream - This implements a tokenizer for the C language. It's
// an iterable sequence of `Token`s.
class TokenStream
{
  // Sequence of tokens corresponding to a source file.
  std::vector<Token> tokens; 

  TokenStream(std::vector<Token> tokens, const SourceManager &SM) :
    tokens(std::move(tokens)), src_mgr(SM) {}

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

} // namespace ccompiler::lex
