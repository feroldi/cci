#include "cci/lex/lexer.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/util/contracts.hpp"
#include <algorithm>
#include <cassert>
#include <utility>

namespace cci {
namespace {

// token:
//   keyword
//   identifier
//   constant
//   string-literal
//   punctuator

static constexpr TokenKind C11_KEYWORDS[]{
  TokenKind::kw_auto,           TokenKind::kw_break,
  TokenKind::kw_case,           TokenKind::kw_char,
  TokenKind::kw_const,          TokenKind::kw_continue,
  TokenKind::kw_default,        TokenKind::kw_do,
  TokenKind::kw_double,         TokenKind::kw_else,
  TokenKind::kw_enum,           TokenKind::kw_extern,
  TokenKind::kw_float,          TokenKind::kw_for,
  TokenKind::kw_goto,           TokenKind::kw_if,
  TokenKind::kw_inline,         TokenKind::kw_int,
  TokenKind::kw_long,           TokenKind::kw_register,
  TokenKind::kw_restrict,       TokenKind::kw_return,
  TokenKind::kw_short,          TokenKind::kw_signed,
  TokenKind::kw_sizeof,         TokenKind::kw_static,
  TokenKind::kw_struct,         TokenKind::kw_switch,
  TokenKind::kw_typedef,        TokenKind::kw_union,
  TokenKind::kw_unsigned,       TokenKind::kw_void,
  TokenKind::kw_volatile,       TokenKind::kw_while,
  TokenKind::kw__Alignas,       TokenKind::kw__Alignof,
  TokenKind::kw__Atomic,        TokenKind::kw__Bool,
  TokenKind::kw__Complex,       TokenKind::kw__Generic,
  TokenKind::kw__Imaginary,     TokenKind::kw__Noreturn,
  TokenKind::kw__Static_assert, TokenKind::kw__Thread_local,
};

struct LexerContext
{
  CompilerDiagnostics &diag;
  const char *buffer_begin; //< Iterator to the start of the buffer.
  const char *buffer_end; //< Iterator to the end of the buffer.
  std::vector<Token> token_buffer; //< Tokenized buffer.

  LexerContext(CompilerDiagnostics &cd, const char *buf_begin,
               const char *buf_end) noexcept
    : diag(cd), buffer_begin(buf_begin), buffer_end(buf_end)
  {}

  // Calculates a `SourceLocation` for a pointer into the buffer.
  auto location_for_ptr(const char *buffer_ptr) const -> SourceLocation
  {
    return SourceLocation(static_cast<size_t>(buffer_ptr - buffer_begin));
  }

  // Adds a token to the token buffer, with `kind` and `SourceLocation`
  // calculated from `begin` and `end`.
  void add_token(TokenKind kind, const char *tok_begin, const char *tok_end)
  {
    SourceLocation loc_start(static_cast<size_t>(tok_begin - buffer_begin));
    SourceLocation loc_end(static_cast<size_t>(tok_end - buffer_begin));
    token_buffer.emplace_back(kind, SourceRange(loc_start, loc_end));
  }
};

constexpr auto is_nondigit(char C) -> bool
{
  return (C >= 'a' && C <= 'z') || (C >= 'A' && C <= 'Z') || C == '_';
}

constexpr auto is_digit(char C) -> bool { return C >= '0' && C <= '9'; }

constexpr auto is_hexdigit(char C) -> bool
{
  return (C >= '0' && C <= '9') || (C >= 'a' && C <= 'f') ||
         (C >= 'A' && C <= 'F');
}

constexpr auto is_newline(char C) -> bool { return C == '\n' || C == '\r'; }

constexpr auto is_space(char C) -> bool
{
  return C == ' ' || C == '\t' || is_newline(C);
}

// 6.4.1
// keyword: one of
//   auto      else    long      switch    _Atomic
//   break     enum    register  typedef   _Bool
//   case      extern  restrict  union     _Complex
//   char      float   return    unsigned  _Generic
//   const     for     short     void      _Imaginary
//   continue  goto    signed    volatile  _Noreturn
//   default   if      sizeof    while     _Static_assert
//   do        inline  static    _Alignas  _Thread_local
//   double    int     struct    _Alignof
//
// 6.4.2
// identifier-nondigit:
//   nondigit
//   universal-character-name
//   other implementation-defined characters
//
// hex-quad:
//   hexadecimal-digit hexadecimal-digit
//       hexadecimal-digit hexadecimal-digit
//
// universal-character-name:
//     \u hex-quad
//     \U hex-quad  hex-quad
//
// identifier:
//   identifier-nondigit
//   identifier  identifier-nondigit
//   identifier  digit
//
// nondigit: one of
//   _abcdefghijklm
//   nopqrstuvwxyz
//   ABCDEFGHIJKLM
//   NOPQRSTUVWXYZ
//
// digit: one of
//   0123456789

auto is_universal_character_name(const char *begin, const char *end) -> bool
{
  if (std::distance(begin, end) >= 6 && begin[0] == '\\' &&
      (begin[1] == 'u' || begin[1] == 'U'))
  {
    const auto UCN_len = begin[1] == 'u' ? 4 : 8;
    const char *hquads_start = std::next(begin, 2);
    const char *hquads_end = std::find_if_not(
      hquads_start, std::next(hquads_start, UCN_len), is_hexdigit);
    return std::next(hquads_start, UCN_len) == hquads_end;
  }
  return false;
}

auto parse_universal_character_name(LexerContext &, const char *begin,
                                    const char *end) -> const char *
{
  cci_expects(is_universal_character_name(begin, end));
  return std::next(begin, (begin[1] == 'u' ? 4 : 8) + 2);
}

auto parse_identifier(LexerContext &lex, const char *begin, const char *end)
  -> const char *
{
  cci_expects(is_nondigit(begin[0]) || is_universal_character_name(begin, end));

  const char *id_start = begin;
  const char *id_end = [&] {
    auto it = std::next(begin);
    while (it != end)
    {
      if (is_nondigit(*it) || is_digit(*it))
        ++it;
      else if (is_universal_character_name(it, end))
        it = parse_universal_character_name(lex, it, end);
      else
        break;
    }
    return it;
  }();

  for (const TokenKind keyword : C11_KEYWORDS)
  {
    const auto kw_spell = to_string(keyword);

    if (std::equal(id_start, id_end, kw_spell.begin(), kw_spell.end()))
    {
      lex.add_token(keyword, id_start, id_end);
      return id_end;
    }
  }

  lex.add_token(TokenKind::identifier, id_start, id_end);
  return id_end;
}

} // namespace

auto TokenStream::tokenize(CompilerDiagnostics &diag, const char *text_begin,
                           const char *text_end) -> TokenStream
{
  LexerContext lex(diag, text_begin, text_end);

  // Current position into the `text`.
  auto it = std::find_if_not(text_begin, text_end, is_space);

  while (it != text_end)
  {
    // 6.4.2.1 nondigit
    if (is_nondigit(*it) || is_universal_character_name(it, text_end))
    {
      it = parse_identifier(lex, it, text_end);
    }
    else
    {
      diag.report(lex.location_for_ptr(it), diag::err_unknown_character, *it);
      // Skips unknown character.
      std::advance(it, 1);
      continue;
    }

    // Ignores every space in text.
    it = std::find_if_not(it, text_end, is_space);
  }

  lex.add_token(TokenKind::eof, text_end, text_end);
  return TokenStream(std::move(lex.token_buffer));
}

auto TokenStream::consume() -> Token
{
  cci_expects(cur_tok != token_buffer.size());
  return token_buffer[cur_tok++];
}

auto TokenStream::peek(size_t ahead) const -> Token
{
  cci_expects(token_buffer.size() > (cur_tok + ahead));
  return token_buffer[cur_tok + ahead];
}

auto TokenStream::size() const -> size_t
{
  return token_buffer.size() - cur_tok;
}

auto to_string(TokenKind k) -> std::string_view
{
  switch (k)
  {
    case TokenKind::kw_auto:
      return "auto";
    case TokenKind::kw_break:
      return "break";
    case TokenKind::kw_case:
      return "case";
    case TokenKind::kw_char:
      return "char";
    case TokenKind::kw_const:
      return "const";
    case TokenKind::kw_continue:
      return "continue";
    case TokenKind::kw_default:
      return "default";
    case TokenKind::kw_do:
      return "do";
    case TokenKind::kw_double:
      return "double";
    case TokenKind::kw_else:
      return "else";
    case TokenKind::kw_enum:
      return "enum";
    case TokenKind::kw_extern:
      return "extern";
    case TokenKind::kw_float:
      return "float";
    case TokenKind::kw_for:
      return "for";
    case TokenKind::kw_goto:
      return "goto";
    case TokenKind::kw_if:
      return "if";
    case TokenKind::kw_inline:
      return "inline";
    case TokenKind::kw_int:
      return "int";
    case TokenKind::kw_long:
      return "long";
    case TokenKind::kw_register:
      return "register";
    case TokenKind::kw_restrict:
      return "restrict";
    case TokenKind::kw_return:
      return "return";
    case TokenKind::kw_short:
      return "short";
    case TokenKind::kw_signed:
      return "signed";
    case TokenKind::kw_sizeof:
      return "sizeof";
    case TokenKind::kw_static:
      return "static";
    case TokenKind::kw_struct:
      return "struct";
    case TokenKind::kw_switch:
      return "switch";
    case TokenKind::kw_typedef:
      return "typedef";
    case TokenKind::kw_union:
      return "union";
    case TokenKind::kw_unsigned:
      return "unsigned";
    case TokenKind::kw_void:
      return "void";
    case TokenKind::kw_volatile:
      return "volatile";
    case TokenKind::kw_while:
      return "while";
    case TokenKind::kw__Alignas:
      return "_Alignas";
    case TokenKind::kw__Alignof:
      return "_Alignof";
    case TokenKind::kw__Atomic:
      return "_Atomic";
    case TokenKind::kw__Bool:
      return "_Bool";
    case TokenKind::kw__Complex:
      return "_Complex";
    case TokenKind::kw__Generic:
      return "_Generic";
    case TokenKind::kw__Imaginary:
      return "_Imaginary";
    case TokenKind::kw__Noreturn:
      return "_Noreturn";
    case TokenKind::kw__Static_assert:
      return "_Static_assert";
    case TokenKind::kw__Thread_local:
      return "_Thread_local";
    case TokenKind::identifier:
      return "identifier";
    case TokenKind::eof:
      return "<end of input>";
  }

  cci_unreachable();
}

} // namespace cci
