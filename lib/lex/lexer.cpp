#include "cci/lex/lexer.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/util/contracts.hpp"
#include <algorithm>
#include <cassert>
#include <memory>
#include <utility>

namespace cci {
namespace diag {
enum Lex
{
#define DIAG(CODE, LEVEL, FORMAT) CODE,
#include "cci/basic/diagnostics_lex.inc"
#undef DIAG
};
} // namespace diag

template <>
struct diagnostics_error_code<diag::Lex>
{
  constexpr static auto info(diag::Lex code) -> ErrorCodeInfo
  {
#define DIAG(CODE, LEVEL, FORMAT)                                              \
  case diag::CODE: return {LEVEL, FORMAT};
    switch (code)
    {
#include "cci/basic/diagnostics_lex.inc"
    }
#undef DIAG
  }
};

template <>
struct is_diagnostics_error_code<diag::Lex> : std::true_type
{
};

// token:
//   keyword
//   identifier
//   constant
//   string-literal
//   punctuator

static constexpr TokenKind KEYWORD_KINDS[]{
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

constexpr auto is_nondigit(char C) -> bool
{
  return (C >= 'a' && C <= 'z') || (C >= 'A' && C <= 'Z') || C == '_';
}

constexpr auto is_digit(char C) -> bool { return C >= '0' && C <= '9'; }

[[maybe_unused]]
constexpr auto is_nonzero_digit(char C) -> bool { return C >= '1' && C <= '9'; }

[[maybe_unused]]
constexpr auto is_octdigit(char C) -> bool { return C >= '0' && C <= '7'; }

constexpr auto is_hexdigit(char C) -> bool
{
  return (C >= '0' && C <= '9') || (C >= 'a' && C <= 'f') ||
         (C >= 'A' && C <= 'F');
}

constexpr auto is_newline(char C) -> bool { return C == '\n' || C == '\r'; }

constexpr auto is_whitespace(char C) -> bool
{
  return C == ' ' || C == '\t' || C == '\v' || C == '\f' || is_newline(C);
}

constexpr auto hexdigit_value(char C) -> uint32_t
{
  if (is_hexdigit(C))
  {
    if (C >= '0' && C <= '9')
      return static_cast<uint32_t>(C - '0');
    if (C >= 'a' && C <= 'f')
      return static_cast<uint32_t>(C - 'a' + 10);
    if (C >= 'A' && C <= 'F')
      return static_cast<uint32_t>(C - 'A' + 10);
    cci_unreachable();
  }
  return -1U;
}

namespace {

// Calculates the size of an escaped new line. Assumes
// that the slash is already consumed.
// Based on Clang's Lexer::getEscapedNewlineSize.
auto size_for_escaped_newline(const char *ptr) -> int64_t
{
  cci_expects(*std::prev(ptr) == '\\');
  int64_t size = 0;

  // Considers only whitespaces.
  while (is_whitespace(ptr[size]))
  {
    ++size;

    // Might be a \v, \t, or something like that.  In case there isn't any
    // newline, this eventually resolves to a zero size.
    if (!is_newline(ptr[size - 1]))
      continue;

    // Consumes a pair of \r\n or \n\r if there is any.
    if (is_newline(ptr[size]) && ptr[size - 1] != ptr[size])
      ++size;

    return size;
  }

  // Not a newline.
  return 0;
}

// Peeks a character from the input stream and returns it, setting
// the size to how many characters are to be skipped over.
// This handles special cases like escaped newlines and trigraphs*.
//
// * TODO: Will handle trigraphs eventually.
auto peek_char_and_size(const char *ptr, int64_t &size, Token *tok = nullptr)
  -> char
{
  if (*ptr == '\\')
  {
    ++ptr;
    ++size;

    // There's no need to escape anything other than whitespaces.
    if (!is_whitespace(*ptr)) return '\\';

    if (int64_t esc_nl_size = size_for_escaped_newline(ptr))
    {
      if (tok) tok->set_flags(Token::IsDirty);
      ptr += esc_nl_size;
      size += esc_nl_size;
      return peek_char_and_size(ptr, size, tok);
    }

    // Not a newline, just a regular whitespace.
    return '\\';
  }

  // Peek a simple character.
  ++size;
  return *ptr;
}

auto consume_char(const char *ptr, int64_t size, Token *tok = nullptr)
  -> const char *
{
  // Consumes a simple character.
  if (size == 1)
    return ptr + size;

  // Otherwise, reparse it to get the right size.
  size = 0;
  peek_char_and_size(ptr, size, tok);
  return ptr + size;
}

// universal-character-name:
//     \u hex-quad
//     \U hex-quad  hex-quad
//
// hex-quad:
//   hexadecimal-digit hexadecimal-digit
//       hexadecimal-digit hexadecimal-digit

// Tries to read a universal character name.
auto try_read_ucn(Lexer &lex, const char *start_ptr, int64_t &size,
                  Token *tok = nullptr) -> uint32_t
{
  const auto kind = peek_char_and_size(start_ptr, size, tok);
  const int num_hexdigits = kind == 'u' ? 4 : kind == 'U' ? 8 : 0;

  if (num_hexdigits == 0)
    return 0;

  auto cur_ptr = start_ptr + size;
  uint32_t code_point = 0;

  for (int i = 0; i < num_hexdigits; ++i)
  {
    int64_t c_size = 0;
    const char c = peek_char_and_size(cur_ptr, c_size, tok);
    const uint32_t value = hexdigit_value(c);
    if (value == -1U)
    {
      lex.diag.report(lex.location_for_ptr(cur_ptr),
                      diag::warn_ucn_incomplete);
      return 0;
    }
    code_point <<= 4;
    code_point += value;
    cur_ptr += c_size;
    size += c_size;
  }

  const auto slash_loc = lex.location_for_ptr(start_ptr - 1);

  // C11 6.4.3/2: A universal character name shall not specify a character
  // whose short identifier is less than 00A0 other than 0024 ($), 0040 (@),
  // or 0060 ('), nor one in the range D800 through DFFF inclusive.
  if (code_point < 0xA0)
  {
    if (code_point != 0x24 && code_point != 0x40 && code_point != 0x60)
    {
      lex.diag.report(slash_loc, diag::err_ucn_control_character);
      return 0;
    }
  }
  else if (code_point >= 0xD800 && code_point <= 0xDFFF)
  {
    lex.diag.report(slash_loc, diag::err_ucn_invalid);
    return 0;
  }

  if (tok) tok->set_flags(Token::HasUCN);
  return code_point;
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

// Parses an identifier
//
// Assumes that the identifier's head is already parsed.
auto parse_identifier(Lexer &lex, const char *cur_ptr, Token &result) -> bool
{
  char c = *cur_ptr;

  while (is_nondigit(c) || is_digit(c))
    c = *cur_ptr++;

  --cur_ptr;

  // Simple ASCII identifier, no need to parse any UCN.
  if (c != '\\')
  {
    lex.finish_lexing_token(result, cur_ptr, TokenKind::raw_identifier);
    const auto tok_spell = lex.source_mgr.text_slice(result.range);
    for (const TokenKind kw : KEYWORD_KINDS)
    {
      const auto kw_spell = to_string(kw);
      if (tok_spell == kw_spell)
      {
        result.kind = kw;
        break;
      }
    }
  }
  else
  {
    int64_t size = 0;
    c = peek_char_and_size(cur_ptr, size);
    cur_ptr = consume_char(cur_ptr, size, &result);
    while (true)
    {
      if (size = 0; c == '\\' && try_read_ucn(lex, cur_ptr, size, &result))
      {
        cur_ptr += size;
        c = peek_char_and_size(cur_ptr, size);
        continue;
      }
      else if (!(is_nondigit(c) || is_digit(c)))
        break;

      c = peek_char_and_size(cur_ptr, size, &result);
      while (is_nondigit(c) || is_digit(c))
      {
        cur_ptr = consume_char(cur_ptr, size);
        c = peek_char_and_size(cur_ptr, size, &result);
      }
    }

    lex.finish_lexing_token(result, cur_ptr, TokenKind::identifier);
  }

  return true;
}

// integer-constant:
//   decimal-constant integer-suffix[opt]
//   octal-constant integer-suffix[opt]
//   hexadecimal-constant integer-suffix[opt]
//
// decimal-constant:
//   nonzero-digit
//   decimal-constant digit
//
// octal-constant:
//   0
//   octal-constant octal-digit
//
// hexadecimal-constant:
//   hexadecimal-prefix hexadecimal-digit
//   hexadecimal-constant hexadecimal-digit
//
// hexadecimal-prefix: one of
//   0x 0X
//
// nonzero-digit: one of
//   1 2 3 4 5 6 7 8
//
// octal-digit: one of
//   0 1 2 3 4 5 6 7
//
// hexadecimal-digit: one of
//   0 1 2 3 4 5 6 7
//   a b c d e f
//   A B C D E F
//
// integer-suffix:
//   unsigned-suffix long-suffix[opt]
//   unsigned-suffix long-long-suffix
//   long-suffix unsigned-suffix[opt]
//   long-long-suffix unsigned-suffix[opt]
//
// unsigned-suffix: one of
//   u U
//
// long-suffix: one of
//   l L
//
// long-long-suffix: one of
//   ll LL

//auto parse_integer_constant(Lexer &, const char *cur_ptr) -> const char *
//{
//  cci_unreachable();
//}

} // namespace

auto Lexer::lex(Token &result) -> bool
{
  auto cur_ptr = buffer_ptr =
    std::find_if_not(buffer_ptr, buffer_end, is_whitespace);
  int64_t size = 0;
  char ch = peek_char_and_size(cur_ptr, size, &result);
  std::advance(cur_ptr, size);

  switch (ch)
  {
    case '\0':
      return false;

    case '\\':
      if (size = 0; try_read_ucn(*this, cur_ptr, size, &result))
        return parse_identifier(*this, buffer_ptr, result);
      else
        cci_unreachable();

    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
    case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
    case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
    case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':
    case 'X': case 'Y': case 'Z': case '_':
      return parse_identifier(*this, cur_ptr, result);

    default:
      diag.report(location_for_ptr(buffer_ptr), diag::err_unknown_character, ch);
      break;
  }

  return true;
}

auto TokenStream::tokenize(const SourceManager &source_mgr) -> TokenStream
{
  return TokenStream(Lexer(source_mgr));
}

auto TokenStream::peek() -> Token
{
  if (!cur_tok)
  {
    if (Token tok; lexer.lex(tok))
      cur_tok = tok;
    else
      cur_tok = Token(TokenKind::eof, SourceLocation());
  }

  return *cur_tok;
}

auto TokenStream::consume() -> Token
{
  cci_expects(!empty());
  Token tok = peek();
  cur_tok.reset();
  return tok;
}

auto TokenStream::empty() -> bool
{
  return peek().is(TokenKind::eof);
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
    case TokenKind::raw_identifier:
      return "identifier";
    case TokenKind::integer_constant:
      return "integer constant";
    case TokenKind::eof:
      return "<end of input>";
  }

  cci_unreachable();
}

} // namespace cci
