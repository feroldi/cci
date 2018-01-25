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

// The following helper functions are based off on Clang's lexer implementation.
//
// TODO: These helper functions would better benefit from more comments
// and documentation.

// Calculates the size of an escaped new line. Assumes that the slash is already
// consumed.
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

constexpr bool is_trivial_character(char c)
{
  return c != '\\';
}

// Peeks a character from the input stream and returns it, setting the size to
// how many characters are to be skipped over. This handles special cases like
// escaped newlines and trigraphs*.
//
// * TODO: Will handle trigraphs eventually.
auto peek_char_and_size_nontrivial(const char *ptr, int64_t &size,
                                   Token *tok = nullptr) -> char
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
      return peek_char_and_size_nontrivial(ptr, size, tok);
    }

    // Not a newline, just a regular whitespace.
    return '\\';
  }

  // TODO: Trigraphs.

  // Peek a simple character.
  ++size;
  return *ptr;
}

auto peek_char_advance(const char *&ptr, Token &tok) -> char
{
  if (is_trivial_character(*ptr)) return *ptr++;
  int64_t size = 0;
  char c = peek_char_and_size_nontrivial(ptr, size, &tok);
  ptr += size;
  return c;
}

auto peek_char_and_size(const char *ptr, int64_t &size)
  -> char
{
  if (is_trivial_character(*ptr))
  {
    size = 1;
    return *ptr;
  }
  size = 0;
  return peek_char_and_size_nontrivial(ptr, size, nullptr);
}

auto consume_char(const char *ptr, int64_t size, Token &tok)
  -> const char *
{
  // Consumes a simple character.
  if (size == 1)
    return ptr + size;

  // Otherwise, reparse it to get the right size.
  size = 0;
  peek_char_and_size_nontrivial(ptr, size, &tok);
  return ptr + size;
}

// FIXME: missing standard reference.
// universal-character-name:
//     \u hex-quad
//     \U hex-quad  hex-quad
//
// hex-quad:
//   hexadecimal-digit hexadecimal-digit
//       hexadecimal-digit hexadecimal-digit

// Tries to read a universal character name.
auto try_read_ucn(Lexer &lex, const char *&start_ptr, const char *slash_ptr,
                  Token *tok = nullptr) -> uint32_t
{
  int64_t char_size = 0;
  const auto kind = peek_char_and_size(start_ptr, char_size);
  const int num_hexdigits = kind == 'u' ? 4 : kind == 'U' ? 8 : 0;

  if (num_hexdigits == 0)
    return 0;

  auto cur_ptr = start_ptr + char_size;
  const auto slash_loc = lex.location_for_ptr(slash_ptr);
  uint32_t code_point = 0;

  // Parses the UCN, ignoring any escaped newlines.
  for (int i = 0; i < num_hexdigits; ++i)
  {
    const char c = peek_char_and_size(cur_ptr, char_size);
    const uint32_t value = hexdigit_value(c);
    if (value == -1U)
    {
      lex.diag.report(slash_loc, diag::warn_ucn_incomplete);
      return 0;
    }
    code_point <<= 4;
    code_point += value;
    cur_ptr += char_size;
  }

  // Take into account that this token might have escaped newlines,
  // so make any needed changes to tok. If no token is passed, then
  // just set start_ptr, it's good to go.
  if (tok)
  {
    tok->set_flags(Token::HasUCN);
    // Just set start_ptr if the UCN isn't dirty.
    if (std::distance(start_ptr, cur_ptr) == num_hexdigits + 2)
      start_ptr = cur_ptr;
    else
      while (start_ptr != cur_ptr)
        peek_char_advance(start_ptr, *tok);
  }
  else
    start_ptr = cur_ptr;

  // C11 6.4.3/2: A universal character name shall not specify a character
  // whose short identifier is less than 00A0 other than 0024 ($), 0040 (@),
  // or 0060 ('), nor one in the range D800 through DFFF inclusive.
  if (code_point < 0xA0)
  {
    if (code_point != 0x24 && code_point != 0x40 && code_point != 0x60)
    {
      lex.diag.report(slash_loc, diag::err_ucn_invalid);
      return 0;
    }
  }
  else if (code_point >= 0xD800 && code_point <= 0xDFFF)
  {
    lex.diag.report(slash_loc, diag::err_ucn_invalid);
    return 0;
  }

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

// Note: `size` is the size of the peeked '\' character.
auto try_advance_identifier_ucn(Lexer &lex, const char *&cur_ptr, int64_t size,
                                Token &result) -> bool
{
  auto ucn_ptr = cur_ptr + size;
  if (uint32_t code_point = try_read_ucn(lex, ucn_ptr, cur_ptr, nullptr);
      code_point == 0)
    return false;
  const auto ucn_size = std::distance(cur_ptr, ucn_ptr);
  if ((ucn_size == 6 && cur_ptr[1] == 'u') ||
      (ucn_size == 10 && cur_ptr[1] == 'U'))
    cur_ptr = ucn_ptr;
  else
    while (cur_ptr != ucn_ptr)
      peek_char_advance(cur_ptr, result);
  return true;
}

// Parses an identifier
//
// Assumes that the identifier's head is already parsed.
auto lex_identifier(Lexer &lex, const char *cur_ptr, Token &result) -> bool
{
  char c = *cur_ptr++;

  // Most of the heavy work can be avoided if the identifier is
  // formed by ASCII characters only.
  while (is_nondigit(c) || is_digit(c))
    c = *cur_ptr++;

  // Backs up to correspond to `c`.
  --cur_ptr;

  // There's dirt, lexes the rest of the identifier.
  if (c == '\\')
  {
    int64_t size = 0;
    c = peek_char_and_size(cur_ptr, size);
    while (true)
    {
      if (c == '\\' && try_advance_identifier_ucn(lex, cur_ptr, size, result))
      {
        c = peek_char_and_size(cur_ptr, size);
        continue;
      }
      else if (!(is_nondigit(c) || is_digit(c)))
        break; // We're done.

      cur_ptr = consume_char(cur_ptr, size, result);
      c = peek_char_and_size(cur_ptr, size);

      // Handles escaped newlines and trigraphs.
      while (is_nondigit(c) || is_digit(c))
      {
        cur_ptr = consume_char(cur_ptr, size, result);
        c = peek_char_and_size(cur_ptr, size);
      }
    }
  }

  lex.form_token(result, cur_ptr, TokenKind::identifier);

  if (!result.has_UCN())
  {
    // Changes the token's kind to a keyword if this happens to be one.
    const auto tok_spell = lex.source_mgr.text_slice(result.range);
    for (const TokenKind kw : KEYWORD_KINDS)
    {
      if (tok_spell == to_string(kw))
      {
        result.kind = kw;
        break;
      }
    }
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

auto lex_numeric_constant(Lexer &lex, const char *cur_ptr, Token &result)
  -> bool
{
  int64_t digit_size = 0;
  char prev = *cur_ptr;
  char c = peek_char_and_size(cur_ptr, digit_size);

  while (is_digit(c) || is_nondigit(c) || c == '.')
  {
    cur_ptr = consume_char(cur_ptr, digit_size, result);
    prev = c;
    c = peek_char_and_size(cur_ptr, digit_size);
  }

  // exponent-part: [C11 6.4.4.2]
  //   'e' sign[opt] digit-sequence
  //   'E' sign[opt] digit-sequence
  if ((c == '+' || c == '-') || (prev == 'e' || prev == 'E'))
    return lex_numeric_constant(
      lex, consume_char(cur_ptr, digit_size, result), result);

  // binary-exponent-part:
  //    'p' sign[opt] digit-sequence
  //    'P' sign[opt] digit-sequence
  if ((c == '+' || c == '-') || (prev == 'p' || prev == 'P'))
    return lex_numeric_constant(
      lex, consume_char(cur_ptr, digit_size, result), result);

  // Found a possibly UCN, lex it and continue.
  if (c == '\\' && try_advance_identifier_ucn(lex, cur_ptr, digit_size, result))
    return lex_numeric_constant(lex, cur_ptr, result);

  lex.form_token(result, cur_ptr, TokenKind::numeric_constant);
  result.set_flags(Token::IsLiteral);
  return true;
}

} // namespace

auto Lexer::lex(Token &result) -> bool
{
  auto cur_ptr = buffer_ptr =
    std::find_if(buffer_ptr, buffer_end, std::not_fn(is_whitespace));
  int64_t size = 0;
  char ch = peek_char_and_size(cur_ptr, size);
  cur_ptr += size;

  switch (ch)
  {
    case '\0':
      return false;

    case '\\':
      if (uint32_t code_point = try_read_ucn(*this, cur_ptr, buffer_ptr, nullptr);
          code_point != 0)
        return lex_identifier(*this, buffer_ptr, result);
      else
        break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      return lex_numeric_constant(*this, cur_ptr, result);

    case '.':
    {
      int64_t size = 0;
      char c = peek_char_and_size(cur_ptr, size);
      if (is_digit(c))
        return lex_numeric_constant(*this, consume_char(cur_ptr, size, result),
                                    result);
      else
        cci_unreachable();
    }

    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
    case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
    case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
    case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':
    case 'X': case 'Y': case 'Z': case '_':
      return lex_identifier(*this, cur_ptr, result);

    default:
      break;
  }

  diag.report(location_for_ptr(buffer_ptr), diag::err_unknown_character, ch);
  form_token(result, cur_ptr, TokenKind::unknown);

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
      return "identifier";
    case TokenKind::numeric_constant:
      return "numeric constant";
    case TokenKind::unknown:
      return "<unknown>";
    case TokenKind::eof:
      return "<end of input>";
  }

  cci_unreachable();
}

} // namespace cci
