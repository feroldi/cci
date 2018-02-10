#include "cci/lex/lexer.hpp"
#include "cci/lex/unicode_char_set.hpp"
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/unicode.hpp"
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

namespace {

constexpr TokenKind KEYWORD_KINDS[]{
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

constexpr auto is_newline(char C) -> bool { return C == '\n' || C == '\r'; }

constexpr auto is_whitespace(char C) -> bool
{
  return C == ' ' || C == '\t' || C == '\v' || C == '\f' || is_newline(C);
}

constexpr auto is_ascii(char C) -> bool
{
  return static_cast<unsigned char>(C) < 128;
}

template <typename ErrorCode, typename... Args>
void report(Lexer &lex, const char *ctx, ErrorCode err_code, Args &&... args)
{
  auto &diag = lex.source_mgr.get_diagnostics();
  return diag.report(lex.location_for_ptr(ctx), err_code,
                     std::forward<Args>(args)...);
}

// The following few helper functions are inspired by Clang's lexer
// implementation.
//
// The lexer works with a few useful functions. Because the C grammar is a
// little complex, it is not possible to implement a lexer that iterates over
// ASCII characters char by char without any special handling. There are things
// like escaped newlines, trigraphs and UCNs which make lexing a bit more
// difficult. With that said, these few functions implement a "peek and consume"
// interface that handles all of those special syntax. The ideia is that in
// order to consume a character, you specify the size of it, i.e. the number of
// characters that theoretically compose only one character.
//
// For example, a trigraph like '??!' would have size 3, where the first '?'
// would signal that it is not a trivial character, and needs special care.
// After peeking it, you get the decoded character ('|'), and the size to be
// skipped over (3). When consuming this peeked character, the buffer pointer
// will end up past the end of the trigraph, i.e. `ptr + 3`, where `ptr` is the
// current buffer pointer.
//
// In case the trigraph '??/' is found, depending on what's next to it, peeking
// it wouldn't return '\', but rather the character after the escaped new-line
// (if there is any), so the size of these special characters would be 3 (the
// trigraph itself) + the size of the escaped new-line (which may be 2 or 3,
// depending on whether there is a carriage return). This goes on and on until
// a trivial character is found, which finally terminates the peeking process.

// Calculates the size of an escaped newline. Assumes that the slash character
// is already consumed. Whitespaces between the slash and the newline are
// considered as ill-formed.
//
// \param ptr The position past the backslash ('\') character.
//
// \return The distance between `ptr` and the first character after the escaped
//         newline.
auto size_for_escaped_newline(const char *ptr) -> int64_t
{
  int64_t nl_size = 0;

  if (is_newline(ptr[nl_size]))
  {
    ++nl_size;

    // Consumes a pair of \r\n or \n\r if there is any.
    if (is_newline(ptr[nl_size]) && ptr[nl_size - 1] != ptr[nl_size])
      ++nl_size;

    return nl_size;
  }

  // Not a newline.
  return 0;
}

// Checks whether a character needs any special care.
//
// Things like trigraphs and escaped newlines are examples of such special
// characters. They need to be properly consumed, therefore you can't just
// advance the buffer pointer by 1.
//
// \param c The character to be checked.
// \return true if character doesn't need special care.
constexpr bool is_trivial_character(char c)
{
  return c != '?' && c != '\\';
}

// C11 5.2.1.1 Trigraph sequences.
constexpr auto decode_trigraph_letter(char letter)
{
  switch (letter)
  {
    case '=': return '#';
    case '(': return '[';
    case '/': return '\\';
    case ')': return ']';
    case '\'': return '^';
    case '<': return '{';
    case '!': return '|';
    case '>': return '}';
    case '-': return '~';
    default: return '\0';
  }
}

// Peeks a character from the input stream and returns it, setting the size to
// how many characters are to be skipped over. This handles special cases like
// escaped newlines and trigraphs*.
//
// \param ptr Buffer pointer from which to peek a character.
// \param size Size accumulator. For normal use, set it to 0.
// \param tok Token being formed, if any.
//
// \return The character pointed by `ptr`, or a character after escaped
// new-line, or a decoded trigraph, all along with the character's size.
auto peek_char_and_size_nontrivial(const char *ptr, int64_t size,
                                   Token *tok = nullptr)
  -> std::pair<char, int64_t>
{
  if (*ptr == '\\')
  {
    ++ptr;
    ++size;

backslash:
    // There's no need to escape anything other than whitespaces.
    if (!is_whitespace(*ptr)) return {'\\', size};

    if (int64_t nlsize = size_for_escaped_newline(ptr))
    {
      if (tok) tok->set_flags(Token::IsDirty);
      ptr += nlsize;
      size += nlsize;
      return peek_char_and_size_nontrivial(ptr, size, tok);
    }

    // Not a newline, just a regular whitespace.
    return {'\\', size};
  }

  // Trigraphs.
  if (ptr[0] == '?' && ptr[1] == '?')
  {
    if (const char c = decode_trigraph_letter(ptr[2]))
    {
      ptr += 3;
      size += 3;
      if (tok) tok->set_flags(Token::IsDirty);
      if (c == '\\') goto backslash;
      return {c, size};
    }
  }

  // Peek a simple character.
  return {*ptr, size + 1};
}

// Peeks a character from `ptr` and advances it.
//
// This is the same as peek_char_and_size, except that the buffer pointer is
// properly advanced to the next simple character in the buffer.
//
// \param ptr Buffer pointer from which to peek and advance.
// \param tok Token being formed.
//
// \return The character pointed by ptr before advancing.
auto peek_char_advance(const char *&ptr, Token &tok) -> char
{
  if (is_trivial_character(*ptr)) return *ptr++;
  const auto [c, size] = peek_char_and_size_nontrivial(ptr, 0, &tok);
  ptr += size;
  return c;
}

// Peeks a character and its size from the buffer pointer.
//
// If the character pointed by `ptr` is simple, then this is the fast path: it
// returns `*ptr` and a size of 1. Otherwise, this function falls back to
// `peek_char_and_size_nontrivial`.
//
// \param ptr Buffer pointer from which to peek a character.
//
// \return The character pointed by `ptr`, and the size to get to the next
//         consumable character.
auto peek_char_and_size(const char *ptr) -> std::pair<char, int64_t>
{
  if (is_trivial_character(*ptr))
    return {*ptr, 1};
  return peek_char_and_size_nontrivial(ptr, 0, nullptr);
}

// Consumes the buffer pointer.
//
// This is meant to be used along with `peek_char_and_size`, as it returns a
// buffer pointer by repeeking the same character if `size` doesn't correspond
// to the size of a simple character.  This reiteration is needed in order to
// set any special flags to the token `tok` being formed.
//
// \param ptr Buffer pointer from which to consume a peeked character.
// \param size The size of the character to be consumed.
// \param tok Token being formed.
//
// \return A buffer pointer past the peeked (non-)trivial character.
auto consume_char(const char *ptr, int64_t size, Token &tok) -> const char *
{
  // Consumes a simple character.
  if (size == 1)
    return std::next(ptr);
  // Otherwise, reparse it to get the right size.
  size = peek_char_and_size_nontrivial(ptr, 0, &tok).second;
  return ptr + size;
}

// universal-character-name: [C11 6.4.3/1]
//     '\u' hex-quad
//     '\U' hex-quad  hex-quad
//
// hex-quad:
//   hexadecimal-digit hexadecimal-digit
//       hexadecimal-digit hexadecimal-digit
//
// Reads a universal character name.
//
// Parses a \u or \U UCN and calculates the code point represented by it. If
// such code point isn't in a valid range as defined by [C11 6.4.3], reports
// diagnostics and returns 0, but still consumes the buffer pointer.  Ill-formed
// UCNs prevent the buffer pointer from being consumed, however.
//
// \param lex The lexer.
// \param start_ptr Buffer pointer to the UCN kind ('u' or 'U').
// \param slash_ptr Buffer pointer to the UCN slash ('\') before its kind.
// \param tok The token being formed, if any.
//
// \return The code point represented by the UCN.
auto try_read_ucn(Lexer &lex, const char *&start_ptr, const char *slash_ptr,
                  Token *tok = nullptr) -> uint32_t
{
  const auto [kind, kind_size] = peek_char_and_size(start_ptr);
  const int num_hexdigits = kind == 'u' ? 4 : kind == 'U' ? 8 : 0;

  if (num_hexdigits == 0)
    return 0;

  auto cur_ptr = start_ptr + kind_size;
  uint32_t code_point = 0;

  // Parses the UCN, ignoring any escaped newlines.
  for (int i = 0; i < num_hexdigits; ++i)
  {
    const auto [c, char_size] = peek_char_and_size(cur_ptr);
    const uint32_t value = hexdigit_value(c);
    if (value == -1U)
    {
      report(lex, slash_ptr, diag::warn_ucn_incomplete);
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
      report(lex, slash_ptr, diag::err_ucn_invalid);
      return 0;
    }
  }
  else if (code_point >= 0xD800 && code_point <= 0xDFFF)
  {
    report(lex, slash_ptr, diag::err_ucn_invalid);
    return 0;
  }

  return code_point;
}

// Lexes a UCN that is part of an identifier.
//
// This makes sure that the lexed UCN is a valid character for an identifier.
//
// \param lex The lexer.
// \param cur_ptr Buffer pointer that points to the slash ('\'). This pointer
//                is updated to point past the end of the UCN only if the UCN
//                is well-formed in an identifier.
// \param size Size of the peeked slash character ('\').
// \param result Token being formed.
//
// \return true if UCN is well-formed for an identifier.
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

// Lexes a UTF-8 character that is part of an identifier.
//
// \param lex The lexer.
// \param cur_ptr Buffer pointer at the start of the UTF-8 sequence.
//
// \return true if a UTF-8 sequence is parsed, false otherwise.
auto try_advance_identifier_utf8(Lexer &lex, const char *&cur_ptr) -> bool
{
  cci_expects(!is_ascii(cur_ptr[0]));
  auto uni_ptr = reinterpret_cast<const std::byte *>(cur_ptr);
  auto uni_end = reinterpret_cast<const std::byte *>(lex.buffer_end);
  if (uint32_t code_point = 0;
      utf8::decode_sequence(uni_ptr, uni_end, code_point) ==
      utf8::DecodeResult::Ok)
  {
    if (is_allowed_id_char(code_point))
    {
      if (code_point == 0x037E) // GREEK QUESTION MARK (U+037E)
        report(lex, cur_ptr, diag::warn_greek_question_mark);
      cur_ptr = reinterpret_cast<const char *>(uni_ptr);
      return true;
    }
  }
  return false;
}

// identifier: [C11 6.4.2]
//   identifier-nondigit
//   identifier  identifier-nondigit
//   identifier  digit
//
// identifier-nondigit:
//   nondigit
//   universal-character-name
//   other implementation-defined characters
//
// nondigit: one of
//   _abcdefghijklm
//   nopqrstuvwxyz
//   ABCDEFGHIJKLM
//   NOPQRSTUVWXYZ
//
// digit: one of
//   0123456789
//
// Lexes an identifier. Assumes that the identifier's head is already consumed.
//
// \param lex The lexer.
// \param cur_ptr A pointer into the buffer that is past the first identifier's
//                character.
// \param result Token being formed.
//
// \return true if identifier was successfully formed.
auto lex_identifier(Lexer &lex, const char *cur_ptr, Token &result) -> bool
{
  char c = *cur_ptr++;

  // Most of the heavy work can be avoided if the identifier is
  // formed by ASCII characters only.
  while (is_nondigit(c) || is_digit(c) || c == '$')
    c = *cur_ptr++;

  // Backs up to correspond to `c`.
  --cur_ptr;

  // There's dirt, lexes the rest of the identifier.
  if (!(is_ascii(c) && is_trivial_character(c)))
  {
    auto [c, size] = peek_char_and_size(cur_ptr);
    while (true)
    {
      if (c == '\\' && try_advance_identifier_ucn(lex, cur_ptr, size, result))
      {
        std::tie(c, size) = peek_char_and_size(cur_ptr);
        continue;
      }
      else if (!is_ascii(c) && try_advance_identifier_utf8(lex, cur_ptr))
      {
        std::tie(c, size) = peek_char_and_size(cur_ptr);
        continue;
      }
      else if (!(is_nondigit(c) || is_digit(c)))
        break; // We're done.

      cur_ptr = consume_char(cur_ptr, size, result);
      std::tie(c, size) = peek_char_and_size(cur_ptr);

      // Handles escaped newlines and trigraphs.
      while (is_nondigit(c) || is_digit(c) || c == '$')
      {
        cur_ptr = consume_char(cur_ptr, size, result);
        std::tie(c, size) = peek_char_and_size(cur_ptr);
      }
    }
  }

  lex.form_token(result, cur_ptr, TokenKind::identifier);

  // Changes the token's kind to a keyword if this happens to be one.
  if (!result.has_UCN())
  {
    // FIXME: This is rather slow.
    auto spelling = result.spelling(lex.source_mgr);
    for (auto kind : KEYWORD_KINDS)
    {
      if (spelling == to_string(kind))
      {
        result.kind = kind;
        break;
      }
    }
  }

  return true;
}

// Lexes a numeric literal constant, i.e. integer-constant and
// floating-constant. Assumes that the first digit is already consumed.
//
// This just matches a regex that validates such constants. Syntax checking is
// delayed to semantic analysis.
//
// \param lex The lexer.
// \param cur_ptr Pointer past the first digit of the numeric constant.
// \param result Token being formed.
//
// \return true if numeric constant was successfully formed.
auto lex_numeric_constant(Lexer &lex, const char *cur_ptr, Token &result)
  -> bool
{
  auto [c, digit_size] = peek_char_and_size(cur_ptr);
  char prev = c;

  // Matches the regex /[0-9a-zA-Z.]*/.
  while (is_digit(c) || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
         c == '.')
  {
    cur_ptr = consume_char(cur_ptr, digit_size, result);
    prev = c;
    std::tie(c, digit_size) = peek_char_and_size(cur_ptr);
  }

  // If we stumbled upon something that doesn't seem to be part of a numeric
  // constant, check whether it's a (possibly binary) exponent of a floating
  // constant. If so, continue lexing, otherwise stop munching.

  // exponent-part: [C11 6.4.4.2]
  //   'e' sign[opt] digit-sequence
  //   'E' sign[opt] digit-sequence
  // binary-exponent-part:
  //    'p' sign[opt] digit-sequence
  //    'P' sign[opt] digit-sequence
  if ((c == '+' || c == '-') ||
      (prev == 'e' || prev == 'E' || prev == 'p' || prev == 'P'))
    return lex_numeric_constant(lex, consume_char(cur_ptr, digit_size, result),
                                result);

  lex.form_token(result, cur_ptr, TokenKind::numeric_constant);
  result.set_flags(Token::IsLiteral);
  return true;
}

// Skips a line comment, returning a pointer past the end of the comment.
// Assumes that the // part is already lexed.
//
// \param lex The lexer.
// \param cur_ptr Buffer pointer which points past the second '/' comment
//                character.
//
// \return A pointer past the end of the comment, i.e. the newline.
auto skip_line_comment(Lexer &lex, const char *cur_ptr) -> const char *
{
  auto [c, c_size] = peek_char_and_size(cur_ptr);

  // C11 6.4.9/2: Except within a character constant, a string literal, or a
  // comment, the characters // introduce a comment that includes all multibyte
  // characters up to, but not including, the next new-line character. The
  // contents of such a comment are examined only to identify multibyte
  // characters and to find the terminating new-line character.
  while (true)
  {
    if (is_newline(c))
    {
      cur_ptr += c_size;
      break; // We're done.
    }

    // End of input; ill-formed program. Even though this is assured to never
    // happen, we still let this check here.
    if (c == '\0')
    {
      report(lex, cur_ptr, diag::err_unterminated_comment, "line");
      break;
    }

    cur_ptr += c_size;
    std::tie(c, c_size) = peek_char_and_size(cur_ptr);
  }

  return cur_ptr;
}

// Skips a block comment, returning a pointer past the end of the comment, i.e.
// after the */ part. Assumes that the // part is already lexed.
//
// \param lex The lexer.
// \param cur_ptr Buffer pointer which points past the '*' from /*
//                string.
//
// \return A pointer past the end of the comment.
auto skip_block_comment(Lexer &lex, const char *cur_ptr) -> const char *
{
  auto [c, c_size] = peek_char_and_size(cur_ptr);
  char prev = c;

  // Could be recursive, but that might not be a good idea.  This also could be
  // improved upon. Right now it handles trigraphs and escaped newlines, which
  // does the job just fine.
  while (true)
  {
    // C11 6.4.9/1: Except within a character constant, a string literal, or a
    // comment, the characters /* introduce a comment. The contents of such a
    // comment are examined only to identify multibyte characters and to find the
    // characters */ that terminate it. 83)
    //
    // 83) Thus, /* ... */ comments do not nest.
    if (c == '/' && prev == '*')
    {
      cur_ptr += c_size;
      break; // We're done.
    }

    // Missing the terminating */ block comment.
    if (c == '\0')
    {
      report(lex, cur_ptr, diag::err_unterminated_comment, "block");
      break;
    }

    cur_ptr += c_size;
    prev = c;
    std::tie(c, c_size) = peek_char_and_size(cur_ptr);
  }

  return cur_ptr;
}

// character-constant: [C11 6.4.4.4]
//    ' c-char-sequence '
//    L' c-char-sequence '
//    L' c-char-sequence '
//    U' c-char-sequence '
//
// c-char-sequence:
//    c-char
//    c-char-sequence c-char
//
// c-char:
//    any member of the source character set except
//      the single-quote ', backslash \, or new-line character
//    escape-sequence
//
// escape-sequence:
//    simple-escape-sequence
//    octal-escape-sequence
//    hexadecimal-escape-sequence
//    universal-character-name
//
// simple-escape-sequence: one of
//    \' \" \? \\ \a \b \f \n \r \t \v
//
// octal-escape-sequence:
//    \ octal-digit
//    \ octal-digit octal-digit
//    \ octal-digit octal-digit octal-digit
//
// hexadecimal-escape-sequence:
//    \x hexadecimal-digit
//    hexadecimal-escape-sequence hexadecimal-digit
//
// Lexes a character constant literal. Assumes that the prefix and ' are already
// lexed.
//
// \param lex The lexer.
// \param cur_ptr Buffer pointer that points past the ' character.
// \param result Token being formed.
// \param char_kind Token kind of this character constant. This is given by the
//                 character constant's prefix (or the lack thereof).
//
// \return true if character constant is successfully lexed.
auto lex_character_constant(Lexer &lex, const char *cur_ptr, Token &result,
                            const TokenKind char_kind) -> bool
{
  cci_expects(char_kind == TokenKind::char_constant ||
              char_kind == TokenKind::utf8_char_constant ||
              char_kind == TokenKind::utf16_char_constant ||
              char_kind == TokenKind::utf32_char_constant ||
              char_kind == TokenKind::wide_char_constant);

  char c = peek_char_advance(cur_ptr, result);

  if (c == '\'')
  {
    report(lex, lex.buffer_ptr, diag::err_empty_character);
    lex.form_token(result, cur_ptr, TokenKind::unknown);
    return true;
  }

  while (c != '\'')
  {
    // Skips this character for now. Decoding and checking of escape sequences
    // occur later on in semantic analysis.
    if (c == '\\')
      c = *cur_ptr++;

    else if (is_newline(c) || c == '\0')
    {
      report(lex, lex.buffer_ptr, diag::err_unterminated_char_or_string, '\'',
             to_string(char_kind));
      lex.form_token(result, std::prev(cur_ptr), TokenKind::unknown);
      return true;
    }

    c = peek_char_advance(cur_ptr, result);
  }

  lex.form_token(result, cur_ptr, char_kind);
  result.set_flags(Token::IsLiteral);
  return true;
}

// string-literal: [C11 6.4.5]
//    encoding-prefix[opt] " s-char-sequence[opt] "
//
// encoding-prefix:
//    u8
//    u
//    U
//    L
//
// s-char-sequence:
//    s-char
//    s-char-sequence s-char
//
// s-char:
//    any member of the source character set except
//      the double-quote ", backslash \, or new-line character.
//    escape-sequence
//
// Lexes a string literal. Assumes that the prefix and " are already lexed.
//
// \param lex The lexer.
// \param cur_ptr Buffer pointer that points pat the " character.
// \param result Token being formed.
// \param str_kind Token kind of this string literal. This is given by the
//                 string literals's prefix (or the lack thereof).
//
// \return true if string literal is successfully lexed.
auto lex_string_literal(Lexer &lex, const char *cur_ptr, Token &result,
                        const TokenKind str_kind) -> bool
{
  cci_expects(str_kind == TokenKind::string_literal ||
              str_kind == TokenKind::utf8_string_literal ||
              str_kind == TokenKind::utf16_string_literal ||
              str_kind == TokenKind::utf32_string_literal ||
              str_kind == TokenKind::wide_string_literal);

  char c = peek_char_advance(cur_ptr, result);

  while (c != '"')
  {
    // Skips this character for now. Decoding and checking of escape sequences
    // occur later on in semantic analysis.
    if (c == '\\')
      c = *cur_ptr++;

    else if (is_newline(c) || c == '\0')
    {
      report(lex, lex.buffer_ptr, diag::err_unterminated_char_or_string, '"',
             to_string(str_kind));
      lex.form_token(result, std::prev(cur_ptr), TokenKind::unknown);
      return true;
    }

    c = peek_char_advance(cur_ptr, result);
  }

  lex.form_token(result, cur_ptr, str_kind);
  result.set_flags(Token::IsLiteral);
  return true;
}

// Lexes an identifier that starts with a UCN or a UTF-8 character.
//
// \param lex The lexer.
// \param cur_ptr Buffer pointer past the lexed code point.
// \param code_point Identifier's UTF-8 code point head.
// \param result Token being formed.
//
// \return true if identifier is lexed.
auto lex_unicode(Lexer &lex, const char *cur_ptr, uint32_t code_point,
                 Token &result) -> bool
{
  if (is_allowed_id_char(code_point) &&
      is_allowed_initially_id_char(code_point))
  {
    // FIXME: This check is duplicated in try_advance_identifier_utf8. Find
    // a way to remove this check from here.
    if (code_point == 0x037E) // GREEK QUESTION MARK (U+037E)
      report(lex, lex.buffer_ptr, diag::warn_greek_question_mark);
    return lex_identifier(lex, cur_ptr, result);
  }
  lex.form_token(result, cur_ptr, TokenKind::unknown);
  return true;
}

// Lexes the next token in the source buffer.
//
// \param lex The lexer.
// \param cur_ptr Pointer into the source buffer from which to lex a token.
// \param result Output parameter to which a lexed token is set on success.
//
// \return true if a token was lexed, and false if end of input is reached.
auto lex_token(Lexer &lex, const char *cur_ptr, Token &result) -> bool
{
  // Skips any whitespace before the token.
  cur_ptr = std::find_if(cur_ptr, lex.buffer_end, std::not_fn(is_whitespace));
  lex.buffer_ptr = cur_ptr;

  auto [ch, ch_size] = peek_char_and_size(cur_ptr);
  cur_ptr = consume_char(cur_ptr, ch_size, result);

  auto kind = TokenKind::unknown;

  switch (ch)
  {
    case '\0':
      return false; // End of input.

    case '\\':
      if (uint32_t code_point = try_read_ucn(lex, cur_ptr, lex.buffer_ptr, nullptr);
          code_point != 0)
      {
        if (utf8::is_whitespace(code_point))
        {
          lex.buffer_ptr = cur_ptr;
          return lex_token(lex, cur_ptr, result);
        }
        return lex_unicode(lex, cur_ptr, code_point, result);
      }
      else
        break;

    case '[':
      kind = TokenKind::l_bracket;
      break;
    case ']':
      kind = TokenKind::r_bracket;
      break;
    case '(':
      kind = TokenKind::l_paren;
      break;
    case ')':
      kind = TokenKind::r_paren;
      break;
    case '{':
      kind = TokenKind::l_brace;
      break;
    case '}':
      kind = TokenKind::r_brace;
      break;

    case '.':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (is_digit(ch))
        return lex_numeric_constant(lex, consume_char(cur_ptr, ch_size, result),
                                    result);
      else if (ch == '.')
      {
        if (const auto [after, after_size] = peek_char_and_size(cur_ptr + ch_size);
            after == '.')
        {
          kind = TokenKind::ellipsis;
          cur_ptr = consume_char(consume_char(cur_ptr, ch_size, result),
                                 after_size, result);
        }
      }
      else
        kind = TokenKind::period;
      break;

    case '-':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '>')
      {
        kind = TokenKind::arrow;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else if (ch == '-')
      {
        kind = TokenKind::minusminus;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else if (ch == '=')
      {
        kind = TokenKind::minusequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::minus;
      break;

    case '+':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '+')
      {
        kind = TokenKind::plusplus;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else if (ch == '=')
      {
        kind = TokenKind::plusequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::plus;
      break;

    case '&':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '&')
      {
        kind = TokenKind::ampamp;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else if (ch == '=')
      {
        kind = TokenKind::ampequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::ampersand;
      break;

    case '*':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '=')
      {
        kind = TokenKind::starequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::star;
      break;

    case '~':
      kind = TokenKind::tilde;
      break;

    case '/':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '/')
      {
        // NOTE: Don't handle line comments that are actually an operator and a
        // block comment in C89. E.g. `a //**/ b`, which should be `a / b` in
        // C89, but is currently parsed as `a`, because of C11's line comments.
        lex.buffer_ptr = skip_line_comment(lex, cur_ptr + ch_size);
        return lex_token(lex, lex.buffer_ptr, result);
      }
      else if (ch == '*')
      {
        lex.buffer_ptr = skip_block_comment(lex, cur_ptr + ch_size);
        return lex_token(lex, lex.buffer_ptr, result);
      }
      else if (ch == '=')
      {
        kind = TokenKind::slashequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::slash;
      break;

    case '%':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '=')
      {
        kind = TokenKind::percentequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else if (ch == '>') // %> digraph.
      {
        kind = TokenKind::r_brace;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else if (ch == ':') // %: digraph.
      {
        cur_ptr = consume_char(cur_ptr, ch_size, result);
        std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
        const auto [after, after_size] = peek_char_and_size(cur_ptr + ch_size);
        if (ch == '%' && after == ':')
        {
          // %:%: digraph
          kind = TokenKind::hashhash;
          cur_ptr = consume_char(consume_char(cur_ptr, ch_size, result),
                                 after_size, result);
        }
        else
          kind = TokenKind::hash;
      }
      else
        kind = TokenKind::percent;
      break;

    case '<':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '<')
      {
        if (const auto [after, after_size] = peek_char_and_size(cur_ptr + ch_size);
            after == '=')
        {
          kind = TokenKind::lesslessequal;
          cur_ptr = consume_char(consume_char(cur_ptr, ch_size, result),
                                 after_size, result);
        }
        else
        {
          kind = TokenKind::lessless;
          cur_ptr = consume_char(cur_ptr, ch_size, result);
        }
      }
      else if (ch == '=')
      {
        kind = TokenKind::lessequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else if (ch == ':') // <: digraph.
      {
        kind = TokenKind::l_bracket;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else if (ch == '%') // <% digraph.
      {
        kind = TokenKind::l_brace;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::less;
      break;

    case '>':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '>')
      {
        if (const auto [after, after_size] = peek_char_and_size(cur_ptr + ch_size);
            after == '=')
        {
          kind = TokenKind::greatergreaterequal;
          cur_ptr = consume_char(consume_char(cur_ptr, ch_size, result),
                                 after_size, result);
        }
        else
        {
          kind = TokenKind::greater;
          cur_ptr = consume_char(cur_ptr, ch_size, result);
        }
      }
      else if (ch == '=')
      {
        kind = TokenKind::greaterequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::greater;
      break;

    case '=':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '=')
      {
        kind = TokenKind::equalequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::equal;
      break;

    case '!':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '=')
      {
        kind = TokenKind::exclamaequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::exclama;
      break;

    case '^':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '=')
      {
        kind = TokenKind::caretequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::caret;
      break;

    case '|':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '|')
      {
        kind = TokenKind::pipepipe;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else if (ch == '=')
      {
        kind = TokenKind::pipeequal;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::pipe;
      break;

    case '?':
      kind = TokenKind::question;
      break;

    case ':':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '>') // :> digraph.
      {
        kind = TokenKind::r_bracket;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::colon;
      break;

    case ';':
      kind = TokenKind::semi;
      break;

    case ',':
      kind = TokenKind::comma;
      break;

    case '#':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '#')
      {
        kind = TokenKind::hashhash;
        cur_ptr = consume_char(cur_ptr, ch_size, result);
      }
      else
        kind = TokenKind::hash;
      break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      return lex_numeric_constant(lex, cur_ptr, result);

    case 'L':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '\'')
        return lex_character_constant(lex,
                                      consume_char(cur_ptr, ch_size, result),
                                      result, TokenKind::wide_char_constant);
      if (ch == '"')
        return lex_string_literal(lex, consume_char(cur_ptr, ch_size, result),
                                  result, TokenKind::wide_string_literal);
      return lex_identifier(lex, cur_ptr, result);

    case 'u':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '\'')
        return lex_character_constant(lex,
                                      consume_char(cur_ptr, ch_size, result),
                                      result, TokenKind::utf16_char_constant);
      if (ch == '8')
      {
        if (const auto [after, after_size] = peek_char_and_size(cur_ptr + ch_size);
            after == '"')
        {
          cur_ptr = consume_char(cur_ptr, ch_size, result);
          return lex_string_literal(lex,
                                    consume_char(cur_ptr, after_size, result),
                                    result, TokenKind::utf8_string_literal);
        }
      }
      if (ch == '"')
        return lex_string_literal(lex, consume_char(cur_ptr, ch_size, result),
                                  result, TokenKind::utf16_string_literal);
      return lex_identifier(lex, cur_ptr, result);

    case 'U':
      std::tie(ch, ch_size) = peek_char_and_size(cur_ptr);
      if (ch == '\'')
        return lex_character_constant(lex,
                                      consume_char(cur_ptr, ch_size, result),
                                      result, TokenKind::utf32_char_constant);
      if (ch == '"')
        return lex_string_literal(lex, consume_char(cur_ptr, ch_size, result),
                                  result, TokenKind::utf32_string_literal);
      [[fallthrough]];

    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': /*   'u'*/
    case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
    case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
    case 'J': case 'K': /*   'L'*/case 'M': case 'N': case 'O': case 'P':
    case 'Q': case 'R': case 'S': case 'T': /*   'U'*/case 'V': case 'W':
    case 'X': case 'Y': case 'Z': case '_': case '$':
      return lex_identifier(lex, cur_ptr, result);

    case '\'':
      return lex_character_constant(lex, cur_ptr, result,
                                    TokenKind::char_constant);

    case '"':
      return lex_string_literal(lex, cur_ptr, result,
                                TokenKind::string_literal);

    default:
    {
      if (is_ascii(ch))
        break;

      --cur_ptr;
      auto uni_ptr = reinterpret_cast<const std::byte *>(cur_ptr);
      auto uni_end = reinterpret_cast<const std::byte *>(lex.buffer_end);
      if (uint32_t code_point = 0;
          utf8::decode_sequence(uni_ptr, uni_end, code_point) ==
          utf8::DecodeResult::Ok)
      {
        cur_ptr = reinterpret_cast<const char *>(uni_ptr);
        if (utf8::is_whitespace(code_point))
        {
          lex.buffer_ptr = cur_ptr;
          return lex_token(lex, cur_ptr, result);
        }
        return lex_unicode(lex, cur_ptr, code_point, result);
      }
    }
  }

  if (kind == TokenKind::unknown)
    report(lex, lex.buffer_ptr, diag::err_unknown_character, ch);

  lex.form_token(result, cur_ptr, kind);
  return true;
}

} // namespace

// Parses the spelling of a token, expanding any trigraphs and escaped
// new-lines, thus returning the uncanonical representation of this token's
// characters.
auto Token::spelling(const SourceManager &source_mgr) const -> std::string
{
  const std::string_view raw_text = source_mgr.text_slice(source_range());
  if (!is_dirty())
    return std::string(raw_text);
  std::string spelling;
  spelling.reserve(raw_text.size());
  auto it = raw_text.begin();
  while (it != raw_text.end())
  {
    const auto [c, size] = peek_char_and_size_nontrivial(it, 0, nullptr);
    spelling.push_back(c);
    it += size;
  }
  cci_ensures(spelling.size() < raw_text.size());
  return spelling;
}

// Lexes a token in the position `buffer_ptr`. Most of the lexing occurs here.
auto Lexer::next_token() -> std::optional<Token>
{
  if (Token result; lex_token(*this, buffer_ptr, result))
    return result;
  return std::nullopt;
}

auto to_string(TokenKind k) -> std::string_view
{
  switch (k)
  {
    case TokenKind::kw_auto: return "auto";
    case TokenKind::kw_break: return "break";
    case TokenKind::kw_case: return "case";
    case TokenKind::kw_char: return "char";
    case TokenKind::kw_const: return "const";
    case TokenKind::kw_continue: return "continue";
    case TokenKind::kw_default: return "default";
    case TokenKind::kw_do: return "do";
    case TokenKind::kw_double: return "double";
    case TokenKind::kw_else: return "else";
    case TokenKind::kw_enum: return "enum";
    case TokenKind::kw_extern: return "extern";
    case TokenKind::kw_float: return "float";
    case TokenKind::kw_for: return "for";
    case TokenKind::kw_goto: return "goto";
    case TokenKind::kw_if: return "if";
    case TokenKind::kw_inline: return "inline";
    case TokenKind::kw_int: return "int";
    case TokenKind::kw_long: return "long";
    case TokenKind::kw_register: return "register";
    case TokenKind::kw_restrict: return "restrict";
    case TokenKind::kw_return: return "return";
    case TokenKind::kw_short: return "short";
    case TokenKind::kw_signed: return "signed";
    case TokenKind::kw_sizeof: return "sizeof";
    case TokenKind::kw_static: return "static";
    case TokenKind::kw_struct: return "struct";
    case TokenKind::kw_switch: return "switch";
    case TokenKind::kw_typedef: return "typedef";
    case TokenKind::kw_union: return "union";
    case TokenKind::kw_unsigned: return "unsigned";
    case TokenKind::kw_void: return "void";
    case TokenKind::kw_volatile: return "volatile";
    case TokenKind::kw_while: return "while";
    case TokenKind::kw__Alignas: return "_Alignas";
    case TokenKind::kw__Alignof: return "_Alignof";
    case TokenKind::kw__Atomic: return "_Atomic";
    case TokenKind::kw__Bool: return "_Bool";
    case TokenKind::kw__Complex: return "_Complex";
    case TokenKind::kw__Generic: return "_Generic";
    case TokenKind::kw__Imaginary: return "_Imaginary";
    case TokenKind::kw__Noreturn: return "_Noreturn";
    case TokenKind::kw__Static_assert: return "_Static_assert";
    case TokenKind::kw__Thread_local: return "_Thread_local";
    case TokenKind::identifier: return "identifier";
    case TokenKind::numeric_constant: return "numeric constant";
    case TokenKind::char_constant: return "character constant";
    case TokenKind::utf8_char_constant: return "utf-8 character constant";
    case TokenKind::utf16_char_constant: return "utf-16 character constant";
    case TokenKind::utf32_char_constant: return "utf-32 character constant";
    case TokenKind::wide_char_constant: return "wide character constant";
    case TokenKind::string_literal: return "string literal";
    case TokenKind::utf8_string_literal: return "utf-8 string literal";
    case TokenKind::utf16_string_literal: return "utf-16 string literal";
    case TokenKind::utf32_string_literal: return "utf-32 string literal";
    case TokenKind::wide_string_literal: return "wide string literal";
    case TokenKind::l_bracket: return "[";
    case TokenKind::r_bracket: return "]";
    case TokenKind::l_paren: return "(";
    case TokenKind::r_paren: return ")";
    case TokenKind::l_brace: return "{";
    case TokenKind::r_brace: return "}";
    case TokenKind::period: return ".";
    case TokenKind::arrow: return "->";
    case TokenKind::plusplus: return "++";
    case TokenKind::minusminus: return "--";
    case TokenKind::ampersand: return "&";
    case TokenKind::star: return "*";
    case TokenKind::plus: return "+";
    case TokenKind::minus: return "-";
    case TokenKind::tilde: return "~";
    case TokenKind::exclama: return "!";
    case TokenKind::slash: return "/";
    case TokenKind::percent: return "%";
    case TokenKind::lessless: return "<<";
    case TokenKind::greatergreater: return ">>";
    case TokenKind::less: return "<";
    case TokenKind::greater: return ">";
    case TokenKind::lesslessequal: return "<<=";
    case TokenKind::greatergreaterequal: return ">>=";
    case TokenKind::equalequal: return "==";
    case TokenKind::exclamaequal: return "!=";
    case TokenKind::caret: return "^";
    case TokenKind::pipe: return "|";
    case TokenKind::ampamp: return "&&";
    case TokenKind::pipepipe: return "||";
    case TokenKind::question: return "?";
    case TokenKind::colon: return ":";
    case TokenKind::semi: return ";";
    case TokenKind::ellipsis: return "...";
    case TokenKind::equal: return "=";
    case TokenKind::starequal: return "*=";
    case TokenKind::slashequal: return "/=";
    case TokenKind::percentequal: return "%=";
    case TokenKind::plusequal: return "+=";
    case TokenKind::minusequal: return "-=";
    case TokenKind::lessequal: return "<=";
    case TokenKind::greaterequal: return ">=";
    case TokenKind::ampequal: return "&=";
    case TokenKind::caretequal: return "^=";
    case TokenKind::pipeequal: return "|=";
    case TokenKind::comma: return ",";
    case TokenKind::hash: return "#";
    case TokenKind::hashhash: return "##";
    case TokenKind::unknown: return "<unknown>";
    case TokenKind::eof: return "<end of input>";
  }

  cci_unreachable();
}

} // namespace cci
