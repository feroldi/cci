#include "ccompiler/Lex/LexerContext.hpp"
#include "ccompiler/Util/Contracts.hpp"
#include "ccompiler/Util/MemoryBuffer.hpp"
#include <utility>
#include <algorithm>
#include <cassert>

namespace ccompiler::lex {
namespace {

// token:
//   keyword
//   identifier
//   constant
//   string-literal
//   punctuator

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
};

class LexerContext
{
private:
  // Begin and end iterators into the source buffer.
  const char *buffer_begin;
  const char *buffer_end;

  // A pointer into the source content buffer of the current
  // position to be lexed.
  const char *buffer_ptr;

  LexerContext(const char *buf_beg, const char *buf_ptr, const char *buf_end) :
    buffer_begin(buf_beg), buffer_ptr(buf_ptr), buffer_end(buf_end)
  {
    Expects(*buffer_end == '\0' && "End of file should contain a zero character");
  }

public:
  LexerContext(const util::MemoryBuffer &MB)
    : LexerContext(MB.begin(), MB.begin(), MB.end()) {}

  // Returns the current buffer pointer and advances it.
  const char *consume_char()
  {
    Expects(buffer_ptr != buffer_end);
    return buffer_ptr++;
  }

  // Consumes the buffer pointer until the predicate P evaluates
  // to false.
  //
  // \returns buffer pointer when P(C) == false.
  template <typename Predicate>
  const char *consume_until(Predicate P)
  {
    while (auto C = consume_char(); C && P(C)) {}
    return buffer_ptr;
  }
};

constexpr bool is_nondigit(char C)
{
  return (C >= 'a' && C <= 'z') || (C >= 'A' && C <= 'Z') || C == '_';
}

constexpr bool is_digit(char C)
{
  return C >= '0' && C <= '9';
}

constexpr bool is_hexdigit(char C)
{
  return is_digit(C) || (C >= 'a' && C <= 'f') || (C >= 'A' && C <= 'F');
}

// TODO
// identifier-nondigit:
//   nondigit
//   universal-character-name                    [TODO]
//   other implementation-defined characters
//
// hex-quad:                                     [TODO]
//     hexadecimal-digit hexadecimal-digit
//       hexadecimal-digit hexadecimal-digit
//
// [6.4.3]
// universal-character-name:                     [TODO]
//     \u hex-quad
//     \U hex-quad  hex-quad
//
// [6.4.2.1]/1
// identifier:
//   identifier-nondigit
//   identifier  identifier-nondigit
//   identifier  digit
//
// identifier-nondigit:
//   nondigit
//   universal-character-name                    [TODO]
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

void lex_identifier(LexerContext &LC, Token &result)
{
  auto id_start = LC.consume_char();
  auto id_end = LC.consume_until([] (char C) { is_nondigit(C) || is_digit(C); });

  result = Token(TokenKind::identifier, SourceRange(id_start, id_end));
}


} // namespace
} // namespace ccompiler::lex
