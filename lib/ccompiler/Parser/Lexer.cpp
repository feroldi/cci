#include "ccompiler/include/Util/Contracts.hpp"
#include <utility>
#include <algorithm>

namespace
{

// token:
//   keyword
//   identifier
//   constant
//   string-literal
//   punctuator

// keyword: one of
//   auto
//   break
//   case
//   char
//   const
//   continue
//   default
//   do
//   double
//   else
//   enum
//   extern
//   float
//   for
//   goto
//   if
//   inline
//   int
//   long
//   register
//   restrict
//   return
//   short
//   signed
//   sizeof
//   static
//   struct
//   switch
//   typedef
//   union
//   unsigned
//   void
//   volatile
//   while
//   _Alignas
//   _Alignof
//   _Atomic
//   _Bool
//   _Complex
//   _Generic
//   _Imaginary
//   _Noreturn
//   _Static_assert
//   _Thread_local


enum class Token
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

  // Identifiers
  //
  // [6.4.2.1]/2
  // An identifier is a sequence of nondigit characters (including the
  // underscore _, the lowercase and uppercase Latin letters, and other
  // characters) and digits, which designates one or more entities as
  // described in 6.2.1. Lowercase and uppercase letters are distinct.
  // There is no specific limit on the maximum length of an identifier.
  Identifier,
};

struct LexerContext
{
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

// identifier-nondigit:
//   nondigit
//   universal-character-name                    [TODO]
//   other implementation-defined characters
constexpr bool is_identifier_nondigit(char C)
{
  return is_nondigit(C);
}

// hex-quad:
//     hexadecimal-digit hexadecimal-digit hexadecimal-digit hexadecimal-digit
auto parse_hex_quad(LexerContext& lexer, SourceLocation begin, SourceLocation end)
  -> SourceLocation
{
  Expects(is_hexdigit(*begin));

  if (std::distance(begin, end) >= 4)
  {
    auto it = std::find_if_not(begin, std::next(begin, 4), is_hexdigit);

    if (std::distance(begin, it) == 4)
      return it;
  }

  return end;
}

constexpr bool is_universal_character_name(SourceLocation begin, SourceLocation end)
{
  return std::distance(begin, end) >= 2 && begin[0] == '\\' && (begin[1] == 'u' || begin[1] == 'U');
}

// universal-character-name [6.4.3]:
//     \u hex-quad
//     \U hex-quad  hex-quad
//
// [6.4.2.1]/3
// Each universal character name in an identifier shall designate a character whose encoding
// in ISO/IEC 10646 falls into one of the ranges specified in D.1.
// The initial character
// shall not be a universal character name designating a character whose encoding falls into
// one of the ranges specified in D.2. An implementation may allow multibyte characters
// that are not part of the basic source character set to appear in identifiers; which characters
// and their correspondence to universal character names is implementation-defined
//
// Note: it doesn't add any new tokens to the stream. A valid UCN
// returns a source location != end.
auto parse_universal_character_name(LexerContext& lexer, SourceLocation begin, SourceLocation end)
  -> SourceLocation
{
  Expects(is_universal_character_name(begin, end));

  const auto hquad_begin = std::next(begin, 2);

  parse_hex_quad(lexer, hquad_begin,
}

// identifier [6.4.2.1]/1:
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
auto parse_identifier(LexerContext& lexer, SourceLocation begin,
                      SourceLocation end) -> SourceLocation
{
  Expects(is_identifier_nondigit(*begin));

  auto id_end = std::find_if_not(std::next(begin), end, [] (char C) {
    return is_identifier_nondigit(C) || is_digit(C);
  });

  lexer.add_token(Token::Identifier, SourceRange(begin, id_end));
}

} // namespace
