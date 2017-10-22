#include "ccompiler/Lex/Lexer.hpp"
#include "ccompiler/Util/Contracts.hpp"
#include "ccompiler/Util/MemoryBuffer.hpp"
#include <utility>
#include <algorithm>
#include <cassert>

namespace ccompiler::lex {

auto Token::text(const SourceManager &SM) const -> std::string_view
{
  // TODO
  return {};
}

auto to_string(TokenKind K) -> const char *
{
  switch (K)
  {
    default: Unreachable();
    case TokenKind::Kw_auto: return "auto";
    case TokenKind::Kw_break: return "break";
    case TokenKind::Kw_case: return "case";
    case TokenKind::Kw_char: return "char";
    case TokenKind::Kw_const: return "const";
    case TokenKind::Kw_continue: return "continue";
    case TokenKind::Kw_default: return "default";
    case TokenKind::Kw_do: return "do";
    case TokenKind::Kw_double: return "double";
    case TokenKind::Kw_else: return "else";
    case TokenKind::Kw_enum: return "enum";
    case TokenKind::Kw_extern: return "extern";
    case TokenKind::Kw_float: return "float";
    case TokenKind::Kw_for: return "for";
    case TokenKind::Kw_goto: return "goto";
    case TokenKind::Kw_if: return "if";
    case TokenKind::Kw_inline: return "inline";
    case TokenKind::Kw_int: return "int";
    case TokenKind::Kw_long: return "long";
    case TokenKind::Kw_register: return "register";
    case TokenKind::Kw_restrict: return "restrict";
    case TokenKind::Kw_return: return "return";
    case TokenKind::Kw_short: return "short";
    case TokenKind::Kw_signed: return "signed";
    case TokenKind::Kw_sizeof: return "sizeof";
    case TokenKind::Kw_static: return "static";
    case TokenKind::Kw_struct: return "struct";
    case TokenKind::Kw_switch: return "switch";
    case TokenKind::Kw_typedef: return "typedef";
    case TokenKind::Kw_union: return "union";
    case TokenKind::Kw_unsigned: return "unsigned";
    case TokenKind::Kw_void: return "void";
    case TokenKind::Kw_volatile: return "volatile";
    case TokenKind::Kw_while: return "while";
    case TokenKind::Kw__Alignas: return "_Alignas";
    case TokenKind::Kw__Alignof: return "_Alignof";
    case TokenKind::Kw__Atomic: return "_Atomic";
    case TokenKind::Kw__Bool: return "_Bool";
    case TokenKind::Kw__Complex: return "_Complex";
    case TokenKind::Kw__Generic: return "_Generic";
    case TokenKind::Kw__Imaginary: return "_Imaginary";
    case TokenKind::Kw__Noreturn: return "_Noreturn";
    case TokenKind::Kw__Static_assert: return "_Static_assert";
    case TokenKind::Kw__Thread_local: return "_Thread_local";
    case TokenKind::identifier: return "identifier";
    case TokenKind::eof: return "<end of input>";
  }
}

} // namespace ccompiler::lex

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
