#include "cci/lex/lexer.hpp"
#include "cci/util/contracts.hpp"

namespace cci {

struct BuiltinType
{
  enum class Kind
  {
    Void,
    SChar,
    UChar,
    WChar,
    Char16,
    Char32,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    Float,
    Double,
    LongDouble,
    Bool,
  };

  Kind kind;
};

struct Parser
{
  Lexer &lex;
  Token tok;

  Parser(Lexer &lex) noexcept : lex(lex) {}
};

// primary-expression:
//   identifier
//   constant
//   string-literal
//   '(' expression ')'
//   generic-selection

struct IntegerConstant
{
  BuiltinType type;
};

} // namespace cci
