#include "cci/semantics/sema.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/lex/lexer.hpp"
#include "cci/lex/literal_parser.hpp"
#include "cci/util/small_vector.hpp"
#include <memory>
#include <string_view>

static_assert(2 == sizeof(char16_t),
              "UTF-16 string literals assume that char16_t is 2 bytes long");
static_assert(4 == sizeof(char32_t),
              "UTF-32 string literals assume that char32_t is 4 bytes long");

using namespace cci;

auto Sema::act_on_numeric_constant(const Token &tok) -> std::unique_ptr<Expr>
{
  cci_expects(tok.is(TokenKind::numeric_constant));

  if (tok.size() == 1)
  {
    const char digit = tok.raw_spelling(lex.source_manager())[0];
    QualifiedType ty(std::make_unique<BuiltinType>(BuiltinTypeKind::Int),
                     Qualifiers::None);
    return std::make_unique<IntegerLiteral>(digit - '0', std::move(ty),
                                            tok.location());
  }

  small_string<64> spell_buffer;
  std::string_view spelling = lex.get_spelling(tok, spell_buffer);
  NumericConstantParser literal(lex, spelling, tok.location());

  if (literal.has_error)
    return nullptr;

  if (literal.is_integer_literal())
  {
    auto [val, overflowed] = literal.to_integer();
    if (!overflowed)
    {
      // TODO: Discover this numeric constant's actual type.
      QualifiedType ty(std::make_unique<BuiltinType>(BuiltinTypeKind::Int),
                       Qualifiers::None);
      return std::make_unique<IntegerLiteral>(val, std::move(ty),
                                              tok.location());
    }
  }
  else if (literal.is_floating_literal())
  {
    cci_unreachable(); // TODO: Implement me!
  }

  return nullptr;
}

auto Sema::act_on_char_constant(const Token &tok)
  -> std::unique_ptr<CharacterConstant>
{
  cci_expects(tok.is_one_of(
    TokenKind::char_constant, TokenKind::utf16_char_constant,
    TokenKind::utf32_char_constant, TokenKind::wide_char_constant));

  small_string<8> spell_buffer;
  std::string_view spelling = lex.get_spelling(tok, spell_buffer);
  CharConstantParser literal(lex, spelling, tok.location(), tok.kind,
                             context.target_info());

  if (literal.has_error)
    return nullptr;

  // [C11 6.4.4.4p10]: An integer character constant has type int.
  auto char_type = BuiltinTypeKind::Int;
  auto char_kind = CharacterConstantKind::Ascii;

  switch (literal.kind)
  {
    case TokenKind::utf16_char_constant:
      char_type = BuiltinTypeKind::Char16;
      char_kind = CharacterConstantKind::UTF16;
      break;
    case TokenKind::utf32_char_constant:
      char_type = BuiltinTypeKind::Char32;
      char_kind = CharacterConstantKind::UTF32;
      break;
    case TokenKind::wide_char_constant:
      char_type = BuiltinTypeKind::WChar;
      char_kind = CharacterConstantKind::Wide;
      break;
    default: cci_expects(TokenKind::char_constant == literal.kind);
  }

  QualifiedType char_ty(std::make_unique<BuiltinType>(char_type),
                        Qualifiers::None);
  return std::make_unique<CharacterConstant>(
    literal.value, char_kind, std::move(char_ty), tok.location());
}

auto Sema::act_on_string_literal(span<const Token> string_toks)
  -> std::unique_ptr<StringLiteral>
{
  cci_expects(!string_toks.empty());
  StringLiteralParser literal(lex, string_toks, context.target_info());
  if (literal.has_error)
    return nullptr;

  cci_expects(literal.char_byte_width == 1 || literal.char_byte_width == 2 ||
              literal.char_byte_width == 4);

  small_vector<SourceLocation, 1> tok_locs;
  for (auto tok : string_toks)
    tok_locs.push_back(tok.location());

  auto elem_type_kind = BuiltinTypeKind::Char;
  auto str_kind = StringLiteralKind::Ascii;

  switch (literal.kind)
  {
    case TokenKind::utf8_string_literal:
      str_kind = StringLiteralKind::UTF8;
      break;
    case TokenKind::utf16_string_literal:
      elem_type_kind = BuiltinTypeKind::Char16;
      str_kind = StringLiteralKind::UTF16;
      break;
    case TokenKind::utf32_string_literal:
      elem_type_kind = BuiltinTypeKind::Char32;
      str_kind = StringLiteralKind::UTF32;
      break;
    case TokenKind::wide_string_literal:
      elem_type_kind = BuiltinTypeKind::WChar;
      str_kind = StringLiteralKind::Wide;
      break;
    default: cci_expects(TokenKind::string_literal == literal.kind);
  }

  auto element_ty =
    QualifiedType(std::make_unique<BuiltinType>(elem_type_kind), 0);

  auto str_ty =
    QualifiedType(std::make_unique<ConstantArrayType>(
                    std::move(element_ty), literal.num_string_chars() + 1),
                  0);

  std::vector<std::byte> str_storage;
  str_storage.resize(literal.byte_length());
  std::byte *const storage_ptr = str_storage.data();
  std::string_view str = literal.string();

  // Length of string including null character.
  const size_t length = literal.num_string_chars() + 1;

  if (literal.char_byte_width == 1)
  {
    auto data = new (storage_ptr) char[length];
    std::memcpy(data, str.data(), length * sizeof(*data));
  }
  else if (literal.char_byte_width == 2)
  {
    auto data = new (storage_ptr) char16_t[length];
    std::memcpy(data, str.data(), length * sizeof(*data));
  }
  else
  {
    cci_expects(literal.char_byte_width == 4);
    auto data = new (storage_ptr) char32_t[length];
    std::memcpy(data, str.data(), length * sizeof(*data));
  }

  return std::make_unique<StringLiteral>(std::move(str_ty),
                                         std::move(str_storage), str_kind,
                                         literal.char_byte_width, tok_locs);
}
