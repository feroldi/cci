#include "cci/semantics/sema.hpp"
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/expr.hpp"
#include "cci/ast/type.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/literal_parser.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/util/small_vector.hpp"
#include <memory>
#include <string_view>

static_assert(2 == sizeof(char16_t),
              "UTF-16 string literals assume that char16_t is 2 bytes long");
static_assert(4 == sizeof(char32_t),
              "UTF-32 string literals assume that char32_t is 4 bytes long");

using namespace cci;

auto Sema::act_on_numeric_constant(const Token &tok)
  -> std::optional<arena_ptr<Expr>>
{
  cci_expects(tok.is(Category::numeric_constant));

  if (tok.size() == 1)
  {
    const char digit =
      scan.source_map().range_to_snippet(tok.source_range())[0];
    return new (context)
      IntegerLiteral(digit - '0', context.int_ty, tok.location());
  }

  small_string<64> spell_buffer;
  std::string_view spelling = scan.get_spelling(tok, spell_buffer);
  NumericConstantParser literal(scan, spelling, tok.location());

  if (literal.has_error)
    return nullptr;

  if (literal.is_integer_literal())
  {
    auto [val, overflowed] = literal.to_integer();

    if (overflowed)
    {
      // We could just stop here, but let's be friends with the user and try to
      // parse and diagnose the source code as much as possible.
      diag.report(tok.location(), "integer literal is too large");
    }

    bool allow_unsigned = literal.is_unsigned || literal.radix != 10;
    QualType integer_ty;
    size_t width = 0;

    // Apply [C11 6.4.4.1p5]: The type of an integer constant is the first of
    // the corresponding list in which its value can be represented.
    if (!literal.is_long && !literal.is_long_long)
    {
      size_t int_width = context.target_info().int_width;
      if (val >> int_width == 0)
      {
        if (!literal.is_unsigned && (val >> (int_width - 1)) == 0)
          integer_ty = context.int_ty;
        else if (allow_unsigned)
          integer_ty = context.uint_ty;
        width = int_width;
      }
    }

    if (!integer_ty && !literal.is_long_long)
    {
      size_t long_width = context.target_info().long_width;
      if (val >> long_width == 0)
      {
        if (!literal.is_unsigned && (val >> (long_width - 1)) == 0)
          integer_ty = context.long_ty;
        else if (allow_unsigned)
          integer_ty = context.ulong_ty;
        width = long_width;
      }
    }

    if (!integer_ty)
    {
      size_t long_long_width = context.target_info().long_long_width;
      if (val >> long_long_width == 0)
      {
        if (!literal.is_unsigned && (val >> (long_long_width - 1)) == 0)
          integer_ty = context.long_long_ty;
        else if (allow_unsigned)
          integer_ty = context.ulong_long_ty;
        width = long_long_width;
      }
    }

    if (!integer_ty)
    {
      // [C11 6.4.4.1p6] says that "if an integer constant cannot be
      // represented by any type in its list and has no extended integer
      // type, then the integer constant has no type." However, leaving an
      // expression without a type breaks the code generator and whatnot, so
      // we'll just do whatever GCC or Clang does here. GCC attempts to use
      // __int128, an extended integer type (which [C11 6.4.4.1p6] suggests
      // doing), and Clang settles down to unsigned long long. Given we don't
      // support any extended types, unsigned long long will be the chosen one.
      diag.report(tok.location(),
                  "integer literal is too large to fit in any standard type; "
                  "leaving it as unsigned long long");
      integer_ty = context.ulong_long_ty;
      width = context.target_info().long_long_width;
    }

    // Truncates the result.
    // FIXME: This 64 is hardcoded, get the correct maximum width from the
    // target info.
    val &= -1U >> (64 - width);

    return new (context) IntegerLiteral(val, integer_ty, tok.location());
  }
  else if (literal.is_floating_literal())
  {
    cci_unreachable(); // TODO: Implement me!
  }

  return nullptr;
}

auto Sema::act_on_char_constant(const Token &tok)
  -> std::optional<arena_ptr<CharacterConstant>>
{
  cci_expects(
    tok.is_one_of(Category::char_constant, Category::utf16_char_constant,
                  Category::utf32_char_constant, Category::wide_char_constant));

  small_string<8> spell_buffer;
  std::string_view spelling = scan.get_spelling(tok, spell_buffer);
  CharConstantParser literal(scan, spelling, tok.location(), tok.category(),
                             context.target_info());

  if (literal.has_error)
    return nullptr;

  // [C11 6.4.4.4p10]: An integer character constant has type int.
  QualType char_type = context.int_ty;
  auto char_kind = CharacterConstantKind::Ascii;

  switch (literal.category)
  {
    case Category::utf16_char_constant:
      char_type = context.char16_t_ty;
      char_kind = CharacterConstantKind::UTF16;
      break;
    case Category::utf32_char_constant:
      char_type = context.char32_t_ty;
      char_kind = CharacterConstantKind::UTF32;
      break;
    case Category::wide_char_constant:
      char_type = context.wchar_ty;
      char_kind = CharacterConstantKind::Wide;
      break;
    default: cci_expects(Category::char_constant == literal.category);
  }

  return new (context)
    CharacterConstant(literal.value, char_kind, char_type, tok.location());
}

auto Sema::act_on_string_literal(span<const Token> string_toks)
  -> std::optional<arena_ptr<StringLiteral>>
{
  cci_expects(!string_toks.empty());
  StringLiteralParser literal(scan, string_toks, context.target_info());
  if (literal.has_error)
    return nullptr;

  cci_expects(literal.char_byte_width == 1 || literal.char_byte_width == 2 ||
              literal.char_byte_width == 4);

  QualType elem_type = context.char_ty;
  auto str_kind = StringLiteralKind::Ascii;

  switch (literal.category)
  {
    case Category::utf8_string_literal:
      str_kind = StringLiteralKind::UTF8;
      break;
    case Category::utf16_string_literal:
      elem_type = context.char16_t_ty;
      str_kind = StringLiteralKind::UTF16;
      break;
    case Category::utf32_string_literal:
      elem_type = context.char32_t_ty;
      str_kind = StringLiteralKind::UTF32;
      break;
    case Category::wide_string_literal:
      elem_type = context.wchar_ty;
      str_kind = StringLiteralKind::Wide;
      break;
    default: cci_expects(Category::string_literal == literal.category);
  }

  // Length of string literal including null character.
  const size_t chars_count = literal.num_string_chars() + 1;
  const size_t bytes_count = literal.byte_length();

  // FIXME: Save this type in ASTContext so it can be reused as canonical.
  auto str_ty = QualType(
    new (context) ConstantArrayType(elem_type, chars_count), Qualifiers::None);

  arena_ptr<std::byte> str_data = new (context) std::byte[bytes_count];

  if (literal.char_byte_width == 1)
    new (str_data) char[chars_count];
  else if (literal.char_byte_width == 2)
    new (str_data) char16_t[chars_count];
  else
  {
    cci_expects(literal.char_byte_width == 4);
    new (str_data) char32_t[chars_count];
  }

  std::memcpy(str_data, literal.string().data(), bytes_count);

  const size_t num_concatenated = string_toks.size();
  arena_ptr<srcmap::ByteLoc> tok_locs =
    new (context) srcmap::ByteLoc[num_concatenated];

  auto locs_ptr = tok_locs;
  for (const Token &tok : string_toks)
    *locs_ptr++ = tok.location();
  cci_ensures(locs_ptr == tok_locs + num_concatenated);

  return new (context)
    StringLiteral(str_ty, span(str_data, bytes_count), str_kind,
                  literal.char_byte_width, span(tok_locs, num_concatenated));
}

auto Sema::act_on_paren_expr(arena_ptr<Expr> expr, srcmap::ByteLoc left,
                             srcmap::ByteLoc right)
  -> std::optional<arena_ptr<ParenExpr>>
{
  return new (context) ParenExpr(expr, left, right);
}
