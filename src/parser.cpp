#include <memory>
#include <vector>
#include "cpp/contracts.hpp"
#include "cpp/format.hpp"
#include "cpp/variant.hpp"
#include "lexer.hpp"
#include "source_manager.hpp"
#include "program.hpp"
#include "parser.hpp"

namespace ccompiler
{

namespace
{

struct ParserError
{
  TokenStream::iterator where;
  std::string error;

  explicit ParserError(TokenStream::iterator where)
    : where(where)
  {}

  explicit ParserError(TokenStream::iterator where, std::string error)
    : where(where), error(std::move(error))
  {}
};

struct ParserSuccess
{
  std::shared_ptr<SyntaxTree> tree;

  explicit ParserSuccess(std::shared_ptr<SyntaxTree> tree)
    : tree(std::move(tree))
  {}

  explicit ParserSuccess(std::nullptr_t)
    : tree(nullptr)
  {}
};

// FIXME: use LLVM SmallVector so dynamic allocation only happens
// for more than one error.
using ParserFailure = std::vector<ParserError>;

using ParserState = variant<ParserSuccess, ParserFailure>;

struct ParserResult
{
  ParserState state;
  TokenStream::iterator end;

  explicit ParserResult(ParserState state, TokenStream::iterator end)
    : state(std::move(state)), end(end)
  {}

  operator bool() const noexcept
  {
    return is<ParserSuccess>(this->state);
  }
};

template <typename... Args>
auto make_error(Args&&... args) -> ParserState
{
  return ParserFailure{ParserError(std::forward<Args>(args)...)};
}

template <typename... Args>
auto make_success(Args&&... args) -> ParserState
{
  return ParserSuccess(std::make_shared<SyntaxTree>(std::forward<Args>(args)...));
}

void add_error(ParserState& state, ParserError error)
{
  if (is<ParserSuccess>(state))
  {
    state = make_error(std::move(error));
  }
  else
  {
    get<ParserFailure>(state).emplace_back(std::move(error));
  }
}

void add_error(ParserState& state, ParserFailure errors)
{
  for (auto& error : errors)
  {
    add_error(state, std::move(error));
  }
}

struct ParserContext
{
  ProgramContext& program;
  const TokenStream& token_stream;
};

using TokenData = TokenStream::TokenData;
using TokenIterator = TokenStream::iterator;

auto parse_constant(ParserContext& parser, TokenIterator begin, TokenIterator end)
  -> ParserResult
{
  Expects(begin != end);

  ParserState state = ParserSuccess(nullptr);

  switch (begin->type)
  {
    case TokenType::CharConstant:
    case TokenType::IntegerConstant:
    case TokenType::OctIntegerConstant:
    case TokenType::HexIntegerConstant:
    case TokenType::FloatConstant:
    case TokenType::StringConstant:
      state = make_success(NodeType::Constant, begin);
      break;

    default:
      add_error(state, ParserError(begin, "expected constant"));
  }

  return ParserResult(std::move(state), std::next(begin));
}

} // namespace

// TODO
auto SyntaxTree::parse(ProgramContext& program, const TokenStream& tokens)
  -> std::shared_ptr<SyntaxTree>
{
  ParserContext context{program, tokens};

  auto result = parse_constant(context, tokens.begin(), tokens.end());

  if (result)
  {
    return get<ParserSuccess>(std::move(result.state)).tree;
  }

  return nullptr;
}

} // namespace ccompiler
