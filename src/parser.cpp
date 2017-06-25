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

using TokenData = TokenStream::TokenData;
using TokenIterator = TokenStream::iterator;

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
  std::unique_ptr<SyntaxTree> tree;

  explicit ParserSuccess(std::unique_ptr<SyntaxTree> tree)
    : tree(std::move(tree))
  {}

  explicit ParserSuccess(std::nullptr_t)
    : tree(nullptr)
  {}
};

// TODO: use LLVM SmallVector so dynamic allocation only happens
// for more than one error.
using ParserFailure = std::vector<ParserError>;

using ParserState = variant<ParserSuccess, ParserFailure>;

struct ParserResult
{
  ParserState state;
  TokenStream::iterator it; //< next token (TODO should rename it?)

  explicit ParserResult(ParserState state, TokenStream::iterator it)
    : state(std::move(state)), it(it)
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

  explicit ParserContext(ProgramContext& program, const TokenStream& tokens)
    : program(program),
      token_stream(tokens)
  {}
};

template <typename Rule, typename... Rules>
auto parse_one_of(ParserContext& parser, TokenIterator begin,
                  TokenIterator end, Rule rule, Rules&&... rules)
  -> ParserResult
{
  if (auto result = rule(parser, begin, end))
  {
    return result;
  }

  if constexpr (sizeof...(rules))
  {
    return parse_one_of(parser, begin, end, std::forward<Rules>(rules)...);
  }
  else
  {
    return ParserResult(make_error(begin), end);
  }
}

template <typename Rule, typename Condition>
auto parse_until_if(ParserContext& parser, Rule rule, TokenIterator begin,
                    TokenIterator end, Condition cond)
  -> ParserResult
{
  auto tree = std::make_unique<SyntaxTree>();
  auto it = begin;

  // Tracks errors. Keeps in a successful state
  // if no error occurs.
  ParserState state = ParserSuccess(nullptr);

  while (it != end && cond(it))
  {
    auto result = rule(parser, it, end);

    if (is<ParserSuccess>(result.state))
    {
      tree->add_child(get<ParserSuccess>(std::move(result.state)).tree);
    }
    else
    {
      add_error(state, get<ParserFailure>(std::move(result.state)));
    }

    it = result.it;
  }

  if (is<ParserSuccess>(state))
  {
    return ParserResult(ParserSuccess(std::move(tree)), it);
  }
  else
  {
    return ParserResult(std::move(state), it);
  }
}

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
      state = ParserSuccess(std::make_unique<SyntaxTree>(NodeType::Constant, *begin));
      break;

    default:
      add_error(state, ParserError(begin, "expected constant"));
  }

  return ParserResult(std::move(state), std::next(begin));
}

} // namespace

// TODO
auto SyntaxTree::parse(ProgramContext& program, const TokenStream& tokens)
  -> std::unique_ptr<SyntaxTree>
{
  ParserContext context{program, tokens};

  auto result = parse_until_if(
      context, parse_constant, tokens.begin(), tokens.end(),
      [] (TokenIterator it) {
        return it->type == TokenType::IntegerConstant;
      });

  if (result)
  {
    return get<ParserSuccess>(std::move(result.state)).tree;
  }

  return nullptr;
}

} // namespace ccompiler
