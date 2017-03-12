#include "../src/lexer.cpp"

namespace test
{

struct LexerState
{
  string_view input;
  LexerContext context;
  const char* it;

  explicit LexerState(string_view input) noexcept :
    input{input}, context{}, it{input.begin()}
  {}

  template <typename F>
  auto operator() (F&& f) -> const TokenData&
  {
    it = std::forward<F>(f)(context, it, input.end());
    return context.tokens.back();
  }
};

void test_lexer_parse_constant()
{
  fmt::print(stderr, "Testing lexer_parse_constant ... ");

  // IntegerConstants.
  {
    LexerState lexer{"42 0x22fx"};

    auto t1 = lexer(lexer_parse_constant);

    assert(t1.type == TokenType::IntegerConstant);
    assert(t1.data == "42");

    // Should fail
    auto t2 = lexer(lexer_parse_constant);

    assert(lexer.context.tokens.size() == 1);
    assert(t2.type == TokenType::IntegerConstant);
    assert(t2.data == "42");
  }

  // FloatConstants.
  {
    LexerState lexer{"123.f 0.2 1. (1.f) 0.ff .0f"};

    {
      auto t = lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == "123.f");
    }

    {
      auto t = lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == "0.2");
    }

    {
      auto t = lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == "1.");
    }

    {
      auto t = lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == "1.f");
    }

    {
      // Should fail; nothing gets pushed into context.
      auto t = lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == "1.f");

      // Should also skip ill-formed constant.
    }

    {
      auto t= lexer(lexer_parse_constant);
      assert(t.type == TokenType::FloatConstant);
      assert(t.data == ".0f");
    }
  }

  fmt::print(stderr, "Success.\n");
}

void test_lexer_parse_identifier()
{
  fmt::print(stderr, "Testing lexer_parse_identifier ... ");

  LexerState lexer{"abc42() _KeepMoving_Forward 42fail{}success_1 if"};

  {
    auto t = lexer(lexer_parse_identifier);
    assert(t.type == TokenType::Identifier);
    assert(t.data == "abc42");
  }

  {
    auto t = lexer(lexer_parse_identifier);
    assert(t.type == TokenType::Identifier);
    assert(t.data == "_KeepMoving_Forward");
  }

  {
    // Should fail; nothing gets pushed into context.
    auto t = lexer(lexer_parse_identifier);
    assert(t.type == TokenType::Identifier);
    assert(t.data == "_KeepMoving_Forward");

    // Should also skip ill-formed identifier.
  }

  {
    auto t = lexer(lexer_parse_identifier);
    assert(t.type == TokenType::Identifier);
    assert(t.data == "success_1");
  }

  {
    auto t = lexer(lexer_parse_identifier);
    assert(t.type == TokenType::If);
    assert(t.data == "if");
  }

  fmt::print(stderr, "Success.\n");
}

} // namespace test

int main()
{
  test::test_lexer_parse_constant();
  test::test_lexer_parse_identifier();
}

