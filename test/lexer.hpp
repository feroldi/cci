#pragma once

#include <cassert>
#include "../src/lexer.hpp"

namespace test
{

void tokenize_constants()
{
  auto text = string_view("123 123.4 1.f 2. 0x42 'F' '\\n' \"this is string\" ");
  auto tokens = lexer_tokenize_text(text);

  assert( tokens.size() == 8 );

  assert( tokens[0].type == TokenType::IntegerConstant );
  assert( tokens[0].data == "123" );

  assert( tokens[1].type == TokenType::FloatConstant );
  assert( tokens[1].data == "123.4" );

  assert( tokens[2].type == TokenType::FloatConstant );
  assert( tokens[2].data == "1.f" );

  assert( tokens[3].type == TokenType::FloatConstant );
  assert( tokens[3].data == "2." );

  assert( tokens[4].type == TokenType::IntegerConstant );
  assert( tokens[4].data == "0x42" );

  assert( tokens[5].type == TokenType::CharConstant );
  assert( tokens[5].data == "'F'" );

  assert( tokens[6].type == TokenType::CharConstant );
  assert( tokens[6].data == "'\\n'" );

  assert( tokens[7].type == TokenType::StringConstant );
  assert( tokens[7].data == "\"this is string\"" );
}

void run_tokenizer()
{
  tokenize_constants();
}

}

