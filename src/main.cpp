#include "lexer.hpp"

int main()
{
  auto tokens = lexer_parse("int main() { return 0; }\n");
  return tokens.size();
}

