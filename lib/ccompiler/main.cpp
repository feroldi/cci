#include <cstdio>
#include <string>
#include "cpp/format.hpp"
#include "cpp/contracts.hpp"
#include "utils/args.hpp"
#include "program.hpp"
#include "lexer.hpp"
#include "parser.hpp"

namespace cc = ccompiler;

static void show_help()
{
  fmt::print(stderr, "ccompiler 0.1-alpha\n");
}

int main(int argc, char** argv)
{
  // skip program name
  std::advance(argv, 1);
  argc -= 1;

  try
  {
    const auto opts = cc::Options::parse_arguments(argc, argv);

    if (opts.show_help)
    {
      show_help();
      return 0;
    }

    auto program = cc::ProgramContext(opts);

    for (const auto& filename : opts.source_filenames)
    {
      auto source = cc::SourceManager::from_path(filename);
      auto token_stream = cc::TokenStream::parse(program, source);

      if (program.has_errors())
        return 1;

      auto tree = cc::SyntaxTree::parse(program, token_stream);

      if (opts.dump_ast && tree)
      {
        tree->dump();
      }
    }

    if (opts.syntax_only)
    {
      return 0;
    }
  }
  catch (const std::exception& e)
  {
    fmt::print(stderr, "ccompiler: fatal: {}\n", e.what());
  }
}
