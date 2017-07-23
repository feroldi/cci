#include "cpp/contracts.hpp"
#include "cpp/format.hpp"
#include "cpp/string_view.hpp"
#include "utils/args.hpp"
#include "program.hpp"
#include "source_manager.hpp"

namespace ccompiler
{

auto Options::parse_arguments([[maybe_unused]] int argc, char**& argv) -> Options
{
  using utils::opt_exists;
  using utils::opt_get;
  using utils::is_opt;

  auto opts = Options{};

  while (*argv != nullptr)
  {
    if (opt_exists(argv, "-h", "--help"))
    {
      opts.show_help = true;
      break;
    }
    else if (opt_exists(argv, "", "-pedantic"))
    {
      opts.pedantic = true;
    }
    else if (opt_exists(argv, "", "-pedantic-errors"))
    {
      opts.pedantic = true;
      opts.pedantic_errors = true;
    }
    else if (auto filename = opt_get(argv, "-o", "--out"); !filename.empty())
    {
      opts.output_filename = std::string(filename.begin(), filename.end());
    }
    else if (auto level = opt_get(argv, "-O", "--optimization-level"); !level.empty())
    {
      // FIXME should use exception-free `std::from_chars`
      int lvl = std::stoi(std::string(level.begin(), level.end()));
      assert(lvl > 0);
      opts.optimization_level = static_cast<uint32_t>(lvl);
    }
    else if (auto warn_opt = opt_get(argv, "-W", ""); !warn_opt.empty())
    {
      if (warn_opt == "error")
      {
        opts.warning_as_error = true;
      }
    }
    else if (auto flag = opt_get(argv, "-f", ""); !flag.empty())
    {
      if (flag == "syntax-only")
      {
        opts.syntax_only = true;
      }
    }
    else if (is_opt(argv))
    {
      throw utils::invalid_option(fmt::format("unknown option: `{}'", *argv));
    }
    else
    {
      // Unknown but invalid parameters are treated like source filenames.
      opts.source_filenames.emplace_back(*argv);
      std::advance(argv, 1);
    }
  }

  if (!opts.show_help && opts.source_filenames.empty())
  {
    throw std::logic_error("missing input files");
  }

  return opts;
}

void Options::dump(std::FILE* out) const
{
  size_t indent_level = 0;

  auto output = [&] (const fmt::StringRef& content) {
    fmt::print(out, "{}{}\n", std::string(indent_level * 4, ' '), content);
  };

  output("{");
  indent_level += 1;

  output(fmt::format("\"pedantic\": {},", this->pedantic));
  output(fmt::format("\"pedantic-errors\": {},", this->pedantic_errors));
  output(fmt::format("\"warning_as_error\": {},", this->warning_as_error));
  output(fmt::format("\"syntax_only\": {},", this->syntax_only));
  output(fmt::format("\"output_filename\": \"{}\",", this->output_filename));
  output(fmt::format("\"optimization_level\": {},", this->optimization_level));

  output("\"source_filenames\": [");
  indent_level += 1;

  for (const auto& filename : this->source_filenames)
  {
    output(fmt::format("\"{}\",", filename)); //< FIXME no comma after last item
  }

  indent_level -= 1;
  output("]");
  indent_level -= 1;
  output("}");
}

auto base_format_error(const char* from, DiagLevel level, const optional<LineInfo>& line_info,
                       string_view description) -> std::string
{
  std::string message;
  message.reserve(256);

  if (from)
  {
    message += from;
    message.push_back(':');
  }
  else
  {
    message += "ccompiler:";
  }

  if (line_info)
  {
    message +=
      fmt::format("{}:{}:", line_info->pos.line_no, line_info->pos.column_no);
  }

  message += [&] {
    switch (level)
    {
      case DiagLevel::Note:
        return " note: ";
      case DiagLevel::Warning:
        return " warning: ";
      case DiagLevel::Error:
        return " error: ";
      case DiagLevel::Fatal:
        return " fatal: ";
      default:
        Unreachable();
    }
  }();

  message += description.data();
  message.push_back('\n');

  if (line_info)
  {
    auto [pos, line, range] = *line_info;
    std::string highlight;
    highlight.reserve(pos.column_no + range.size());

    for (int i = 0; i < static_cast<int>(pos.column_no); ++i)
    {
      highlight.push_back(*std::next(line.begin(), i) == '\t' ? '\t' : ' ');
    }

    highlight.back() = '^';

    if (range.size() > 1)
      highlight += std::string(range.size() - 1, '~');

    message +=
      fmt::format("{}\n{}", string_ref(line), highlight);
  }

  return message;
}

} // namespace ccompiler
