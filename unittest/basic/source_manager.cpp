#include "cci/util/contracts.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/basic/diagnostics.hpp"
#include "gtest/gtest.h"
#include <string>
#include <string_view>

namespace {

using std::string_view_literals::operator""sv;

TEST(SourceManagerTest, calcLineOffsets)
{
  std::string_view code =
    "int main()\n"
    "{\n"
    "  return 0;\n"
    "}\n";

  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto src_mgr = cci::SourceManager::from_buffer(diag, std::string(code));

  ASSERT_EQ(code, src_mgr.full_text());

  {
    cci::SourceLocation int_begin(0ul);
    cci::SourceLocation int_end(3ul);

    EXPECT_EQ("int"sv, src_mgr.text_slice(cci::SourceRange(int_begin, int_end)));
    EXPECT_EQ(std::pair(1ul, 1ul), src_mgr.translate_to_linecolumn(int_begin));
    EXPECT_EQ(std::pair(1ul, 4ul), src_mgr.translate_to_linecolumn(int_end));
    EXPECT_EQ("int main()"sv, src_mgr.text_line(int_begin));
  }

  {
    cci::SourceLocation return_begin(15ul);
    cci::SourceLocation return_end(21ul);

    EXPECT_EQ("return"sv,
              src_mgr.text_slice(cci::SourceRange(return_begin, return_end)));
    EXPECT_EQ(std::pair(3ul, 3ul), src_mgr.translate_to_linecolumn(return_begin));
    EXPECT_EQ(std::pair(3ul, 9ul), src_mgr.translate_to_linecolumn(return_end));
    EXPECT_EQ("  return 0;"sv, src_mgr.text_line(return_begin));
  }

#if CCI_ENABLE_CONTRACTS
  EXPECT_THROW(src_mgr.text_line(cci::SourceLocation(code.size())),
               broken_contract);
#endif
};

} // namespace
