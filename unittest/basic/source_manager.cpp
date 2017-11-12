#include "cci/basic/source_manager.hpp"
#include "gtest/gtest.h"
#include <string>
#include <string_view>

namespace
{

using std::string_view_literals::operator""sv;

TEST(SourceManagerTest, calcLineOffsets)
{
  std::string code = "int main()\n{\n  return 0;\n}\n";
  auto sm = cci::SourceManager::from_buffer(code);

  EXPECT_EQ(code, sm.get_text());

  {
    cci::SourceLocation int_begin(0u);
    cci::SourceLocation int_end(3u);

    EXPECT_EQ("int"sv, sm.get_text(cci::SourceRange(int_begin, int_end)));
    EXPECT_EQ(std::pair(1u, 1u), sm.get_linecol(int_begin));
    EXPECT_EQ(std::pair(1u, 4u), sm.get_linecol(int_end));
    EXPECT_EQ("int main()", sm.get_line_text(int_begin));
  }

  {
    // Locates "return".
    cci::SourceLocation return_begin(15u);
    cci::SourceLocation return_end(21u);

    EXPECT_EQ("return"sv,
              sm.get_text(cci::SourceRange(return_begin, return_end)));
    EXPECT_EQ(std::pair(3u, 3u), sm.get_linecol(return_begin));
    EXPECT_EQ(std::pair(3u, 9u), sm.get_linecol(return_end));
    EXPECT_EQ("  return 0;", sm.get_line_text(return_begin));
  }
};

} // anonymouns namespace
