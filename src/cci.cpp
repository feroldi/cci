#include "fmt/format.h"
#include <string>

#ifdef CCI_USING_GIT_REVISION
auto cci_git_refspec() -> const char *;
auto cci_git_hash() -> const char *;
auto cci_git_tag() -> const char *;
#else
auto cci_git_refspec() -> const char * { return ""; }
auto cci_git_hash() -> const char * { return ""; }
auto cci_git_tag() -> const char * { return ""; }
#endif

static auto cci_get_build_version() -> std::string
{
  return fmt::format("{} {} {}", cci_git_refspec(), cci_git_tag(),
                     cci_git_hash());
}

int main()
{
  fmt::print(stderr, "{}\n", cci_get_build_version());
}
