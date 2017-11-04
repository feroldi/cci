#include "fmt/format.h"
#include <string>

#ifdef CCI_USING_GIT_REVISION
extern const char *const GIT_REFSPEC;
extern const char *const GIT_HASH;
extern const char *const GIT_TAG;
#else
constexpr const char *const GIT_REFSPEC = "";
constexpr const char *const GIT_HASH = "";
constexpr const char *const GIT_TAG = "";
#endif

[[maybe_unused]]
static auto cci_get_build_version() -> std::string
{
  return fmt::format("{} {} {}", GIT_REFSPEC, GIT_TAG, GIT_HASH);
}

int main()
{
}
