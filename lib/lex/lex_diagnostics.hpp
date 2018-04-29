#pragma once

#include "cci/util/contracts.hpp"

namespace cci {
namespace diag {
enum Lex
{
#define DIAG(CODE, LEVEL, FORMAT) CODE,
#include "cci/basic/diagnostics_lex.inc"
#undef DIAG
};
} // namespace diag

template <>
struct diagnostics_error_code<diag::Lex>
{
  constexpr static auto info(diag::Lex code) -> ErrorCodeInfo
  {
#define DIAG(CODE, LEVEL, FORMAT)                                              \
  case diag::CODE: return {LEVEL, FORMAT};
    switch (code)
    {
      default: cci_unreachable();
#include "cci/basic/diagnostics_lex.inc"
    }
#undef DIAG
  }
};

template <>
struct is_diagnostics_error_code<diag::Lex> : std::true_type
{};
} // namespace cci
