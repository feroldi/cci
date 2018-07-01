#pragma once

#include "cci/util/contracts.hpp"

namespace cci {
namespace diag {
enum Sema
{
#define DIAG(CODE, LEVEL, FORMAT) CODE,
#include "cci/basic/diagnostics_sema.inc"
#undef DIAG
};
} // namespace diag

template <>
struct diagnostics_error_code<diag::Sema>
{
  constexpr static auto info(diag::Sema code) -> ErrorCodeInfo
  {
#define DIAG(CODE, LEVEL, FORMAT)                                              \
  case diag::CODE: return {LEVEL, FORMAT};
    switch (code)
    {
      default: cci_unreachable();
#include "cci/basic/diagnostics_sema.inc"
    }
#undef DIAG
  }
};

template <>
struct is_diagnostics_error_code<diag::Sema> : std::true_type
{};
} // namespace cci
