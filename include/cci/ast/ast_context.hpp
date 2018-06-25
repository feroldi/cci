#pragma once
#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/langopts.hpp"
#include "cci/util/span.hpp"
#include <cstdint>

namespace cci {

struct ASTContext
{
private:
  const SourceManager &src_mgr;
  CompilerDiagnostics &diags;
  const TargetInfo &target;

public:
  ASTContext(const SourceManager &sm, CompilerDiagnostics &d,
             const TargetInfo &ti)
    : src_mgr(sm), diags(d), target(ti)
  {}

  auto source_mgr() const -> const SourceManager & { return src_mgr; }
  auto diagnostics() const -> CompilerDiagnostics & { return diags; }
  auto target_info() const -> const TargetInfo & { return target; }
};

} // namespace cci
