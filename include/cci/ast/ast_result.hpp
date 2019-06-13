#pragma once
#include "cci/ast/arena_types.hpp"
#include <optional>

namespace cci {
template <typename T>
using ActionResult = std::optional<arena_ptr<T>>;
using ExprResult = ActionResult<Expr>;
} // namespace cci
