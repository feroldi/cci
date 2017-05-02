#pragma once

#include <string>
#include "../cpp/optional.hpp"
#include "../cpp/string_view.hpp"

namespace utils
{

auto read_stream(string_view path) -> optional<std::string>;

} // namespace utils

