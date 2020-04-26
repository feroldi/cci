#pragma once

#include "cci/syntax/source_map.hpp"
#include "cci/syntax/token.hpp"
#include "cci/util/contracts.hpp"
#include <atomic>
#include <functional>
#include <initializer_list>
#include <optional>
#include <string>
#include <variant>

namespace cci::syntax {

using namespace cci::srcmap;

enum class Diag
{
    expected_but_got,
    integer_literal_overflow,
    integer_literal_too_large,
    invalid_digit,
    invalid_suffix,
    missing_exponent_digits,
    missing_binary_exponent,
    missing_ucn_escape_hex_digits,
    invalid_ucn,
    escape_out_of_range,
    missing_escape_digits,
    unknown_escape_sequence,
    unicode_too_large_for_unit,
    char_const_empty,
    char_const_overflow,
    nonstandard_string_concat,
    incomplete_ucn,
    unterminated_comment,
    unterminated_char_const,
    unterminated_string_literal,
    unknown_character,
    invalid_unicode_char,
    typecheck_subscript_value,
    typecheck_subscript_not_integer,
};

enum class DiagnosticLevel
{
    Error,
};

enum class ArgKind
{
    Str,
    Int,
    TokenKind,
};

struct Arg
{
    std::string_view name;
    ArgKind kind;

    Arg(std::string_view name, ArgKind kind) : name(name), kind(kind) {}
};

struct DiagnosticDescriptor
{
    DiagnosticLevel level;
    std::string_view message;
    std::vector<Arg> args;
};

class DiagnosticBag
{
public:
    DiagnosticBag() = default;

    auto empty() -> bool { return true; }
};

/// Information about a diagnostic.
struct Diagnostic
{
    using Arg = std::variant<int, std::string>;

    DiagnosticDescriptor *descriptor;

    /// Location from where the diagnostic was reported.
    std::optional<ByteLoc> caret_loc;

    /// Location spans that are related to this diagnostic.
    std::vector<ByteSpan> spans;

    /// Arguments for the format message.
    std::vector<std::pair<std::string_view, Arg>> args;

    Diagnostic(DiagnosticDescriptor *descriptor,
               std::optional<ByteLoc> caret_loc)
        : descriptor(descriptor), caret_loc(caret_loc)
    {}
};

/// Helper class to construct a `Diagnostic`.
struct DiagnosticBuilder
{
    DiagnosticBuilder(DiagnosticDescriptor &descriptor)
        : descriptor(&descriptor)
    {}

    DiagnosticBuilder(DiagnosticBuilder &&) = default;
    DiagnosticBuilder &operator=(DiagnosticBuilder &&) = default;

    auto build() -> Diagnostic
    {
        auto diag = Diagnostic(this->descriptor, this->caret_loc);
        diag.spans = std::move(spans);
        diag.args = std::move(args);
        return diag;
    }

    auto caret_at(ByteLoc caret_loc) -> DiagnosticBuilder &
    {
        cci_expects(!this->caret_loc.has_value());
        this->caret_loc = caret_loc;
        return *this;
    }

    auto with_span(ByteSpan span) -> DiagnosticBuilder &
    {
        this->spans.push_back(span);
        return *this;
    }

    template <typename Arg>
    auto with_arg(std::string_view param, Arg &&arg) -> DiagnosticBuilder &
    {
        this->args.emplace_back(param, std::forward<Arg>(arg));
        return *this;
    }

private:
    DiagnosticDescriptor *descriptor;
    std::optional<ByteLoc> caret_loc;
    std::vector<ByteSpan> spans;
    std::vector<std::pair<std::string_view, Diagnostic::Arg>> args;
};

} // namespace cci::syntax
