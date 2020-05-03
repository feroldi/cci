#pragma once

#include "cci/syntax/source_map.hpp"
#include "cci/syntax/token.hpp"
#include "cci/util/contracts.hpp"
#include <atomic>
#include <compare>
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

enum class DiagnosticParamKind
{
    Str,
    Int,
    TokenKind,
};

struct DiagnosticParam
{
    std::string_view name;
    DiagnosticParamKind kind;

    DiagnosticParam(std::string_view name, DiagnosticParamKind kind)
        : name(name), kind(kind)
    {}
};

struct DiagnosticDescriptor
{
    DiagnosticLevel level;
    std::string_view message;
    std::vector<DiagnosticParam> args;

    auto get_param(std::string_view name) const
        -> std::optional<DiagnosticParam>
    {
        auto param_it =
            std::find_if(this->args.begin(), this->args.end(),
                         [name](auto param) { return param.name == name; });
        if (param_it != this->args.end())
            return *param_it;
        else
            return std::nullopt;
    }
};

struct Arg
{
    DiagnosticParamKind param_kind;

    Arg(DiagnosticParamKind param_kind) : param_kind(param_kind) {}

    auto operator<=>(const Arg &) const = default;
};

struct StrArg final : Arg
{
    std::string value;

    StrArg(std::string &&value)
        : Arg(DiagnosticParamKind::Str), value(std::move(value))
    {}

    bool operator==(const StrArg &) const = default;
};

struct IntArg final : Arg
{
    int value;

    IntArg(int value) : Arg(DiagnosticParamKind::Int), value(value) {}

    bool operator==(const IntArg &) const = default;
};

struct TokenKindArg final : Arg
{
    TokenKind value;

    TokenKindArg(TokenKind value)
        : Arg(DiagnosticParamKind::TokenKind), value(value)
    {}

    bool operator==(const TokenKindArg &) const = default;
};

using DiagnosticArgBase = std::variant<IntArg, StrArg, TokenKindArg>;

struct DiagnosticArg final : DiagnosticArgBase
{
    using DiagnosticArgBase::DiagnosticArgBase;
    using DiagnosticArgBase::operator=;

    auto param_kind() const -> DiagnosticParamKind
    {
        return std::visit([](auto &&arg) { return arg.param_kind; }, *this);
    }

    template <typename T>
    auto get_as() const -> const T *
    {
        return std::get_if<T>(this);
    }
};

/// Information about a diagnostic.
struct Diagnostic
{
    const DiagnosticDescriptor *descriptor;

    /// Location from where the diagnostic was reported.
    std::optional<ByteLoc> caret_location;

    /// Location spans that are related to this diagnostic.
    std::vector<ByteSpan> spans;

    /// Arguments for the format message.
    std::vector<std::pair<std::string_view, DiagnosticArg>> args;

    Diagnostic(const DiagnosticDescriptor *descriptor,
               std::optional<ByteLoc> caret_loc)
        : descriptor(descriptor), caret_location(caret_loc)
    {}

    bool operator==(const Diagnostic &) const = default;
};

class DiagnosticBag
{
    std::vector<Diagnostic> diagnostics;

public:
    using iterator = std::vector<Diagnostic>::const_iterator;

    DiagnosticBag() = default;

    auto empty() const -> bool { return this->diagnostics.empty(); }

    void add(Diagnostic &&diagnostic)
    {
        this->diagnostics.push_back(std::move(diagnostic));
    }

    auto begin() const -> iterator { return std::begin(diagnostics); }
};

/// Helper class to construct a `Diagnostic`.
class DiagnosticBuilder
{
    const DiagnosticDescriptor *descriptor;
    std::optional<ByteLoc> caret_loc;
    std::vector<ByteSpan> spans;
    std::vector<std::pair<std::string_view, DiagnosticArg>> args;

public:
    DiagnosticBuilder(const DiagnosticDescriptor &descriptor)
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
    auto with_arg(std::string_view param_name, Arg &&arg) -> DiagnosticBuilder &
    {
        cci_expects(check_param_is_not_set(param_name));

        auto diag_arg = DiagnosticArg(make_arg(std::forward<Arg>(arg)));

        cci_expects(check_arg_kind_matches_param_kind(param_name, diag_arg));

        this->args.emplace_back(param_name, std::move(diag_arg));
        return *this;
    }

private:
    auto make_arg(int value) -> IntArg { return value; }
    auto make_arg(std::string value) -> StrArg { return value; }
    auto make_arg(TokenKind token_kind) -> TokenKindArg { return token_kind; }

    auto check_arg_kind_matches_param_kind(std::string_view param_name,
                                           DiagnosticArg &arg) const -> bool
    {
        auto param = this->descriptor->get_param(param_name);
        cci_expects(param.has_value());
        return arg.param_kind() == param->kind;
    }

    auto check_param_is_not_set(std::string_view param_name) const -> bool
    {
        auto arg_it = std::find_if(
            this->args.begin(), this->args.end(),
            [param_name](auto arg) { return arg.first == param_name; });
        return arg_it == this->args.end();
    }
};

} // namespace cci::syntax
