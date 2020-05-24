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

namespace cci::diag2 {

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
    std::string_view message;
    std::vector<DiagnosticParam> params;

    auto get_param_by_name(std::string_view name) const
        -> std::optional<DiagnosticParam>
    {
        auto param_it =
            std::find_if(this->params.begin(), this->params.end(),
                         [name](auto param) { return param.name == name; });
        if (param_it != this->params.end())
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
    syntax::TokenKind value;

    TokenKindArg(syntax::TokenKind value)
        : Arg(DiagnosticParamKind::TokenKind), value(value)
    {}

    bool operator==(const TokenKindArg &) const = default;
};

struct DiagnosticArg final
{
    using ArgType = std::variant<IntArg, StrArg, TokenKindArg>;

    ArgType arg;

    DiagnosticArg(ArgType arg) : arg(std::move(arg)) {}

    bool operator==(const DiagnosticArg &) const = default;

    auto param_kind() const -> DiagnosticParamKind
    {
        return std::visit([](auto &&arg) { return arg.param_kind; }, arg);
    }

    template <typename T>
    auto get_as() const -> const T *
    {
        return std::get_if<T>(arg);
    }
};

/// Information about a diagnostic.
struct Diagnostic
{
    const DiagnosticDescriptor *descriptor;

    /// Location from where the diagnostic was reported.
    std::optional<syntax::ByteLoc> caret_location;

    /// Location spans that are related to this diagnostic.
    std::vector<syntax::ByteSpan> spans;

    /// Arguments for the format message.
    std::vector<std::pair<std::string_view, DiagnosticArg>> args;

    Diagnostic(const DiagnosticDescriptor *descriptor,
               std::optional<syntax::ByteLoc> caret_loc)
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
    std::optional<syntax::ByteLoc> caret_loc;
    std::vector<syntax::ByteSpan> spans;
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

    auto caret_at(syntax::ByteLoc caret_loc) -> DiagnosticBuilder &
    {
        cci_expects(!this->caret_loc.has_value());
        this->caret_loc = caret_loc;
        return *this;
    }

    auto with_span(syntax::ByteSpan span) -> DiagnosticBuilder &
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
    auto make_arg(syntax::TokenKind token_kind) -> TokenKindArg
    {
        return token_kind;
    }

    auto check_arg_kind_matches_param_kind(std::string_view param_name,
                                           DiagnosticArg &arg) const -> bool
    {
        auto param = this->descriptor->get_param_by_name(param_name);
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

} // namespace cci::diag2
