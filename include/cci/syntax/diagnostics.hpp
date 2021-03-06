#pragma once

#include "cci/syntax/source_map.hpp"
#include "cci/syntax/token.hpp"
#include "cci/util/contracts.hpp"
#include "cci/util/small_vector.hpp"
#include <atomic>
#include <functional>
#include <optional>
#include <string>
#include <variant>

namespace cci::diag {

struct Handler;

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

/// Information about a diagnostic.
struct Diagnostic
{
    using Arg = std::variant<syntax::TokenKind, char>;

    /// Location from where the diagnostic was reported.
    syntax::SourceLoc loc;

    /// The diagnostic message.
    Diag msg;

    /// Location ranges that are related to this diagnostic.
    std::vector<syntax::ByteSpan> ranges;

    /// Arguments for the format message.
    std::vector<Arg> args;

    Diagnostic(syntax::SourceLoc loc, Diag msg) : loc(loc), msg(msg) {}
};

/// Helper class to construct a `Diagnostic`.
struct DiagnosticBuilder
{
    DiagnosticBuilder(syntax::SourceLoc loc, Diag msg, Handler &handler)
        : handler(&handler), diag(std::make_unique<Diagnostic>(loc, msg))
    {}

    DiagnosticBuilder(DiagnosticBuilder &&) = default;
    DiagnosticBuilder &operator=(DiagnosticBuilder &&) = default;

    /// Adds a source range to give more context to the diagnostic.
    auto ranges(std::initializer_list<syntax::ByteSpan> ranges)
        -> DiagnosticBuilder &
    {
        for (const syntax::ByteSpan r : ranges)
            this->diag->ranges.push_back(r);
        return *this;
    }

    /// Adds an argument to the diagnostic.
    template <typename... Args>
    auto args(Args &&... args) -> DiagnosticBuilder &
    {
        ((void)this->diag->args.push_back(std::forward<Args>(args)), ...);
        return *this;
    }

    /// Hands the diagnostic to the handler.
    ~DiagnosticBuilder() noexcept(!CCI_CONTRACTS);

private:
    /// The handler to which the diagnostic will be handed.
    Handler *handler;

    /// Diagnostic which is being constructed.
    std::unique_ptr<Diagnostic> diag;
};

/// A diagnostic handler.
//
/// Diagnostics are reported and treated through this handler. Once reported, a
/// diagnostic is passed to an emitter (a function callback) responsible to
/// treat it. It may do anything like aborting compilation process, or just
/// completely ignore diagnostics.
struct Handler
{
    const syntax::SourceMap &source_map;

    /// The emitter type.
    using Emitter = std::function<void(const Diagnostic &)>;

    /// Constructs a handler with a given `Emitter` and the `SourceMap`
    /// associated with it.
    Handler(Emitter emitter, const syntax::SourceMap &source_map)
        : source_map(source_map), emitter(std::move(emitter))
    {}

    Handler(Handler &&other) = delete;
    Handler &operator=(Handler &&other) = delete;

    /// Helper function to facilitate the construction of a `DiagnosticBuilder`.
    auto report(syntax::ByteLoc loc, Diag msg) -> DiagnosticBuilder
    {
        DiagnosticBuilder builder(source_map.lookup_source_location(loc), msg,
                                  *this);
        return builder;
    }

    /// Checks whether there has been any errors reported.
    auto has_errors() const -> bool { return this->err_count_ > 0; }

    /// Returns how many errors have been reported.
    auto err_count() const -> size_t { return this->err_count_.load(); }

    /// Sets a new emitter to be called at diagnostic emission, overriding the
    /// old one.
    void set_emitter(Emitter emitter) { this->emitter = std::move(emitter); }

protected:
    Emitter emitter;
    friend struct DiagnosticBuilder;

    /// Emits a diagnostic.
    void emit(std::unique_ptr<Diagnostic> diag)
    {
        cci_expects(diag);
        this->emitter(*diag);
        this->bump_err_count();
    }

private:
    std::atomic<size_t> err_count_ = 0;

    /// Increases the error count by one.
    void bump_err_count() { this->err_count_.fetch_add(1); }
};

/// Returns a diagnostic emitter that just ignores diagnostics.
auto ignoring_emitter() -> Handler::Emitter;

} // namespace cci::diag
