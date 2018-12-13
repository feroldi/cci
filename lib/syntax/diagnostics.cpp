#include "cci/syntax/diagnostics.hpp"

namespace cci::diag {

DiagnosticBuilder::~DiagnosticBuilder() noexcept(!CCI_CONTRACTS)
{
    this->handler.emit(std::move(diag));
}

auto ignoring_emitter() -> Handler::Emitter
{
    return [](const Diagnostic &) {};
}

} // namespace cci::diag
