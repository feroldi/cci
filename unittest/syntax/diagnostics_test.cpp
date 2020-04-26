#include "cci/syntax/diagnostics_new.hpp"
#include "cci/syntax/source_map.hpp"
#include "gtest/gtest.h"
#include <string_view>

using namespace std::string_view_literals;
using namespace std::string_literals;

using cci::syntax::Arg;
using cci::syntax::ArgKind;
using cci::syntax::ByteLoc;
using cci::syntax::ByteSpan;
using cci::syntax::Diag;
using cci::syntax::Diagnostic;
using cci::syntax::DiagnosticBag;
using cci::syntax::DiagnosticBuilder;
using cci::syntax::DiagnosticDescriptor;
using cci::syntax::DiagnosticLevel;

namespace {

struct DiagnosticsTest : ::testing::Test
{
protected:
    DiagnosticDescriptor custom_descriptor{
        .level = DiagnosticLevel::Error,
        .message = "a diagnostic description",
    };
};

TEST_F(DiagnosticsTest, initialDiagnosticBagIsEmpty)
{
    auto bag = DiagnosticBag();
    EXPECT_EQ(true, bag.empty());
}

TEST_F(DiagnosticsTest, builderWithDescriptor)
{
    auto diag = DiagnosticBuilder(custom_descriptor).build();
    EXPECT_EQ(&custom_descriptor, diag.descriptor);
}

TEST_F(DiagnosticsTest, builderWithCarret)
{
    auto diag =
        DiagnosticBuilder(custom_descriptor).caret_at(ByteLoc(42)).build();

    EXPECT_EQ(&custom_descriptor, diag.descriptor);
    EXPECT_EQ(ByteLoc(42), diag.caret_loc);
}

TEST_F(DiagnosticsTest, builderWithSpans)
{
    auto diag = DiagnosticBuilder(custom_descriptor)
                    .with_span(ByteSpan(ByteLoc(0), ByteLoc(3)))
                    .with_span(ByteSpan(ByteLoc(5), ByteLoc(10)))
                    .build();

    EXPECT_EQ(&custom_descriptor, diag.descriptor);

    ASSERT_EQ(2, diag.spans.size());
    EXPECT_EQ(ByteSpan(ByteLoc(0), ByteLoc(3)), diag.spans[0]);
    EXPECT_EQ(ByteSpan(ByteLoc(5), ByteLoc(10)), diag.spans[1]);
}

TEST_F(DiagnosticsTest, builderWithArgs)
{
    auto diag = DiagnosticBuilder(custom_descriptor)
                    .with_arg("a", 42)
                    .with_arg("b", "foo")
                    .build();

    EXPECT_EQ(&custom_descriptor, diag.descriptor);

    ASSERT_EQ(2, diag.args.size());
    EXPECT_EQ(std::pair("a"sv, Diagnostic::Arg(42)), diag.args[0]);
    EXPECT_EQ(std::pair("b"sv, Diagnostic::Arg("foo"s)), diag.args[1]);
}

} // namespace
