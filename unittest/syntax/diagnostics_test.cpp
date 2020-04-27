#include "cci/syntax/diagnostics_new.hpp"
#include "cci/syntax/source_map.hpp"
#include "gtest/gtest.h"
#include <string_view>

using namespace std::string_view_literals;
using namespace cci::syntax;

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
    const auto bag = DiagnosticBag();
    EXPECT_EQ(true, bag.empty());
}

TEST_F(DiagnosticsTest, addingToDiagnosticBagMakesItNonEmpty)
{
    auto diag = DiagnosticBuilder(custom_descriptor).build();
    auto bag = DiagnosticBag();

    bag.add(std::move(diag));

    EXPECT_EQ(false, bag.empty());
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
    EXPECT_EQ(ByteLoc(42), diag.caret_location);
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
    DiagnosticDescriptor descriptor{
        .level = DiagnosticLevel::Error,
        .message = "a diagnostic description",
        .args =
            {
                DiagnosticParam("a", DiagnosticParamKind::Int),
                DiagnosticParam("b", DiagnosticParamKind::Str),
                DiagnosticParam("c", DiagnosticParamKind::TokenKind),
            },
    };

    auto diag = DiagnosticBuilder(descriptor)
                    .with_arg("a", 42)
                    .with_arg("b", "foo")
                    .with_arg("c", cci::TokenKind::comma)
                    .build();

    EXPECT_EQ(&descriptor, diag.descriptor);

    ASSERT_EQ(3, diag.args.size());

    EXPECT_EQ("a", diag.args[0].first);
    EXPECT_EQ(DiagnosticArg(IntArg(42)), diag.args[0].second);

    EXPECT_EQ("b", diag.args[1].first);
    EXPECT_EQ(DiagnosticArg(StrArg("foo")), diag.args[1].second);

    EXPECT_EQ("c", diag.args[2].first);
    EXPECT_EQ(DiagnosticArg(TokenKindArg(cci::TokenKind::comma)),
              diag.args[2].second);
}

} // namespace

