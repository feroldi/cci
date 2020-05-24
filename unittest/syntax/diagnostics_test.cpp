#include "cci/syntax/diagnostics_new.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/syntax/token.hpp"
#include "gtest/gtest.h"
#include <string_view>

using namespace std::string_view_literals;

using cci::diag2::Diagnostic;
using cci::diag2::DiagnosticArg;
using cci::diag2::DiagnosticBag;
using cci::diag2::DiagnosticBuilder;
using cci::diag2::DiagnosticDescriptor;
using cci::diag2::DiagnosticParam;
using cci::diag2::DiagnosticParamKind;
using cci::diag2::IntArg;
using cci::diag2::StrArg;
using cci::diag2::TokenKindArg;
using cci::syntax::ByteLoc;
using cci::syntax::ByteSpan;
using cci::syntax::TokenKind;

namespace {

struct DiagnosticsTest : ::testing::Test
{
protected:
    auto make_descriptor() -> DiagnosticDescriptor
    {
        return {
            .message = "a diagnostic description",
            .params = {},
        };
    }

    auto make_custom_descriptor(std::string_view message)
        -> DiagnosticDescriptor
    {
        return {
            .message = message,
            .params = {},
        };
    }

    auto build_diag(const DiagnosticDescriptor &descriptor) -> Diagnostic
    {
        return DiagnosticBuilder(descriptor).build();
    }
};

TEST_F(DiagnosticsTest, initialDiagnosticBagIsEmpty)
{
    const auto bag = DiagnosticBag();
    EXPECT_EQ(true, bag.empty());
}

TEST_F(DiagnosticsTest, addingToDiagnosticBagMakesItNonEmpty)
{
    auto custom_descriptor = make_descriptor();
    auto diag = DiagnosticBuilder(custom_descriptor).build();
    auto bag = DiagnosticBag();

    bag.add(std::move(diag));

    EXPECT_EQ(false, bag.empty());
}

// TODO: This is a DiagnosticBagIterator test, so move it to a better place.
TEST_F(DiagnosticsTest, diagnosticBagBeginPointsToFirstDiagnostic)
{
    auto descriptor_a = make_custom_descriptor("message A");
    auto descriptor_b = make_custom_descriptor("message B");

    auto diag_a = build_diag(descriptor_a);
    auto diag_b = build_diag(descriptor_b);

    auto bag = DiagnosticBag();

    bag.add(std::move(diag_a));
    bag.add(std::move(diag_b));

    auto it = bag.begin();

    EXPECT_EQ(build_diag(descriptor_a), *it);
    EXPECT_NE(build_diag(descriptor_b), *it);

    ++it;

    EXPECT_NE(build_diag(descriptor_a), *it);
    EXPECT_EQ(build_diag(descriptor_b), *it);
}

TEST_F(DiagnosticsTest, builderWithDescriptor)
{
    auto custom_descriptor = make_descriptor();
    auto diag = DiagnosticBuilder(custom_descriptor).build();
    EXPECT_EQ(&custom_descriptor, diag.descriptor);
}

TEST_F(DiagnosticsTest, builderWithCarret)
{
    auto custom_descriptor = make_descriptor();
    auto diag =
        DiagnosticBuilder(custom_descriptor).caret_at(ByteLoc(42)).build();

    EXPECT_EQ(&custom_descriptor, diag.descriptor);
    EXPECT_EQ(ByteLoc(42), diag.caret_location);
}

TEST_F(DiagnosticsTest, builderWithSpans)
{
    auto custom_descriptor = make_descriptor();
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
        .message = "a diagnostic description",
        .params =
            {
                DiagnosticParam("a", DiagnosticParamKind::Int),
                DiagnosticParam("b", DiagnosticParamKind::Str),
                DiagnosticParam("c", DiagnosticParamKind::TokenKind),
            },
    };

    auto diag = DiagnosticBuilder(descriptor)
                    .with_arg("a", 42)
                    .with_arg("b", "foo")
                    .with_arg("c", TokenKind::comma)
                    .build();

    EXPECT_EQ(&descriptor, diag.descriptor);

    ASSERT_EQ(3, diag.args.size());

    EXPECT_EQ("a", diag.args[0].first);
    EXPECT_EQ(DiagnosticArg(IntArg(42)), diag.args[0].second);

    EXPECT_EQ("b", diag.args[1].first);
    EXPECT_EQ(DiagnosticArg(StrArg("foo")), diag.args[1].second);

    EXPECT_EQ("c", diag.args[2].first);
    EXPECT_EQ(DiagnosticArg(TokenKindArg(TokenKind::comma)),
              diag.args[2].second);
}

} // namespace
