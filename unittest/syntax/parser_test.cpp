#include "../compiler_fixture.hpp"
#include "cci/ast/arena_types.hpp"
#include "cci/ast/ast_context.hpp"
#include "cci/ast/expr.hpp"
#include "cci/langopts.hpp"
#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/parser.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/source_map.hpp"
#include "gtest/gtest.h"
#include <memory>

using cci::ast::ASTContext;
using cci::ast::IntegerLiteral;
using cci::diag::Diag;
using cci::diag::Diagnostic;
using cci::syntax::Parser;
using cci::syntax::Scanner;
using cci::syntax::Sema;
using cci::syntax::TokenKind;

namespace
{

struct ParserTest : cci::test::CompilerFixture
{
protected:
    cci::TargetInfo target_info;
    std::unique_ptr<Scanner> scanner;
    std::unique_ptr<ASTContext> ast_context;
    std::unique_ptr<Sema> sema;
    std::unique_ptr<Parser> parser;

    void build_parser(std::string_view source)
    {
        const auto &file = create_filemap("parser_test.c", std::string(source));
        scanner = std::make_unique<Scanner>(file, diag_handler);
        ast_context = std::make_unique<ASTContext>(target_info);
        sema = std::make_unique<Sema>(*scanner, *ast_context);
        parser = std::make_unique<Parser>(*scanner, *sema);
    }
};

TEST_F(ParserTest, integerLiteral)
{
    build_parser("42\n");
    const auto expr = parser->parse_expression().value();
    const auto lit = expr->get_as<IntegerLiteral>();
    EXPECT_TRUE(nullptr != lit);
    EXPECT_EQ(42, lit->value());
}

TEST_F(ParserTest, unbalancedParens)
{
    build_parser("(((42))+\n");
    parser->parse_expression();
    EXPECT_EQ(Diag::expected_but_got, peek_diag().msg);
    EXPECT_EQ(Diagnostic::Arg(TokenKind::r_paren), peek_diag().args[0]);
    EXPECT_EQ(Diagnostic::Arg(TokenKind::plus), pop_diag().args[1]);
}

} // namespace
