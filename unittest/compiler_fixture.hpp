#pragma once

#include "cci/syntax/diagnostics.hpp"
#include "cci/syntax/scanner.hpp"
#include "cci/syntax/source_map.hpp"
#include "cci/syntax/token.hpp"
#include "gtest/gtest.h"
#include <ostream>
#include <queue>
#include <type_traits>

namespace cci::test {

struct CompilerFixture : ::testing::Test
{
protected:
    srcmap::SourceMap source_map;
    diag::Handler diag_handler;
    std::queue<diag::Diagnostic> diags;

    CompilerFixture()
        : source_map()
        , diag_handler([this](const diag::Diagnostic &d) { diags.push(d); },
                       source_map)
    {}

    void TearDown() override
    {
        EXPECT_TRUE(diags.empty()) << "size(diags) = " << diags.size();
    }

    auto create_filemap(std::string name, std::string source)
        -> const cci::srcmap::FileMap &
    {
        return source_map.create_owned_filemap(std::move(name),
                                               std::move(source));
    }

    auto get_source_text(const cci::Token &tok) const -> std::string_view
    {
        return source_map.range_to_snippet(tok.source_range);
    }

    auto get_lexeme(const cci::Token &tok) const -> std::string
    {
        std::string lexeme;
        lexeme.resize(tok.size());
        const size_t len =
            Scanner::get_spelling_to_buffer(tok, lexeme.data(), source_map);
        lexeme.resize(len);
        return lexeme;
    }

    auto pop_diag() -> diag::Diagnostic
    {
        EXPECT_FALSE(diags.empty());
        auto d = diags.front();
        diags.pop();
        return d;
    }

    auto peek_diag() -> const diag::Diagnostic &
    {
        EXPECT_FALSE(diags.empty());
        return diags.front();
    }
};

} // namespace cci::test

namespace cci {

inline void PrintTo(const Category category, std::ostream *os) noexcept
{
    *os << to_string(category);
}

namespace diag {

inline void PrintTo(const Diag msg, std::ostream *os) noexcept
{
    *os << "Diag(" << static_cast<std::underlying_type_t<diag::Diag>>(msg)
        << ")";
}

} // namespace diag

} // namespace cci
