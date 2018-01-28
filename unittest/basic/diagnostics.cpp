#include "cci/basic/source_manager.hpp"
#include "cci/basic/diagnostics.hpp"
#include "gtest/gtest.h"

namespace cci::diag {
enum UDErrorCode
{
  warn_test_message,
  err_test_message,
};
}

namespace cci {
template <>
struct diagnostics_error_code<diag::UDErrorCode>
{
  constexpr static auto info(diag::UDErrorCode code) -> ErrorCodeInfo
  {
    switch (code)
    {
      case diag::warn_test_message:
        return {Severity::Warning, "warning message for {0}"};
      case diag::err_test_message:
        return {Severity::Error, "error message ({0} and {1})"};
    }
  }
};

template <>
struct is_diagnostics_error_code<diag::UDErrorCode> : std::true_type
{
};
} // namespace ::cci


namespace {

TEST(DiagnosticsTest, fatalError)
{
  using cci::nocontext;
  using cci::Severity;
  const char *code = "int x;\n";
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto src_mgr =
    cci::SourceManager::from_buffer(diag, code);
  diag.set_max_errors(1);

  EXPECT_THROW(diag.report(Severity::Error, nocontext, "answer is {}", 42),
               cci::CompilerFatalError);

  EXPECT_NO_THROW({
    diag.set_max_errors(128);
    diag.report(Severity::Error, nocontext, "all good, no fatal errors yet");
  });

  EXPECT_THROW(diag.set_max_errors(1), cci::CompilerFatalError);
}

TEST(DiagnosticsTest, outputLevelMapping)
{
  using cci::nocontext;
  using cci::Severity;
  const char *code = "int x;\n";
  cci::DiagnosticsOptions opts;

  {
    opts.is_warning_as_error = false;
    cci::CompilerDiagnostics diag(opts);
    auto src_mgr = cci::SourceManager::from_buffer(diag, code);
    diag.report(Severity::Warning, nocontext, "this should be a warning");
    EXPECT_FALSE(diag.has_errors());
    EXPECT_TRUE(diag.has_warnings());
  }

  {
    opts.is_warning_as_error = true; //< Note true.
    cci::CompilerDiagnostics diag(opts);
    auto src_mgr = cci::SourceManager::from_buffer(diag, code);
    diag.report(Severity::Warning, nocontext, "this should be an error");
    EXPECT_TRUE(diag.has_errors());
    EXPECT_FALSE(diag.has_warnings());
  }

  {
    opts.is_pedantic = false;
    opts.is_pedantic_as_error = false;
    cci::CompilerDiagnostics diag(opts);
    auto src_mgr = cci::SourceManager::from_buffer(diag, code);
    diag.report(Severity::Extension, nocontext, "this should not be emitted");
    EXPECT_FALSE(diag.has_errors());
    EXPECT_FALSE(diag.has_warnings());
  }

  {
    opts.is_pedantic = true; //< Note true.
    opts.is_pedantic_as_error = false;
    cci::CompilerDiagnostics diag(opts);
    auto src_mgr = cci::SourceManager::from_buffer(diag, code);
    diag.report(Severity::Extension, nocontext, "this should be a warning");
    EXPECT_FALSE(diag.has_errors());
    EXPECT_TRUE(diag.has_warnings());
  }

  {
    opts.is_pedantic = false; //< Note false.
    opts.is_pedantic_as_error = true; //< Note true.
    cci::CompilerDiagnostics diag(opts);
    auto src_mgr = cci::SourceManager::from_buffer(diag, code);
    diag.report(Severity::Extension, nocontext, "this should be an error");
    EXPECT_TRUE(diag.has_errors());
    EXPECT_FALSE(diag.has_warnings());
  }

  {
    opts.is_verbose = false;
    cci::CompilerDiagnostics diag(opts);
    auto src_mgr = cci::SourceManager::from_buffer(diag, code);
    diag.report(Severity::Remark, nocontext, "this should not be emitted");
    EXPECT_FALSE(diag.has_errors());
    EXPECT_FALSE(diag.has_warnings());
  }

  {
    opts.is_verbose = true; //< Note true
    cci::CompilerDiagnostics diag(opts);
    auto src_mgr = cci::SourceManager::from_buffer(diag, code);
    diag.report(Severity::Remark, nocontext, "this should be a mention");
    EXPECT_FALSE(diag.has_errors());
    EXPECT_FALSE(diag.has_warnings());
  }
}

TEST(DiagnosticsTest, carret)
{
  using cci::nocontext;
  using cci::Severity;
  const char *code = "int x;\n";
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto src_mgr = cci::SourceManager::from_buffer(diag, code);
  cci::SourceLocation loc(4ull);
  diag.report(loc, cci::diag::note_declared_at);
}

TEST(DiagnosticsTest, userDefinedErrorCode)
{
  using cci::nocontext;
  using cci::Severity;
  const char *code = "int x;\n";
  cci::DiagnosticsOptions opts;
  cci::CompilerDiagnostics diag(opts);
  auto src_mgr = cci::SourceManager::from_buffer(diag, code);
  cci::SourceLocation loc(4ull);
  diag.report(loc, cci::diag::warn_test_message, "GTest");
  EXPECT_TRUE(diag.has_warnings());
  diag.report(loc, cci::diag::err_test_message, 1, 2);
  EXPECT_TRUE(diag.has_errors());
}

} // anonymous namespace
