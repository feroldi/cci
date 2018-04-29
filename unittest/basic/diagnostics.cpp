#include "cci/basic/diagnostics.hpp"
#include "cci/basic/source_manager.hpp"
#include "cci/util/contracts.hpp"
#include "gtest/gtest.h"

namespace cci {

namespace diag {
enum UDErrorCode
{
  warn_test_message,
  err_test_message,
  note_select_message,
};
}

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
      case diag::note_select_message:
        return {Severity::Note, "the {:good|bad|ugly}"};
      default: cci_unreachable();
    }
  }
};

template <>
struct is_diagnostics_error_code<diag::UDErrorCode> : std::true_type
{};

} // namespace cci

using namespace cci;

namespace {

TEST(DiagnosticsTest, fatalError)
{
  const char *code = "int x;\n";
  DiagnosticsOptions opts;
  CompilerDiagnostics diag(opts);
  auto src_mgr = SourceManager::from_buffer(diag, code);
  diag.set_max_errors(1);

  EXPECT_THROW(diag.report(Severity::Error, nocontext, "answer is {}", 42),
               CompilerFatalError);

  EXPECT_NO_THROW({
    diag.set_max_errors(128);
    diag.report(Severity::Error, nocontext, "all good, no fatal errors yet");
  });

  EXPECT_THROW(diag.set_max_errors(1), CompilerFatalError);
}

TEST(DiagnosticsTest, outputLevelMapping)
{
  const char *code = "int x;\n";
  DiagnosticsOptions opts;

  {
    opts.is_warning_as_error = false;
    CompilerDiagnostics diag(opts);
    auto src_mgr = SourceManager::from_buffer(diag, code);
    diag.report(Severity::Warning, nocontext, "this should be a warning");
    EXPECT_FALSE(diag.has_errors());
    EXPECT_TRUE(diag.has_warnings());
  }

  {
    opts.is_warning_as_error = true; //< Note true.
    CompilerDiagnostics diag(opts);
    auto src_mgr = SourceManager::from_buffer(diag, code);
    diag.report(Severity::Warning, nocontext, "this should be an error");
    EXPECT_TRUE(diag.has_errors());
    EXPECT_FALSE(diag.has_warnings());
  }

  {
    opts.is_pedantic = false;
    opts.is_pedantic_as_error = false;
    CompilerDiagnostics diag(opts);
    auto src_mgr = SourceManager::from_buffer(diag, code);
    diag.report(Severity::Extension, nocontext, "this should not be emitted");
    EXPECT_FALSE(diag.has_errors());
    EXPECT_FALSE(diag.has_warnings());
  }

  {
    opts.is_pedantic = true; //< Note true.
    opts.is_pedantic_as_error = false;
    CompilerDiagnostics diag(opts);
    auto src_mgr = SourceManager::from_buffer(diag, code);
    diag.report(Severity::Extension, nocontext, "this should be a warning");
    EXPECT_FALSE(diag.has_errors());
    EXPECT_TRUE(diag.has_warnings());
  }

  {
    opts.is_pedantic = false; //< Note false.
    opts.is_pedantic_as_error = true; //< Note true.
    CompilerDiagnostics diag(opts);
    auto src_mgr = SourceManager::from_buffer(diag, code);
    diag.report(Severity::Extension, nocontext, "this should be an error");
    EXPECT_TRUE(diag.has_errors());
    EXPECT_FALSE(diag.has_warnings());
  }

  {
    opts.is_verbose = false;
    CompilerDiagnostics diag(opts);
    auto src_mgr = SourceManager::from_buffer(diag, code);
    diag.report(Severity::Remark, nocontext, "this should not be emitted");
    EXPECT_FALSE(diag.has_errors());
    EXPECT_FALSE(diag.has_warnings());
  }

  {
    opts.is_verbose = true; //< Note true
    CompilerDiagnostics diag(opts);
    auto src_mgr = SourceManager::from_buffer(diag, code);
    diag.report(Severity::Remark, nocontext, "this should be a mention");
    EXPECT_FALSE(diag.has_errors());
    EXPECT_FALSE(diag.has_warnings());
  }
}

TEST(DiagnosticsTest, carret)
{
  const char *code = "int x;\n";
  DiagnosticsOptions opts;
  CompilerDiagnostics diag(opts);
  auto src_mgr = SourceManager::from_buffer(diag, code);
  SourceLocation loc(4ull);
  diag.report(loc, diag::note_declared_at);
}

TEST(DiagnosticsTest, userDefinedErrorCode)
{
  const char *code = "int x;\n";
  DiagnosticsOptions opts;
  CompilerDiagnostics diag(opts);
  auto src_mgr = SourceManager::from_buffer(diag, code);
  SourceLocation loc(4ull);
  diag.report(loc, diag::warn_test_message, "GTest");
  EXPECT_TRUE(diag.has_warnings());
  diag.report(loc, diag::err_test_message, 1, 2);
  EXPECT_TRUE(diag.has_errors());
}

TEST(DiagnosticsTest, selectMessageCase)
{
  DiagnosticsOptions opts;
  CompilerDiagnostics diag(opts, stderr);
  diag.report(nocontext, diag::note_select_message, selector{0});
  diag.report(nocontext, diag::note_select_message, selector{1});
  diag.report(nocontext, diag::note_select_message, selector{2});
}

} // anonymous namespace
