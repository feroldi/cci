// General.
DIAG(fatal_too_many_errors, ::cci::Severity::Error, "too many errors emitted, stopping now")
DIAG(note_declared_at, ::cci::Severity::Note, "declared here")
DIAG(note_to_match_this, ::cci::Severity::Note, "to match this {0}")

// Lexing and parsing.
DIAG(err_expected, ::cci::Severity::Error, "expected {0}")
DIAG(err_expected_either, ::cci::Severity::Error, "expected either {0} or {1}")
DIAG(err_expected_after, ::cci::Severity::Error, "expected {0} after {1}")
