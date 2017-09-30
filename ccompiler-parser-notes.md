
# Two-phase parser: syntax parser and semantics analyser.

Maybe it'd be a good idea to make the parser go through
two phases at the same time: instead of parsing the whole
source, and only then doing the semantics analysis (building
the final ast), make it so the syntax parser is an iterator,
and the semantics analyses run through this iterator. Very similar
to a generator.

# Write both front-end and back-end, but don't depend on each other.

First write the front-end (diagnostics, ast etc). Then move on to
writing the back-end (IR, codegen, linking etc). When you're done,
use an AST consumer to transform an AST into the back-end's IR.

# Make a good interface for TokenStream, and use free functions for the parser.

But still using a separate ast action to invoke semantic analyses.

# Don't store the AST when errors occur.

Once an error is detected, just go on with parsing, but only for syntax checking.
Throw the AST away and just check syntax and semantics.
