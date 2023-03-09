# XMLI System Test
The `xmli` file is an intermediate file that serves as a handoff between
TAMER and the XSLT-based compiler:

```
xml -> (TAMER) -> xmli -> (TAME XSLT) -> xmlo
```

TAMER gets the first shot at processing, and then the compilation process
continues with the XSLT-based compiler.  This allows TAMER to incrementally
augment and manipulate the source file and remove responsibilities from
TAME XSLT.

Tests in this directory ensure that this process is working as
intended.  TAMER's failure to perform a proper handoff will cause TAME XSLT
to compile sources incorrectly, since TAMER will have rewritten them to
something else.

This handoff is more than just echoing tokens back into a file---it
_derives_ a new program from the state of the ASG.  This program may have a
slightly different representation than the original sources, but it must
express an equivalent program, and the program must be at least as
performant when emitted by TAME XSLT.

# Running Tests
Test are prefixed with `test-*` and are executable.  They must be invoked
with the environment variable `PATH_TAMEC` set to the path of `tamec`
relative to the working directory.

Test cases are organized into sub-directories with `src.xml` and
`expected.xml` files.  `src.xml` will be compiled with `tamec`, its output
formatted with `xmllint --format`, and `diff`'d against the
`xmllint`-formatted output of `expected.xml`.

