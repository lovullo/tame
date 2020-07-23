TAME Release Notes
==================
This file contains notes for each release of TAME since v17.4.0.

TAME uses [semantic versioning][].  Any major version number increment
indicates that backwards-incompatible changes have been introduced in that
version.  Each such version will be accompanied by notes that provide a
migration path to resolve incompatibilities.

[semantic versioning]: https://semver.org/

TAME developers: Add new changes under a "NEXT" heading as part of the
commits that introduce the changes.  To make a new release, run
`tools/mkrelease`, which will handle updating the heading for you.


NEXT
====

Miscellaneous
-------------
- `build-aux/lsimports` was causing Gawk to complain about the third
  argument to `gensub`; fixed.


v17.5.0 (2020-07-15)
====================
This release adds support for experimental human-guided tail call
optimizations (TCO) to resolve issues of stack exhaustion during runtime for
tables with a large number of rows after having applied the first
predicate.  This feature should not be used outside of `tame-core`, and will
be done automatically by TAMER in the future.

`tame-core`
-----------
- `vector/filter/mrange`, used by the table lookup system, has had its
  mutually recursive function inlined and now uses TCO.
  - This was the source of stack exhaustion on tables whose predicates were
    unable to filter rows sufficiently.

Compiler
--------
- Experimental guided tail call optimizations (TCO) added to XSLT-based
  compiler, allowing a human to manually indicate recursive calls in tail
  position.
  - This is undocumented and should only be used by `tame-core`.  The
    experimental warning will be removed in future releases if the behavior
    proves to be sound.
  - TAMER will add support for proper tail calls that will be detected
    automatically.


v17.4.3 (2020-07-02)
====================
This release fixes a bug caused by previous refactoring that caused
unresolved externs to produce an obscure and useless error for the end
user.

Linker
------
- Provide useful error for unresolved identifiers.
  - This was previously falling through to an `unreachable!` block,
    producing a very opaque and useless internal error message.


v17.4.2 (2020-05-13)
====================
This release adds GraphML output for linked objects to allow us to
inspect the graph.

Linker
------
- Add `--emit` oprion to `tamer/src/bin/tameld.rs` that allows us to specify
  the type of output we want.
- Minor refactoring.

Miscellaneous
-------------
- Added `make` target to build linked GraphML files.
- Updated `make *.xmle` target to explicitly state it is emitting `xmle`.
- Added Cypher script to use in Neo4J after a GraphML file is imported.
- `RELEASES.md`
  - Add missing link to semver.org.
  - Fix `tame-core` heading, which was erroneously Org-mode-styled.
  - Rephrase and correct formatting of an introduction paragraph.


v17.4.1 (2020-04-29)
====================
This release refactors the linker, adds additional tests, and improves
errors slightly.  There are otherwise no functional changes.

Compiler
--------
- Refactor proof-of-concept dependency graph construction code.
- Improvements to error abstraction which will later aid in reporting.

Miscellaneous
-------------
- `RELEASES.md` added.
- `tools/mkrelease` added to help automate updating `RELEASES.md`.
- `build-aux/release-check` added to check releases.
  - This is invoked both by `tools/mkrelease` and by CI via
    `.gitlab-ci.yml` on tags.


v17.4.0 (2020-04-17)
====================
This release focuses on moving some code out of the existing XSLT-based
compiler so that the functionality does not need to be re-implemented in
TAMER.  There are no user-facing changes aside form the introduction of two
new templates, which are not yet expected to be used directly.

`tame-core`
-----------
- New `rate-each` template to replace XSLT template in compiler.
- New `yields` template to replace XSLT template in compiler.
- Users should continue to use `rate-each` and `yields` as before rather
  than invoking the new templates directly.
  - The intent is to remove the `t` namespace prefix in the future so that
    templates will be applied automatically.

Compiler
--------
- XSLT-based compiler now emits `t:rate-each` in place of the previous XSLT
  template.
- XSLT-based compiler now emits `t:yields` in place of the previous XSLT
  template.
