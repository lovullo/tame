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