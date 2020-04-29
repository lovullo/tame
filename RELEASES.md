TAME Release Notes
==================
This file contains notes for each release of TAME since v17.4.0.

TAME uses [semantic versioning][].  Any major version number change
represents backwards-incompatible changes.  Each such version will be
accompanied by notes that provide a migration path to resolve
incompatibilities.

[semantic versioning]: https://semver.org/

TAME developers: Add new changes under a "NEXT" heading as part of the
commits that introduce the changes.  To make a new release, run
=tools/mkrelease=, which will handle updating the heading for you.


NEXT
====

Miscellaneous
-------------
- `RELEASES.md`
  - Add missing link to semver.org.
  - Fix `tame-core` heading, which was erroneously Org-mode-styled.


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
