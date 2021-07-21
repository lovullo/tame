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
Compiler
--------
- Place constants into static section in linked executable.
  - This was the case in the old linker before the `tameld`
    proof-of-concept.  The benefits are significant when large constants are
    used (e.g. for large tables of data).
- Do not report value list optimization error on duplicate conjunctive
  predicates.
  - This doesn't emit code any differently, it merely permits the situation,
    which can occur in generated code.


v18.0.2 (2021-07-15)
====================
This is a bugfix release that corrects issues with the Summary Page compiler
and corrects behavior with the new classification system (that is currently
off unless explicitly requested).

Compiler
--------
- Make Summary Page less chatty.
- Fix incorrect package name for generated worksheet packages.
- Restrict `TRUE`-match optimization to classification matches (class
  composition).
  - This was mistakenly not considering the domain of the match, and
    therefore was applying the optimization in situations where it should
    not.  Results of previous classifications are currently the only place
    we guarantee a boolean value.
- Apply classification alias optimization to any `1`-valued constant match.
  - Previously applied only to `TRUE`.

Summary Page
------------
- Correctly generate input fields for params using imported types.
  - This is a long-standing (nearly 10-year-old) bug.


v18.0.1 (2021-06-24)
====================
This is a minor maintenance release.

Compiler
--------
- Remove internal notice when new system is used to emit code for a
  particular classification.



v18.0.0 (2021-06-23)
====================
This release focuses primarily on compiler optimizations that affect runtime
performance (both CPU and memory).  The classification system has undergone
a rewrite, but the new system is gated behind a template-based feature flag
`_use-new-classification-system_` (see Core below).  Many optimizations
listed below are _not_ affected by this toggle.

Compiler
--------
- Numerous compiler optimizations including (but not limited to):
  - Classification system rewrite with significant correctness and
    performance improvements, with significantly less generated code.
    - There is more work to be done in TAMER.
    - This change is gated behind a feature toggle (see
      `_use-new-classification-system_` in Core below).
  - Significant reduction in byte count of JavaScript target output.
  - Classifications with single-`TRUE` predicate matches are aliases and now
    compile more efficiently.
  - Classifications that are a disjunction of conjunctions with a common
    predicate will have the common predicate hoisted out, resulting in more
    efficeint code generation.
  - Classifications with equality matches entirely on a single param are
    compiled into a `Set` lookup.
  - Most self-executing functions in target JavaScript code have been
    eliminated, resulting in a performance improvement.
  - Floating point truncation now takes place without using `toFixed` in
    JavaScript target, eliminating expensive number->string->number
    conversions.
  - Code paths are entirely skipped if a calculation predicate indicates
    that it should not be executed, rather than simply multiplying by 0
    after performing potentially expensive calculations.
  - A bunch of wasteful casting has been eliminated, supplanted by proper
    casting of param inputs.
  - Unnecessary debug output removed, significantly improving performance in
    certain cases.
  - Single-predicate any/all blocks stripped rather than being extracted
    into separate classifications.
  - Extracted any/all classifications are inlined at the reference site when
    the new classification system is enabled, reducing the number of
    temporaries created at runtime in JavaScript.
- Summary Page now displays values of `lv:match/@on` instead of debug
  values.
  - This provides more useful information and is not subject to the
    confusing reordering behavior of the compiler that is not reflected on
    the page.
  - Changes that have not yet been merged will remove debug values for the
    classification system.

Core
----
- New feature flag template `_use-new-classification-system_`.
  - This allows selectively enabling code generation for the new
    classification system, which has BC breaks in certain buggy situations.
    See `core/test/core/class` package for more information.
- Remove `core/aggregate`.
  - This package is not currently utilized and is dangerous---it could
    easily aggregate unintended values if used carelessly.  Those who know
    what they are doing can use `sym-set` if such a thing is a good thing
    within the given context, and proper precautions are taken (as many
    templates already do today).

Rust
----
- Version bump from 1.42.0 to 1.48.0 now that intra-doc links has been
  stabalized.

Miscellaneous
-------------
- `build-aux/progtest-runner` will now deterministically concatenate files
  based on name rather than some unspecified order.


v17.9.0 (2021-05-27)
====================
This is a documentation/design release, introducing The TAME Programming
Language in `design/tpl`.

Compiler
-------
- Allow the mapping of flag values from `program.xml`.

Design
------
- Introduce The TAME Programming Language.


v17.8.1 (2021-03-18)
====================
This release contains a bufix for recent build changes in v17.8.0 that were
causing, under some circumstances, builds to fail during dependency
generation.  It also contains minor improvements and cleanup.

Build System
------------
- [bugfix] Lookup tables will no longer build `rater/core/vector/table` when
  geneating the `xml` package.
  - This was causing problems during `suppliers.mk` dependency generation.
    The dependency is still in place for the corresponding `xmlo` file.
  - This was broken by v17.8.0.
- Minor improvements to `tame` and `tamed` scripts to ensure that certain
  unlikely failures are not ignored.


v17.8.0 (2021-02-23)
====================
This release contains changes to the build system to accommodate
liza-proguic's introduction of step-based packages (in place of a monolithic
`package-dfns.xml`), as well as miscellaneous improvements.

Compiler
--------
- `rater.xsd`, used for certain validations of TAME's grammar, has been
  updated to an out-of-tree version; it had inadvertently gotten out of
  date, and the discrepency won't happen again in the future.
  - Further, limits on the length of `@yields` identifiers have been
    removed; the lack of namespacing and generation of identifiers from
    templates can yield longer identifier names.

Build System
------------
- Only modify `.version.xml` timestamp when hash changes.  This allows
  its use as a dependency without forcefully rebuilding each and every time.
- `configure` will no longer immediately generate `suppliers.mk`.
  - Additionally, `build-aux/suppmk-gen`, which `configure` directly invoked
    until now, was removed in favor of generic rules in `Makefile.am`.
- Step-level imports in program definitions are now recognized to
  accommodate liza-proguic's step-level package generation.
- Step-level program packages are now properly accounted for as dependencies
  for builds.
- `supplier.mk` is now automatically regenerated when source files
  change.  This previously needed to be done manually when imports changed.
  - `supplier.mk` generation will no longer be verbose (it'll instead be
    only one line), which makes it more amenable to more frequent
    regeneration.


v17.7.0 (2020-12-09)
====================
This release provides tail-call optimizations aimed at the query system in
core.

Compiler
--------
- [bugfix] Recursive calls using TCO will wait to overwrite their function
  arguments until all expressions calculating the new argument values have
  completed.

`tame-core`
-----------
- `mrange` is now fully tail-recursive and has experimental TCO applied.
  - It was previously only recursive for non-matching rows.


v17.6.5 (2020-12-03)
====================
This release improves Summary Page performance when populating the page with
data loaded from an external source.

Summary Page
------------
- Populating the DOM with loaded data now runs in linear time.


v17.6.4 (2020-11-23)
====================
This release tolerates invalid map inputs in certain circumstances.

Compiler
--------
- Tolerate non-string inputs to `uppercase` and `hash` map methods.


v17.6.3 (2020-11-03)
====================
- Update the CDN used to get MathJax.


v17.6.2 (2020-10-01)
====================
- Optionally include a "program.mk" file if it is present in the project's root
  directory. This allows us to move program specific tasks outside of TAME.


v17.6.1 (2020-09-23)
====================
Compiler
--------
- `lv:param-class-to-yields` will now trigger a failure rather than relying
  on propagating bad values, which may not result in failure if the symbol
  is represented by another type (non-class) of object.

Miscellaneous
-------------
- `package-lock.json` additions.


v17.6.0 (2020-08-19)
====================
This release provides a new environment variable for JVM tuning.  It does
not provide any new compiler features or performance enhancements in itself,
though it enables optimizations through JVM tuning.

Compiler
--------
- The new environment variable `TAMED_JAVA_OPTS` can now be used to provide
  arguments to the JVM.  This feature was added to support heap ratio
  tuning.

Miscellaneous
-------------
- `build-aux/lsimports` was causing Gawk to complain about the third
  argument to `gensub`; fixed.
- `bootstrap` will test explicitly whether `hoxsl` is a symbol link, since
  `-e` fails if the symlink is broken.


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
