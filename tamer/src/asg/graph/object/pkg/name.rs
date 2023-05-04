// Package name
//
//  Copyright (C) 2014-2023 Ryan Specialty, LLC.
//
//  This file is part of TAME.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

//! Package name and canonicalization.
//!
//! See [`CanonicalName`] for more information.

use crate::{
    diagnose::{Annotate, AnnotatedSpan, Diagnostic},
    fmt::{DisplayWrapper, TtQuote},
    parse::{util::SPair, Token},
    span::Span,
    sym::{
        st::raw::{FW_SLASH, FW_SLASH_DOT},
        GlobalSymbolIntern, GlobalSymbolInternBytes, GlobalSymbolResolve,
    },
};
use std::{error::Error, fmt::Display};

/// Canonical package name.
///
/// A _canonical name_ is a package name that serves as a unique
///   representation of a package---that is,
///     it should not be possible to identify the same package by more than
///     one [`CanonicalName`].
/// In practice,
///   this means that the name begins with a forward slash;
///     contains no `..` or `.` as path-like components;
///     does not contain `//`;
///     and does not end in a trailing slash.
/// A canonical name is represented by [`CanonicalName`].
///
/// By representing package names in this way,
///   we are able to index them uniquely in any context in which they may
///   appear,
///     ensuring that we are able to uphold the commutative
///     definition/reference requirements of TAME.
///
/// Package names may develop further in the future;
///   for now,
///     their names are somewhat informal and usually generated from the
///     path on the filesystem relative to the project root.
///
/// Namespecs
/// =========
/// A _namespec_ is intended for package imports,
///   and represents a transformation that can be applied to a parent
///   package to produce the name of a package to import.
///
/// Namespecs (and package names) are designed to _look_ like paths,
///   and are indeed derived from paths on the filesystem,
///   but TAMER does not produce any hierarchy from that appearance.
/// Namespec resolution is a _purely lexical_ operation that is entirely
///   distinct from the operating system,
///     and the resulting name will be used to resolve package lookups
///     _relative to the project root_.[^symlink]
///
/// That is:
///   the purpose of a namespec is that of _convenience and
///   maintainability_;
///     it serves as an alternative to always providing a canonical name as
///     an import.
///
///   - If a namespec contains a leading slash,
///       then it is _absolute_ and will used verbatim as a
///       [`CanonicalName`].
///     All canonical names are therefore absolute namespecs,
///       providing unambiguous context-free package names.
///
///   - Otherwise, a namespec is _relative_ and must be resolved against a
///       parent [`CanonicalName`].
///     First,
///       the final component of the parent name
///         (the last slash and everything that follows)
///         is removed,
///           which intuitively matches the behavior of resolving against
///           sibling packages in the same directory.
///
///     - If the namespec contains any number of leading parent
///         markers `../`,
///           they will strip off a portion of [`CanonicalName`] up to and
///           including the last `/`,
///             recursively.
///       So `../quux` applied to `/bar/baz` resolves to `/quux`,
///         like a relative path on a Unix-like filesystem.
///       (As previously stated,
///         `/bar/baz` first becomes `/bar` so that the relative path
///         resolves as a sibling.)
///
///     - Otherwise,
///         the relative name is simply concatenated with the remaining
///         parent [`CanonicalName`],
///           separated by a `/`.
///
/// A namespec cannot use more parent markers than there are `/`s in the
///   parent [`CanonicalName`].
/// When viewing a name as a path,
///   this means that a namespec cannot traverse outside of the project
///   root.
///
/// A namespec cannot contain `.` in a context that would be interpreted by
///   a filesystem to mean the current directory,
///     since this would allow for multiple [`CanonicalName`]s for a given
///     package,
///       which is then not canonical.
/// Similarly,
///   a [`CanonicalName`] cannot contain `//`.
///
/// [^symlink]: This distinction is important.
///   Since a namespec always produces a [`CanonicalName`] which is treated
///   as a path relative to the project root,
///     paths will act intuitively despite symlinks,
///     which may otherwise exhibit unintuitive behavior when using `../`.
///
/// [`Span`] Characteristics
/// =========================
/// At the time of writing,
///   TAMER does not yet have an abstraction that indicates whether a given
///   [`SPair`] is an original lexeme tied to its original [`Span`].
/// It is assumed that [`CanonicalName`] is only ever constructed from
///   an [`SPair`] with its original [`Span`],
///     either via [`Self::from_canonical`] or
///     [`Self::resolve_namespec_rel`].
///
/// When a [`CanonicalNameError`] is raised,
///   its spans will only ever be derived from what it perceives as an
///   original [`Span`].
///
/// A [`CanonicalName`] may be constructed from a relative namespec via
///   [`Self::resolve_namespec_rel`].
/// In that case,
///   the span of the relative namespec is retained in the resulting
///   [`CanonicalName`].
/// Since a namespec can only contain parent markers (`..`) at the _head_ of
///   the namespec,
///     it will always be the case that the tail of a [`CanonicalName`] has
///     a proper span.
/// As long as we only slice up the span as far as we know is valid given
///   its construction,
///     we can return a [`Span`] in errors that is constructed from the tail
///     of the would-be [`CanonicalName`].
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct CanonicalName(SPair);

impl CanonicalName {
    /// Verify that the provided `name` is in canonical form,
    ///   otherwise produce an error.
    ///
    /// For more information on canonical names,
    ///   see [`Self`].
    pub fn from_canonical(name: SPair) -> Result<Self, CanonicalNameError> {
        let s = name.symbol().lookup_str();

        // TODO: We shouldn't slice up a string and span separately,
        //   since the two may not be directly associated
        //     (e.g. the symbol may not be a lexeme matching the associated
        //     span if it's generated).
        // See the section "Span Characteristics" above.
        if s.ends_with('/') {
            Err(CanonicalNameError::TrailingSlash(
                name,
                name.span().slice_tail(1),
            ))
        } else if s.starts_with('/') {
            Ok(Self(validate_components(name, s)?))
        } else {
            Err(CanonicalNameError::LeadingSlashExpected(
                name,
                // Slash should be inserted _just before_ the beginning of
                //   the span.
                // This also works with zero-length spans.
                name.span().slice_head(0),
            ))
        }
    }

    /// Resolve a namespec relative to this canonical name.
    ///
    /// For more information on relative paths,
    ///   see [`Self`].
    pub fn resolve_namespec_rel(
        &self,
        namespec: SPair,
    ) -> Result<Self, CanonicalNameError> {
        let Self(parent) = self;

        let s = namespec.symbol().lookup_str();

        if s.starts_with('/') {
            Self::from_canonical(namespec)
        } else {
            let mut parent_parts = parent.symbol().lookup_str().rsplit('/');
            let mut rel_parts = s.split('/').peekable();

            let mut rm_bytes = 0;

            // The rightmost component of the parent is eliminated,
            //   in much the same way that you'd expect a relative path to
            //   be a sibling of any current file.
            parent_parts.next();

            while rel_parts.peek() == Some(&"..") {
                rel_parts.next();

                let _ = parent_parts.next().filter(|s| s != &"").ok_or_else(
                    || {
                        CanonicalNameError::TooManyParentMarkers(
                            namespec,
                            namespec.span().slice(rm_bytes, 2),
                            *self,
                        )
                    },
                )?;

                rm_bytes += 3; // "../"
                               //    ^ this is a lie if we've ended `rel_parts`
            }

            let mut new = String::from(parent_parts.remainder().unwrap_or(""));

            if rel_parts.peek().is_some() {
                new.push('/');
                rel_parts.intersperse("/").collect_into(&mut new);
            }

            // If we're empty,
            //   it's more intuitive to indicate that we went too far
            //   relative to the parent rather than throwing an error about
            //   an invalid canonical name.
            if new.is_empty() {
                Err(CanonicalNameError::TooManyParentMarkers(
                    namespec,
                    namespec.span().slice(rm_bytes - 3, 2),
                    *self,
                ))
            } else {
                Self::from_canonical(SPair(new.intern(), namespec.span()))
            }
        }
    }
}

/// Produce an error if the provided name contains a parent marker `..` or
///   `.` in any of its components
///     (strings between `/`).
///
/// Conceptually,
///   this catches the equivalent of current and parent paths components,
///     such as `foo/../bar` and `./foo/bar`.
///
/// This will iterate through the entire name,
///   which is unnecessary work if we came from
///   [`CanonicalName::resolve_namespec_rel`],
///     but it's not much work and it's a safer implementation with fewer
///     conditions.
///
/// The purpose of this function is to provide guidance and rationale to the
///   user,
///     so it checks for very specific cases even if something more general
///     could have sufficed.
/// This produces very specific spans,
///   which also makes the scanning more challenging.
fn validate_components(
    name: SPair,
    s: &str,
) -> Result<SPair, CanonicalNameError> {
    let bytes = s.as_bytes();
    let len = bytes.len();

    // Was not worth introducing a regex library,
    //   but might be worth replacing this if we've since introduced it.
    // Equivalent to `/..(/|$)|/.(/$)|//`,
    //   but most of the code is for span slicing and error construction.
    bytes
        .windows(2) // A canonical path will always be at least 2 bytes
        .enumerate()
        // Again, be mindful of "Span Characteristics" above.
        .find_map(|(pos, w)| match (w, bytes.get(pos + 2)) {
            // `..`
            (b"/.", Some(b'.'))
                if matches!(bytes.get(pos + 3), Some(b'/') | None,) =>
            {
                Some(Err::<(), _>(CanonicalNameError::NonLeadingParentMarker(
                    name,
                    name.span().rslice(len - pos - 1, 2),
                )))
            }
            // `.`
            (b"/.", la @ (Some(b'/') | None)) => {
                let roff = la.is_some() as usize;
                Some(Err(CanonicalNameError::UselessComponent(
                    name,
                    SPair(
                        // ;_;
                        bytes[pos + roff..pos + roff + 2]
                            .intern_utf8()
                            .unwrap_or(FW_SLASH_DOT),
                        name.span().rslice(len - pos - roff, 2),
                    ),
                )))
            }
            (b"//", _) => Some(Err(CanonicalNameError::UselessComponent(
                name,
                SPair(FW_SLASH, name.span().rslice(len - pos - 1, 1)),
            ))),
            _ => None,
        })
        .transpose()
        .map(|_| name)
}

impl From<CanonicalName> for SPair {
    fn from(value: CanonicalName) -> Self {
        match value {
            CanonicalName(spair) => spair,
        }
    }
}

impl From<CanonicalName> for Span {
    fn from(value: CanonicalName) -> Self {
        match value {
            CanonicalName(spair) => spair.into(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CanonicalNameError {
    /// Attempted to create a new [`CanonicalName`] using a symbol that was
    ///   expected to already be in canonical form,
    ///     but was missing a leading slash.
    ///
    /// The latter span is the position at which a leading slash was
    ///   expected.
    LeadingSlashExpected(SPair, Span),

    /// Namespecs cannot include trailing slashes since it makes the package
    ///   name look like a directory.
    TrailingSlash(SPair, Span),

    /// A relative namespec attempts to go past the project root because it
    ///   includes too many `..` markers relative to a given canonical
    ///   name.
    ///
    /// The first [`SPair`] represents the failed relative namespec,
    ///   the [`Span`] represents the marker that caused the failure,
    ///   and the [`CanonicalName`] represents the parent that the namespec
    ///   is being resolved against.
    TooManyParentMarkers(SPair, Span, CanonicalName),

    /// Relative namespecs may only have parent markers (`..`) in a leading
    ///   position.
    ///
    /// The provided [`Span`] is the position of the marker that is in
    ///   error.
    NonLeadingParentMarker(SPair, Span),

    /// A portion of the namespec can be removed and still result in the
    ///   same namespec when interpreted as a path on the filesystem.
    ///
    /// This restriction is important to ensure that multiple namespecs
    ///   cannot be used to represent the same package,
    ///     which would cause package index lookup failures.
    UselessComponent(SPair, SPair),
}

impl Display for CanonicalNameError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use CanonicalNameError::*;

        match self {
            LeadingSlashExpected(name, _) => write!(
                f,
                "non-canonical package name {} where canonical name was expected",
                TtQuote::wrap(name),
            ),

            TrailingSlash(name, _) => write!(
                f,
                "package namespec {} must not include a trailing slash",
                TtQuote::wrap(name),
            ),

            TooManyParentMarkers(name, _, _) => write!(
                f,
                "package namespec {} resolves outside of package root",
                TtQuote::wrap(name),
            ),

            NonLeadingParentMarker(name, _) => write!(
                f,
                "package namespec {} contains parent marker in a \
                    non-head position",
                TtQuote::wrap(name),
            ),

            UselessComponent(name, part) => write!(
                f,
                "package namespec {} contains an invalid {}",
                TtQuote::wrap(name),
                TtQuote::wrap(part),
            ),
        }
    }
}

impl Error for CanonicalNameError {}

impl Diagnostic for CanonicalNameError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        use CanonicalNameError::*;

        match self {
            LeadingSlashExpected(_, at) => vec![
                at.error("expected name beginning with '/'"),
                at.help(
                    "canonical names begin with '/' and uniquely identify a \
                        package relative to a project root",
                ),
            ],

            TrailingSlash(_, at) => vec![
                at.error("trailing slash not permitted in package namespec"),
                at.help(
                    "a trailing slash makes a package name look like a directory, \
                        but a package is always associated with an \
                        explicit file",
                )
            ],

            TooManyParentMarkers(_, at, parent) => vec![
                parent.note(format!(
                    "parent package is {}",
                    TtQuote::wrap(Into::<SPair>::into(*parent))
                )),
                at.error("this marker exceeds the parent package depth"),
                at.help(
                    "the marker \"..\" treats the parent package name like a \
                       path and moves into parent directories"
                ),
                at.help(
                    "for both practical and security reasons, \
                        you cannot resolve past the project root \
                        relative to the parent package"
                ),
            ],

            NonLeadingParentMarker(_, at) => vec![
                at.error(
                    "parent marker must only appear at the head of a namespec"
                ),
                at.help(
                    "for example: the namespec `../bar` is valid, \
                        but `../foo/../bar` is not permitted"
                ),
                at.help(
                    "this restriction helps to avoid unnecessarily confusing \
                        namespecs"
                )
            ],

            UselessComponent(_, part) => vec![
                part.error("this component is not permitted here"),
                part.help(format!(
                    "remove the unnecessary {} from the namespec",
                    TtQuote::wrap(part)
                )),
                part.help(
                    "since namespecs are mapped to paths on the filesystem, \
                        certain strings are not permitted that would \
                        otherwise allow the same package to be represented \
                        by multiple different namespecs"
                )
            ],
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::span::dummy::{DUMMY_CONTEXT as DC, *};

    type Sut = CanonicalName;

    #[test]
    fn slash_prefixed_name_is_canonical() {
        let name = SPair("/foo/bar".into(), S1);

        assert_eq!(
            name,
            Sut::from_canonical(name)
                .expect("failed to canonicalize name")
                .into()
        );
    }

    #[test]
    fn from_canonical_fails_with_non_prefixed_name() {
        let a = DC.span(0, 13);
        let b = DC.span(0, 0); // expected location of '/'

        let name = SPair("not/canonical".into(), a);
        //                [-----------]
        //                0          12
        //                |     A
        //                B (zero-length)

        assert_eq!(
            Err(CanonicalNameError::LeadingSlashExpected(name, b)),
            Sut::from_canonical(name),
        );
    }

    #[test]
    fn from_canonical_must_not_end_in_trailing_slash() {
        let a = DC.span(0, 16);
        let b = DC.span(15, 1);

        let name = SPair("/trailing/slash/".into(), a);
        //                [--------------]
        //                0             15
        //                        A      |
        //                               B

        assert_eq!(
            Err(CanonicalNameError::TrailingSlash(name, b)),
            Sut::from_canonical(name),
        );
    }

    // Relative namespec without `..`.
    #[test]
    fn resolve_namespec_against_canonical_without_parent() {
        let parent = canonical(SPair("/parent/pkg".into(), S1));
        //                                    ^^^ this is stripped
        let rel = SPair("rel/to/parent".into(), S2);

        assert_eq!(
            // Note that this assumes the span of the _child_ since it's
            //   more specific and what the user almost certainly wants if
            //   there is a diagnostic message referencing this name.
            Ok(canonical(SPair("/parent/rel/to/parent".into(), S2))),
            parent.resolve_namespec_rel(rel),
        );
    }

    #[test]
    fn resolve_absolute_namespec_overrides_parent() {
        let parent = canonical(SPair("/parent/to/override".into(), S1));
        let rel = SPair("/abs/name".into(), S2);

        // A canonical name is an absolute namespec.
        #[rustfmt::skip]
        assert_eq!(
            Ok(canonical(rel)),
            parent.resolve_namespec_rel(rel),
        );
    }

    // Resolving parent markers
    //   (e.g. "../foo")
    //   is a _purely lexical_ operation.
    #[test]
    fn resolve_namespec_with_leading_parent_markers_and_compatible_parent() {
        let parent = canonical(SPair("/one/two/three".into(), S1));
        //  note that the rightmost component  ^^^^^
        //    will be stripped _before_ processing
        //    the first `..`

        assert_eq!(
            parent.resolve_namespec_rel(SPair("../a/pkg".into(), S2)),
            Ok(canonical(SPair("/one/a/pkg".into(), S2))),
        );

        assert_eq!(
            parent.resolve_namespec_rel(SPair("../../b/pkg".into(), S3)),
            Ok(canonical(SPair("/b/pkg".into(), S3))),
        );

        assert_eq!(
            parent.resolve_namespec_rel(SPair("..".into(), S4)),
            Ok(canonical(SPair("/one".into(), S4))),
        );
    }

    #[test]
    fn resolve_namespec_with_leading_parent_markers_too_far() {
        let parent = canonical(SPair("/parent/too/short".into(), S1));

        let a = DC.span(0, 15);
        let b = DC.span(6, 2);
        let rel = SPair("../../../foo/bar".into(), a);
        //               [-----++-------]
        //               0     ||      15
        //                   A ||
        //                     []
        //                     6 `7
        //                     B
        //                      `this marker goes one too far

        assert_eq!(
            Err(CanonicalNameError::TooManyParentMarkers(rel, b, parent)),
            parent.resolve_namespec_rel(rel),
        );
    }

    #[test]
    fn resolve_namespec_with_leading_parent_markers_yielding_empty() {
        let parent = canonical(SPair("/will/be/empty/pkg".into(), S1));

        let a = DC.span(0, 8);
        let b = DC.span(6, 2);
        let rel = SPair("../../..".into(), a);
        //               [-----+]
        //               0     ||`7
        //                   A ||
        //                     []
        //                     6 `7
        //                     B
        //                      `only goes too far because the result would
        //                         be `/` which is not a canonical name

        assert_eq!(
            Err(CanonicalNameError::TooManyParentMarkers(rel, b, parent)),
            parent.resolve_namespec_rel(rel),
        );
    }

    #[test]
    fn resolve_namespec_with_leading_parent_markers_trailing_slash() {
        let parent = canonical(SPair("/will/be/empty/pkg".into(), S1));

        let a = DC.span(0, 13);
        let b = DC.span(12, 1);
        let rel = SPair("../../../foo/".into(), a);
        //               [-----------]
        //               0           |`12
        //                     A     |
        //                           B

        assert_eq!(
            Err(CanonicalNameError::TrailingSlash(
                SPair("/foo/".into(), a),
                b,
            )),
            parent.resolve_namespec_rel(rel),
        );
    }

    #[test]
    fn resolve_namespec_with_non_head_parent_marker() {
        let parent = canonical(SPair("/foo/pkg".into(), S1));

        let a = DC.span(0, 10);
        let b = DC.span(4, 2);
        let rel = SPair("bar/../baz".into(), a);
        //               [---++---]
        //               0   ||   9
        //                 A ||
        //                   []
        //                   4 `5
        //                   B

        assert_eq!(
            Err(CanonicalNameError::NonLeadingParentMarker(
                SPair("/foo/bar/../baz".into(), a),
                b,
            )),
            parent.resolve_namespec_rel(rel),
        );

        // The same error should happen for a supposedly-canonical name too.
        let c = DC.span(0, 11);
        let d = DC.span(5, 2);
        let name = SPair("/bar/../baz".into(), c);
        //               [----++---]
        //               0    ||  10
        //                  C ||
        //                    []
        //                    4 `5
        //                    D

        assert_eq!(
            Err(CanonicalNameError::NonLeadingParentMarker(name, d,)),
            CanonicalName::from_canonical(name),
        );
    }

    // The above catches `/../`,
    //   but make sure we catch `/..$`.
    #[test]
    fn resolve_namespec_with_non_head_parent_marker_at_tail() {
        let parent = canonical(SPair("/foo/pkg".into(), S1));

        let a = DC.span(0, 6);
        let b = DC.span(4, 2);
        let rel = SPair("bar/..".into(), a);
        //               [---+]
        //               0   ||`5
        //                 A ||
        //                   []
        //                   4 `5
        //                   B

        assert_eq!(
            Err(CanonicalNameError::NonLeadingParentMarker(
                SPair("/foo/bar/..".into(), a),
                b,
            )),
            parent.resolve_namespec_rel(rel),
        );
    }

    // `/./` doesn't have a special lexical meaning for `CanonicalName`,
    //   which is the problem---when
    //     we go to resolve the name as a path,
    //       multiple names would resolve to the same package.
    #[test]
    fn resolve_namespec_with_single_dot() {
        let parent = canonical(SPair("/foo/pkg".into(), S1));

        let a = DC.span(0, 9);
        let b = DC.span(0, 2);
        let rel = SPair("./bar/baz".into(), a);
        //               [+------]
        //               0|      8
        //               []  A
        //                1
        //               B

        // "./" can be removed from the above path and still retain the same
        //   meaning.
        assert_eq!(
            Err(CanonicalNameError::UselessComponent(
                SPair("/foo/./bar/baz".into(), a),
                SPair("./".into(), b),
            )),
            parent.resolve_namespec_rel(rel),
        );
    }

    #[test]
    fn canonical_name_with_single_dot_middle() {
        let a = DC.span(0, 10);
        let b = DC.span(5, 2);
        let name = SPair("/foo/./bar".into(), a);
        //               [-----++---]
        //               0     ||   9
        //                   A []
        //                     5 `6
        //                     B

        // "./" can be removed from the above path and still retain the same
        //   meaning.
        assert_eq!(
            Err(CanonicalNameError::UselessComponent(
                name,
                SPair("./".into(), b),
            )),
            CanonicalName::from_canonical(name),
        );
    }

    // This error is phrased as the same problem as the above,
    //   but in reality,
    //   `.` in a tail position would act as its own package name if we
    //   append an extension to it
    //     (e.g. `/foo/bar/..xmlo`).
    // Best to flat out reject that.
    #[test]
    fn canonical_name_with_single_dot_end() {
        let a = DC.span(0, 10);
        let b = DC.span(8, 2);
        let name = SPair("/foo/bar/.".into(), a);
        //               [--------+]
        //               0        |9
        //                   A    []
        //                        8
        //                        B

        // "/." can be removed from the above path and still retain the same
        //   meaning.
        assert_eq!(
            Err(CanonicalNameError::UselessComponent(
                name,
                SPair("/.".into(), b),
            )),
            CanonicalName::from_canonical(name),
        );
    }

    // Adjacent slashes are collapsed into a single slash by many Unix-like
    //   filesystems,
    //     and so are equivalent to `./`.
    #[test]
    fn canonical_name_double_slash() {
        let a = DC.span(0, 9);
        let b = DC.span(5, 1);
        let name = SPair("/foo//bar".into(), a);
        //               [-----+--]
        //               0     |  8
        //                  A  ⌷
        //                     6
        //                     B

        // A "/" can be removed from the above path and still retain the
        //   same meaning.
        assert_eq!(
            Err(CanonicalNameError::UselessComponent(
                name,
                SPair("/".into(), b),
            )),
            CanonicalName::from_canonical(name),
        );
    }

    // Make sure we're not too naive and restrictive in our search.
    #[test]
    fn canonical_name_permits_parent_marker_like_strings() {
        CanonicalName::from_canonical(SPair("/foo/..bar/baz".into(), S1))
            .unwrap();
        CanonicalName::from_canonical(SPair("/foo/bar/baz..".into(), S2))
            .unwrap();
    }

    fn canonical(name: SPair) -> CanonicalName {
        CanonicalName::from_canonical(name).expect(&format!(
            "broken test case: failed instantiate CanonicalName \
                    with \"{name}\""
        ))
    }
}
