// TAMER diagnostic system panics
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

//! Panic with diagnostic information.
//!
//! The system will produce diagnostic information using [`Error`]s
//!   implementing [`Diagnostic`] for most cases.
//! However,
//!   sometimes the system enters an unexpected and inconsistent state that
//!   is either not worth the effort of trying to recover from,
//!     or cannot be recovered from because it represents a bug in the
//!     compiler itself.
//! In cases such as those,
//!   a panic may be a more suitable alternative,
//!   but panics are not able to utilize spans to present additional
//!   information that the user may use to attempt to work around the issue
//!     themselves while awaiting a fix.
//!
//!   - The macro [`diagnostic_panic!`] acts like panic,
//!       but accepts a vector of [`AnnotatedSpan`]s
//!         (just like those produced by [`Diagnostic::describe`])
//!         as its first argument to produce a diagnostic report alongside
//!         the panic.
//!   - The [`DiagnosticPanic`] trait provides alternatives to `unwrap` and
//!       `expect` methods,
//!         and utilizes [`diagnostic_panic!`].
//!     It is implemented for common types
//!       (and will be expanded as needed).
//!
//! Panics produced with [`diagnostic_panic!`] will output an obnoxious
//!   message stating that the error is a bug in TAMER and should be
//!   reported.

use super::{AnnotatedSpan, Diagnostic, FsSpanResolver, VisualReporter};
use std::{
    cell::Cell,
    error::Error,
    fmt::{self, Debug, Display},
};

// Macro exports are unintuitive.
#[cfg(doc)]
use crate::diagnostic_panic;
use crate::f::ThunkOrStaticRef;

/// The type of [`Reporter`](crate::diagnose::Reporter) used to produce
///   reports during panic operations.
pub type PanicReporter = VisualReporter<FsSpanResolver>;

/// Container for ad-hoc diagnostic data for panics.
///
/// This is public only because it is needed at the expansion site of
///   [`diagnostic_panic!`].
/// You should not use this outside of panics.
///
/// It is intended to be rendered _once_,
///   after which its [`AnnotatedSpan`] vector will be consumed and become
///   empty.
/// The diagnostic API doesn't take ownership over the error being
///   described.
pub struct DiagnosticDesc<'a>(pub String, pub Cell<Vec<AnnotatedSpan<'a>>>);

impl<'a> Debug for DiagnosticDesc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DiagnosticDesc")
    }
}

impl<'a> Error for DiagnosticDesc<'a> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl<'a> Display for DiagnosticDesc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self(summary, _) => write!(f, "{}", summary),
        }
    }
}

impl<'a> Diagnostic for DiagnosticDesc<'a> {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        match self {
            Self(_, desc) => desc.take(),
        }
    }
}

/// Produce a panic with diagnostic information and a rather obnoxious
///   message describing this issue as a bug in TAMER.
///
/// The first argument is of the same form as the return value of
///   [`Diagnostic::describe`].
///
/// This should be used in place of [`panic!`] whenever possible.
/// It uses the same diagnostic system as normal errors,
///   allowing you to produce complex reports consisting of any number of
///   spans.
/// Considering that this error halts the system and therefore may mask
///   other useful errors,
///     it is important that this provide useful information if at all
///     possible so that the user has some chance of working around the
///     problem and getting themselves unstuck.
#[macro_export]
macro_rules! diagnostic_panic {
    ($desc_data:expr, $($panic_args:tt)*) => {
        $crate::diagnostic_panic!(
            @panic!, $desc_data, $($panic_args)*
        )
    };

    (@$macro:ident!, $desc_data:expr, $($panic_args:tt)*) => {
        $crate::diagnostic_panic!(
            @panic!,
            @title "THIS IS A BUG IN TAMER",
            $desc_data,
            $($panic_args)*
        )
    };

    (@$macro:ident!, @title $title:expr, $desc_data:expr, $($panic_args:tt)*) => {{
        use $crate::diagnose::Reporter;

        let mut reporter = $crate::diagnose::panic::PanicReporter::new(
            Default::default()
        );

        let summary = format!($($panic_args)*);
        let desc = $crate::diagnose::panic::DiagnosticDesc(
            summary,
            std::cell::Cell::new($desc_data),
        );

        // Be extra obnoxious.
        // This shouldn't ever happen except under exceedingly
        //   exceptional circumstances,
        //     so it's acceptable to make a big deal about it.
        $macro!(
            "internal error:\n{report}\n\
\x1b[0;31m
!!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ !!!
!!! {title:^53} !!!
!!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ !!!
!!! This message means that TAMER has encountered an      !!!
!!! unrecoverable error that forced it to terminate       !!!
!!! processing.                                           !!!
!!!                                                       !!!
!!! TAMER has attempted to provide you with contextual    !!!
!!! information above that might allow you to work around !!!
!!! this problem until it can be fixed.                   !!!
!!!                                                       !!!
!!! Please report this error, including the above         !!!
!!! diagnostic output beginning with 'internal error:'.   !!!
!!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ !!!
\x1b[0m",
            title = $title,
            report = reporter.render(&desc),
        )
    }};
}

/// Produce a panic with diagnostic information and a rather obnoxious
///   message describing this issue as a bug in TAMER,
///     but only if debug assertions are enabled.
///
/// This simply gates [`diagnostic_panic!`] behind a `debug_assertions` cfg
///   check.
#[macro_export]
macro_rules! debug_diagnostic_panic {
    ($desc_data:expr, $($panic_args:tt)*) => {
        #[cfg(debug_assertions)]
        $crate::diagnostic_panic!($desc_data, $($panic_args)*);
    }
}

/// Produce a panic using [`unreachable!`] with diagnostic information and a
///   rather obnoxious message describing this issue as a bug in TAMER.
///
/// This should be used in place of [`diagnostic_panic`] wherever
///   [`unreachable!`] would be used.
#[macro_export]
macro_rules! diagnostic_unreachable {
    ($desc_data:expr, $($panic_args:tt)*) => {
        $crate::diagnostic_panic!(
            @unreachable!, $desc_data, $($panic_args)*
        )
    }
}

/// Produce a panic using [`todo!`] with diagnostic information and a rather
///   obnoxious mesage describing this issue as a bug in TAMER.
///
/// This should be used in place of [`diagnostic_panic!`] wherever [`todo!`]
///   would be used.
///
/// Why This Macro Over `todo!`?
/// ============================
/// The [`todo!`] macro is used during development to indicate that a
///   portion of the system is not yet complete.
/// Ideally,
///   only a developer will hit this code path.
///
/// However,
///   when frequently releasing changes
///     (continuous integration),
///     end users may end up hitting those code paths,
///       either because they've enabled the feature flag associated with it
///       or because the code path was not expected to be reachable in
///       practice
///         (and so was not flagged).
/// In this case,
///   providing diagnostic information that highlights the specific input
///   that caused that path to be reached will help the user to not only
///   report the issue,
///     but also to figure out ways to work around it until the feature has
///     been completed.
///
/// Developers may most often hit this macro via unit tests,
///   where the diagnostic information is not likely to be useful.
/// However,
///   use of this diagnostic macro does allow the developer to run the
///   system on real-life inputs over the course of development to observe
///   how the system interacts.
/// The diagnostic message associated with this macro provides a quick and
///   valuable feedback loop in that case.
#[macro_export]
macro_rules! diagnostic_todo {
    ($desc_data:expr, $($panic_args:tt)*) => {
        $crate::diagnostic_panic!(
            @todo!,
            @title "THIS IS AN UNFINISHED FEATURE IN TAMER",
            $desc_data,
            $($panic_args)*
        )
    }
}

/// Alternatives to `unwrap` and `expect` that utilize
///   [`diagnostic_panic!`].
pub trait DiagnosticPanic {
    /// Type to produce after unwrapping.
    type Inner;

    /// Attempt to return the inner value,
    ///   consuming `self`.
    ///
    /// This is an alternative to the usual `unwrap` method,
    ///   producing diagnostic information in the event of a failure.
    /// See [`diagnostic_panic!`] for more information.
    ///
    /// # Panics
    /// Panics if the inner value is not available.
    /// For a custom message,
    ///   use [`DiagnosticPanic::diagnostic_expect`].
    fn diagnostic_unwrap<'a>(
        self,
        fdesc: impl FnOnce() -> Vec<AnnotatedSpan<'a>>,
    ) -> Self::Inner;

    /// Attempt to return the inner value,
    ///   consuming `self`.
    ///
    /// This is an alternative to the usual `expect` method,
    ///   producing diagnostic information in the event of a failure.
    /// See [`diagnostic_panic!`] for more information.
    ///
    /// Unlike Rust's `expect`,
    ///   this uses [`ThunkOrStaticRef`] to force deferring of any
    ///   message-related computation to avoid situations like those in
    ///     Clippy's `expect_fun_call` lint.
    ///
    /// # Panics
    /// Panics if the inner value is not available with a custom `msg`.
    fn diagnostic_expect<'a, M: ThunkOrStaticRef<str>>(
        self,
        fdesc: impl FnOnce() -> Vec<AnnotatedSpan<'a>>,
        msg: M,
    ) -> Self::Inner
    where
        M::Output: Display;
}

impl<T> DiagnosticPanic for Option<T> {
    type Inner = T;

    fn diagnostic_unwrap<'a>(
        self,
        fdesc: impl FnOnce() -> Vec<AnnotatedSpan<'a>>,
    ) -> Self::Inner {
        match self {
            Some(val) => val,
            // Same message as `Option::unwrap`
            None => diagnostic_panic!(
                fdesc(),
                "called `Option::unwrap()` on a `None` value"
            ),
        }
    }

    fn diagnostic_expect<'a, M: ThunkOrStaticRef<str>>(
        self,
        fdesc: impl FnOnce() -> Vec<AnnotatedSpan<'a>>,
        msg: M,
    ) -> Self::Inner
    where
        M::Output: Display,
    {
        match self {
            Some(val) => val,
            None => diagnostic_panic!(fdesc(), "{}", msg.call()),
        }
    }
}

impl<T, E> DiagnosticPanic for Result<T, E>
where
    E: Debug,
{
    type Inner = T;

    fn diagnostic_unwrap<'a>(
        self,
        fdesc: impl FnOnce() -> Vec<AnnotatedSpan<'a>>,
    ) -> Self::Inner {
        match self {
            Ok(val) => val,
            // Same message as `Result::unwrap`
            Err(e) => diagnostic_panic!(
                fdesc(),
                "called `Result::unwrap()` on an `Err` value: {e:?}"
            ),
        }
    }

    fn diagnostic_expect<'a, M: ThunkOrStaticRef<str>>(
        self,
        fdesc: impl FnOnce() -> Vec<AnnotatedSpan<'a>>,
        msg: M,
    ) -> Self::Inner
    where
        M::Output: Display,
    {
        match self {
            Ok(val) => val,
            Err(e) => diagnostic_panic!(fdesc(), "{}: {e:?}", msg.call()),
        }
    }
}

/// Convenience methods for [`Option`]-like data.
///
/// See also [`DiagnosticPanic`].
pub trait DiagnosticOptionPanic<T> {
    /// Expect that a value is [`None`]-like,
    ///   otherwise panic with diagnostic information generated from inner
    ///   [`Some`]-like value.
    fn diagnostic_expect_none<'a>(
        self,
        fdesc: impl FnOnce(T) -> Vec<AnnotatedSpan<'a>>,
        msg: &'static str,
    );
}

impl<T> DiagnosticOptionPanic<T> for Option<T> {
    fn diagnostic_expect_none<'a>(
        self,
        fdesc: impl FnOnce(T) -> Vec<AnnotatedSpan<'a>>,
        msg: &'static str,
    ) {
        match self {
            None => (),
            Some(x) => diagnostic_panic!(fdesc(x), "{}", msg),
        }
    }
}

#[cfg(test)]
mod test {
    use super::DiagnosticOptionPanic;

    #[should_panic = "__expected_panic__"]
    #[test]
    fn panic_on_none() {
        struct UniqFoo;

        Some(UniqFoo)
            .diagnostic_expect_none(|_: UniqFoo| vec![], "__expected_panic__")
    }
}
