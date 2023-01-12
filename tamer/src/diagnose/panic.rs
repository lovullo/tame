// TAMER diagnostic system panics
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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

    (@$macro:ident!, $desc_data:expr, $($panic_args:tt)*) => {{
        use $crate::diagnose::Reporter;

        let mut reporter = $crate::diagnose::panic::PanicReporter::new(
            Default::default()
        );

        let summary = format!($($panic_args)*);
        let desc = $crate::diagnose::panic::DiagnosticDesc(
            summary,
            std::cell::Cell::new($desc_data),
        );

        $macro!(
            "internal error:\n{}\n{}",
            reporter.render(&desc),
            // Be extra obnoxious.
            // This shouldn't ever happen except under exceedingly
            //   exceptional circumstances,
            //     so it's acceptable to make a big deal about it.
            "\x1b[0;31m
!!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ !!!
!!!                THIS IS A BUG IN TAMER                 !!!
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
\x1b[0m"
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
    fn diagnostic_unwrap(self, desc: Vec<AnnotatedSpan>) -> Self::Inner;

    /// Attempt to return the inner value,
    ///   consuming `self`.
    ///
    /// This is an alternative to the usual `expect` method,
    ///   producing diagnostic information in the event of a failure.
    /// See [`diagnostic_panic!`] for more information.
    ///
    /// # Panics
    /// Panics if the inner value is not available with a custom `msg`.
    fn diagnostic_expect(
        self,
        desc: Vec<AnnotatedSpan>,
        msg: &str,
    ) -> Self::Inner;
}

impl<T> DiagnosticPanic for Option<T> {
    type Inner = T;

    fn diagnostic_unwrap(self, desc: Vec<AnnotatedSpan>) -> Self::Inner {
        match self {
            Some(val) => val,
            // Same message as `Option::unwrap`
            None => diagnostic_panic!(
                desc,
                "called `Option::unwrap()` on a `None` value"
            ),
        }
    }

    fn diagnostic_expect(
        self,
        desc: Vec<AnnotatedSpan>,
        msg: &str,
    ) -> Self::Inner {
        match self {
            Some(val) => val,
            None => diagnostic_panic!(desc, "{}", msg),
        }
    }
}

impl<T, E> DiagnosticPanic for Result<T, E>
where
    E: Debug,
{
    type Inner = T;

    fn diagnostic_unwrap(self, desc: Vec<AnnotatedSpan>) -> Self::Inner {
        match self {
            Ok(val) => val,
            // Same message as `Result::unwrap`
            Err(e) => diagnostic_panic!(
                desc,
                "called `Result::unwrap()` on an `Err` value: {e:?}"
            ),
        }
    }

    fn diagnostic_expect(
        self,
        desc: Vec<AnnotatedSpan>,
        msg: &str,
    ) -> Self::Inner {
        match self {
            Ok(val) => val,
            Err(e) => diagnostic_panic!(desc, "{}: {e:?}", msg),
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
        msg: &str,
    );
}

impl<T> DiagnosticOptionPanic<T> for Option<T> {
    fn diagnostic_expect_none<'a>(
        self,
        fdesc: impl FnOnce(T) -> Vec<AnnotatedSpan<'a>>,
        msg: &str,
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
