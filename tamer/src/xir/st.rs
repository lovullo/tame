// Static XML symbols
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

//! Static XML symbols.
//!
//! This is analogous to [`crate::sym::st`].

pub mod qname {
    //! Static [`QName`]s.

    use crate::sym::st::*;
    use crate::sym::{
        CIdentStaticSymbolId, StaticSymbolId, TameIdentStaticSymbolId,
    };

    #[cfg(doc)]
    use super::super::QName;

    /// A static symbol that can be safely converted into a [`QName`] without
    ///   any checks.
    ///
    /// This must only be implemented on static symbol types that are known to
    ///   be valid QNames.
    pub trait QNameCompatibleStaticSymbolId: StaticSymbolId {}

    impl QNameCompatibleStaticSymbolId for CIdentStaticSymbolId {}
    impl QNameCompatibleStaticSymbolId for TameIdentStaticSymbolId {}

    #[doc(hidden)]
    // rustfmt is over-indenting the doc annotations at the time of writing.
    #[rustfmt::skip]
    macro_rules! qname_const_inner {
        ($name:ident = :$local:ident) => {
            #[doc=concat!(
                "QName with no namespace prefix and local name [`",
                stringify!($local),
                "`].",
            )]
            pub const $name: crate::xir::QName =
                crate::xir::QName::st_cid_local(&$local);
        };

        ($name:ident = $prefix:ident:$local:ident) => {
            #[doc=concat!(
                "QName with namespace prefix [`",
                stringify!($prefix),
                "`] and local name [`",
                stringify!($local),
                "`].",
            )]
            pub const $name: crate::xir::QName =
                crate::xir::QName::st_cid(&$prefix, &$local);
        };
    }

    /// Construct a series of [`QName`] constants.
    ///
    /// The syntax for each constant is `NAME: [PREFIX]:LOCAL`,
    ///   where `PREFIX` is optional.
    ///
    /// See [`crate::sym::st`] for usable symbol constants.
    macro_rules! qname_const {
        ($($name:ident: $($prefix:ident)? : $local:ident,)*) => {
            $(
                qname_const_inner!($name = $($prefix)?:$local);
            )*
        }
    }

    qname_const! {
        QN_DESC: :L_DESC,
        QN_DIM: :L_DIM,
        QN_DTYPE: :L_DTYPE,
        QN_ELIG_CLASS_YIELDS: L_PREPROC:L_ELIG_CLASS_YIELDS,
        QN_EXTERN: :L_EXTERN,
        QN_FRAGMENT: L_PREPROC:L_FRAGMENT,
        QN_FRAGMENTS: L_PREPROC:L_FRAGMENTS,
        QN_FROM: L_PREPROC:L_FROM,
        QN_GENERATED: L_PREPROC:L_GENERATED,
        QN_ID: :L_ID,
        QN_ISOVERRIDE: :L_ISOVERRIDE,
        QN_LV_PACKAGE: L_LV:L_PACKAGE,
        QN_L_DEP: L_L:L_DEP,
        QN_L_EXEC: L_L:L_EXEC,
        QN_L_FROM: L_L:L_FROM,
        QN_L_MAP_EXEC: L_L:L_MAP_EXEC,
        QN_L_MAP_FROM: L_L:L_MAP_FROM,
        QN_L_RETMAP_EXEC: L_L:L_RETMAP_EXEC,
        QN_L_STATIC: L_L:L_STATIC,
        QN_NAME: :L_NAME,
        QN_PACKAGE: :L_PACKAGE,
        QN_PARENT: :L_PARENT,
        QN_PROGRAM: :L_PROGRAM,
        QN_P_SYM: L_PREPROC:L_SYM,
        QN_SRC: :L_SRC,
        QN_SYM: L_PREPROC:L_SYM,
        QN_SYMTABLE: L_PREPROC:L_SYMTABLE,
        QN_SYM_DEP: L_PREPROC:L_SYM_DEP,
        QN_SYM_DEPS: L_PREPROC:L_SYM_DEPS,
        QN_SYM_REF: L_PREPROC:L_SYM_REF,
        QN_TITLE: :L_TITLE,
        QN_TYPE: :L_TYPE,
        QN_UUROOTPATH: :L_UUROOTPATH,
        QN_VIRTUAL: :L_VIRTUAL,
        QN_XMLNS: :L_XMLNS,
        QN_XMLNS_L: L_XMLNS:L_L,
        QN_XMLNS_PREPROC: L_XMLNS:L_PREPROC,
        QN_YIELDS: :L_YIELDS,
    }
}
