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

use crate::sym::st::*;

pub mod prefix {
    //! Static [`Prefix`]es.

    use super::super::Prefix;
    use super::*;

    /// Namespace prefix [`L_C`] used for calculation expressions.
    pub const NS_C: Prefix = Prefix::st_cid(&L_C);

    /// Namespace prefix [`L_T`] used for short-hand template application.
    pub const NS_T: Prefix = Prefix::st_cid(&L_T);
}

pub mod qname {
    //! Static [`QName`]s.

    use super::*;
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
        QN_XMLNS_PREPROC: L_XMLNS:L_PREPROC,
        QN_XMLNS: :L_XMLNS,
        QN_XMLNS_C: L_XMLNS:L_C,
        QN_XMLNS_L: L_XMLNS:L_L,
        QN_XMLNS_LV: L_XMLNS:L_LV,
        QN_XMLNS_T: L_XMLNS:L_T,

        QN_ALL: :L_ALL,
        QN_ANY: :L_ANY,
        QN_ANY_OF: :CC_ANY_OF,
        QN_APPLY_TEMPLATE: :L_APPLY_TEMPLATE,
        QN_AS: :L_AS,
        QN_BASE_TYPE: :L_BASE_TYPE,
        QN_CLASS: :L_CLASS,
        QN_CLASSIFY: :L_CLASSIFY,
        QN_CONST: :L_CONST,
        QN_CORE: :L_CORE,
        QN_DASH: :L_DASH,
        QN_DEFAULT: :L_DEFAULT,
        QN_DESC: :L_DESC,
        QN_DIM: :L_DIM,
        QN_DISPLAY: :L_DISPLAY,
        QN_DOT: :L_DOT,
        QN_DTYPE: :L_DTYPE,
        QN_DYN_NODE: :L_DYN_NODE,
        QN_ENUM: :L_ENUM,
        QN_EQ: :L_EQ,
        QN_ERROR: :L_ERROR,
        QN_EXPAND_BARRIER: :L_EXPAND_BARRIER,
        QN_EXPAND_FUNCTION: :L_EXPAND_FUNCTION,
        QN_EXPAND_GROUP: :L_EXPAND_GROUP,
        QN_EXPAND_SEQUENCE: :L_EXPAND_SEQUENCE,
        QN_EXPORT: :L_EXPORT,
        QN_EXTERN: :L_EXTERN,
        QN_FOR_EACH: :L_FOR_EACH,
        QN_FROM: :L_FROM,
        QN_FUNCTION: :L_FUNCTION,
        QN_GENERATES: :L_GENERATES,
        QN_GENSYM: :L_GENSYM,
        QN_GENTLE_NO: :L_GENTLE_NO,
        QN_ID: :L_ID,
        QN_IDENTIFIER: :L_IDENTIFIER,
        QN_IF: :L_IF,
        QN_IGNORE_MISSING: :L_IGNORE_MISSING,
        QN_IMPORT: :L_IMPORT,
        QN_INDEX: :L_INDEX,
        QN_INLINE_TEMPLATE: :L_INLINE_TEMPLATE,
        QN_ISOVERRIDE: :L_ISOVERRIDE,
        QN_ITEM: :L_ITEM,
        QN_KEY: :L_KEY,
        QN_LABEL: :L_LABEL,
        QN_LOCAL: :L_LOCAL,
        QN_LOWER: :L_LOWER,
        QN_LV_IMPORT: L_LV:L_IMPORT,
        QN_LV_PACKAGE: L_LV:L_PACKAGE,
        QN_L_DEP: L_L:L_DEP,
        QN_L_EXEC: L_L:L_EXEC,
        QN_L_FROM: L_L:L_FROM,
        QN_L_MAP_EXEC: L_L:L_MAP_EXEC,
        QN_L_MAP_FROM: L_L:L_MAP_FROM,
        QN_L_RETMAP_EXEC: L_L:L_RETMAP_EXEC,
        QN_L_STATIC: L_L:L_STATIC,
        QN_MAP: :L_MAP,
        QN_MATCH: :L_MATCH,
        QN_META: :L_META,
        QN_METHOD: :L_METHOD,
        QN_NAME: :L_NAME,
        QN_NAME_PREFIX: :L_NAME_PREFIX,
        QN_NO: :L_NO,
        QN_NOVALIDATE: :L_NOVALIDATE,
        QN_OF: :L_OF,
        QN_ON: :L_ON,
        QN_OVERRIDE: :L_OVERRIDE,
        QN_PACKAGE: :L_PACKAGE,
        QN_PARAM: :L_PARAM,
        QN_PARAM_ADD: :L_PARAM_ADD,
        QN_PARAM_CLASS_TO_YIELDS: :L_PARAM_CLASS_TO_YIELDS,
        QN_PARAM_COPY: :L_PARAM_COPY,
        QN_PARAM_INHERIT: :L_PARAM_INHERIT,
        QN_PARAM_META: :L_PARAM_META,
        QN_PARAM_SYM_VALUE: :L_PARAM_SYM_VALUE,
        QN_PARAM_TYPEDEF_LOOKUP: :L_PARAM_TYPEDEF_LOOKUP,
        QN_PARAM_VALUE: :L_PARAM_VALUE,
        QN_PARENT: :L_PARENT,
        QN_PASS: :L_PASS,
        QN_PATH: :L_PATH,
        QN_PREFIX: :L_PREFIX,
        QN_PROGRAM: :L_PROGRAM,
        QN_PROGRAM_MAP: :L_PROGRAM_MAP,
        QN_RATE: :L_RATE,
        QN_RATER: :L_RATER,
        QN_RATE_EACH: :L_RATE_EACH,
        QN_RETURN_MAP: :L_RETURN_MAP,
        QN_RMDASH: :L_RMDASH,
        QN_RMUNDERSCORE: :L_RMUNDERSCORE,
        QN_SCALAR: :L_SCALAR,
        QN_SECTION: :L_SECTION,
        QN_SET: :L_SET,
        QN_SNAKE: :L_SNAKE,
        QN_SRC: :L_SRC,
        QN_SUFFIX: :L_SUFFIX,
        QN_SYM: :L_SYM,
        QN_SYM_SET: :L_SYM_SET,
        QN_TEMPLATE: :L_TEMPLATE,
        QN_TERMINATE: :L_TERMINATE,
        QN_TEXT: :L_TEXT,
        QN_TITLE: :L_TITLE,
        QN_TO: :L_TO,
        QN_TRANSFORM: :L_TRANSFORM,
        QN_TRANSLATE: :L_TRANSLATE,
        QN_TYPE: :L_TYPE,
        QN_TYPEDEF: :L_TYPEDEF,
        QN_UCFIRST: :L_UCFIRST,
        QN_UNION: :L_UNION,
        QN_UNIQUE: :L_UNIQUE,
        QN_UNLESS: :L_UNLESS,
        QN_UPPER: :L_UPPER,
        QN_UUROOTPATH: :L_UUROOTPATH,
        QN_VALUE: :L_VALUE,
        QN_VALUES: :L_VALUES,
        QN_VIRTUAL: :L_VIRTUAL,
        QN_WARNING: :L_WARNING,
        QN_WORKSHEET: :L_WORKSHEET,
        QN_YIELD: :L_YIELD,
        QN_YIELDS: :L_YIELDS,

        QN_C_APPLY: L_C:L_APPLY,
        QN_C_ARG: L_C:L_ARG,
        QN_C_CAR: L_C:L_CAR,
        QN_C_CASE: L_C:L_CASE,
        QN_C_CASES: L_C:L_CASES,
        QN_C_CDR: L_C:L_CDR,
        QN_C_CEIL: L_C:L_CEIL,
        QN_C_CONS: L_C:L_CONS,
        QN_C_CONST: L_C:L_CONST,
        QN_C_EQ: L_C:L_EQ,
        QN_C_EXPT: L_C:L_EXPT,
        QN_C_FLOOR: L_C:L_FLOOR,
        QN_C_GT: L_C:L_GT,
        QN_C_GTE: L_C:L_GTE,
        QN_C_INDEX: L_C:L_INDEX,
        QN_C_LENGTH_OF: L_C:L_LENGTH_OF,
        QN_C_LET: L_C:L_LET,
        QN_C_LT: L_C:L_LT,
        QN_C_LTE: L_C:L_LTE,
        QN_C_NE: L_C:L_NE,
        QN_C_OTHERWISE: L_C:L_OTHERWISE,
        QN_C_PRODUCT: L_C:L_PRODUCT,
        QN_C_QUOTIENT: L_C:L_QUOTIENT,
        QN_C_RECURSE: L_C:L_RECURSE,
        QN_C_SUM: L_C:L_SUM,
        QN_C_VALUE: L_C:L_VALUE,
        QN_C_VALUES: L_C:L_VALUES,
        QN_C_VALUE_OF: L_C:L_VALUE_OF,
        QN_C_VECTOR: L_C:L_VECTOR,
        QN_C_WHEN: L_C:L_WHEN,

        QN_P_ELIG_CLASS_YIELDS: L_PREPROC:L_ELIG_CLASS_YIELDS,
        QN_P_FRAGMENT: L_PREPROC:L_FRAGMENT,
        QN_P_FRAGMENTS: L_PREPROC:L_FRAGMENTS,
        QN_P_FROM: L_PREPROC:L_FROM,
        QN_P_GENERATED: L_PREPROC:L_GENERATED,
        QN_P_SYM: L_PREPROC:L_SYM,
        QN_P_SYMTABLE: L_PREPROC:L_SYMTABLE,
        QN_P_SYM_DEP: L_PREPROC:L_SYM_DEP,
        QN_P_SYM_DEPS: L_PREPROC:L_SYM_DEPS,
        QN_P_SYM_REF: L_PREPROC:L_SYM_REF,
    }
}
