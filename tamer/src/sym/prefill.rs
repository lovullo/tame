// Pre-interned strings
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

//! Pre-interned strings.
//!
//! These strings are expected to be encountered nearly every run,
//!   and substitute static strings that would otherwise appear hard-coded
//!   in the system and have to be interned to be compared against other
//!   values.
//!
//! See the [parent module](super) for more information.

use super::{Interner, SymbolId, SymbolIndexSize};
use crate::global;

/// Static symbol identifier that is stable between runs of the same version
///   of TAMER.
///
/// This symbol id is allocated at compile-time.
///
/// Safety
/// ======
/// All objects implementing this trait must have the same byte
///   representation as its inner [`SymbolId`].
pub unsafe trait StaticSymbolId<Ix: SymbolIndexSize = global::ProgSymSize>:
    private::Sealed
{
    // Traits cannot contain constant functions.
    // See [`st_as_sym`] below.
}

/// Convert any [`StaticSymbolId`] into its inner [`SymbolId`].
///
/// Static symbols are typed to convey useful information to newtypes that
///   wish to wrap or compose them.
/// This function peels back that type information to expose the inner
///   symbol.
///
/// Safety and Rationale
/// ====================
/// This function does its best to work around the limitation in Rust that
///   traits cannot contain constant functions
///   (at the time of writing).
///
/// To do this,
///   we require that every object of type [`StaticSymbolId`] have _the same
///   byte representation_ as [`SymbolId`].
/// Since Rust optimizes away simple newtype wrappers,
///   this means that we can simply cast the value to a symbol.
///
/// For example, if we have `StaticSymbolId<u32>`,
///   this would cast to a `SymbolId<u32>`.
/// The inner value of `SymbolId<u32>` is
///   `<u32 as SymbolIndexSize>::NonZero`,
///     which has the same byte representation as `u32`.
///
/// This would normally be done using [`std::mem::transmute`],
///   which ensures that the two types have compatible sizes.
/// Unfortunately,
///   the types here do not have fixed size and constant functions are
///   unable to verify that they are compatible at the time of writing.
/// We therefore must use [`std::mem::transmute_copy`] to circumvent this
///   size check.
///
/// Circumventing this check is safe given our trait bounds for all static
///   symbols in this module and its children.
/// However,
///   for this safety to hold,
///   we must ensure that no outside modules can implement
///   [`StaticSymbolId`] on their own objects.
/// For this reason,
///   [`StaticSymbolId`] implements [`private::Sealed`].
///
/// With that,
///   we get [`SymbolId`] polymorphism despite Rust's limitations.
///
/// A Note About Nightly
/// ====================
/// At the time of writing,
///   though,
///   this _does_ require two unstable features:
///     `const_fn_trait_bound` and `const_transmute_copy`.
/// We can get rid of the latter using raw pointer casts,
///   just as it does,
///   but since we're already relying on unstable flags,
///     we may as well use it while we require nightly for other things as
///     well.
///
/// `const_fn_trait_bound` cannot be removed in this situation without
///   another plan.
/// `const_panic` could be used with an enum,
///   but that still requires nightly.
pub const fn st_as_sym<T, Ix>(st: &T) -> SymbolId<Ix>
where
    T: StaticSymbolId<Ix>,
    Ix: SymbolIndexSize,
{
    // SAFETY: A number of precautions are taken to make this a safe and
    // sensible transformation; see function doc above.
    SymbolId(unsafe { std::mem::transmute_copy(st) })
}

/// Generate a newtype containing a condensed [`SymbolId`].
macro_rules! static_symbol_newtype {
    ($(#[$attr:meta])* $name:ident<$size:ty>) => {
        $(#[$attr])*
        /// This is a statically-allocated symbol.
        ///
        /// This symbol is generated at compile-time and expected to be
        ///   available in the 32-bit global interner once it has been
        ///   initialized.
        #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(<$size as SymbolIndexSize>::NonZero);

        // Mark this as a static symbol type, ensuring that it size is fully
        //   compatible with the underlying `SymbolId` so as not to cause
        //   problems with `st_as_sym`.
        impl private::Sealed for $name {}
        unsafe impl StaticSymbolId<$size> for $name {}
        assert_eq_size!($name, SymbolId<$size>);

        impl $name {
            const fn new(id: $size) -> Self {
                Self(unsafe {
                    <$size as SymbolIndexSize>::NonZero::new_unchecked(id)
                })
            }

            /// Cast static symbol into a [`SymbolId`] suitable for the global
            ///   program-level interner.
            ///
            /// This is safe since global interner will always contain this
            ///   symbol before it can be read.
            pub const fn as_sym(self) -> SymbolId<$size> {
                SymbolId(self.0)
            }

            pub const fn as_usize(self) -> usize {
                self.0.get() as usize
            }
        }

        impl From<$name> for SymbolId<$size> {
            fn from(st: $name) -> Self {
                st.as_sym()
            }
        }
    };
}

/// Generate a series of newtypes and the macro `static_symbol_ty!` which
///   can be used to take a short identifier and convert it into its full
///   type identifier.
macro_rules! static_symbol_newtypes {
    ($($(#[$attr:meta])* $short:ident: $ty:ident<$size:ty>,)*) => {
        $(
            static_symbol_newtype!($(#[$attr])* $ty<$size>);
        )*

        macro_rules! static_symbol_ty {
            $(
                ($short) => {
                    $ty
                };
            )*
        }
    }
}

/// Generate symbols for preinterned strings.
///
/// These symbols,
///   rather than being generated by the global internment system,
///   are generated statically.
/// Once the global interner is initialized
///   (see [parent module](`super`)),
///   which is on first access,
///   these symbols will reference valid values.
macro_rules! static_symbol_consts {
    (@i $i:expr; <$size:ty> $name:ident: $ty:ident $str:expr, $($tail:tt)*) => {
        #[doc=concat!(
            "Interned `",
            stringify!($ty),
            "` ",
            static_symbol_consts!(@!str $ty $str),
            "."
        )]
        #[doc=""]
        #[doc=concat!(
            "For the raw (untyped) version, see [`raw::",
            stringify!($name),
            "`]."
        )]
        pub const $name: static_symbol_ty!($ty) =
            <static_symbol_ty!($ty)>::new($i);

        // Recurse until no tail is left (terminating condition below).
        static_symbol_consts!{
            // This will result in 1 + 1 + 1 + 1 ... and will eventually hit
            // the recursion limit if we have too many static symbols, after
            // which time we may have to switch methodology.
            @i $i + 1;
            <$size>

            $($tail)*
        }
    };

    // Terminating condition.
    (@i $i:expr; <$size:ty>) => {
        /// Number of statically allocated symbols.
        ///
        /// This can be used to help determine a base capacity for
        ///   collections holding [`SymbolId`]s.
        pub const ST_COUNT: usize = $i - 1;
    };

    // Whitespace with newlines causes rustdoc parsing issues.
    (@!str ws $str:expr) => {
        "whitespace"
    };

    (@!str $ty:ident $str:expr) => {
        concat!("string `\"", $str, "\"`")
    };
}

/// Statically allocate [`SymbolId`]s for the provided symbols,
///   and schedule their static strings to be interned upon initialization
///   of the global interner.
///
/// This generates `fill`,
///   which the global interners call by default.
/// Any interner may optionally invoke this,
///   immediately after initialization,
///     /before/ any internment requests.
macro_rules! static_symbols {
    (<$size:ty>; $($name:ident : $ty:ident $str:expr),*) => {
        static_symbol_consts! {
            // Index 0 is not valid, so begin at 1
            @i 1;
            <$size>

            $(
                $name: $ty $str,
            )*
        }

        /// Expose each of the [typed static symbols](super) as raw
        ///   [`SymbolId`] values.
        ///
        /// These constants are useful for `match` expressions and any other
        ///   contexts where the type of the symbol must match,
        ///     and where the static type metadata is unimportant.
        ///
        /// This is equivalent to calling `as_sym` on the static newtype,
        ///   or using [`st_as_sym`](super::super::st_as_sym).
        pub mod raw {
            use super::SymbolId;

            $(
                #[doc=concat!(
                    "Raw (untyped) interned `",
                    stringify!($ty),
                    "` ",
                    static_symbols!(@!str $ty $str),
                    "."
                )]
                #[doc=""]
                #[doc=concat!(
                    "For the typed version, see [`super::",
                    stringify!($name),
                    "`]."
                )]
                pub const $name: SymbolId<$size> = super::$name.as_sym();
            )*
        }

        /// Fill a new interner with static symbols.
        ///
        /// Panics
        /// ======
        /// This function will panic if the interner has any symbols,
        ///   which would cause misalignment with the generated constants.
        pub(in super::super) fn fill<'a, I, Ix>(interner: I) -> I
        where
            I: Interner<'a, Ix>,
            Ix: SymbolIndexSize
        {
            assert!(
                interner.len() == 0,
                "cannot fill non-empty Interner with static symbols"
            );

            // This array does not exist as a constant, because that would
            //   require that we count the number of items first for the
            //   sake of the type definition.
            // This is more convenient.
            [
                $(
                    $str,
                )*
            ].into_iter().for_each(|sym| { interner.intern(sym); });

            interner
        }
    };

    // Whitespace with newlines causes rustdoc parsing issues.
    (@!str ws $str:expr) => {
        "whitespace"
    };

    (@!str $ty:ident $str:expr) => {
        concat!("string `\"", $str, "\"`")
    };
}

static_symbol_newtypes! {
    /// A symbol suitable as a C-style identifier.
    ///
    /// This is the traditional `[a-zA-Z_][a-zA-Z0-9_]*`,
    ///   common in many programming languages.
    cid: CIdentStaticSymbolId<global::ProgSymSize>,

    /// Base-10 (decimal) integer value as a string.
    dec: DecStaticSymbolId<global::ProgSymSize>,

    /// A symbol resembling a QName of the form `prefix:local`.
    ///
    /// A symbol of this type does _not_ mean that the symbol is intended to
    ///   be a QName;
    ///     this is merely a way to describe it.
    /// For example,
    ///   `map:head` is intended as an identifier type,
    ///     not a QName.
    qname: QnameIdentStaticSymbolId<global::ProgSymSize>,

    /// This symbol serves only as a marker in the internment pool to
    ///   delimit symbol ranges;
    ///     its string value is incidental and should not be relied upon.
    mark: MarkStaticSymbolId<global::ProgSymSize>,

    /// A symbol suitable as a TAME identifier.
    ///
    /// This is [`CIdentStaticSymbolId`] with `-` added:
    ///   `[a-zA-Z_-][a-zA-Z0-9_-]*`.
    /// This is also suitable as an XML node or attribute name.
    tid: TameIdentStaticSymbolId<global::ProgSymSize>,

    /// Symbol representing a URI.
    ///
    /// This is intended for use primarily as an XML namespace.
    /// URIs are expected to _not_ contain quotes and other characters that
    ///   may need escaping in XML attributes.
    uri: UriStaticSymbolId<global::ProgSymSize>,

    /// Any other generic string that does not fit into any particular type.
    str: GenericStaticSymbolId<global::ProgSymSize>,

    /// Common strings of whitespace
    ///   (where a character of whitespace is `[ \n]`).
    ///
    /// There are certainly other whitespace characters,
    ///   but this is intended to be conservative to address only the most
    ///   common cases.
    ws: WhitespaceStaticSymbolId<global::ProgSymSize>,

    /// Static 16-bit [`Span`](crate::span::Span) context.
    ///
    /// These contexts are intended for use in generated code where a better
    ///   context cannot be derived.
    ctx: ContextStaticSymbolId<u16>,

    /// This symbol serves only as a marker in the internment pool to
    ///   delimit symbol ranges;
    ///     its string value is incidental and should not be relied upon.
    mark16: Mark16StaticSymbolId<u16>,
}

/// Static symbols (pre-allocated).
///
/// Each of the constants in this module represent a [`SymbolId`] statically
///   allocated at compile-time.
/// The strings that they represent are automatically populated into the
///   global interners when the interner is first accessed.
///
/// _You should always use the generated constant to reference these
///    symbols!_
/// Do not rely upon their integer value,
///   as it _will_ change over time.
/// The sole exception is to use marker symbols to identify ranges
///   of symbols;
///     see [`MarkStaticSymbolId`].
///
/// See [`crate::sym`] for more information on static symbols.
///
/// `static` is a keyword in Rust,
///   so we shorten the module name to `st`.
///
/// The constants follow a naming convention:
///   - `L_` indicates that the identifier is all-lowercase.
pub mod st {
    use super::*;

    // Convert `0 ≤ n ≤ 9` into a static symbol representing a single
    //   decimal digit.
    //
    // Panics
    // ======
    // This will panic if `n > 9`.
    pub fn decimal1(n: u8) -> DecStaticSymbolId {
        assert!(n < 10);

        // The symbols are expected to be in a very specific position in the
        //   pool (n+1).
        // This is verified by tests at the bottom of this file.
        DecStaticSymbolId(unsafe {
            <global::ProgSymSize as SymbolIndexSize>::NonZero::new_unchecked(
                (n as global::ProgSymSize) + 1,
            )
        })
    }

    impl From<u8> for DecStaticSymbolId {
        // Convert `0 ≤ n ≤ 9` into a static symbol representing a single
        //   decimal digit.
        //
        // See [`decimal1`].
        fn from(n: u8) -> Self {
            decimal1(n)
        }
    }

    /// Whether the provided symbol is part of the static symbol list that
    ///   is pre-interned.
    #[inline]
    pub fn is_pre_interned(sym: SymbolId) -> bool {
        let symid = sym.as_usize();
        symid <= END_STATIC.as_usize()
    }

    /// Whether the given [`SymbolId`] is within a group of symbols
    ///   delimited by markers `a` and `b`.
    ///
    /// This provides a _reasonably_ efficient way to compare a [`SymbolId`]
    ///   against a large set of [`SymbolId`]s.
    /// There are more efficient ways to accomplish this,
    ///   though,
    ///   if performance ever does become a concern;
    ///     the current implementation is kept simple until then.
    #[inline]
    pub fn is_between_markers(
        a: MarkStaticSymbolId,
        b: MarkStaticSymbolId,
        sym: SymbolId,
    ) -> bool {
        let symid = sym.as_usize();
        symid > a.as_usize() && symid < b.as_usize()
    }

    /// Whether the provided [`SymbolId`] is recognized as a common
    ///   whitespace symbol in the preinterned symbol list.
    ///
    /// If this returns `true`,
    ///   then this is a quick way to determine that the provided
    ///   [`SymbolId`] does contain only whitespace.
    /// However,
    ///   this is _not_ comprehensive and never will be,
    ///     so an answer of `false` means "it may or may not be whitespace";
    ///       you should fall back to other methods of checking for
    ///       whitespace if this fails.
    #[inline]
    pub fn is_common_whitespace(sym: SymbolId) -> bool {
        is_between_markers(WS_SYM_START, WS_SYM_END, sym)
    }

    /// Attempt to make a quick determination without a memory lookup
    ///   (symbol resolution) whether the given [`SymbolId`]'s string
    ///   representation definitely contains the given byte value.
    ///
    /// A value of [`None`] means "maybe, maybe not",
    ///   indicating that the caller ought to fall back to a slower check
    ///   that utilizes the symbol's resolved string.
    /// A value of [`Some`] indicates that `sym`,
    ///   were it to be resolved,
    ///   definitely does or does not contain the byte `ch`.
    ///
    /// This is intended to encapsulate special,
    ///   loosely-defined cases where we can test that the interned symbols
    ///   actually properly adhere to the implementation of this function.
    #[inline]
    pub fn quick_contains_byte(sym: SymbolId, ch: u8) -> Option<bool> {
        match (is_pre_interned(sym), ch) {
            // No control characters or null bytes.
            (true, 0..=0x1F) => Some(false),

            // No characters outside the 7-bit ASCII range.
            (true, 0x80..) => Some(false),

            // Or the character range immediately preceding it,
            //   where 7F == DEL.
            // They are explicitly listed here so that readers do not have
            //   to consult an ASCII table to avoid unintentional bugs.
            (true, b'{' | b'|' | b'}' | b'~' | 0x7F) => Some(false),

            // We don't check for anything else (yet).
            (true, _) => None,

            // We cannot possibly know statically whether dynamically
            //   interned symbols contain any particular byte.
            (false, _) => None,
        }
    }

    static_symbols! {
        <crate::global::ProgSymSize>;

        // Decimal strings are expected to be at index (n+1).
        // See `decimal1`.
        N0: dec "0",
        N1: dec "1",
        N2: dec "2",
        N3: dec "3",
        N4: dec "4",
        N5: dec "5",
        N6: dec "6",
        N7: dec "7",
        N8: dec "8",
        N9: dec "9",

        L_ALL: cid "all",
        L_ANY: cid "any",
        L_APPLY: cid "apply",
        L_APPLY_TEMPLATE: tid "apply-template",
        L_ARG: cid "arg",
        L_AS: cid "as",
        L_BASE_TYPE: tid "base-type",
        L_BOOLEAN: cid "boolean",
        L_C: cid "c",
        L_CAR: cid "car",
        L_CASE: cid "case",
        L_CASES: cid "cases",
        L_CDR: cid "cdr",
        L_CEIL: cid "ceil",
        L_CGEN: cid "cgen",
        L_CLASS: cid "class",
        L_CLASSIFY: cid "classify",
        L_CONS: cid "cons",
        L_CONST: cid "const",
        L_CORE: cid "core",
        L_DASH: cid "dash",
        L_DEFAULT: cid "default",
        L_DEP: cid "dep",
        L_DESC: cid "desc",
        L_DIM: cid "dim",
        L_DISPLAY: cid "display",
        L_DOT: cid "dot",
        L_DTYPE: cid "dtype",
        L_DYN_NODE: tid "dyn-node",
        L_ELIG_CLASS_YIELDS: tid "elig-class-yields",
        L_EMPTY: cid "empty",
        L_ENUM: cid "enum",
        L_EQ: cid "eq",
        L_ERROR: cid "error",
        L_EXEC: cid "exec",
        L_EXPAND_BARRIER: tid "expand-barrier",
        L_EXPAND_FUNCTION: tid "expand-function",
        L_EXPAND_GROUP: tid "expand-group",
        L_EXPAND_SEQUENCE: tid "expand-sequence",
        L_EXPORT: cid "export",
        L_EXPT: cid "expt",
        L_EXTERN: cid "extern",
        L_FALSE: cid "false",
        L_FLOAT: cid "float",
        L_FLOOR: cid "floor",
        L_FOR_EACH: tid "for-each",
        L_FRAGMENT: cid "fragment",
        L_FRAGMENTS: cid "fragments",
        L_FROM: cid "from",
        L_FUNC: cid "func",
        L_FUNCTION: cid "function",
        L_GEN: cid "gen",
        L_GENERATED: cid "generated",
        L_GENERATES: cid "generates",
        L_GENSYM: cid "gensym",
        L_GENTLE_NO: tid "gentle-no",
        L_GT: cid "gt",
        L_GTE: cid "gte",
        L_ID: cid "id",
        L_IDENTIFIER: cid "identifier",
        L_IF: cid "if",
        L_IGNORE_MISSING: tid "ignore-missing",
        L_IMPORT: cid "import",
        L_INDEX: cid "index",
        L_INLINE_TEMPLATE: tid "inline-template",
        L_INTEGER: cid "integer",
        L_ISOVERRIDE: cid "isoverride",
        L_ITEM: cid "item",
        L_KEY: cid "key",
        L_L: cid "l",
        L_LABEL: cid "label",
        L_LENGTH_OF: tid "length-of",
        L_LET: cid "let",
        L_LOCAL: cid "local",
        L_LOWER: cid "lower",
        L_LPARAM: cid "lparam",
        L_LT: cid "lt",
        L_LTE: cid "lte",
        L_LV: cid "lv",
        L_MAP: cid "map",
        L_MAP_EXEC: tid "map-exec",
        L_MAP_FROM: tid "map-from",
        L_MAP_HEAD: qname "map:head",
        L_MAP_TAIL: qname "map:tail",
        L_MATCH: cid "match",
        L_META: cid "meta",
        L_METHOD: cid "method",
        L_NAME: cid "name",
        L_NAME_PREFIX: tid "name-prefix",
        L_NE: cid "ne",
        L_NO: cid "no",
        L_NOVALIDATE: cid "novalidate",
        L_OF: cid "of",
        L_ON: cid "on",
        L_OTHERWISE: cid "otherwise",
        L_OVERRIDE: cid "override",
        L_PACKAGE: cid "package",
        L_PARAM: cid "param",
        L_PARAM_ADD: tid "param-add",
        L_PARAM_CLASS_TO_YIELDS: tid "param-class-to-yields",
        L_PARAM_COPY: tid "param-copy",
        L_PARAM_INHERIT: tid "param-inherit",
        L_PARAM_META: tid "param-meta",
        L_PARAM_SYM_VALUE: tid "param-sym-value",
        L_PARAM_TYPEDEF_LOOKUP: tid "param-typedef-lookup",
        L_PARAM_VALUE: tid "param-value",
        L_PARENT: cid "parent",
        L_PASS: cid "pass",
        L_PATH: cid "path",
        L_PREFIX: cid "prefix",
        L_PREPROC: cid "preproc",
        L_PRODUCT: cid "product",
        L_PROGRAM: cid "program",
        L_PROGRAM_MAP: tid "program-map",
        L_QUOTIENT: cid "quotient",
        L_RATE: cid "rate",
        L_RATER: cid "rater",
        L_RATE_EACH: cid "rate-each",
        L_RECURSE: cid "recurse",
        L_RETMAP: cid "retmap",
        L_RETMAP_EXEC: tid "retmap-exec",
        L_RETMAP_HEAD: qname "retmap:head",
        L_RETMAP_TAIL: qname "retmap:tail",
        L_RETURN_MAP: tid "return-map",
        L_RMDASH: cid "rmdash",
        L_RMUNDERSCORE: cid "rmunderscore",
        L_SCALAR: cid "scalar",
        L_SECTION: cid "section",
        L_SET: cid "set",
        L_SNAKE: cid "snake",
        L_SRC: cid "src",
        L_STATIC: cid "static",
        L_SUFFIX: cid "suffix",
        L_SUM: cid "sum",
        L_SYM: cid "sym",
        L_SYMTABLE: cid "symtable",
        L_SYM_DEP: cid "sym-dep",
        L_SYM_DEPS: cid "sym-deps",
        L_SYM_REF: cid "sym-ref",
        L_SYM_SET: tid "sym-set",
        L_T: cid "t",
        L_TEMPLATE: cid "template",
        L_TERMINATE: cid "terminate",
        L_TEXT: cid "text",
        L_TITLE: cid "title",
        L_TO: cid "to",
        L_TPL: cid "tpl",
        L_TRANSFORM: cid "transform",
        L_TRANSLATE: cid "translate",
        L_TRUE: cid "true",
        L_TYPE: cid "type",
        L_TYPEDEF: cid "typedef",
        L_UCFIRST: cid "ucfirst",
        L_UNION: cid "union",
        L_UNIQUE: cid "unique",
        L_UNLESS: cid "unless",
        L_UPPER: cid "upper",
        L_UUROOTPATH: cid "__rootpath",
        L_VALUE: cid "value",
        L_VALUES: cid "values",
        L_VALUE_OF: cid "value-of",
        L_VECTOR: cid "vector",
        L_VIRTUAL: cid "virtual",
        L_WARNING: cid "warning",
        L_WHEN: cid "when",
        L_WITH_PARAM: tid "with-param",
        L_WORKSHEET: cid "worksheet",
        L_XMLNS: cid "xmlns",
        L_YIELD: cid "yield",
        L_YIELDS: cid "yields",

        L_TPLP_VALUES: str "@values@",

        FW_SLASH: str "/",
        FW_SLASH_DOT: str "/.",

        CC_ANY_OF: cid "anyOf",

        U_TRUE: cid "TRUE",

        URI_LV_CALC: uri "http://www.lovullo.com/calc",
        URI_LV_LINKER: uri "http://www.lovullo.com/rater/linker",
        URI_LV_PREPROC: uri "http://www.lovullo.com/rater/preproc",
        URI_LV_PROGRAM_MAP: uri "http://www.lovullo.com/rater/map",
        URI_LV_RATER: uri "http://www.lovullo.com/rater",
        URI_LV_TPL: uri "http://www.lovullo.com/rater/apply-template",
        URI_LV_WORKSHEET: uri "http://www.lovullo.com/rater/worksheet",

        // Common whitespace.
        //
        // _This does not represent all forms of whitespace!_
        // Clearly,
        //   but it is worth emphasizing.
        //
        // The intent of these whitespace symbols is to provide a means to
        //   determine whether that symbol represents a common form of
        //   whitespace,
        //     before falling back to a more expensive symbol dereference
        //     and (likely-)linear scan.
        //
        // This list is preliminary and ought to be measured by evaluating a
        //   real-world codebase;
        //     it ought not to bloat the symbol table,
        //       but ought to get the most common cases so as not to fall
        //       back to a more expensive dereferencing of a symbol and
        //       subsequent scanning.
        //
        // There are improvements that can be made here,
        //   such as aligning the symbol ids such that whitespace can be
        //   asserted with a bitmask.
        WS_SYM_START: mark "###WS_START",
        WS_EMPTY: ws "",
        WS_SP1: ws " ",
        WS_SP2: ws "  ",
        WS_SP3: ws "   ",
        WS_SP4: ws "    ",
        WS_SP5: ws "     ",
        WS_SP6: ws "      ",
        WS_SP7: ws "       ",
        WS_SP8: ws "        ",
        WS_LF1: ws "\n",
        WS_LF2: ws "\n\n",
        WS_LF1_SP1: ws "\n ",
        WS_LF1_SP2: ws "\n  ",
        WS_LF1_SP3: ws "\n   ",
        WS_LF1_SP4: ws "\n    ",
        WS_LF1_SP5: ws "\n     ",
        WS_LF1_SP6: ws "\n      ",
        WS_LF1_SP7: ws "\n       ",
        WS_LF1_SP8: ws "\n        ",
        WS_LF2_SP1: ws "\n\n ",
        WS_LF2_SP2: ws "\n\n  ",
        WS_LF2_SP3: ws "\n\n   ",
        WS_LF2_SP4: ws "\n\n    ",
        WS_LF2_SP5: ws "\n\n     ",
        WS_LF2_SP6: ws "\n\n      ",
        WS_LF2_SP7: ws "\n\n       ",
        WS_LF2_SP8: ws "\n\n        ",
        WS_SYM_END: mark "###WS_END",

        // [Symbols will be added here as they are needed.]

        // Marker indicating the end of the static symbols
        //   (this must always be last).
        END_STATIC: mark "###END"
    }
}

/// Static 16-bit symbols (pre-allocated).
///
/// These symbols are intended for situations where a smaller symbol size is
///   necessary.
/// Presently,
///   this includes only the [`Span`](crate::span::Span) context.
///
/// See also [st](super::st) for general static symbols.
pub mod st16 {
    use super::*;

    static_symbols! {
        <u16>;

        // Special contexts.
        CTX_DUMMY: ctx "#!DUMMY",
        CTX_UNKNOWN: ctx "#!UNKNOWN",
        CTX_LINKER: ctx "#!LINKER",

        // [Symbols will be added here as they are needed.]

        // Marker indicating the end of the static symbols
        //   (this must always be last).
        END_STATIC: mark16 "###END"
    }
}

/// Non-public module that can contain public traits.
///
/// The problem this module tries to solve is preventing anything outside of
///   this crate from implementing the `StaticSymbolId` trait,
///     since doing so opens us up to undefined behavior when transmuting
///     via [`st_as_sym`](super::st_as_sym).
mod private {
    /// Extend this trait to prevent other modules from implementing the
    ///   subtype.
    ///
    /// Since other modules extend [`StaticSymbolId`](super::StaticSymbolId)
    ///   for their own traits,
    ///     this trait must be `pub`.
    /// But, since it is contained within a private module,
    ///   it is not possible to import the trait to implement it on other
    ///   things.
    pub trait Sealed {}
}

#[cfg(test)]
mod test {
    use super::{st, st16, DecStaticSymbolId};
    use crate::sym::{GlobalSymbolIntern, GlobalSymbolResolve, SymbolId};

    #[test]
    fn global_sanity_check_st() {
        // If we _don't_ prefill, make sure we're not starting at the first
        // offset when interning, otherwise it'll look correct.
        let new: SymbolId = "force offset".intern();

        assert!(
            new.as_usize() > st::END_STATIC.as_usize(),
            "a new global symbol allocation was not > END_STATIC, \
             indicating that prefill is either not working or that \
             the prefill contains duplicate strings!"
        );

        // Further sanity check to make sure indexes align as expected,
        // not that you wouldn't otherwise notice that the whole system is
        // broken, but this ought to offer a more direct hint as to what
        // went wrong.
        assert_eq!(st::L_TRUE.as_sym(), "true".intern());
        assert_eq!(st::L_FALSE.as_sym(), "false".intern());
    }

    // Just ensure raw symbols are available and match.
    #[test]
    fn sanity_check_st_raw() {
        assert_eq!(st::L_TRUE.as_sym(), st::raw::L_TRUE);
    }

    #[test]
    fn global_sanity_check_st16() {
        // If we _don't_ prefill, make sure we're not starting at the first
        // offset when interning, otherwise it'll look correct.
        let new: SymbolId<u16> = "force offset".intern();

        assert!(
            new.as_usize() > st16::END_STATIC.as_usize(),
            "a new 16-bit global symbol allocation was not > END_STATIC, \
             indicating that prefill is either not working or that \
             the prefill contains duplicate strings!"
        );
    }

    #[test]
    fn decimal1_0_to_9() {
        for n in 0..=9 {
            assert_eq!(st::decimal1(n).as_sym().lookup_str(), n.to_string());

            // From<u8>
            assert_eq!(
                DecStaticSymbolId::from(n).as_sym().lookup_str(),
                n.to_string()
            );
        }
    }

    #[test]
    #[should_panic]
    fn decimal1_gt_9_panics() {
        st::decimal1(10);
    }

    #[test]
    fn st_count_matches_actual_count() {
        // This assumes that static symbols begin at 1 and end at
        //   END_STATIC.
        assert_eq!(
            st::END_STATIC.as_usize(),
            st::ST_COUNT,
            "st::ST_COUNT does not match the number of static symbols"
        );
    }

    // [`quick_contains_bytes`] is asking for trouble if it's not properly
    //   maintained.
    // It is expected that its implementation is manually verified,
    //   and it is written in a way that is clear and unambiguous.
    // With that said,
    //   this does some minor spot-checking.
    #[test]
    fn quick_contains_byte_verify() {
        use super::super::GlobalSymbolResolve;
        use memchr::memchr;
        use st::quick_contains_byte;

        // No static symbols will contain control characters.
        assert_eq!(quick_contains_byte(st::L_TRUE.into(), 0x01), Some(false));

        // But we don't know about dynamically-allocated ones.
        assert_eq!(
            quick_contains_byte("NOT A PREINTERNED SYM".into(), 0x01),
            None
        );

        // We chose to explicitly keep certain characters out of the
        //   preinterned list.
        // Let's verify that is the case by iterating through _all of the
        //   static interns_.
        for sym_id in 1..=st::ST_COUNT {
            let sym = unsafe { SymbolId::from_int_unchecked(sym_id as u32) };

            // If you get an error in this block,
            //   that means that you have added a symbol that violates
            //   assumptions made in `quick_contains_byte`.
            // Either that implementation needs changing and this test
            //   updated,
            //     or you need to not add that symbol to the static symbol
            //     list.
            for ch in b'{'..=0x7F {
                assert_eq!(
                    memchr(ch, sym.lookup_str().as_bytes()),
                    None,
                    "Pre-interned static symbol {sym:?} \
                       contains unexpected byte 0x{ch:X}"
                );
            }
        }
    }
}
