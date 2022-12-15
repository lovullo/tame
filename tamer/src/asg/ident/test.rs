// Tests for ASG identifiers
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

use super::*;
use crate::{
    num::Dim,
    span::dummy::*,
    sym::{GlobalSymbolIntern, SymbolId},
};

// Note that Ident has no variants capable of None
#[test]
fn ident_name() {
    let name = "name".into();
    let spair = SPair(name, S1);

    assert_eq!(spair, Ident::Missing(spair).name());

    assert_eq!(
        spair,
        Ident::Ident(spair, IdentKind::Meta, Source::default()).name()
    );

    assert_eq!(
        spair,
        Ident::Extern(spair, IdentKind::Meta, Source::default()).name()
    );

    assert_eq!(
        spair,
        Ident::IdentFragment(
            spair,
            IdentKind::Meta,
            Source::default(),
            "".intern()
        )
        .name()
    );
}

#[test]
fn ident_kind() {
    let name = SPair("foo".into(), S1);
    let kind = IdentKind::Class(Dim::Matrix);

    assert_eq!(None, Ident::Missing(name).kind());

    assert_eq!(
        Some(&kind),
        Ident::Ident(name, kind.clone(), Source::default()).kind()
    );

    assert_eq!(
        Some(&kind),
        Ident::Extern(name, kind.clone(), Source::default()).kind()
    );

    assert_eq!(
        Some(&kind),
        Ident::IdentFragment(
            name,
            kind.clone(),
            Source::default(),
            "".intern()
        )
        .kind()
    );
}

#[test]
fn ident_src() {
    let name = SPair("foo".into(), S1);
    let src = Source {
        desc: Some("test source".into()),
        ..Default::default()
    };

    assert_eq!(None, Ident::Missing(name).src());

    assert_eq!(
        Some(&src),
        Ident::Ident(name, IdentKind::Meta, src.clone()).src()
    );

    assert_eq!(
        None,
        Ident::Extern(name, IdentKind::Meta, src.clone()).src()
    );

    assert_eq!(
        Some(&src),
        Ident::IdentFragment(name, IdentKind::Meta, src.clone(), "".intern())
            .src()
    );
}

#[test]
fn ident_fragment() {
    let name = SPair("foo".into(), S1);
    let text = "foo".into();

    assert_eq!(None, Ident::Missing(name).fragment());

    assert_eq!(
        None,
        Ident::Ident(name, IdentKind::Meta, Source::default()).fragment()
    );

    assert_eq!(
        None,
        Ident::Extern(name, IdentKind::Meta, Source::default()).fragment()
    );

    assert_eq!(
        Some(text),
        Ident::IdentFragment(name, IdentKind::Meta, Source::default(), text,)
            .fragment()
    );
}

#[test]
fn ident_missing() {
    let name = SPair("foo".into(), S1);
    assert_eq!(Ident::Missing(name), Ident::declare(name));
}

#[test]
fn resolved_on_missing() {
    let name = SPair("foo".into(), S1);

    assert_eq!(
        Ident::declare(name).resolved(),
        Err(UnresolvedError::Missing(name))
    );
}

#[test]
fn ident_resolved() {
    let sym = "foo".into();
    let kind = IdentKind::Meta;
    let src = Source {
        desc: Some("ident ctor".into()),
        ..Default::default()
    };

    assert_eq!(
        Ident::declare(SPair(sym, S1))
            .resolve(S2, kind.clone(), src.clone())
            .unwrap(),
        Ident::Ident(SPair(sym, S2), kind.clone(), src.clone()),
    );
}

#[test]
fn resolved_on_ident() {
    let sym = "ident resolve".into();
    let kind = IdentKind::Meta;
    let src = Source {
        desc: Some("ident ctor".into()),
        ..Default::default()
    };

    assert_eq!(
        Ident::declare(SPair(sym, S1))
            .resolve(S2, kind.clone(), src.clone())
            .unwrap()
            .resolved()
            .unwrap(),
        &Ident::Ident(SPair(sym, S2), kind.clone(), src.clone()),
    );
}

// Note that we don't care about similar sources.  It's expected
// that the system populating the ASG will only resolve local
// symbols, and so redeclarations should represent that multiple
// packages have the same local symbol.
#[test]
fn ident_object_redeclare_same_src() {
    let sym = "redecl".into();
    let kind = IdentKind::Meta;
    let src = Source::default();

    let first = Ident::declare(SPair(sym, S1))
        .resolve(S2, kind.clone(), src.clone())
        .unwrap();

    // Resolve twice, as if we encountered two local symbols.
    assert_eq!(
        first.clone().resolve(S3, kind.clone(), src.clone()),
        Err((first, TransitionError::Redeclare(SPair(sym, S2), S3))),
    );
}

mod extern_ {
    use super::*;

    #[test]
    fn ident_object() {
        let sym = "extern".into();
        let kind = IdentKind::Class(Dim::Vector);
        let src = Source {
            desc: Some("extern".into()),
            ..Default::default()
        };

        assert_eq!(
            Ident::declare(SPair(sym, S1)).extern_(
                S2,
                kind.clone(),
                src.clone()
            ),
            Ok(Ident::Extern(SPair(sym, S2), kind, src)),
        );
    }

    #[test]
    fn resolved_on_extern() {
        let sym = "extern resolved".into();
        let kind = IdentKind::Class(Dim::Vector);
        let pkg_name: SymbolId = "pkg/name".intern();
        let src = Source {
            pkg_name: Some(pkg_name),
            desc: Some("extern".into()),
            ..Default::default()
        };

        assert_eq!(
            Ident::Extern(SPair(sym, S1), kind.clone(), src.clone()).resolved(),
            Err(UnresolvedError::Extern(SPair(sym, S1), kind)),
        );
    }

    // Extern first, then identifier
    #[test]
    fn redeclare_compatible_resolves() {
        let sym = "extern_re_pre".into();
        let kind = IdentKind::Class(Dim::Matrix);
        let src = Source {
            desc: Some("okay".into()),
            ..Default::default()
        };

        // Compatible kind, should resolve.
        let result = Ident::declare(SPair(sym, S1))
            .extern_(S2, kind.clone(), Source::default())
            .and_then(|o| o.resolve(S3, kind.clone(), src.clone()));

        assert_eq!(Ok(Ident::Ident(SPair(sym, S3), kind, src)), result);
    }

    // Identifier first, then extern
    #[test]
    fn redeclare_compatible_resolves_post() {
        let sym = "extern_re_post".into();
        let kind = IdentKind::Class(Dim::Vector);
        let src = Source {
            desc: Some("okay".into()),
            ..Default::default()
        };

        // Compatible kind, should resolve.
        let result = Ident::declare(SPair(sym, S1))
            .resolve(S2, kind.clone(), src.clone())
            .and_then(|o| o.extern_(S3, kind.clone(), Source::default()));

        // Note that we keep the span of the concrete identifier,
        //   despite the extern being encountered later.
        assert_eq!(Ok(Ident::Ident(SPair(sym, S2), kind, src)), result,);
    }

    #[test]
    fn redeclare_another_extern() {
        let sym = "extern_extern".into();
        let kind = IdentKind::Class(Dim::Scalar);
        let src_first = Source {
            desc: Some("first src".into()),
            ..Default::default()
        };
        let src_second = Source {
            desc: Some("second src".into()),
            ..Default::default()
        };

        let result = Ident::declare(SPair(sym, S1))
            .extern_(S2, kind.clone(), src_first.clone())
            .and_then(|o| o.extern_(S3, kind.clone(), src_second));

        // Note that, if it resolves, it should keep what is
        // _existing_, meaning that it must keep the first src.
        assert_eq!(Ok(Ident::Extern(SPair(sym, S2), kind, src_first)), result);
    }

    // Extern first, then identifier
    #[test]
    fn redeclare_post_incompatible_kind() {
        let sym = "extern_re_bad_post".into();
        let kind = IdentKind::Class(Dim::Matrix);
        let src = Source {
            desc: Some("bad kind".into()),
            ..Default::default()
        };

        let orig = Ident::declare(SPair(sym, S1))
            .extern_(S2, kind.clone(), Source::default())
            .unwrap();

        // Incompatible kind
        let kind_bad = IdentKind::Meta;
        assert_eq!(
            orig.clone().resolve(S3, kind_bad.clone(), src),
            Err((
                orig,
                TransitionError::ExternResolution(
                    SPair(sym, S2),
                    kind,
                    (kind_bad, S3)
                )
            )),
        );
    }

    // Identifier first, then extern
    #[test]
    fn redeclare_pre_incompatible_kind() {
        let sym = "extern_re_bad_pre".into();
        let kind_orig = IdentKind::Class(Dim::Vector);
        let src = Source {
            desc: Some("bad kind".into()),
            ..Default::default()
        };

        let orig = Ident::declare(SPair(sym, S1))
            .resolve(S2, kind_orig.clone(), src.clone())
            .unwrap();

        // Extern with incompatible kind.
        let kind_extern = IdentKind::Meta;

        assert_eq!(
            orig.clone()
                .extern_(S3, kind_extern.clone(), Source::default()),
            Err((
                orig,
                TransitionError::ExternResolution(
                    SPair(sym, S2),
                    kind_orig,
                    (kind_extern, S3)
                )
            )),
        );
    }
}

#[test]
fn add_fragment_to_ident() {
    let sym = "tofrag".into();
    let src = Source {
        generated: true,
        ..Default::default()
    };

    let kind = IdentKind::Meta;
    let ident = Ident::declare(SPair(sym, S1))
        .resolve(S2, kind.clone(), src.clone())
        .unwrap();
    let text = FragmentText::from("a fragment");

    assert_eq!(
        ident.set_fragment(text.clone()),
        Ok(Ident::IdentFragment(SPair(sym, S2), kind, src, text)),
    );
}

#[test]
fn resolved_on_fragment() {
    let sym = "tofrag resolved".into();
    let src = Source {
        generated: true,
        ..Default::default()
    };

    let kind = IdentKind::Meta;
    let ident = Ident::declare(SPair(sym, S1))
        .resolve(S2, kind.clone(), src.clone())
        .unwrap();
    let text = FragmentText::from("a fragment for resolved()");

    assert_eq!(
        ident.set_fragment(text.clone()).unwrap().resolved(),
        Ok(&Ident::IdentFragment(SPair(sym, S2), kind, src, text)),
    );
}

#[test]
fn add_fragment_to_fragment_fails() {
    let sym = "badsym".into();
    let ident = Ident::declare(SPair(sym, S1))
        .resolve(S2, IdentKind::Meta, Source::default())
        .unwrap();

    let ident_with_frag = ident
        .set_fragment("orig fragment".into())
        .expect("set_fragment failed");

    // Since it's already a fragment, this should fail.
    assert_eq!(
        ident_with_frag.clone().set_fragment("replacement".into()),
        Err((
            ident_with_frag,
            TransitionError::BadFragmentDest(SPair(sym, S2))
        )),
    );
}

mod override_ {
    use super::*;

    #[test]
    fn declare_virtual_ident_first() {
        let sym = "virtual".into();
        let over_src_name = "src".into();
        let kind = IdentKind::Meta;

        let virt = Ident::declare(SPair(sym, S1))
            .resolve(
                S2,
                kind.clone(),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let over_src = Source {
            virtual_: true, // this needn't be set, but see below
            override_: true,
            src: Some(over_src_name),
            ..Default::default()
        };

        let result = virt.resolve(S3, kind.clone(), over_src.clone());

        // Overriding should clear any virtual flag that may have
        // been set to prevent override-overrides.
        let expected_src = Source {
            virtual_: false,
            ..over_src
        };

        assert_eq!(
            Ok(Ident::Ident(SPair(sym, S3), kind, expected_src)),
            result
        );
    }

    // Override is encountered before the virtual
    #[test]
    fn declare_virtual_ident_after_override() {
        let sym = "virtual_second".into();
        let virt_src_name = "virt_src".into();
        let kind = IdentKind::Meta;

        let over_src = Source {
            virtual_: true, // this needn't be set, but see below
            override_: true,
            ..Default::default()
        };

        let over = Ident::declare(SPair(sym, S1))
            .resolve(S2, kind.clone(), over_src.clone())
            .unwrap();

        let virt_src = Source {
            virtual_: true,
            src: Some(virt_src_name),
            ..Default::default()
        };

        let result = over.resolve(S3, kind.clone(), virt_src.clone());

        // Overriding should clear any virtual flag that may have
        // been set to prevent override-overrides.  We should also
        // take the override source even though virtual was second.
        let expected_src = Source {
            virtual_: false,
            ..over_src
        };

        assert_eq!(
            Ok(Ident::Ident(SPair(sym, S3), kind, expected_src)),
            result
        );
    }

    #[test]
    fn declare_override_non_virtual() {
        let sym = "non_virtual".into();
        let kind = IdentKind::Meta;

        let non_virt = Ident::declare(SPair(sym, S1))
            .resolve(
                S2,
                kind.clone(),
                Source {
                    virtual_: false,
                    ..Default::default()
                },
            )
            .unwrap();

        let over_src = Source {
            override_: true,
            ..Default::default()
        };

        // This isn't the purpose of the test, but we want to make
        // sure that the non-virtual override error occurs before
        // the kind error.
        let bad_kind = IdentKind::Cgen(Dim::Vector);

        assert_eq!(
            non_virt.clone().resolve(S3, bad_kind, over_src.clone()),
            Err((
                non_virt,
                TransitionError::NonVirtualOverride(SPair(sym, S2), S3)
            ))
        );
    }

    #[test]
    fn declare_virtual_ident_incompatible_kind() {
        let sym = "virtual".into();
        let src_sym: SymbolId = "src".into();
        let kind = IdentKind::Meta;

        let virt = Ident::declare(SPair(sym, S1))
            .resolve(
                S2,
                kind.clone(),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let over_src = Source {
            override_: true,
            src: Some(src_sym),
            ..Default::default()
        };

        let bad_kind = IdentKind::Cgen(Dim::Vector);

        assert_eq!(
            virt.clone().resolve(S3, bad_kind.clone(), over_src.clone()),
            Err((
                virt,
                TransitionError::VirtualOverrideKind(
                    SPair(sym, S2),
                    kind,
                    (bad_kind, S3)
                )
            )),
        );
    }

    // Encounter virtual first and override second should cause the
    // fragment to be cleared to make way for the new fragment.
    #[test]
    fn declare_override_virtual_ident_fragment_virtual_first() {
        let sym = "virtual".into();
        let over_src = "src".into();
        let kind = IdentKind::Meta;

        // Remember: override is going to come first...
        let over_src = Source {
            override_: true,
            src: Some(over_src),
            ..Default::default()
        };

        // ...and virt second.
        let virt_src = Source {
            virtual_: true,
            ..Default::default()
        };

        let over = Ident::declare(SPair(sym, S1))
            .resolve(S2, kind.clone(), over_src.clone())
            .unwrap();

        // So we should _keep_ this fragment, since it represent the
        // override, even though it's appearing first.
        let text = FragmentText::from("keep me");
        let over_frag = over.set_fragment(text.clone());

        assert_eq!(
            over_frag,
            Ok(Ident::IdentFragment(
                SPair(sym, S2),
                kind.clone(),
                over_src.clone(),
                text.clone(),
            )),
        );

        // Overriding should _not_ have cleared the fragment since
        // the override was encountered _first_, so we want to keep
        // its fragment.
        let result =
            over_frag
                .unwrap()
                .resolve(S3, kind.clone(), virt_src.clone());
        assert_eq!(
            result,
            Ok(Ident::IdentFragment(
                SPair(sym, S3),
                kind.clone(),
                over_src.clone(),
                text.clone()
            )),
        );

        // Finally, after performing this transition, we will
        // inevitably encounter the fragment for the virtual
        // identifier, which we must ignore.  So we must make sure
        // that encountering it will not cause an error, because we
        // still have an IdentFragment at this point.
        assert_eq!(
            Ok(Ident::IdentFragment(
                SPair(sym, S3),
                kind,
                over_src.clone(),
                text.clone()
            )),
            result.unwrap().set_fragment("virt fragment".into()),
        );
    }

    // Encountering _override_ first and virtual second should _not_
    // clear the fragment, otherwise the virtual fragment will take
    // precedence over the override.
    #[test]
    fn declare_override_virtual_ident_fragment_override_first() {
        let sym = "virtual".into();
        let over_src = "src".into();
        let kind = IdentKind::Meta;

        let virt_src = Source {
            virtual_: true,
            ..Default::default()
        };

        let virt = Ident::declare(SPair(sym, S1))
            .resolve(S2, kind.clone(), virt_src.clone())
            .unwrap();
        let text = FragmentText::from("remove me");
        let virt_frag = virt.set_fragment(text.clone());

        assert_eq!(
            virt_frag,
            Ok(Ident::IdentFragment(
                SPair(sym, S2),
                kind.clone(),
                virt_src,
                text
            )),
        );

        let over_src = Source {
            override_: true,
            src: Some(over_src),
            ..Default::default()
        };

        assert_eq!(
            virt_frag
                .unwrap()
                .resolve(S3, kind.clone(), over_src.clone()),
            // The act of overriding the object should have cleared any
            // existing fragment, making way for a new fragment to take its
            // place as soon as it is discovered.  (So, back to an
            // Ident::Ident.)
            Ok(Ident::Ident(SPair(sym, S3), kind, over_src)),
        );
    }

    #[test]
    fn declare_override_virtual_ident_fragment_incompatible_type() {
        let sym = "virtual".into();
        let over_src = "src".into();
        let kind = IdentKind::Meta;

        let virt_src = Source {
            virtual_: true,
            ..Default::default()
        };

        let virt = Ident::declare(SPair(sym, S1))
            .resolve(S2, kind.clone(), virt_src.clone())
            .unwrap();
        let virt_frag = virt.set_fragment("".into()).unwrap();

        let over_src = Source {
            override_: true,
            src: Some(over_src),
            ..Default::default()
        };

        let bad_kind = IdentKind::Cgen(Dim::Vector);
        assert_eq!(
            virt_frag
                .clone()
                .resolve(S3, bad_kind.clone(), over_src.clone()),
            Err((
                virt_frag,
                TransitionError::VirtualOverrideKind(
                    SPair(sym, S2),
                    kind,
                    (bad_kind, S3)
                )
            )),
        );
    }
}

fn add_ident_kind_ignores(given: IdentKind, expected: IdentKind) {
    let sym = "tofrag".into();
    let src = Source {
        generated: true,
        ..Default::default()
    };

    let obj = Ident::declare(SPair(sym, S1))
        .resolve(S2, given, src.clone())
        .unwrap();

    let fragment = "a fragment".intern();

    assert_eq!(
        obj.set_fragment(fragment),
        Ok(Ident::IdentFragment(
            SPair(sym, S2),
            expected,
            src,
            fragment
        )),
    );
}

#[test]
fn add_fragment_to_ident_map_head() {
    add_ident_kind_ignores(IdentKind::MapHead, IdentKind::MapHead)
}

#[test]
fn add_fragment_to_ident_map_tail() {
    add_ident_kind_ignores(IdentKind::MapTail, IdentKind::MapTail)
}

#[test]
fn add_fragment_to_ident_retmap_head() {
    add_ident_kind_ignores(IdentKind::RetMapHead, IdentKind::RetMapHead)
}

#[test]
fn add_fragment_to_ident_retmap_tail() {
    add_ident_kind_ignores(IdentKind::RetMapTail, IdentKind::RetMapTail)
}
