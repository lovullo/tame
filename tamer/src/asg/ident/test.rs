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
use crate::num::Dim;
use crate::sym::{GlobalSymbolIntern, SymbolId};

// Note that Ident has no variants capable of None
#[test]
fn ident_object_name() {
    let sym: SymbolId = "sym".intern();

    assert_eq!(sym, Ident::Missing(sym).name());

    assert_eq!(
        sym,
        Ident::Ident(sym, IdentKind::Meta, Source::default()).name()
    );

    assert_eq!(
        sym,
        Ident::Extern(sym, IdentKind::Meta, Source::default()).name()
    );

    assert_eq!(
        sym,
        Ident::IdentFragment(
            sym,
            IdentKind::Meta,
            Source::default(),
            "".intern()
        )
        .name()
    );
}

#[test]
fn ident_object_kind() {
    let sym: SymbolId = "sym".intern();
    let kind = IdentKind::Class(Dim::Matrix);

    assert_eq!(None, Ident::Missing(sym).kind());

    assert_eq!(
        Some(&kind),
        Ident::Ident(sym, kind.clone(), Source::default()).kind()
    );

    assert_eq!(
        Some(&kind),
        Ident::Extern(sym, kind.clone(), Source::default()).kind()
    );

    assert_eq!(
        Some(&kind),
        Ident::IdentFragment(sym, kind.clone(), Source::default(), "".intern())
            .kind()
    );
}

#[test]
fn ident_object_src() {
    let sym: SymbolId = "sym".intern();
    let src = Source {
        desc: Some("test source".into()),
        ..Default::default()
    };

    assert_eq!(None, Ident::Missing(sym).src());

    assert_eq!(
        Some(&src),
        Ident::Ident(sym, IdentKind::Meta, src.clone()).src()
    );

    assert_eq!(None, Ident::Extern(sym, IdentKind::Meta, src.clone()).src());

    assert_eq!(
        Some(&src),
        Ident::IdentFragment(sym, IdentKind::Meta, src.clone(), "".intern())
            .src()
    );
}

#[test]
fn ident_object_fragment() {
    let sym: SymbolId = "sym".intern();
    let text = "foo".into();

    assert_eq!(None, Ident::Missing(sym).fragment());

    assert_eq!(
        None,
        Ident::Ident(sym, IdentKind::Meta, Source::default()).fragment()
    );

    assert_eq!(
        None,
        Ident::Extern(sym, IdentKind::Meta, Source::default()).fragment()
    );

    assert_eq!(
        Some(text),
        Ident::IdentFragment(sym, IdentKind::Meta, Source::default(), text,)
            .fragment()
    );
}

#[test]
fn ident_object_missing() {
    let sym: SymbolId = "missing".intern();
    assert_eq!(Ident::Missing(sym), Ident::declare(sym));
}

#[test]
fn resolved_on_missing() {
    let sym: SymbolId = "missing".intern();

    let result = Ident::declare(sym)
        .resolved()
        .expect_err("expected error asserting resolved() on missing");

    match result {
        UnresolvedError::Missing { name: e_name } => {
            assert_eq!(sym, e_name);
        }
        _ => panic!("expected UnresolvedError {:?}", result),
    }
}

#[test]
fn ident_object_ident() {
    let sym: SymbolId = "ident".intern();
    let kind = IdentKind::Meta;
    let src = Source {
        desc: Some("ident ctor".into()),
        ..Default::default()
    };

    assert_eq!(
        Ident::Ident(sym, kind.clone(), src.clone()),
        Ident::declare(sym)
            .resolve(kind.clone(), src.clone())
            .unwrap(),
    );
}

#[test]
fn resolved_on_ident() {
    let sym: SymbolId = "ident resolve".intern();
    let kind = IdentKind::Meta;
    let src = Source {
        desc: Some("ident ctor".into()),
        ..Default::default()
    };

    assert_eq!(
        &Ident::Ident(sym, kind.clone(), src.clone()),
        Ident::declare(sym)
            .resolve(kind.clone(), src.clone())
            .unwrap()
            .resolved()
            .unwrap(),
    );
}

// Note that we don't care about similar sources.  It's expected
// that the system populating the ASG will only resolve local
// symbols, and so redeclarations should represent that multiple
// packages have the same local symbol.
#[test]
fn ident_object_redeclare_same_src() {
    let sym: SymbolId = "redecl".intern();
    let kind = IdentKind::Meta;
    let src = Source::default();

    let first = Ident::declare(sym)
        .resolve(kind.clone(), src.clone())
        .unwrap();

    // Resolve twice, as if we encountered two local symbols.
    let result = first
        .clone()
        .resolve(kind.clone(), src.clone())
        .expect_err("expected error redeclaring identifier");

    match result {
        (orig, TransitionError::Redeclare { name }) => {
            assert_eq!(first, orig);
            assert_eq!(sym, name);
        }
        _ => {
            panic!("expected TransitionError::Redeclare: {:?}", result)
        }
    }
}

mod extern_ {
    use super::*;

    #[test]
    fn ident_object() {
        let sym: SymbolId = "extern".intern();
        let kind = IdentKind::Class(Dim::Vector);
        let src = Source {
            desc: Some("extern".into()),
            ..Default::default()
        };

        assert_eq!(
            Ok(Ident::Extern(sym, kind.clone(), src.clone())),
            Ident::declare(sym).extern_(kind, src),
        );
    }

    #[test]
    fn resolved_on_extern() {
        let sym: SymbolId = "extern resolved".intern();
        let kind = IdentKind::Class(Dim::Vector);
        let pkg_name: SymbolId = "pkg/name".intern();
        let src = Source {
            pkg_name: Some(pkg_name),
            desc: Some("extern".into()),
            ..Default::default()
        };

        let result = Ident::Extern(sym, kind.clone(), src.clone())
            .resolved()
            .expect_err("expected error asserting resolved() on extern");

        match result {
            UnresolvedError::Extern {
                name: e_name,
                kind: e_kind,
                pkg_name: e_pkg_name,
            } => {
                assert_eq!(sym, e_name);
                assert_eq!(kind, e_kind);
                assert_eq!(Some(pkg_name), e_pkg_name);
            }
            _ => panic!("expected UnresolvedError: {:?}", result),
        }
    }

    #[test]
    fn resolved_on_extern_error_fmt_without_pkg() {
        let meta = IdentKind::Meta;
        let err = UnresolvedError::Extern {
            name: "foo".into(),
            kind: IdentKind::Meta,
            pkg_name: None,
        };

        let msg = format!("{}", err);

        assert!(msg.contains("`foo`"));
        assert!(msg.contains("in `<unknown>`"));
        assert!(msg.contains(&format!("`{}`", meta)));
    }

    #[test]
    fn resolved_on_extern_error_fmt_with_pkg() {
        let meta = IdentKind::Meta;
        let pkg = "pkg".into();

        let err = UnresolvedError::Extern {
            name: "foo".into(),
            kind: IdentKind::Meta,
            pkg_name: Some(pkg),
        };

        let msg = format!("{}", err);

        assert!(msg.contains("`foo`"));
        assert!(msg.contains(&format!("in `{}`", pkg)));
        assert!(msg.contains(&format!("`{}`", meta)));
    }

    // Extern first, then identifier
    #[test]
    fn redeclare_compatible_resolves() {
        let sym: SymbolId = "extern_re_pre".intern();
        let kind = IdentKind::Class(Dim::Matrix);
        let src = Source {
            desc: Some("okay".into()),
            ..Default::default()
        };

        // Compatible kind, should resolve.
        let result = Ident::declare(sym)
            .extern_(kind.clone(), Source::default())
            .and_then(|o| o.resolve(kind.clone(), src.clone()));

        assert_eq!(Ok(Ident::Ident(sym, kind, src)), result,);
    }

    // Identifier first, then extern
    #[test]
    fn redeclare_compatible_resolves_post() {
        let sym: SymbolId = "extern_re_post".intern();
        let kind = IdentKind::Class(Dim::Vector);
        let src = Source {
            desc: Some("okay".into()),
            ..Default::default()
        };

        // Compatible kind, should resolve.
        let result = Ident::declare(sym)
            .resolve(kind.clone(), src.clone())
            .and_then(|o| o.extern_(kind.clone(), Source::default()));

        assert_eq!(Ok(Ident::Ident(sym, kind, src)), result,);
    }

    #[test]
    fn redeclare_another_extern() {
        let sym: SymbolId = "extern_extern".intern();
        let kind = IdentKind::Class(Dim::Scalar);
        let src_first = Source {
            desc: Some("first src".into()),
            ..Default::default()
        };
        let src_second = Source {
            desc: Some("second src".into()),
            ..Default::default()
        };

        let result = Ident::declare(sym)
            .extern_(kind.clone(), src_first.clone())
            .and_then(|o| o.extern_(kind.clone(), src_second));

        // Note that, if it resolves, it should keep what is
        // _existing_, meaning that it must keep the first src.
        assert_eq!(Ok(Ident::Extern(sym, kind, src_first)), result);
    }

    // Extern first, then identifier
    #[test]
    fn redeclare_post_incompatible_kind() {
        let sym: SymbolId = "extern_re_bad_post".intern();
        let kind = IdentKind::Class(Dim::Matrix);
        let src = Source {
            desc: Some("bad kind".into()),
            ..Default::default()
        };

        let orig = Ident::declare(sym)
            .extern_(kind.clone(), Source::default())
            .unwrap();

        // Incompatible kind
        let kind_bad = IdentKind::Meta;
        let result = orig.clone().resolve(kind_bad.clone(), src);

        match result {
            Err((given_orig, err @ _)) => {
                assert_eq!(orig, given_orig);

                if let TransitionError::ExternResolution {
                    name: e_name,
                    expected: e_expected,
                    given: e_given,
                } = err.clone()
                {
                    assert_eq!(sym, e_name);
                    assert_eq!(kind, e_expected);
                    assert_eq!(kind_bad, e_given);
                }

                // Formatted error
                let msg = format!("{}", err);

                assert!(msg.contains(&format!("{}", sym)));
                assert!(msg.contains(&format!("{}", kind)));
                assert!(msg.contains(&format!("{}", kind_bad)));
            }
            _ => panic!("expected failure: {:?}", result),
        }
    }

    // Identifier first, then extern
    #[test]
    fn redeclare_pre_incompatible_kind() {
        let sym: SymbolId = "extern_re_bad_pre".intern();
        let kind_given = IdentKind::Class(Dim::Vector);
        let src = Source {
            desc: Some("bad kind".into()),
            ..Default::default()
        };

        let orig = Ident::declare(sym)
            .resolve(kind_given.clone(), src.clone())
            .unwrap();

        // Extern with incompatible kind.
        let kind_extern = IdentKind::Meta;
        let result =
            orig.clone().extern_(kind_extern.clone(), Source::default());

        match result {
            Err((given_orig, err @ _)) => {
                assert_eq!(orig, given_orig);

                if let TransitionError::ExternResolution {
                    name: e_name,
                    expected: e_expected,
                    given: e_given,
                } = err.clone()
                {
                    assert_eq!(sym, e_name);
                    assert_eq!(kind_extern, e_expected);
                    assert_eq!(kind_given, e_given);
                }

                // Formatted error
                let msg = format!("{}", err);

                assert!(msg.contains(&format!("{}", sym)));
                assert!(msg.contains(&format!("{}", kind_extern)));
                assert!(msg.contains(&format!("{}", kind_given)));
            }
            _ => panic!("expected failure: {:?}", result),
        }
    }
}

#[test]
fn add_fragment_to_ident() {
    let sym: SymbolId = "tofrag".intern();
    let src = Source {
        generated: true,
        ..Default::default()
    };

    let kind = IdentKind::Meta;
    let ident = Ident::declare(sym)
        .resolve(kind.clone(), src.clone())
        .unwrap();
    let text = FragmentText::from("a fragment");
    let ident_with_frag = ident.set_fragment(text.clone());

    assert_eq!(
        Ok(Ident::IdentFragment(sym, kind, src, text)),
        ident_with_frag,
    );
}

#[test]
fn resolved_on_fragment() {
    let sym: SymbolId = "tofrag resolved".intern();
    let src = Source {
        generated: true,
        ..Default::default()
    };

    let kind = IdentKind::Meta;
    let ident = Ident::declare(sym)
        .resolve(kind.clone(), src.clone())
        .unwrap();
    let text = FragmentText::from("a fragment for resolved()");
    let ident_with_frag = ident.set_fragment(text.clone());

    assert_eq!(
        Ok(&Ident::IdentFragment(sym, kind, src, text)),
        ident_with_frag.unwrap().resolved(),
    );
}

#[test]
fn add_fragment_to_fragment_fails() {
    let sym: SymbolId = "badsym".intern();
    let ident = Ident::declare(sym)
        .resolve(IdentKind::Meta, Source::default())
        .unwrap();

    let ident_with_frag = ident
        .set_fragment("orig fragment".into())
        .expect("set_fragment failed");

    // Since it's already a fragment, this should fail.
    let err = ident_with_frag
        .clone()
        .set_fragment("replacement".intern())
        .expect_err("Expected failure");

    match err {
        (orig, TransitionError::BadFragmentDest { .. }) => {
            assert_eq!(ident_with_frag, orig);
        }
        _ => panic!("expected TransitionError::BadFragmentDest: {:?}", err),
    }
}

mod override_ {
    use super::*;

    #[test]
    fn declare_virtual_ident_first() {
        let sym: SymbolId = "virtual".intern();
        let over_src = "src".intern();
        let kind = IdentKind::Meta;

        let virt = Ident::declare(sym)
            .resolve(
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
            src: Some(over_src),
            ..Default::default()
        };

        let result = virt.resolve(kind.clone(), over_src.clone());

        // Overriding should clear any virtual flag that may have
        // been set to prevent override-overrides.
        let expected_src = Source {
            virtual_: false,
            ..over_src
        };

        assert_eq!(Ok(Ident::Ident(sym, kind, expected_src)), result);
    }

    // Override is encountered before the virtual
    #[test]
    fn declare_virtual_ident_after_override() {
        let sym: SymbolId = "virtual_second".intern();
        let virt_src = "virt_src".intern();
        let kind = IdentKind::Meta;

        let over_src = Source {
            virtual_: true, // this needn't be set, but see below
            override_: true,
            ..Default::default()
        };

        let over = Ident::declare(sym)
            .resolve(kind.clone(), over_src.clone())
            .unwrap();

        let virt_src = Source {
            virtual_: true,
            src: Some(virt_src),
            ..Default::default()
        };

        let result = over.resolve(kind.clone(), virt_src.clone());

        // Overriding should clear any virtual flag that may have
        // been set to prevent override-overrides.  We should also
        // take the override source even though virtual was second.
        let expected_src = Source {
            virtual_: false,
            ..over_src
        };

        assert_eq!(Ok(Ident::Ident(sym, kind, expected_src)), result);
    }

    #[test]
    fn declare_override_non_virtual() {
        let sym: SymbolId = "non_virtual".intern();
        let kind = IdentKind::Meta;

        let non_virt = Ident::declare(sym)
            .resolve(
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

        let result = non_virt
            .clone()
            .resolve(bad_kind, over_src.clone())
            .expect_err("expected error");

        match result {
            (ref orig, TransitionError::NonVirtualOverride { ref name }) => {
                assert_eq!(orig, &non_virt);
                assert_eq!(sym, *name);

                // Formatted error
                let (_, err) = result;
                let msg = format!("{}", err);

                assert!(msg.contains(&format!("{}", sym)));
            }
            (_, TransitionError::VirtualOverrideKind { .. }) => {
                panic!("kind check must happen _after_ virtual check")
            }
            _ => panic!(
                "expected TransitionError::VirtualOverrideKind {:?}",
                result
            ),
        }
    }

    #[test]
    fn declare_virtual_ident_incompatible_kind() {
        let sym: SymbolId = "virtual".intern();
        let src_sym: SymbolId = "src".intern();
        let kind = IdentKind::Meta;

        let virt = Ident::declare(sym)
            .resolve(
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
        let result = virt
            .clone()
            .resolve(bad_kind.clone(), over_src.clone())
            .expect_err("expected error");

        match result {
            (
                ref orig,
                TransitionError::VirtualOverrideKind {
                    ref name,
                    ref existing,
                    ref given,
                },
            ) => {
                assert_eq!(orig, &virt);

                assert_eq!(sym, *name);
                assert_eq!(&kind, existing);
                assert_eq!(&bad_kind, given);

                // Formatted error
                let (_, err) = result;
                let msg = format!("{}", err);

                assert!(msg.contains(&format!("{}", sym)));
                assert!(msg.contains(&format!("{}", kind)));
                assert!(msg.contains(&format!("{}", bad_kind)));
            }
            _ => panic!(
                "expected TransitionError::VirtualOverrideKind {:?}",
                result
            ),
        }
    }

    // Encounter virtual first and override second should cause the
    // fragment to be cleared to make way for the new fragment.
    #[test]
    fn declare_override_virtual_ident_fragment_virtual_first() {
        let sym: SymbolId = "virtual".intern();
        let over_src = "src".intern();
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

        let over = Ident::declare(sym)
            .resolve(kind.clone(), over_src.clone())
            .unwrap();

        // So we should _keep_ this fragment, since it represent the
        // override, even though it's appearing first.
        let text = FragmentText::from("keep me");
        let over_frag = over.set_fragment(text.clone());

        assert_eq!(
            Ok(Ident::IdentFragment(
                sym,
                kind.clone(),
                over_src.clone(),
                text.clone(),
            )),
            over_frag,
        );

        let result = over_frag.unwrap().resolve(kind.clone(), virt_src.clone());

        // Overriding should _not_ have cleared the fragment since
        // the override was encountered _first_, so we want to keep
        // its fragment.
        assert_eq!(
            Ok(Ident::IdentFragment(
                sym,
                kind.clone(),
                over_src.clone(),
                text.clone()
            )),
            result
        );

        // Finally, after performing this transition, we will
        // inevitably encounter the fragment for the virtual
        // identifier, which we must ignore.  So we must make sure
        // that encountering it will not cause an error, because we
        // still have an IdentFragment at this point.
        assert_eq!(
            Ok(Ident::IdentFragment(
                sym,
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
        let sym: SymbolId = "virtual".intern();
        let over_src = "src".intern();
        let kind = IdentKind::Meta;

        let virt_src = Source {
            virtual_: true,
            ..Default::default()
        };

        let virt = Ident::declare(sym)
            .resolve(kind.clone(), virt_src.clone())
            .unwrap();
        let text = FragmentText::from("remove me");
        let virt_frag = virt.set_fragment(text.clone());

        assert_eq!(
            Ok(Ident::IdentFragment(sym, kind.clone(), virt_src, text)),
            virt_frag,
        );

        let over_src = Source {
            override_: true,
            src: Some(over_src),
            ..Default::default()
        };

        let result = virt_frag.unwrap().resolve(kind.clone(), over_src.clone());

        // The act of overriding the object should have cleared any
        // existing fragment, making way for a new fragment to take its
        // place as soon as it is discovered.  (So, back to an
        // Ident::Ident.)
        assert_eq!(Ok(Ident::Ident(sym, kind, over_src)), result);
    }

    #[test]
    fn declare_override_virtual_ident_fragment_incompatible_type() {
        let sym: SymbolId = "virtual".intern();
        let over_src = "src".intern();
        let kind = IdentKind::Meta;

        let virt_src = Source {
            virtual_: true,
            ..Default::default()
        };

        let virt = Ident::declare(sym)
            .resolve(kind.clone(), virt_src.clone())
            .unwrap();
        let virt_frag = virt.set_fragment("".into()).unwrap();

        let over_src = Source {
            override_: true,
            src: Some(over_src),
            ..Default::default()
        };

        let bad_kind = IdentKind::Cgen(Dim::Vector);
        let result = virt_frag
            .clone()
            .resolve(bad_kind.clone(), over_src.clone())
            .expect_err("expected error");

        match result {
            (
                ref orig,
                TransitionError::VirtualOverrideKind {
                    ref name,
                    ref existing,
                    ref given,
                },
            ) => {
                assert_eq!(orig, &virt_frag);

                assert_eq!(sym, *name);
                assert_eq!(&kind, existing);
                assert_eq!(&bad_kind, given);

                // Formatted error
                let (_, err) = result;
                let msg = format!("{}", err);

                assert!(msg.contains(&format!("{}", sym)));
                assert!(msg.contains(&format!("{}", kind)));
                assert!(msg.contains(&format!("{}", bad_kind)));
            }
            _ => panic!(
                "expected TransitionError::VirtualOverrideKind {:?}",
                result
            ),
        }
    }
}

fn add_ident_kind_ignores(given: IdentKind, expected: IdentKind) {
    let sym: SymbolId = "tofrag".intern();
    let src = Source {
        generated: true,
        ..Default::default()
    };

    let obj = Ident::declare(sym).resolve(given, src.clone()).unwrap();

    let fragment = "a fragment".intern();
    let obj_with_frag = obj.set_fragment(fragment);

    assert_eq!(
        Ok(Ident::IdentFragment(sym, expected, src, fragment)),
        obj_with_frag,
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
