#![allow(non_upper_case_globals)]

use std::ops::{BitAnd, BitOr, Not, RangeFrom, RangeTo, Sub};
use crate::term::EitherTerms;
use super::*;

/// A trait to represent a match sequence, a hand (rhs) of production definition.
/// It is also used to describe a single "alternative" in production definition.
//pub trait Sequence {}

/// Describe a single match, can be a single term match, or a group, another production,
/// and special predicates/directives controlling match sequence.
pub trait Match {
    /// consumes match (if owned) and returns a capture
    fn capture(self) -> MatchCapture;
}

/// `&` operator provides a single alternative definition for a production
impl<P: Node, M: Match> BitAnd<M> for &'static Production<P> {
    // This & operator should take only one definition
    type Output = ();

    fn bitand(self, rhs: M) -> Self::Output {
        let capture = rhs.capture();
        GrammarSetup::add_production_hand(self.name, self as &'static dyn CreateNode, capture);
    }
}

impl<P: Node, M: Match> BitOr<M> for &'static Production<P> {
    type Output = Self;

    fn bitor(self, rhs: M) -> Self::Output {
        let create = self as &'static dyn CreateNode;
        GrammarSetup::add_production_hand(self.name, create, rhs.capture());
        self
    }
}

/// Puts an end to alternatives
impl<P: Node> BitOr<()> for &'static Production<P> {
    type Output = ();

    fn bitor(self, _: ()) -> Self::Output {}
}

/// `&` operator provides a single alternative definition for a group
impl<M: Match> BitAnd<M> for &'static Group {
    // This & operator should take only one definition
    type Output = ();

    fn bitand(self, rhs: M) -> Self::Output {
        GrammarSetup::add_group_hand(self.name, rhs.capture());
    }
}

impl<M: Match> BitOr<M> for &'static Group {
    type Output = Self;

    fn bitor(self, rhs: M) -> Self::Output {
        GrammarSetup::add_group_hand(self.name, rhs.capture());
        self
    }
}

impl BitOr<()> for &'static Group {
    type Output = ();

    fn bitor(self, _: ()) -> Self::Output {}
}

impl<P: Node> BitOr<&'static Production<P>> for &'static Choice<P> {
    // This & operator should take only one definition
    type Output = Self;

    fn bitor(self, rhs: &'static Production<P>) -> Self::Output {
        GrammarSetup::add_choice_hand(self.name, rhs.capture());
        self
    }
}

impl<P: Node, F: Node> BitOr<TransformedProd<P, F>> for &'static Choice<P> {
    // This & operator should take only one definition
    type Output = Self;

    fn bitor(self, rhs: TransformedProd<P, F>) -> Self::Output {
        GrammarSetup::add_choice_hand(self.name, rhs.capture());
        self
    }
}

impl<P: Node, F: Node> BitOr<TransformedChoice<P, F>> for &'static Choice<P> {
    // This & operator should take only one definition
    type Output = Self;

    fn bitor(self, rhs: TransformedChoice<P, F>) -> Self::Output {
        GrammarSetup::add_choice_hand(self.name, rhs.capture());
        self
    }
}

impl<P: Node> BitOr<()> for &'static Choice<P> {
    type Output = ();

    fn bitor(self, _: ()) -> Self::Output {}
}

impl<P: Node> Match for &'static Production<P> {
    fn capture(self) -> MatchCapture {
        MatchCapture::Production {
            quant: Default::default(),
            name: self.name,
            part: None,
            transform: None,
        }
    }
}

impl Match for &'static Group {
    fn capture(self) -> MatchCapture {
        MatchCapture::Group {
            quant: Default::default(),
            name: self.name,
        }
    }
}

impl<P: Node> Match for &'static Choice<P> {
    fn capture(self) -> MatchCapture {
        MatchCapture::Production {
            quant: Default::default(),
            name: self.name,
            part: None,
            transform: None,
        }
    }
}

impl<M: Match> Match for (M,) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![self.0.capture()]
        }
    }
}

impl<M0: Match, M1: Match> Match for (M0, M1) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![self.0.capture(), self.1.capture()]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match> Match for (M0, M1, M2) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![self.0.capture(), self.1.capture(), self.2.capture()]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match> Match for (M0, M1, M2, M3) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture()]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match> Match for (M0, M1, M2, M3, M4) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture()
            ]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match, M5: Match>
Match for (M0, M1, M2, M3, M4, M5) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture(), self.5.capture()
            ]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match, M5: Match, M6: Match>
Match for (M0, M1, M2, M3, M4, M5, M6) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture(), self.5.capture(), self.6.capture()
            ]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match, M5: Match, M6: Match, M7: Match>
Match for (M0, M1, M2, M3, M4, M5, M6, M7) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture(), self.5.capture(), self.6.capture(), self.7.capture()
            ]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match, M5: Match, M6: Match, M7: Match, M8: Match>
Match for (M0, M1, M2, M3, M4, M5, M6, M7, M8) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture(), self.5.capture(), self.6.capture(), self.7.capture(),
                self.8.capture()
            ]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match, M5: Match, M6: Match, M7: Match, M8: Match, M9: Match>
Match for (M0, M1, M2, M3, M4, M5, M6, M7, M8, M9) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture(), self.5.capture(), self.6.capture(), self.7.capture(),
                self.8.capture(), self.9.capture()
            ]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match, M5: Match, M6: Match, M7: Match, M8: Match, M9: Match, M10: Match>
Match for (M0, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture(), self.5.capture(), self.6.capture(), self.7.capture(),
                self.8.capture(), self.9.capture(), self.10.capture()
            ]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match, M5: Match, M6: Match, M7: Match, M8: Match, M9: Match, M10: Match, M11: Match>
Match for (M0, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture(), self.5.capture(), self.6.capture(), self.7.capture(),
                self.8.capture(), self.9.capture(), self.10.capture(), self.11.capture()
            ]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match, M5: Match, M6: Match, M7: Match, M8: Match, M9: Match, M10: Match, M11: Match, M12: Match>
Match for (M0, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture(), self.5.capture(), self.6.capture(), self.7.capture(),
                self.8.capture(), self.9.capture(), self.10.capture(), self.11.capture(),
                self.12.capture()
            ]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match, M5: Match, M6: Match, M7: Match, M8: Match, M9: Match, M10: Match, M11: Match, M12: Match, M13: Match>
Match for (M0, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture(), self.5.capture(), self.6.capture(), self.7.capture(),
                self.8.capture(), self.9.capture(), self.10.capture(), self.11.capture(),
                self.12.capture(), self.13.capture()
            ]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match, M5: Match, M6: Match, M7: Match, M8: Match, M9: Match, M10: Match, M11: Match, M12: Match, M13: Match, M14: Match>
Match for (M0, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture(), self.5.capture(), self.6.capture(), self.7.capture(),
                self.8.capture(), self.9.capture(), self.10.capture(), self.11.capture(),
                self.12.capture(), self.13.capture(), self.14.capture()
            ]
        }
    }
}

impl<M0: Match, M1: Match, M2: Match, M3: Match, M4: Match, M5: Match, M6: Match, M7: Match, M8: Match, M9: Match, M10: Match, M11: Match, M12: Match, M13: Match, M14: Match, M15: Match>
Match for (M0, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15) {
    fn capture(self) -> MatchCapture {
        MatchCapture::Sequence {
            quant: Quant::default(),
            sequence: vec![
                self.0.capture(), self.1.capture(), self.2.capture(), self.3.capture(),
                self.4.capture(), self.5.capture(), self.6.capture(), self.7.capture(),
                self.8.capture(), self.9.capture(), self.10.capture(), self.11.capture(),
                self.12.capture(), self.13.capture(), self.14.capture(), self.15.capture()
            ]
        }
    }
}

impl<M: Match> Match for [M; 1] {
    fn capture(self) -> MatchCapture {
        let [item] = self;
        let mut c = item.capture();
        let Some(quant) = c.quant_mut() else {
            panic!("{c:?} is incompatible with optional brackets []")
        };
        if quant.not {
            panic!("{c:?} is incompatible with optional brackets []: cannot 'not match' optionally")
        }
        quant.opt = true;
        c
    }
}

impl<T: EitherTerms> Match for T {
    fn capture(self) -> MatchCapture {
        let (iter, not) = self.iter_and_not();

        let quant = if not { Quant::not() } else { Quant::default() };
        let mut v = iter.collect::<Vec<_>>();

        if v.len() == 0 { panic!("empty either terms not allowed") }
        if v.len() == 1 {
            let term: Box<dyn Any> = Box::new(v.pop().unwrap());

            MatchCapture::Term {
                quant,
                term,
            }
        } else {
            let terms = v.into_iter().map(|t| {
                let term: Box<dyn Any> = Box::new(t);
                term
            }).collect::<Vec<_>>();

            MatchCapture::TermOneOf {
                quant,
                terms,
            }
        }
    }
}

impl Match for &'static str {
    fn capture(self) -> MatchCapture {
        let term: Box<dyn Any> = Box::new(self);
        MatchCapture::Term {
            quant: Default::default(),
            term,
        }
    }
}

impl<M: Match> Match for RangeFrom<M> {
    fn capture(self) -> MatchCapture {
        let mut c = self.start.capture();
        let Some(quant) = c.quant_mut() else {
            panic!("{c:?} is incompatible with .. many")
        };
        if quant.not {
            panic!("{c:?} is incompatible with .. many: cannot 'not match' many")
        }
        quant.many = true;
        c
    }
}

impl<M: Match> Match for RangeTo<M> {
    fn capture(self) -> MatchCapture {
        let mut c = self.end.capture();
        let Some(quant) = c.quant_mut() else {
            panic!("{c:?} is incompatible with .. many")
        };
        if quant.not {
            panic!("{c:?} is incompatible with .. many: cannot 'not match' many")
        }
        quant.many = true;
        c
    }
}

/// Grammar marker unit that can be used as an end of hands/alternatives using '|'.
/// Can be used as "keyword" `end` or as just unit ().
/// It is not necessary to use it, but there's a warning about unused result of '|'.
/// So you either suppress warning with `#[allow(unused_must_use)]`, assign the expression
/// to underscore variable `let _ = `, or "put an end" to this using unit `()` or `end`.
pub const end: () = ();

pub struct Either {}

/// Grammar "keyword" `or` to prefix inline alternatives
pub const or: &Either = &Either {};

pub struct InlineEither {
    not: bool,
    alternative: Vec<MatchCapture>
}

impl Not for &'static Either {
    type Output = InlineEither;

    fn not(self) -> Self::Output {
        InlineEither {
            not: true,
            alternative: vec![],
        }
    }
}

impl Not for InlineEither {
    type Output = InlineEither;

    fn not(self) -> Self::Output {
        let mut e = self;
        e.not = !e.not;
        e
    }
}

impl<M: Match> BitOr<M> for &'static Either {
    type Output = InlineEither;

    fn bitor(self, rhs: M) -> Self::Output {
        InlineEither {
            not: false,
            alternative: vec![rhs.capture()]
        }
    }
}

impl<M: Match> BitOr<M> for InlineEither {
    type Output = InlineEither;

    fn bitor(self, rhs: M) -> Self::Output {
        let mut m = self;
        m.alternative.push(rhs.capture());
        m
    }
}

impl Match for InlineEither {
    fn capture(self) -> MatchCapture {
        // can be with empty negation
        if self.alternative.len() == 0 {
            return MatchCapture::Noop
        }

        let all_just_terms = self.alternative.iter()
            .all(|c| {
                match c {
                    MatchCapture::Term { quant, .. } => *quant == Quant::default(),
                    _ => false
                }
            });

        let quant = if self.not { Quant::not() } else { Quant::default() };

        if all_just_terms {
            let terms = self.alternative.into_iter()
                .map(|c| {
                    let MatchCapture::Term { term, .. } = c else {
                        panic!("checked that all terms")
                    };
                    term
                })
                .collect::<Vec<_>>();

            MatchCapture::TermOneOf {
                quant,
                terms,
            }
        } else {
            MatchCapture::Either {
                quant,
                variants: self.alternative,
            }
        }
    }
}

pub struct NoBacktrack {}

/// Grammar keyword, instruction to do not backtrack up to this point,
/// marks that if we get here, no other alternative could match,
/// this improves error reporting mainly.
pub const sure: &NoBacktrack = &NoBacktrack {};

impl Match for &'static NoBacktrack {
    fn capture(self) -> MatchCapture {
        MatchCapture::NoBacktrack {}
    }
}

impl<P: Node, L: Node> Sub<&'static Production<L>> for &'static Part<P, L> {
    type Output = LabeledPart;

    fn sub(self, rhs: &'static Production<L>) -> Self::Output {
        GrammarSetup::add_part_label(self.name, self as &dyn AttachPart);
        LabeledPart { name: self.name, capture: rhs.capture() }
    }
}

impl<P: Node, L: Node> Sub<&'static Choice<L>> for &'static Part<P, L> {
    type Output = LabeledPart;

    fn sub(self, rhs: &'static Choice<L>) -> Self::Output {
        GrammarSetup::add_part_label(self.name, self as &dyn AttachPart);
        LabeledPart { name: self.name, capture: rhs.capture() }
    }
}

pub struct LabeledPart {
    name: &'static str,
    capture: MatchCapture,
}

impl Match for LabeledPart {
    fn capture(self) -> MatchCapture {
        let LabeledPart { name, mut capture } = self;
        let MatchCapture::Production { part: ref mut part @ None, .. } = capture else {
            panic!("cannot be a labeled part '{name}': {capture:?}")
        };
        *part = Some(name);
        capture
    }
}

impl<P: Node, F: Node> Sub<&'static Production<F>> for &'static Transform<P, F> {
    type Output = TransformedProd<P, F>;

    fn sub(self, rhs: &'static Production<F>) -> Self::Output {
        GrammarSetup::add_transform(self.name, self as &dyn TransformNode);
        TransformedProd { name: self.name, production: rhs, _pd: PhantomData {} }
    }
}

// for TransformedProd and TransformedChoice:
// the difference with LabeledPart is that we want to preserve type parameter/production
// to typecheck properly as hands of Choice
pub struct TransformedProd<P: Node, F: Node> {
    name: &'static str,
    production: &'static Production<F>,
    _pd: PhantomData<P>,
}

impl<P: Node, F: Node> Match for TransformedProd<P, F> {
    fn capture(self) -> MatchCapture {
        let TransformedProd { name, production, .. } = self;
        let mut capture = production.capture();
        let MatchCapture::Production { transform: ref mut transform @ None, .. } = capture else {
            panic!("cannot be transformed with '{name}': {capture:?}")
        };
        *transform = Some(name);
        capture
    }
}

impl<P: Node, F: Node> Sub<&'static Choice<F>> for &'static Transform<P, F> {
    type Output = TransformedChoice<P, F>;

    fn sub(self, rhs: &'static Choice<F>) -> Self::Output {
        GrammarSetup::add_transform(self.name, self as &dyn TransformNode);
        TransformedChoice { name: self.name, choice: rhs, _pd: PhantomData {} }
    }
}

pub struct TransformedChoice<P: Node, F: Node> {
    name: &'static str,
    choice: &'static Choice<F>,
    _pd: PhantomData<P>,
}

impl<P: Node, F: Node> Match for TransformedChoice<P, F> {
    fn capture(self) -> MatchCapture {
        let TransformedChoice { name, choice, .. } = self;
        let mut capture = choice.capture();
        let MatchCapture::Production { transform: ref mut transform @ None, .. } = capture else {
            panic!("cannot be transformed with '{name}': {capture:?}")
        };
        *transform = Some(name);
        capture
    }
}
