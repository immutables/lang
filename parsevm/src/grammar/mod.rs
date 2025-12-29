//! Type parameter convention:
//! * `C` - (now Node::Context) context delegate, the object which is used as grammar specific
//!     context storage and factory, can be obtained from `Box<dyn Any>` in a passed mut [NodeCtx].
//! * `P` - type of node/product associated with a product. It is usually something like
//!     clean AST type. The parsed product entries as CST. Using "T" for a type can be confused
//!     with "term" or "token" type, so it is "P" for "production".
//! * `L` - type of part that is either sub-product or captured tokens, either way,
//!     the role of a part is that we take `L` and apply it to `P`, as a "part". "L" stands
//!     for "label" (as in "label part"), "P" for "part" can be confused with "production"

pub mod def;
pub mod parse;

mod ops;

use crate::grammar::parse::{FarthestMismatch, Mismatch};
use crate::term::{HasInfo, Idx, Term, Terms};
use annotate_snippets::renderer::DecorStyle;
use annotate_snippets::{AnnotationKind, Level, Renderer, Snippet};
use ops::{compute_grammar, Ops};
use parse::parse;
use std::any::Any;
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt::Write;
use std::marker::PhantomData;
use std::num::NonZeroU16;
use std::ops::Range;

pub trait Node: 'static {
    type Context: 'static;

    #[allow(unused_variables)]
    fn complete(&mut self, context: &mut Self::Context, source_range: Range<usize>, source: &[u8]) {}
}

/// [Node] for `()` allows to use () as node type when the type is not important,
/// i.e. when production is "throw-away" or otherwise a "dummy" one.
impl Node for () {
    type Context = ();
}

/// The context used when building AST
struct NodeCtx<'s> {
    delegate: Box<dyn Any>,

    source: &'s [u8],

    // source range of terms of current production entry
    //source_range: Range<usize>,
}

impl<'s> NodeCtx<'s> {
    pub fn delegate<C: 'static>(&mut self) -> &mut C {
        self.delegate.downcast_mut::<C>()
            .expect("context delegate must be of expected type")
    }
}

/// Abstract production, i.e. choice of several other productions
pub struct Choice<P: Node> {
    name: &'static str,

    _pd2: PhantomData<P>,
}

/// Regular named production producing node
pub struct Production<P: Node> {
    name: &'static str,

    create: fn(context: &mut P::Context) -> P,
}

/// Attached sub-production's result to the host node as a "labeled" part.
pub struct Part<P: Node, L: Node> {
    name: &'static str,

    attach: fn(host: &mut P, part: L),
}

/// Transforms node from F to P
pub struct Transform<P: Node, F: Node> {
    name: &'static str,

    transform: fn(from: F) -> P,
}

trait CreateNode {
    fn create(&self, ctx: &mut NodeCtx) -> Box<dyn Any>;

    fn complete(&self, ctx: &mut NodeCtx, source_range: &Range<usize>, node: &mut Box<dyn Any>);
}

trait AttachPart {
    fn attach_part(&self, ctx: &mut NodeCtx, node: &mut Box<dyn Any>, part: Box<dyn Any>);
}

trait TransformNode {
    fn transform(&self, ctx: &mut NodeCtx, source_range: &Range<usize>, node: Box<dyn Any>) -> Box<dyn Any>;
}

impl<P: Node> CreateNode for Production<P> {
    fn create(&self, ctx: &mut NodeCtx) -> Box<dyn Any> {
        let context = ctx.delegate();
        let node = (self.create)(context);
        Box::new(node)
    }

    fn complete(&self, ctx: &mut NodeCtx, source_range: &Range<usize>, node: &mut Box<dyn Any>) {
        let source = ctx.source;
        let context = ctx.delegate();
        let node: &mut P = node.downcast_mut::<P>().expect("production of expected type");
        node.complete(context, source_range.clone(), source);
    }
}

impl<P: Node, L: Node> AttachPart for Part<P, L> {
    fn attach_part(&self, _: &mut NodeCtx, node: &mut Box<dyn Any>, part: Box<dyn Any>) {
        let part_node: Box<L> = part.downcast::<L>().expect("part of expected type");
        let host_node: &mut P = node.downcast_mut::<P>().expect("host production of expected type");

        (self.attach)(host_node, *part_node);
    }
}

impl<P: Node, F: Node> TransformNode for Transform<P, F> {
    fn transform(&self, ctx: &mut NodeCtx, source_range: &Range<usize>, node: Box<dyn Any>) -> Box<dyn Any> {
        let from_node = *node.downcast::<F>().expect("node of expected type");

        let source = ctx.source;
        let context = ctx.delegate();

        let mut node = (self.transform)(from_node);
        node.complete(context, source_range.clone(), source);
        Box::new(node)
    }
}

/// Named group, is a kind of ephemeral production, in the sense that it would not produce
/// an "important" node, but can be used and reused as in production alternatives
pub struct Group {
    name: &'static str
    // Group had <P: Node> parameter for the type of host production
    // it can be used it. These types are not enforced by DSL as of now and are probably
    // an unnecessary drag. At the same time, any mismatches of the host type and any nested
    // labeled parts will result in runtime panic (a grammar flaw or implementation bug, so it's panic).
}

// used from macro
#[doc(hidden)]
pub const fn declare_production<P: Node>(name: &'static str, create: fn(context: &mut P::Context) -> P) -> Production<P> {
    Production {
        name,
        create
    }
}

// used from macro
#[doc(hidden)]
pub const fn declare_group(name: &'static str) -> Group {
    Group {
        name,
    }
}

// used from macro
#[doc(hidden)]
pub const fn declare_choice<P: Node>(name: &'static str) -> Choice<P> {
    Choice {
        name,
        _pd2: PhantomData {},
    }
}

// used from macro
#[doc(hidden)]
pub const fn declare_part<P: Node, L: Node>(name: &'static str, attach: fn(host: &mut P, part: L)) -> Part<P, L> {
    Part {
        name,
        attach,
    }
}

// used from macro
#[doc(hidden)]
pub const fn declare_transform<P: Node, F: Node>(name: &'static str, transform: fn(from: F) -> P) -> Transform<P, F> {
    Transform {
        name,
        transform,
    }
}

#[macro_export]
macro_rules! production {
    ($name:ident) => {
        pub const $name: &$crate::grammar::Production<()> =
            &$crate::grammar::declare_production::<()>(stringify!($name), |_| ());
    };
    ($name:ident, $P:ty, $($tokens:tt)*) => {
        pub const $name: &$crate::grammar::Production<$P> =
            &$crate::grammar::declare_production::<$P>(stringify!($name), $($tokens)*);
    };
}

#[macro_export]
macro_rules! part {
    ($name:ident) => {
        pub const $name: &$crate::grammar::Part<(), ()> =
            &$crate::grammar::declare_part::<(), ()>(stringify!($name), |_, _| ());
    };
    ($name:ident, $P:ty, $L:ty, $($tokens:tt)*) => {
        pub const $name: &$crate::grammar::Part<$P, $L> =
            &$crate::grammar::declare_part::<$P, $L>(stringify!($name), $($tokens)*);
    };
}

#[macro_export]
macro_rules! transform {
    ($name:ident, $P:ty, $F:ty, $($tokens:tt)*) => {
        pub const $name: &$crate::grammar::Transform<$P, $F> =
            &$crate::grammar::declare_transform::<$P, $F>(stringify!($name), $($tokens)*);
    };
}

#[macro_export]
macro_rules! group {
    ($name:ident) => {
        pub const $name: &$crate::grammar::Group =
            &$crate::grammar::declare_group(stringify!($name));
    };
/*
    ($name:ident, $P:ty) => {
        const $name: &$crate::grammar::Group<$P> =
            &$crate::grammar::declare_group::<$P>(stringify!($name));
    };
*/
}

#[macro_export]
macro_rules! choice {
    ($name:ident) => {
        pub const $name: &$crate::grammar::Choice<()> =
            &$crate::grammar::declare_choice::<()>(stringify!($name));
    };
    ($name:ident, $P:ty) => {
        pub const $name: &$crate::grammar::Choice<$P> =
            &$crate::grammar::declare_choice::<$P>(stringify!($name));
    };
}

thread_local! {
	static GRAMMAR_SETUP: RefCell<Option<GrammarSetup>> = RefCell::new(None);
}

/// Internal definition id.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct DefId(NonZeroU16);

impl DefId {
    fn as_usize(&self) -> usize {
        self.0.get() as usize
    }
}

impl From<u16> for DefId {
    fn from(v: u16) -> Self {
        Self(NonZeroU16::new(v).expect("non zero u16"))
    }
}

impl Into<usize> for DefId {
    fn into(self) -> usize {
        self.0.get() as usize
    }
}

#[allow(unused)]
enum DefinitionEntry {
    /// Dummy, blank definition to align non-zero id assignment with the index in vec
    Blank,
    Production {
        id: DefId,
        name: &'static str,
        create: &'static dyn CreateNode,
        ops_at: Option<u16>,
        hands: Vec<MatchCapture>,
    },
    Group {
        id: DefId,
        name: &'static str,
        ops_at: Option<u16>,
        hands: Vec<MatchCapture>,
    },
    Choice {
        id: DefId,
        name: &'static str,
        ops_at: Option<u16>,
        hands: Vec<MatchCapture>,
    },
    Uninlined {
        id: DefId,
        ops_at: Option<u16>,
    },
    Part {
        id: DefId,
        name: &'static str,
        attach: &'static dyn AttachPart
    },
    Transform {
        id: DefId,
        name: &'static str,
        transform: &'static dyn TransformNode
    }
}

impl DefinitionEntry {
    fn name(&self) -> &'static str {
        match self {
            DefinitionEntry::Production { name, .. } => name,
            DefinitionEntry::Group { name, .. } => name,
            DefinitionEntry::Choice { name, .. } => name,
            DefinitionEntry::Part { name, .. } => name,
            DefinitionEntry::Transform { name, .. } => name,
            DefinitionEntry::Uninlined { .. } => "<uninlined>",
            DefinitionEntry::Blank => "<blank>",
        }
    }
}

// Internal grammar "setup" struct, which collects boxed things from inside `rules_fn` call
// via thread local, then, when grammar is being built, all things gets optimized and unboxed.
struct GrammarSetup {
    definition_count: u16,
    term_alias: Vec<(&'static str, Box<dyn Any>)>,
    definitions: Vec<DefinitionEntry>,
    by_name: HashMap<&'static str, DefId>
}

impl GrammarSetup {
    fn new() -> Self {
        Self {
            definition_count: 0,
            term_alias: vec![],
            definitions: vec![DefinitionEntry::Blank],
            by_name: HashMap::new()
        }
    }

    // These are not exactly as instance methods since actual instance is
    // thread local, but we "encapsulate"(or rather co-locate) that access here.
    // Key here is that these are not generic!.

    fn init() {
        GRAMMAR_SETUP.with(|cell| {
            cell.borrow_mut().replace(Self::new());
        });
    }

    fn pluck() -> Self {
        GRAMMAR_SETUP.with(|cell| {
            cell.borrow_mut().take().expect("grammar setup is present")
        })
    }

    fn add_alias(alias: &'static str, term: Box<dyn Any>) {
        GRAMMAR_SETUP.with(move |cell| {
            let mut borrowed = cell.borrow_mut();
            let this = borrowed.as_mut().expect(RULE_GRAMMAR_SCOPE);

            this.term_alias.push((alias, term))
        });
    }

    fn add_part_label(name: &'static str, attach: &'static dyn AttachPart) {
        GRAMMAR_SETUP.with(|cell| {
            let mut borrowed = cell.borrow_mut();
            let this = borrowed.as_mut().expect(RULE_GRAMMAR_SCOPE);

            let id = this.by_name.entry(name).or_insert_with(|| {
                this.definition_count += 1;
                DefId::from(this.definition_count)
            });
            if Into::<usize>::into(*id) == this.definitions.len() { // newly added
                this.definitions.push(DefinitionEntry::Part {
                    id: *id,
                    name,
                    attach,
                })
            }
        });
    }

    fn add_transform(name: &'static str, transform: &'static dyn TransformNode) {
        GRAMMAR_SETUP.with(|cell| {
            let mut borrowed = cell.borrow_mut();
            let this = borrowed.as_mut().expect(RULE_GRAMMAR_SCOPE);

            let id = this.by_name.entry(name).or_insert_with(|| {
                this.definition_count += 1;
                DefId::from(this.definition_count)
            });
            if Into::<usize>::into(*id) == this.definitions.len() { // newly added
                this.definitions.push(DefinitionEntry::Transform {
                    id: *id,
                    name,
                    transform,
                })
            }
        });
    }

    fn add_production_hand(name: &'static str, create: &'static dyn CreateNode, capture: MatchCapture) {
        GRAMMAR_SETUP.with(|cell| {
            let mut borrowed = cell.borrow_mut();
            let this = borrowed.as_mut().expect(RULE_GRAMMAR_SCOPE);

            let id = this.by_name.entry(name).or_insert_with(|| {
                this.definition_count += 1;
                DefId::from(this.definition_count)
            });

            if id.as_usize() == this.definitions.len() { // newly added
                this.definitions.push(DefinitionEntry::Production {
                    id: *id,
                    name,
                    create,
                    ops_at: None, // not yet initialized
                    hands: vec![capture]
                })
            } else {
                let Some(DefinitionEntry::Production { hands, .. }) = this.definitions.get_mut(id.as_usize())
                else { panic!("production definition named `{name}` #{id:?} clash with another") };

                hands.push(capture);
            }
        });
    }

    fn add_choice_hand(name: &'static str, capture: MatchCapture) {
        GRAMMAR_SETUP.with(|cell| {
            let mut borrowed = cell.borrow_mut();
            let this = borrowed.as_mut().expect(RULE_GRAMMAR_SCOPE);

            let id = this.by_name.entry(name).or_insert_with(|| {
                this.definition_count += 1;
                DefId::from(this.definition_count)
            });

            if id.as_usize() == this.definitions.len() { // newly added
                this.definitions.push(DefinitionEntry::Choice {
                    id: *id,
                    name,
                    ops_at: None, // not yet initialized
                    hands: vec![capture]
                })
            } else {
                let Some(DefinitionEntry::Choice { hands, .. }) = this.definitions.get_mut(id.as_usize())
                else { panic!("choice definition named `{name}` #{id:?} clash with another") };

                hands.push(capture);
            }
        });
    }

    fn add_group_hand(name: &'static str, capture: MatchCapture) {
        GRAMMAR_SETUP.with(|cell| {
            let mut borrowed = cell.borrow_mut();
            let this = borrowed.as_mut().expect(RULE_GRAMMAR_SCOPE);

            let id = *this.by_name.entry(name).or_insert_with(|| {
                this.definition_count += 1;
                DefId::from(this.definition_count)
            });

            if id.as_usize() == this.definitions.len() { // newly added
                this.definitions.push(DefinitionEntry::Group {
                    id,
                    name,
                    ops_at: None, // not yet initialized
                    hands: vec![capture]
                })
            } else {
                let Some(DefinitionEntry::Group { hands, .. }) = this.definitions.get_mut(id.as_usize())
                else { panic!("group definition named `{name}` #{id:?} clash with another") };

                hands.push(capture);
            }
        });
    }
}

pub fn alias<T: Term>(alias: &'static str, term: T) {
    GrammarSetup::add_alias(alias, Box::new(term) as Box<dyn Any>)
}

const RULE_GRAMMAR_SCOPE: &'static str = "should be called in grammar definition function";

/// small structure to configure quantifiers
#[doc(hidden)]
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Quant {
    /// if one or many: 0..1 or 1..N (depending on [opt] `true` or `false` respectively)
    many: bool,
    /// if optional or not: 0..1 or 0..N (depending on [many] `false` or `true` respectively)
    opt: bool,
    /// should not match, if this is set, the other two irrelevant
    not: bool
}

impl Quant {
    fn not() -> Quant {
        Quant { not: true, many: false, opt: false, }
    }
}

#[doc(hidden)]
#[derive(Debug)]
pub enum MatchCapture {
    Noop,
    Sequence {
        quant: Quant,
        sequence: Vec<MatchCapture>
    },
    Either {
        quant: Quant,
        variants: Vec<MatchCapture>
    },
    Term {
        quant: Quant,
        term: Box<dyn Any>,
    },
    TermOneOf {
        quant: Quant,
        terms: Vec<Box<dyn Any>>, // token or &str
    },
    TermLast {
        not: bool,
        term: Box<dyn Any>,
    },
    Group {
        quant: Quant,
        name: &'static str
    },
    Production {
        quant: Quant,
        name: &'static str,
        part: Option<&'static str>,
        transform: Option<&'static str>,
    },
    NoBacktrack {},
}

impl MatchCapture {
    fn quant_mut(&mut self) -> Option<&mut Quant> {
        match self {
            MatchCapture::Sequence { quant, .. } => Some(quant),
            MatchCapture::Term { quant, .. } => Some(quant),
            MatchCapture::TermOneOf { quant, .. } => Some(quant),
            MatchCapture::Group { quant, .. } => Some(quant),
            MatchCapture::Production { quant, .. } => Some(quant),
            _ => None
        }
    }
}

struct ProdEntry {
    id: DefId,
    part: Option<DefId>,
    transform: Option<DefId>,
    /// term index where production begins
    term_at: Idx,
    /// term index where production ends, exclusive
    term_end: Idx,
    /// how many entries need to skip to get to the next sibling
    len: u16,
}

/// Raw (not yet instantiated into AST) result of parsing.
pub struct Productions<P: Node> {
    result: Vec<ProdEntry>,
    // type parameter is used only for type checking when passing it to [Grammar::instantiate]
    _pd: PhantomData<P>,
}

fn instantiate<T: Term>(
    ctx: &mut NodeCtx,
    productions: &[ProdEntry],
    definitions: &[DefinitionEntry],
    terms: &Terms<T>,
    mut host: Option<&mut Box<dyn Any>>
) -> Option<Box<dyn Any>> {
    let mut i = 0;

    while i < productions.len() {
        let prod = &productions[i];
        let id = prod.id;
        let len = prod.len as usize;

        let DefinitionEntry::Production { create, .. } = definitions[id.as_usize()] else {
            panic!("need to handle?");
        };
        let part = try_as_part(prod.part, definitions);
        let transform = try_as_transform(prod.transform, definitions);
        let source_range =
            terms.source_range(prod.term_at).start..
                terms.source_range(prod.term_end).end;

        let mut node = create.create(ctx);

        // from after this entry, up to next sibling (or end)
        let nested = &productions[(i + 1)..(i + len)];

        instantiate(ctx, nested, definitions, terms, Some(&mut node));

        create.complete(ctx, &source_range, &mut node);

        if let Some(transform) = transform {
            node = transform.transform(ctx, &source_range, node);
        }

        if let Some(h) = &mut host {
            if let Some(attach) = part {
                attach.attach_part(ctx, *h, node);
            }
        } else {
            // Case for the root, should be single
            return Some(node);
        }

        // continue to next sibling
        i += len;
    }

    None
}

fn try_as_transform(transform: Option<DefId>, definitions: &[DefinitionEntry]) -> Option<&dyn TransformNode> {
    if let Some(id) = transform {
        if let DefinitionEntry::Transform { transform, .. } = definitions[id.as_usize()] {
            return Some(transform);
        }
    }
    None
}

fn try_as_part(part: Option<DefId>, definitions: &[DefinitionEntry]) -> Option<&dyn AttachPart> {
    if let Some(id) = part {
        if let DefinitionEntry::Part { attach, .. } = definitions[id.as_usize()] {
            return Some(attach);
        }
    }
    None
}

pub struct Grammar<T: Term> {
    definitions: Vec<DefinitionEntry>,
    by_name: HashMap<&'static str, DefId>,
    ops: Vec<Ops<T>>,
}

impl<T: Term> Grammar<T> {
    pub fn with_rules(rules_fn: fn()) -> Self {
        // Set up thread local
        GrammarSetup::init();
        // Replay rules on our thread-local grammar setup
        rules_fn();
        // Tear down thread local, unwrap the value
        compute_grammar(GrammarSetup::pluck())
    }

    pub fn parse<P: Node>(&self, production: &'static Production<P>, terms: &Terms<T>)
        -> Result<Productions<P>, FarthestMismatch<T>> {
        let id = *self.by_name.get(production.name)
            .unwrap_or_else(|| {
                panic!("production doesn't occur in grammar rules '{}'", production.name);
            });
        if let DefinitionEntry::Production { ops_at, .. } = self.definitions[id.as_usize()] {
            let ops_at = ops_at.expect("assigned after grammar creation") as usize;
            parse(ops_at, &self.ops[..], terms)
        } else {
            panic!("mismatched entry type")
        }
    }

    // if production is parsed with no errors, then it's a grammar mistake
    // or implementation error if panic happens.
    pub fn instantiate<P: Node>(&self, context: P::Context, productions: &Productions<P>, terms: &Terms<T>, source: &[u8]) -> P {
        let ctx = &mut NodeCtx {
            delegate: Box::new(context),
            source,
        };
        let boxed = instantiate(ctx, &productions.result[..], &self.definitions[..], terms, None);

        *boxed.expect("properly created root")
            .downcast::<P>()
            .expect("production node of correct type")
    }
}

impl<T: Term + HasInfo> Grammar<T> {
    pub fn name_by_id(&self, id: DefId) -> &'static str {
        self.definitions[id.as_usize()].name()
    }

    pub fn show_ops(&self) -> String {
        let mut s = String::new();

        for (i, op) in self.ops.iter().enumerate() {
            write!(s, "{: >4}| ", i).unwrap();

            match op {
                Ops::Header {} => {
                    write!(s, "head").unwrap();
                }
                Ops::Definition { id, kind } => {
                    let id = id.as_usize();
                    let name = self.definitions[id].name();
                    write!(s, "defn '{name}' #{id} {kind:?}").unwrap();
                }
                Ops::LabelPart { id } => {
                    let id = id.as_usize();
                    let name = self.definitions[id].name();
                    write!(s, "part '{name}' #{id}").unwrap();
                }
                Ops::Transform { id } => {
                    let id = id.as_usize();
                    let name = self.definitions[id].name();
                    write!(s, "tfrm '{name}' #{id}").unwrap();
                }
                Ops::MatchTerm { quantifier, term } => {
                    let info = term.info();
                    let name = info.name;
                    let code = info.code;
                    // just decorative, not strictly necessary.
                    // Maybe it would be better to escape all escape sequences,
                    // but these 2 are fine
                    let alias = info.symbol
                        .replace("\n", "\\n")
                        .replace("\t", "\\t");
                    let q = quantifier.as_symbol();
                    write!(s, "term{q} '{alias}' {name}[{code}]").unwrap();
                }
                Ops::MatchTermOneOf { quantifier, len } => {
                    let q = quantifier.as_symbol();
                    write!(s, "orterm{q} len={len}").unwrap();
                }
                Ops::InlineTerm { term } => {
                    let info = term.info();
                    let name = info.name;
                    let code = info.code;
                    let alias = info.symbol;
                    write!(s, "iterm '{alias}' {name}[{code}]").unwrap();
                }
                Ops::MatchLastTerm { quantifier, term } => {
                    let info = term.info();
                    let name = info.name;
                    let code = info.code;
                    let alias = info.symbol;
                    let q = quantifier.as_symbol();
                    write!(s, "last{q} '{alias}' {name}[{code}]").unwrap();
                }
                Ops::MatchEntry { quantifier, at } => {
                    let q = quantifier.as_symbol();

                    let target = if let Ops::Definition { id, kind } = &self.ops[*at as usize] {
                        let id = id.as_usize();
                        let name = self.definitions[id].name();
                        format!("'{name}' #{id} {kind:?}")
                    } else {
                        "".into()
                    };
                    write!(s, "prod{q} :{at} >> {target}").unwrap();
                }
                Ops::MatchEntryByDef { quantifier, id } => {
                    let q = quantifier.as_symbol();
                    let id = id.as_usize();
                    // these should be rewritten already, but JIC
                    write!(s, "prod{q} #{id}").unwrap();
                }
                Ops::HasNext { at } => {
                    if let Some(at) = at {
                        write!(s, "next :{at}").unwrap();
                    } else {
                        write!(s, "next --").unwrap();
                    }
                }
                Ops::NoBacktrack => {
                    write!(s, "sure").unwrap();
                }
                Ops::Done => {
                    write!(s, "done").unwrap();
                }
            }
            s.push('\n');
        }

        s
    }

    pub fn show_parsed<P: Node>(&self, productions: &Productions<P>, terms: &Terms<T>, source: &[u8]) -> String {
        let mut s = String::new();

        for (i, e) in productions.result.iter().enumerate() {
            let len = e.len as usize;

            write!(s, "{:0>4}–{:0>4} +{:0>4}| ", i, i + len, len).unwrap();

            let source_range =
                terms.source_range(e.term_at).start
                    ..terms.source_range(e.term_end).end;

            let fragment = String::from_utf8_lossy(&source[source_range])
                .replace("\t", "\u{00BB}") // tab-like \u{21E5} is too wide, using narrower symbol
                .split("\n")
                .map(|s| format!("{}", s))
                .collect::<Vec<_>>();

            let fragment = if fragment.is_empty() {
                "\u{2205}".to_string() // empty set
            } else {
                let multiline = fragment.len() > 1;
                let mut fragment = fragment.join(format!("\u{23CE}\n{:15}|{:40}  |", "", "").as_str());
                if multiline {
                    fragment.push('\u{231C}');
                }
                fragment.insert(0, '|');
                fragment
            };

            let prod_name = self.name_by_id(e.id);
            let mut prod_label = String::new();
            if let Some(id) = e.part {
                prod_label.push_str(self.name_by_id(id));
                prod_label.push_str(": ");
            }
            if let Some(id) = e.transform {
                prod_label.push_str(self.name_by_id(id));
                prod_label.push_str(" - ");
            }
            prod_label.push_str(prod_name);

            write!(s, "{:40} {}", prod_label, fragment).unwrap();

            s.push('\n');
        }

        s
    }

    pub fn format_mismatch<'a>(&self,
        mismatch: &FarthestMismatch<T>,
        path: Option<String>,
        terms: &Terms<T>,
        source: &'a [u8]
    ) -> String
    {
        let source = String::from_utf8_lossy(source);
        let range = terms.source_range(mismatch.at);

        let while_expecting = mismatch.expected
            .map(|t| {
                let info = t.info();
                format!(", while expecting '{}' ({})", info.symbol, info.name)
            }).unwrap_or("".into());

        let in_production = mismatch.in_production
            .map(|id| self.definitions[id.as_usize()].name())
            .map(|name| format!(" in `{}`", name))
            .unwrap_or("".into());

        let message = match mismatch.mismatch {
            Mismatch::Unspecified => format!("unspecified {}{}", while_expecting, in_production),
            Mismatch::Expected => format!("stumbled on unexpected terms{}{}", while_expecting, in_production),
            Mismatch::NotExpected => format!("not expected this term{}", in_production),
            Mismatch::Unconsumed => "unrecognized terms".to_string(),
        };

        let annotation = match mismatch.mismatch {
            Mismatch::Unspecified => "Unspecified",
            Mismatch::Expected => "Not expected",
            Mismatch::NotExpected => "Not expected",
            Mismatch::Unconsumed => "Unrecognized",
        };

        let snippet = Snippet::source(source)
            .line_start(1) // starting line number
            .path(path) // optional “file name”
            .annotation(AnnotationKind::Primary.span(range).label(annotation));

        let error = Level::ERROR
            .primary_title(message)
            .id("syntax") // code ?
            .element(snippet);

        let renderer = Renderer::styled().decor_style(DecorStyle::Ascii);
        renderer.render(&[error])
    }
}
