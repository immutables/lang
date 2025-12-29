use crate::term::Term;

use super::*;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(i8)]
pub enum Quantifier {
    /// not match, currently it cannot be combined with below
    Not = -1,
    /// zero on one,
    Opt = 0,
    /// one and only one
    One = 1,
    /// zero or more
    Any = 2,
    /// one or more
    More = 3,
}

impl Quantifier {
    pub fn as_symbol(&self) -> &'static str {
        match self {
            Quantifier::Not => "!",
            Quantifier::Opt => "?",
            Quantifier::One => "",
            Quantifier::Any => "*",
            Quantifier::More => "+",
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum EntryKind {
    Production,
    Choice,
    Group,
    Uninlined,
}

pub enum Ops<T: Term> {
    // just header so we will not have zero addresses
    Header {},

    Definition {
        id: DefId,
        kind: EntryKind,
    },

    LabelPart {
        id: DefId,
    },

    Transform {
        id: DefId,
    },

    MatchTerm {
        quantifier: Quantifier,
        term: T,
    },

    ///
    MatchTermOneOf {
        quantifier: Quantifier,
        len: u16,
    },

    /// This is ad-hoc variable-length encoding for [Ops::MatchTermOneOf],
    /// `InlineTerms` are trailing after it. The number of such trailers
    /// must match `MatchTermOneOf.len`
    InlineTerm {
        term: T,
    },

    /// Match term to the last matched term. The last matched term will never be
    /// the one ignored/skipped by [MaybeSkip::should_skip].
    MatchLastTerm {
        quantifier: Quantifier, // only Not or One supported here
        term: T,
    },

    /// Points to the nested match,
    MatchEntry {
        quantifier: Quantifier,
        at: u16,
    },

    MatchEntryByDef {
        quantifier: Quantifier,
        id: DefId,
    },

    /// Has next alternative to match at specified ops index, if 1at == None1,
    /// than it must be the last in a sequence of alternatives.
    HasNext {
        at: Option<NonZeroU16>
    },

    /// A point of no return, after this match we will either match this alternative hand
    /// completely or fail, we will not backtrack to another alternative.
    NoBacktrack,
    /// Marker for when matching sequence is done.
    /// Possibly, we can do without explicit "done", but let it be, for debugging etc.
    Done,
}

pub fn compute_grammar<T: Term>(setup: GrammarSetup) -> Grammar<T> {
    // we need separate mut access to parts, so we destructure it
    let GrammarSetup { definition_count: mut id_counter, term_alias, mut definitions, by_name } = setup;

    let term_alias = hash_map_alias(term_alias);

    let mut ops = vec![Ops::Header {}]; // starts with grammar header to avoid zero addresses

    assert_eq!(id_counter as usize, definitions.len() - 1);

    // collects uninlined entries
    let mut uninlined = VecDeque::<Uninlined>::new();

    // loop goes only up to initial len, all added uninlined entries go out of band
    for i in 0..definitions.len() {
        let Some((&mut id, hands, ops_at, kind)) = (match &mut definitions[i] {
            DefinitionEntry::Production { id, hands, ops_at, .. } =>
                Some((id, hands, ops_at, EntryKind::Production)),

            DefinitionEntry::Group { id, hands, ops_at, .. } =>
                Some((id, hands, ops_at, EntryKind::Group)),

            DefinitionEntry::Choice { id, hands, ops_at, .. } =>
                Some((id, hands, ops_at, EntryKind::Choice)),

            _ => None
        }) else { continue };

        let at: u16 = ops.len().try_into().expect("ops number exceeds u16::MAX");
        ops_at.replace(at);

        // now, when we marked beginning of the code, see above, we start our entry
        ops.push(Ops::Definition { id, kind });

        let mut prev_try_idx: Option<usize> = None;

        // as we're keeping entries for diagnostics,
        // we are draining (vec of captures) as those will be encoded as ops.

        for h in hands.drain(..) {
            let hand_at = ops.len();
            // Here we're updating HasNext directive of a previous hand
            // The last one will have None as ops index in HasNext
            if let Some(i) = prev_try_idx {
                let Ops::HasNext { at } = &mut ops[i] else { panic!("should be HasNext") };
                let message = "ops index within (0, u16::MAX]";
                let idx: u16 = hand_at.try_into().expect(message);
                let idx: NonZeroU16 = idx.try_into().expect(message);
                at.replace(idx);
            }
            prev_try_idx = Some(hand_at);
            // inserting none, will be updated by at next hand iteration if any
            ops.push(Ops::HasNext { at: None });

            encode_capture(true, h, &mut ops, &mut id_counter, &term_alias, &by_name, &mut uninlined);

            ops.push(Ops::Done);
        }
    }

    // Each uninlined entry can have some nested entries we need to uninline,
    // we use VecDeque as a queue to process them all
    while let Some(Uninlined { id, hands }) = uninlined.pop_front() {
        // this invariant making sure our expectations about the sequence is correct
        assert_eq!(id.as_usize(), definitions.len());

        let ops_at: u16 = ops.len().try_into().expect("ops number exceeds u16::MAX");
        definitions.push(DefinitionEntry::Uninlined {
            id,
            ops_at: Some(ops_at),
        });

        ops.push(Ops::Definition { id, kind: EntryKind::Uninlined });
        // a bit of duplication here, but it's fine

        let mut prev_try_idx: Option<usize> = None;

        for h in hands {
            let hand_at = ops.len();
            // Here we're updating HasNext directive of a previous hand
            // The last one will have None as ops index in HasNext
            if let Some(i) = prev_try_idx {
                let Ops::HasNext { at } = &mut ops[i] else { panic!("should be HasNext") };
                let message = "ops index within (0, u16::MAX]";
                let idx: u16 = hand_at.try_into().expect(message);
                let idx: NonZeroU16 = idx.try_into().expect(message);
                at.replace(idx);
            }
            prev_try_idx = Some(hand_at);
            // inserting none, will be updated by at next hand iteration if any
            ops.push(Ops::HasNext { at: None });

            encode_capture(true, h, &mut ops, &mut id_counter, &term_alias, &by_name, &mut uninlined);

            ops.push(Ops::Done);
        }
    }

    rewrite_match_def_id_to_ops(&definitions, &mut ops);

    Grammar {
        definitions,
        ops,
        by_name
    }
}

struct Uninlined {
    /// assigned unnamed definition id
    id: DefId,
    /// hands of a newly uninlined entry, alternatives
    hands: Vec<MatchCapture>,
}

fn encode_capture<T: Term>(
    is_hand: bool,
    capture: MatchCapture,
    ops: &mut Vec<Ops<T>>,
    id_counter: &mut u16,
    term_aliases: &HashMap<&'static str, T>,
    by_name: &HashMap<&str, DefId>,
    uninlined: &mut VecDeque<Uninlined>)
{
    match capture {
        MatchCapture::Noop => {}
        MatchCapture::Sequence { quant, sequence } => {
            // top level "hand" sequence, do not uninline, just define recursively
            // unless there's quantifier, so we'll uninline it, for the sake of quantifier

            if is_hand && quant == Quant::default() {
                for c in sequence {
                    encode_capture(false, c, ops, id_counter, term_aliases, by_name, uninlined);
                }
            } else {
                // need to uninline this inline sequence as unnamed group
                *id_counter += 1;
                let id = DefId::from(*id_counter);

                // Writing a match to this entry
                ops.push(Ops::MatchEntryByDef {
                    id,
                    quantifier: to_quantifier(&quant),
                });

                uninlined.push_back(Uninlined {
                    id,
                    // recreating sequence, but quantifier is default there
                    hands: vec![
                        MatchCapture::Sequence {
                            quant: Quant::default(),
                            sequence
                        }
                    ],
                })
            }
        }
        MatchCapture::Either { quant, variants } => {
            // need to uninline this inline either as unnamed group
            *id_counter += 1;
            let id = DefId::from(*id_counter);

            // Writing a match to this entry
            ops.push(Ops::MatchEntryByDef {
                id,
                quantifier: to_quantifier(&quant),
            });

            uninlined.push_back(Uninlined {
                id,
                hands: variants,
            })
        }
        MatchCapture::Term { quant, term } => {
            ops.push(Ops::MatchTerm {
                quantifier: to_quantifier(&quant),
                term: extract_term(&term, term_aliases),
            });
        }
        MatchCapture::TermOneOf { quant, terms } => {
            let len = terms.len().try_into().expect("terms len must fit u16");

            ops.push(Ops::MatchTermOneOf {
                quantifier: to_quantifier(&quant),
                len,
            });

            for t in terms {
                ops.push(Ops::InlineTerm {
                    term: extract_term(&t, term_aliases),
                });
            }
        }
        MatchCapture::TermLast { not, term } => {
            ops.push(Ops::MatchLastTerm {
                quantifier: if not { Quantifier::Not } else { Quantifier::One },
                term: extract_term(&term, term_aliases),
            });
        }
        MatchCapture::Group { quant, name } => {
            ops.push(Ops::MatchEntryByDef {
                quantifier: to_quantifier(&quant),
                id: *by_name.get(name).expect("group resolved by name"),
            });
        }
        MatchCapture::Production { quant, name, part, transform } => {
            if let Some(name) = part {
                ops.push(Ops::LabelPart {
                    id: *by_name.get(name).expect("label resolved by name"),
                });
            }
            if let Some(name) = transform  {
                ops.push(Ops::Transform {
                    id: *by_name.get(name).expect("transform resolved by name"),
                });
            }
            ops.push(Ops::MatchEntryByDef {
                quantifier: to_quantifier(&quant),
                id: *by_name.get(name).expect("production resolved by name"),
            });
        }
        MatchCapture::NoBacktrack {} => {
            ops.push(Ops::NoBacktrack);
        }
    }
}

fn to_quantifier(q: &Quant) -> Quantifier {
    if q.not {
        Quantifier::Not
    } else if q.opt && q.many {
        Quantifier::Any
    } else if q.opt {
        Quantifier::Opt
    } else if q.many {
        Quantifier::More
    } else {
        Quantifier::One
    }
}

/// This accounts to that we want, for performance/locality, ops offsets in match entries,
/// while definitions are referencing each other in an order which is not the same as we just
/// iterate all definitions. So ops offsets might not have been defined yet for an entry.
/// So after all offsets are defined, we overwrite from definition id to offset.
fn rewrite_match_def_id_to_ops<T: Term>(definitions: &Vec<DefinitionEntry>, ops: &mut Vec<Ops<T>>) {
    for op in ops.iter_mut() {
        let Ops::MatchEntryByDef { id, quantifier } = op else { continue };
        let at = match definitions[id.as_usize()] {
            DefinitionEntry::Production { ops_at, .. } => ops_at,
            DefinitionEntry::Group { ops_at, .. } => ops_at,
            DefinitionEntry::Choice { ops_at, .. } => ops_at,
            DefinitionEntry::Uninlined { ops_at, .. } => ops_at,
            _ => None
        };
        let Some(&at) = at.as_ref() else {
            panic!("terrible mismatch has happened, incompatible definition or ops")
        };
        // Rewrite this entry to the match pointing at ops id, not definition id
        *op = Ops::MatchEntry { at, quantifier: *quantifier }
    }
}

fn hash_map_alias<T: Term>(aliases: Vec<(&'static str, Box<dyn Any>)>) -> HashMap<&'static str, T> {
    let mut h = HashMap::new();
    for (alias, boxed_token) in aliases {
        let term = boxed_token.downcast::<T>()
            .expect("term of incompatible type");

        h.insert(alias, *term);
    }
    h
}

fn extract_term<T: Term>(boxed: &Box<dyn Any>, aliases: &HashMap<&'static str, T>) -> T {
    if let Some(&term) = boxed.downcast_ref::<T>() {
        term
    } else if let Some(&alias) = boxed.downcast_ref::<&'static str>() {
        let Some(&term) = aliases.get(alias) else {
            panic!("unmapped term alias '{alias}'");
        };
        term
    } else {
        panic!("unusable term type {:?}", boxed.type_id());
    }
}
