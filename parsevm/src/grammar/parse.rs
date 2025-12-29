use super::*;
use crate::grammar::ops::{EntryKind, Ops, Quantifier};
use crate::grammar::parse::Mismatch::Unconsumed;
use crate::term::{Idx, Seeker, Terms};

#[derive(Debug, Default)]
struct ParseCtx<T: Term> {
    term_last_matched_at: Option<usize>,
    // current named production
    in_production: Option<DefId>,
    // poisoned by no backtracking, maybe incoherent implementation
    poisoned: bool,
    farthest_mismatch: FarthestMismatch<T>,
}

#[derive(Debug, Default)]
pub struct FarthestMismatch<T: Term> {
    /// 0 position in term Seeker is before first term
    index: usize,
    /// Term Idx, this one is zero-based, has Idx::default() as OUT_OF_RANGE
    pub at: Idx,
    /// While we can easily look up the actual term using [idx]
    pub expected: Option<T>,
    /// For the case of negative match failed. where actual was equal to not expected term
    pub in_production: Option<DefId>,
    /// Qualifies the kind of mismatch
    pub mismatch: Mismatch,
}

#[derive(Debug, Default)]
#[repr(u8)]
pub enum Mismatch {
    /// Default placeholder,
    #[default]
    Unspecified,
    /// Positive match mismatch, when expected terms did not occur.
    Expected,
    /// Negative match mismatch, not expected terms actually occurred.
    NotExpected,
    /// Unconsumed, leftover terms remaining.
    Unconsumed
}

fn not_least_most_one(quantifier: Quantifier) -> (bool, bool, bool) {
    match quantifier {
        Quantifier::Not => (true, false, false),
        Quantifier::One => (false, true, true),
        Quantifier::Opt => (false, false, true),
        Quantifier::Any => (false, false, false),
        Quantifier::More => (false, true, false),
    }
}

pub fn parse<T: Term, P: Node>(
    ops_at: usize,
    ops: &[Ops<T>],
    terms: &Terms<T>,
) -> Result<Productions<P>, FarthestMismatch<T>> {
    let mut result = Vec::<ProdEntry>::with_capacity(128);
    let mut context = ParseCtx::default();

    let mut seeker = terms.seek();

    while seeker.next_term().should_skip() {
        // this prepares position at first non ignored term
    }

    let ok = match_production(
        &mut context, None, None, false, false, ops_at, ops, &mut seeker, &mut result);

    // process unconsumed terms
    if ok && !unconsumed(&mut context.farthest_mismatch, &seeker) {
        Ok(Productions {
            result,
            _pd: PhantomData::default()
        })
    } else {
        Err(context.farthest_mismatch)
    }
}

fn unconsumed<T: Term>(farthest: &mut FarthestMismatch<T>, terms: &Seeker<T>) -> bool {
    if terms.idx() != Idx::OUT_OF_RANGE {
        farthest.mismatch = Unconsumed;
        farthest.in_production = None;
        farthest.expected = None;
        farthest.index = terms.index;
        farthest.at = terms.idx();
        true
    } else { false }
}

fn match_production<T: Term>(
    context: &mut ParseCtx<T>,
    part: Option<DefId>,
    transform: Option<DefId>,
    not: bool,
    in_not: bool,
    ops_at: usize,
    ops: &[Ops<T>],
    terms: &mut Seeker<T>,
    result: &mut Vec<ProdEntry>
) -> bool {
    if !in_not && context.poisoned { return false } // Not sure...

    let mut p = ops_at;

    let Ops::Definition { id, kind } = ops[p] else { return false }; // or panic?

    let term_on_entry = terms.index;
    let term_idx_start = terms.idx();
    let result_on_entry = result.len();
    let term_last_on_entry = context.term_last_matched_at;

    let descending_part = if kind == EntryKind::Production { None } else { part };
    let descending_transform = if kind == EntryKind::Production { None } else { transform };

    let context_in_production_on_entry = context.in_production;

    if kind == EntryKind::Production || kind == EntryKind::Group {
        // named productions, useful for diagnostic
        context.in_production.replace(id);
    };

    // we record only productions as result, we ignore groups or uninlined
    let create_prod_entry = kind == EntryKind::Production;

    if create_prod_entry {
        result.push(ProdEntry {
            id,
            part,
            transform,
            term_at: term_idx_start,
            term_end: Idx::default(), // updated in the end
            len: 0, // updated in the end
        });
    }

    p += 1; // moving past definition

    let mut backtrack = true;
    let mut done = false;

    while backtrack {
        let Ops::HasNext { at: has_next_at } = ops[p] else { break };

        // make it local to a hand and mutable
        let mut descending_part = descending_part;
        let mut descending_transform = descending_transform;
        p += 1; // moving past has_next

        let sequence_matched = loop {
            match ops[p] {
                Ops::LabelPart { id } => {
                    descending_part = Some(id);
                }
                Ops::Transform { id } => {
                    descending_transform = Some(id);
                }
                Ops::NoBacktrack => {
                    backtrack = false
                }
                Ops::MatchLastTerm { quantifier, term } => {
                    let not = if let Quantifier::Not = quantifier { true } else { false };
                    if let Some(last_at) = context.term_last_matched_at {
                        let matches = last_matches(context, term, not, last_at, terms);
                        if !matches { break false }
                    } else if !not { break false } // if none, negative match always succeeds
                }
                Ops::MatchTerm { quantifier, term } => {
                    let match_once =
                        |not| match_term(context, term, not, terms);

                    if !match_with_quantifier(quantifier, match_once) { break false }
                }
                Ops::MatchTermOneOf { quantifier, len } => {
                    let len = len as usize;
                    let from = p + 1;
                    let either_terms = &ops[from..(from + len)];
                    let match_once = |not|
                        match_either_terms(context, either_terms, not, terms);

                    if !match_with_quantifier(quantifier, match_once) { break false }
                    p += len; // +1 will be added after this iteration
                }
                Ops::MatchEntry { quantifier, at } => {
                    let at = at as usize;
                    let in_not = in_not | not; // propagate that we somewhere nested in not
                    let match_once = |not|
                        match_production(context, descending_part, descending_transform,
                            not, in_not, at, ops, terms, result);

                    if !match_with_quantifier(quantifier, match_once) { break false }
                }
                Ops::Done => { break true }
                // anything else would break our match (or shouldn't be there)
                _ => { break false }
            }
            p += 1; // general advancement +1 to next
        };

        if sequence_matched {
            done = true;
            break;
        }

        if let Some(next_at) = has_next_at {
            // unpacking non-zero u16 offset
            p = next_at.get() as usize;
            // reset token position
            terms.index = term_on_entry;
        } else { break }
    }

    if done && !not {
        // if we matched, and it is not a negation
        // we need to complete entry
        if create_prod_entry {
            let len = result.len();
            let entry = &mut result[result_on_entry];
            // length of entry covers all nested
            entry.len = (len - result_on_entry) as u16;
            // here we need last term, as we advance past skippable/ignored/whitespace
            // we need actual last matched, not the one before current
            if let Some(term_end) = context.term_last_matched_at {
                entry.term_end = terms.raw_to_idx(term_end);
            }
        }
    } else {
        // if we haven't matched or it a negation, we do proper cleanup

        // delete all productions created by this or nested productions
        result.truncate(result_on_entry);

        // return to a position in terms
        terms.index = term_on_entry;
        // and context
        context.term_last_matched_at = term_last_on_entry;
    }

    context.in_production = context_in_production_on_entry;

    if done != not {
        true
    } else {
        // Not sure if it is correct port of backtrack logic
        if !in_not && !backtrack {
            context.poisoned = true;
        }
        false
    }
}

fn match_with_quantifier<F>(quantifier: Quantifier, mut match_one: F) -> bool
where
    F: FnMut(bool) -> bool
{
    let (not, at_least_one, at_most_one) = not_least_most_one(quantifier);
    let mut at_least_one_matched = false;
    loop {
        let matched = match_one(not);
        // if it is a negative match, we've done right away
        if not { break matched }

        if matched {
            if at_most_one { break true }
            at_least_one_matched = true;
            // continue loop here, greedily consuming all matches
        } else if at_least_one && !at_least_one_matched {
            // failed to match at least one term
            break false
        } else {
            debug_assert!(!at_least_one || at_least_one_matched);
            // when we don't match, but we don't require any
            // more matches to proceed, break out of the loop
            // with success match overall
            break true
        }
    }
}

fn last_matches<T: Term>(
    context: &mut ParseCtx<T>,
    expected: T,
    not: bool,
    last_at: usize,
    terms: &mut Seeker<T>,
) -> bool {
    let at = if expected.should_skip() {
        // Special case, if we ask to match skippable character,
        // then last matched is considered the one right before current position.
        // We use raw indexes into terms, so we kinda sure that current index > last_at,
        // and last_at and current are valid indexes. Also, current > previous >= last_at and also valid.
        terms.index - 1
    } else {
        // just use last matched
        last_at
    };

    // btw, we do not advance terms during last_matches
    let actual = terms.by_raw_index(at);

    if (actual == expected) != not {
        // we don't record last match here, because last was already matched
        true
    } else {
        // but we do record last mismatch
        let farthest = &mut context.farthest_mismatch;
        if at >= farthest.index {
            farthest.mismatch = if not { Mismatch::NotExpected } else { Mismatch::Expected };
            farthest.at = terms.raw_to_idx(at);
            farthest.index = at;
            farthest.expected.replace(expected);
            farthest.in_production = context.in_production;
        }
        false
    }
}

fn match_term<T: Term>(
    context: &mut ParseCtx<T>,
    expected: T,
    not: bool,
    terms: &mut Seeker<T>,
) -> bool {
    let at = terms.index;
    let actual = terms.current();

    debug_assert!(!actual.should_skip(), "these should be already skipped");

    if (actual == expected) != not {
        context.term_last_matched_at.replace(at);
        if !not { // when not match, we don't advance
            while terms.next_term().should_skip() {}
        }
        true
    } else {
        // Sorry for duplication, but if extracted too many parameters
        let farthest = &mut context.farthest_mismatch;
        if at >= farthest.index {
            farthest.mismatch = if not { Mismatch::NotExpected } else { Mismatch::Expected };
            farthest.at = terms.raw_to_idx(at);
            farthest.index = at;
            farthest.expected.replace(expected);
            farthest.in_production = context.in_production;
        }
        false
    }
}

fn match_either_terms<T: Term>(
    context: &mut ParseCtx<T>,
    either_terms: &[Ops<T>],
    not: bool,
    terms: &mut Seeker<T>,
) -> bool {
    let at = terms.index;
    let actual = terms.current();

    debug_assert!(!actual.should_skip(), "these should be already skipped");

    let mut some_matched = false;

    for op in either_terms {
        if let Ops::InlineTerm { term } = op {
            if *term == actual {
                some_matched = true;
                break
            }
        }
    }

    if some_matched != not {
        context.term_last_matched_at.replace(at);
        if !not { // when not match, we don't advance
            while terms.next_term().should_skip() {}
        }
        true
    } else {
        let farthest = &mut context.farthest_mismatch;
        if at >= farthest.index {
            farthest.mismatch = if not { Mismatch::NotExpected } else { Mismatch::Expected };
            farthest.at = terms.raw_to_idx(at);
            farthest.index = at;
            farthest.expected = None; // we have many variants, so no particular
            farthest.in_production = context.in_production;
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Just for size measuring & control
    #[derive(Debug, Copy, Clone, Default, Eq, PartialEq, Hash)]
    #[repr(i16)]
    enum HereTerms {
        #[default]
        None = 0,
    }
    impl Term for HereTerms {}

    #[test]
    fn struct_sizes() {
        // just for control
        assert_eq!(size_of::<HereTerms>(), 2);
        // and here we control our ops
        assert_eq!(size_of::<Ops<HereTerms>>(), 4);

        // 128 bits should be fine for an entry, can make it even smaller,
        // but that would require limiting of term indexes sizes to u16
        assert_eq!(size_of::<ProdEntry>(), 16);
    }
}
