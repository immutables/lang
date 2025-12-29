use super::*;

#[derive(Debug, Copy, Clone, Default, Eq, PartialEq, Hash)]
#[repr(i16)]
enum HereTerms {
    #[default]
    None = 0,
    T1 = 1,
    T2 = 2,
    T3 = 3,
}

impl Term for HereTerms {}

#[test]
fn ensure_struct_sizes() {
    assert_eq!(size_of::<HereTerms>(), 2);
    // checks our expectation about alignment
    assert_eq!(size_of::<(HereTerms, u32)>(), 8);
}

fn push_sample_terms(terms: &mut Terms<HereTerms>) {
    use HereTerms::*;

    assert_eq!(terms.len(), 0);

    assert!(terms.push(T1, 2));
    assert!(terms.push(T2, 7));
    assert_eq!(terms.len(), 2);

    assert!(terms.push(T3, 13));
    assert!(terms.push(T1, 21));
    assert_eq!(terms.len(), 4);
}

#[test]
fn terms_pushing() {
    let mut terms = Terms::<HereTerms>::with_position(1);

    push_sample_terms(&mut terms);

    use HereTerms::*;
    assert_eq!(terms[Idx(0)], T1);
    assert_eq!(terms[Idx(1)], T2);
    assert_eq!(terms[Idx(2)], T3);
    assert_eq!(terms[Idx(3)], T1);

    assert_eq!(terms.source_range(Idx(0)), 1..2);
    assert_eq!(terms.source_range(Idx(1)), 2..7);
    assert_eq!(terms.source_range(Idx(2)), 7..13);
    assert_eq!(terms.source_range(Idx(3)), 13..21)
}

#[test]
fn terms_collect() {
    let mut terms = Terms::<HereTerms>::with_position(0);
    push_sample_terms(&mut terms);
    let terms: Vec<HereTerms> = terms.seek().collect();

    use HereTerms::*;
    assert_eq!(terms, vec![T1, T2, T3, T1])
}

#[test]
fn terms_seeking() {
    let mut terms = Terms::<HereTerms>::with_position(0);
    push_sample_terms(&mut terms);
    let mut seeker = terms.seek();

    use HereTerms::*;
    assert_eq!(seeker.len(), 4);
    assert_eq!(seeker.current(), None);
    assert_eq!(seeker.next_term(), T1);
    assert_eq!(seeker.next_term(), T2);
    assert_eq!(seeker.next_term(), T3);
    assert_eq!(seeker.next_term(), T1);
    assert_eq!(seeker.next_term(), None);

    seeker.at_idx(Idx(1));
    assert_eq!(seeker.current(), T2);
    assert_eq!(seeker.next_term(), T3);

    seeker.at_idx(Idx(3));
    assert_eq!(seeker.current(), T1);
    assert_eq!(seeker.idx(), Idx(3));

    seeker.at_idx(Idx(100));
    assert_eq!(seeker.current(), None);

    assert_eq!(seeker.rewind().current(), None);
    assert_eq!(seeker.rewind().next_term(), T1);
    assert_eq!(seeker.idx(), Idx(0));

    assert_eq!(seeker.next_term(), T2);
    assert_eq!(seeker.idx(), Idx(1));

    assert_eq!(seeker.rewind().next_term(), T1);
}

#[test]
fn terms_seeking_ranges() {
    use HereTerms::*;
    let mut terms = Terms::<HereTerms>::with_position(1);
    push_sample_terms(&mut terms);
    let mut seeker = terms.seek();
    assert_eq!(seeker.source_range(), 1..1); // no next yet, an empty range around initial position
    assert_eq!(seeker.next_term(), T1);
    assert_eq!(seeker.source_range(), 1..2);
    assert_eq!(seeker.next_term(), T2);
    assert_eq!(seeker.source_range(), 2..7);
    assert_eq!(seeker.next_term(), T3);
    assert_eq!(seeker.source_range(), 7..13);
    assert_eq!(seeker.next_term(), T1);
    assert_eq!(seeker.source_range(), 13..21);
    assert_eq!(seeker.next_term(), None);
}

#[test]
fn terms_idx() {
    use HereTerms::*;
    let mut terms = Terms::<HereTerms>::with_position(0);

    let no_idx = terms.last_idx();
    // all of these are just the same value
    assert_eq!(no_idx, Idx::default());
    assert_eq!(no_idx, Idx::OUT_OF_RANGE);
    assert!(no_idx.is_out_of_range());

    terms.push(T1, 1);
    assert_eq!(terms.last_idx(), Idx(0));
    assert_eq!(terms[terms.last_idx()], T1);

    terms.push(T2, 2);
    assert_eq!(terms.last_idx(), Idx(1));
    assert_eq!(terms[terms.last_idx()], T2);

    let mut seeker = terms.seek();
    assert_eq!(seeker.idx(), Idx::default());
    assert!(seeker.has_next());
    assert_eq!(seeker.next_term(), T1);
    assert_eq!(seeker.idx(), Idx(0));
    assert!(seeker.has_next());
    assert_eq!(seeker.next_term(), T2);
    assert!(!seeker.has_next());
    assert_eq!(seeker.idx(), Idx(1));
    assert_eq!(seeker.next_term(), None);

    assert!(!seeker.has_next());
    assert!(seeker.idx().is_out_of_range());
}


#[test]
fn terms_push_or_collapse() {
    use HereTerms::*;
    let mut terms = Terms::<HereTerms>::with_position(0);

    terms.push_or_collapse(T1, 2);
    terms.push_or_collapse(T1, 3);
    terms.push_or_collapse(T2, 3);
    terms.push_or_collapse(T2, 5);
    terms.push_or_collapse(T2, 9);

    assert_eq!(terms.seek().collect::<Vec<_>>(), vec![T1, T2]);

    assert_eq!(terms.source_range(Idx(0)), 0..3);
    assert_eq!(terms.source_range(Idx(1)), 3..9);
}

