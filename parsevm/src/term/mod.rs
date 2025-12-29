#[cfg(test)]
mod tests;

use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Index, IndexMut, Range};

/// Term
pub trait Term: Debug + Default + Copy + Eq + Hash + 'static {
    /// Allows ignoring/skipping terms, like whitespace etc
    fn should_skip(&self) -> bool { false }
}

// Not exactly into iter, idk, seems to adhoc, but it works with all the trait impls
pub trait EitherTerms: Clone {
    type Term: Term;
    /// returns ()
    fn iter_and_not(self) -> (impl Iterator<Item=Self::Term>, bool);
}

pub trait HasInfo: Term {
    fn info(&self) -> TermInfo;
}

#[derive(Debug, Clone)]
pub struct TermInfo {
    pub code: i16,
    pub kind: &'static str,
    pub name: &'static str,
    pub symbol: &'static str,
}

/// Index of a term in [Terms] parsing result
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Idx(pub u32);

impl Idx {
    pub const OUT_OF_RANGE: Idx = Idx(u32::MAX);

    pub fn is_out_of_range(&self) -> bool { self.0 == Self::OUT_OF_RANGE.0 }
}

impl Default for Idx {
    fn default() -> Self { Self::OUT_OF_RANGE }
}

impl From<usize> for Idx {
    fn from(value: usize) -> Self { Self(value as u32) }
}

impl Into<usize> for Idx {
    fn into(self) -> usize { self.0 as usize }
}

pub struct Terms<T: Term> {
    terms: Vec<(T, u32)>,
}

impl<T: Term> Terms<T> {
    pub fn with_position(position: usize) -> Self {
        let mut terms = Vec::new();

        terms.push((T::default(), position as u32));
        Self { terms }
    }

    pub fn len(&self) -> usize {
        self.terms.len() - 1
    }

    pub fn seek(&'_ self) -> Seeker<'_, T> {
        Seeker {
            terms: &self.terms,
            index: 0
        }
    }

    /// Changes last terms' end (exclusive) of a source range.
    /// Return value is mostly for fluid invocation in a boolean condition context,
    /// but will be `false` if terms are empty
    pub fn extend(&mut self, position_after: usize) -> bool {
        if let Some(last) = self.terms.last_mut() {
            last.1 = position_after as u32;
            true
        } else { false }
    }

    /// Changes last term. Return value is mostly for fluid invocation
    /// in a boolean condition context, but will be `false` if terms are empty
    pub fn replace(&mut self, term: T) -> bool {
        if let Some(last) = self.terms.last_mut() {
            last.0 = term;
            true
        } else { false }
    }

    pub fn push_or_collapse(&mut self, term: T, position_after: usize) -> bool {
        if let Some(last) = self.terms.last_mut() {
            let position_after = position_after as u32;
            if last.0 == term {
                last.1 = position_after;
                return true;
            }
            if last.1 == position_after {
                return false;
            }
        }
        self.push(term, position_after);
        true
    }

    /// Pushes the last occurring term with its end (exclusive) of a source range.
    /// Return value is mostly for fluid invocation in a boolean condition context, always `true`
    pub fn push(&mut self, term: T, position_after: usize) -> bool {
        self.terms.push((term, position_after as u32));
        true
    }

    pub fn last_idx(&self) -> Idx {
        let l = self.terms.len();
        // this counts 1 for blanket first entry, then 1 for the real first entry
        // so after adding first entry, last_idx would be Idx(0)
        if l >= 2 {
            Idx(l as u32 - 2)
        } else {
            Idx::default()
        }
    }

    /// Return type is a range of usize for composability/interoperability with slices
    pub fn source_range(&self, idx: Idx) -> Range<usize> {
        let i = idx.0 as usize;
        let start = self.terms[i].1 as usize;
        let end = self.terms[i + 1].1 as usize;
        start..end
    }
}

impl<T: Term> Index<Idx> for Terms<T> {
    type Output = T;

    fn index(&self, index: Idx) -> &T {
        &self.terms[index.0 as usize + 1].0
    }
}

impl<T: Term> IndexMut<Idx> for Terms<T> {
    fn index_mut(&mut self, index: Idx) -> &mut T {
        &mut self.terms[index.0 as usize + 1].0
    }
}

#[derive(Clone)]
pub struct Seeker<'t, T: Term> {
    terms: &'t [(T, u32)],
    // for raw access / reset
    pub(crate) index: usize,
}

impl<'t, T: Term> Seeker<'t, T> {
    pub fn len(&self) -> usize {
        self.terms.len() - 1
    }

    pub fn rewind(&mut self) -> &mut Self {
        self.index = 0;
        self
    }

    pub fn idx(&self) -> Idx {
        self.raw_to_idx(self.index)
    }

    pub fn by_raw_index(&self, index: usize) -> T {
        self.terms[index].0
    }

    pub fn raw_to_idx(&self, index: usize) -> Idx {
        let i = index;
        if 0 < i && i < self.terms.len() {
            Idx::from(i - 1)
        } else {
            Idx::OUT_OF_RANGE
        }
    }

    pub fn current(&self) -> T {
        let i = self.index;
        if i >= self.terms.len() { return T::default() }
        self.terms[i].0
    }

    pub fn at_idx(&mut self, idx: Idx) -> &mut Self {
        self.index = idx.0 as usize + 1;
        self
    }

    /// Almost like [Iterator::next], but returns only T,
    /// when T::default() is returned - it is the end
    pub fn next_term(&mut self) -> T {
        let i = self.index + 1;
        let len = self.terms.len();
        if i >= len {
            self.index = len;
            T::default()
        } else {
            self.index = i;
            self.terms[i].0
        }
    }

    pub fn has_next(&self) -> bool {
        self.index + 1 < self.terms.len()
    }

    pub fn source_range(&self) -> Range<usize> {
        let start = self.source_position_before();
        let end = self.source_position_after();
        start..end
    }

    pub fn source_position_before(&self) -> usize {
        let i = self.index;
        if i <= 1 {
            self.terms[0].1 as usize
        } else {
            self.terms[i - 1].1 as usize
        }
    }

    pub fn source_position_after(&self) -> usize {
        let i = self.index;
        self.terms[i].1 as usize
    }

    /// Asserts next_term and its range
    pub fn assert_next(&mut self, term: T, range: Range<usize>) {
        assert_eq!(self.next_term(), term);
        assert_eq!(self.source_range(), range);
    }
}

impl<'t, T: Term> Iterator for Seeker<'t, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next_term();
        if next == T::default() {
            None
        } else {
            Some(next)
        }
    }
}

impl<T: Term + HasInfo> Terms<T> {
    pub fn show(&self, source: &[u8]) -> String {
        let mut seeker = self.seek();

        let mut result = String::new();

        while seeker.has_next() {
            let term = seeker.next_term();

            let range = seeker.source_range();

            let fragment = &source[range.start..range.end.min(source.len())];
            let mut fragment = String::from_utf8_lossy(fragment)
                .replace("\t", "\u{00BB}") // tab-like \u{21E5} is too wide, using narrower symbol
                .split("\n")
                .map(|s| format!("{}", s))
                .collect::<Vec<_>>()
                .join(format!("\u{23CE}\n{:15}|{:40}  ", "", "").as_str());

            // This assumes default token is always something like undefined or end of file
            if term == T::default() {
                // QED/tombstone
                fragment = "\u{220E}".to_string();
            } else if fragment.is_empty() {
                // empty set
                fragment.push('\u{2205}')
            } else {
                // backtick quotes
                fragment.insert(0, '`');
                fragment.push('`');
            }

            let info = term.info();

            let token_info = format!("{} <{}>", info.name, info.kind);
            let token_line = &format!("{:04x}–{:04x} +{:04x}| {:40} {}\n",
                range.start, range.end, range.end - range.start,
                token_info,
                fragment);

            result.push_str(token_line)
        }
        result
    }
}
