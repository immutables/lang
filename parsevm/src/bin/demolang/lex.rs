#![allow(unused)]

use parsevm::swar::*;
use parsevm::term::Terms;
use super::token::Token;

pub fn tokenize(terms: &mut Terms<Token>, source: &[u8], position: &mut usize) {
    let mut p = *position; // copy to local
    let p = &mut p; // pointer to position, makes it easy to pass
    use Token::*;

    loop {
        let mut read: usize = 0;
        let w = read_u64_le(source, *p, &mut read);
        let ch = (w & 0xFF) as u8;
        if ch == 0 { break } // End of input, just break

        match ch {
            b' ' | b'\t' | b'\r' | b'\n' =>
                terms.push_or_collapse(Whitespace, inc(p)),

            b'/' => (match2(w, '/', '/') && line_comment(w, read, terms, source, p))
                || sequence2(w, '/', '=', terms, DivideAssign, p)
                || single(terms, Divide, p),

            b'{' => single(terms, BraceL, p),
            b'}' => single(terms, BraceR, p),

            b'(' => single(terms, ParenL, p),
            b')' => single(terms, ParenR, p),

            b'[' => single(terms, BrackL, p),
            b']' => single(terms, BrackR, p),
            b'=' => sequence2(w, '=', '=', terms, Equals, p)
                || single(terms, Assign, p),

            b',' => single(terms, Comma, p),

            b'.' => sequence3(w, '.', '.', '=', terms, RangeInclusive, p)
                || sequence2(w, '.', '.', terms, RangeExclusiveEnd, p)
                || single(terms, Dot, p),

            b'~' => single(terms, Tilde, p),

            b'*' => sequence2(w, '*', '=', terms, MultiplyAssign, p)
                || single(terms, Multiply, p),

            b'%' => sequence2(w, '%', '=', terms, ModuloAssign, p)
                || single(terms, Modulo, p),

            b'+' => sequence2(w, '+', '+', terms, Increment, p)
                || sequence2(w, '+', '=', terms, PlusAssign, p)
                || single(terms, Plus, p),

            b'-' => sequence2(w, '-', '-', terms, Decrement, p)
                || sequence2(w, '-', '>', terms, ArrowRight, p)
                || sequence2(w, '-', '=', terms, MinusAssign, p)
                || single(terms, Minus, p),

            b'|' => sequence2(w, '|', '|', terms, LogicOr, p)
                || single(terms, Pipe, p),

            b':' => sequence2(w, ':', '=', terms, ColonAssign, p)
                || sequence2(w, ':', ':', terms, Quadot, p)
                || single(terms, Colon, p),

            b'?' => sequence2(w, '?', '?', terms, OrElse, p)
                || single(terms, Question, p),

            b'!' => sequence2(w, '!', '=', terms, NotEquals, p)
                || single(terms, Exclaim, p),

            b'&' => sequence2(w, '&', '&', terms, LogicAnd, p)
                || single(terms, Ampersand, p),

            b'<' => sequence2(w, '<', '=', terms, LowerEquals, p)
                || sequence2(w, '<', '-', terms, ArrowLeft, p)
                || single(terms, Lower, p),

            b'>' => sequence2(w, '>', '=', terms, GreaterEquals, p)
                || sequence3(w, '>', '.', '.', terms, RangeExclusiveBegin, p)
                || single(terms, Greater, p),

            // Keywords
            b'd' => keyword3(w, 'd', 'e', 'f', terms, Def, p)
                || name(w, read, terms, source, p, Name),

            b'_' => {
                // If there's more than just _, we think it's a name
                // otherwise it's a single underscore character/operator
                if name_continues(w, 1) {
                    name(w, read, terms, source, p, Name)
                } else {
                    single(terms, Underscore, p)
                }
            }

            _ => {
                // Character class based dispatch
                let mut recognized = false;
                if ch < CLASSES.len() as u8 {
                    let class = CLASSES[ch as usize];
                    recognized = match class & CLASS_BASE_MASK {
                        CLASS_DIGIT => number(terms, source, p),
                        CLASS_LETTER_LOWERCASE => name(w, read, terms, source, p, Name),
                        // we don't handle any CLASS_IS_NAME_START, just '_' above
                        _ => false,
                    };
                }
                if !recognized {
                    terms.push_or_collapse(Unrecognized, inc(p))
                } else {
                    true
                }
            },
        };
    }

    *position = *p; // copy back from local to passed position pointer
}

fn line_comment(w: u64, read: usize, terms: &mut Terms<Token>, source: &[u8], position: &mut usize) -> bool {
    // making position local, need not forget to update it on the way back
    let mut p = *position + 2; // past "//"
    let p = &mut p;

    // we're past `//` now, have to deal with it
    let mut bytes_per_read = BYTES_PER_READ - 2;
    let mut read = read - 2;
    let mut w = w >> (u8::BITS * 2);

    loop {
        if detect_mask(w, MASK_NEWLINE) != 0 {
            // Found newline, scan to find it
            let mut w = w;
            let mut remaining = read;
            while remaining > 0 {
                let c = w & 0xFF;
                if c == b'\n' as u64 {
                    *p += read - remaining + 1; // include newline
                    *position = *p;
                    return terms.push(Token::LineComment, *p);
                }
                w >>= u8::BITS;
                remaining -= 1;
            }
        }

        // No newline detected
        if read == bytes_per_read {
            *p += bytes_per_read;
            bytes_per_read = BYTES_PER_READ;
            w = read_u64_le(source, *p, &mut read);
            if w != 0 { continue }
        }

        // End of input or read < 8 bytes
        *p += read;
        *position = *p;
        return terms.push(Token::LineComment, *p);
    }
}

fn name(w: u64, read: usize, terms: &mut Terms<Token>, source: &[u8], position: &mut usize, term: Token) -> bool {
    let mut p = *position;
    let p = &mut p;

    *p += 1; // Move past the first character (already verified)

    let mut bytes_per_read = BYTES_PER_READ - 1;
    let mut w = w >> u8::BITS;
    let mut read = read - 1;

    loop {
        let mut remaining = read;
        while remaining > 0 {
            let c = (w & 0xFF) as usize;
            if c >= CLASSES.len() || CLASSES[c] & CLASS_IS_NAME_REST == 0 {
                // Not a name character, we're done
                *p += read - remaining;
                *position = *p;
                return terms.push(term, *p);
            }
            w >>= u8::BITS;
            remaining -= 1;
        }

        // was EOF, no need to read next
        if read != bytes_per_read { break }

        // All bytes were valid name characters, continue
        *p += bytes_per_read;

        // restoring normal BYTES_PER_READ after 1 iteration,
        // where we had one character less (in front)
        w = read_u64_le(source, *p, &mut read);
        bytes_per_read = BYTES_PER_READ;
    }

    *position = *p;
    terms.push(term, *p)
}

pub fn number(terms: &mut Terms<Token>, source: &[u8], p: &mut usize) -> bool {
    let start = *p;
    *p += 1; // Move past first digit

    // non using SWAR here, sorry, just a demo
    while *p < source.len() {
        let c = source[*p] as usize;
        if c <= CLASSES.len() && CLASSES[c] & CLASS_IN_DIGIT != 0 {
            *p += 1;
        } else { break }
    }

    // Backtrack trailing underscores
    while *p > start && source[*p - 1] == b'_' {
        *p -= 1;
    }

    terms.push(Token::IntNumber, *p)
}

/// Increment (pre-increment), by 1. Returns value _after_ increment.
fn inc(p: &mut usize) -> usize {
    let v = *p + 1;
    *p = v;
    v
}

/// Increment (pre-increment) by `increment` parameter. Returns value _after_ increment.
fn inc_by(p: &mut usize, increment: usize) -> usize {
    let v = *p + increment;
    *p = v;
    v
}

/// Tiny wrapper for "symmetry" with sequenceN.
/// Hope it will be just inlined as all sequences etc.
fn single(terms: &mut Terms<Token>, token: Token, p: &mut usize) -> bool {
    terms.push(token, inc(p))
}

fn sequence2(
    w: u64,
    c0: char, c1: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match2(w, c0, c1) && terms.push(token, inc_by(p, 2))
}

fn sequence3(
    w: u64,
    c0: char, c1: char, c2: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match3(w, c0, c1, c2) && terms.push(token, inc_by(p, 3))
}

fn sequence4(
    w: u64,
    c0: char, c1: char, c2: char, c3: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match4(w, c0, c1, c2, c3) && terms.push(token, inc_by(p, 4))
}

fn sequence5(
    w: u64,
    c0: char, c1: char, c2: char, c3: char, c4: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match5(w, c0, c1, c2, c3, c4) && terms.push(token, inc_by(p, 5))
}

fn sequence6(
    w: u64,
    c0: char, c1: char, c2: char, c3: char, c4: char, c5: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match6(w, c0, c1, c2, c3, c4, c5) && terms.push(token, inc_by(p, 6))
}

fn sequence7(
    w: u64,
    c0: char, c1: char, c2: char, c3: char, c4: char, c5: char, c6: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match7(w, c0, c1, c2, c3, c4, c5, c6) && terms.push(token, inc_by(p, 7))
}

fn sequence8(
    w: u64,
    c0: char, c1: char, c2: char, c3: char, c4: char, c5: char, c6: char, c7: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match8(w, c0, c1, c2, c3, c4, c5, c6, c7) && terms.push(token, inc_by(p, 8))
}

fn name_continues(w: u64, after_position: u32) -> bool {
    let next = (w >> (u8::BITS * after_position)) & 0xFF;
    if next <= 128 {
        CLASSES[next as usize] & CLASS_IS_NAME_REST != 0
    } else {
        false
    }
}

fn keyword2(
    w: u64,
    c0: char, c1: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match2(w, c0, c1)
        && !name_continues(w, 2)
        && terms.push(token, inc_by(p, 2))
}

fn keyword3(
    w: u64,
    c0: char, c1: char, c2: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match3(w, c0, c1, c2)
        && !name_continues(w, 3)
        && terms.push(token, inc_by(p, 3))
}

fn keyword4(
    w: u64,
    c0: char, c1: char, c2: char, c3: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match4(w, c0, c1, c2, c3)
        && !name_continues(w, 4)
        && terms.push(token, inc_by(p, 4))
}

fn keyword5(
    w: u64,
    c0: char, c1: char, c2: char, c3: char, c4: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match5(w, c0, c1, c2, c3, c4)
        && !name_continues(w, 5)
        && terms.push(token, inc_by(p, 5))
}

fn keyword6(
    w: u64,
    c0: char, c1: char, c2: char, c3: char, c4: char, c5: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match6(w, c0, c1, c2, c3, c4, c5)
        && !name_continues(w, 6)
        && terms.push(token, inc_by(p, 6))
}

fn keyword7(
    w: u64,
    c0: char, c1: char, c2: char, c3: char, c4: char, c5: char, c6: char,
    terms: &mut Terms<Token>, token: Token, p: &mut usize
) -> bool {
    match7(w, c0, c1, c2, c3, c4, c5, c6)
        && !name_continues(w, 7)
        && terms.push(token, inc_by(p, 7))
}

const BYTES_PER_READ: usize = size_of::<u64>();
const MASK_NEWLINE: u64 = as_mask(b'\n');

const CLASS_BASE_MASK: u8 = 0b1111;
const CLASS_DIGIT: u8 = 0b1;
const CLASS_LETTER_LOWERCASE: u8 = 0b100;
const CLASS_LETTER_UPPERCASE: u8 = 0b110;

const CLASS_IS_LETTER: u8 = 0b100;
const CLASS_IN_DIGIT: u8 = 0b1 << 4;
const CLASS_IS_NAME_START: u8 = 0b100 << 4;
const CLASS_IS_NAME_REST: u8 = 0b1000 << 4;

const CLASSES: &[u8; 128] = &compute_classes();

const fn compute_classes() -> [u8; 128] {
    let mut classes: [u8; 128] = [0; 128];

    let mut c = b'A';
    loop {
        classes[c as usize] = CLASS_LETTER_UPPERCASE | CLASS_IS_LETTER | CLASS_IS_NAME_START | CLASS_IS_NAME_REST;
        if c == b'Z' { break }
        c += 1;
    }

    let mut c = b'a';
    loop {
        classes[c as usize] = CLASS_LETTER_LOWERCASE | CLASS_IS_LETTER | CLASS_IS_NAME_START | CLASS_IS_NAME_REST;
        if c == b'z' { break }
        c += 1;
    }

    let mut c = b'0';
    loop {
        classes[c as usize] = CLASS_DIGIT | CLASS_IN_DIGIT | CLASS_IS_NAME_REST;
        if c == b'9' { break }
        c += 1;
    }

    // not really hex, but we allow underscores in numbers, so
    classes[b'_' as usize] = CLASS_IS_NAME_START | CLASS_IS_NAME_REST | CLASS_IN_DIGIT;

    classes
}
