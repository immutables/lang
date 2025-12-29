//! This module contains routines and constants similar or which are is what is called
//! SWAR (SIMD WITH A REGISTER) techniques. These are not necessarily implemented in a great way,
//! but I'm still learning (after 21 years of programming experience)

pub const SIGN_BIT_EACH_BYTE: u64 = 0x8080808080808080;
pub const ONE_EACH_BYTE: u64 = 0x0101010101010101;

pub const fn as_mask(b: u8) -> u64 {
    (b as u64) * ONE_EACH_BYTE
}

/// Given the read 64-bit word (here, usually in LE, but it doesn't matter for this fn),
/// Detects if there are occurrences of the character (byte) specified by mask.
/// The result is u64 which might have some bytes not zero-ed. So if the return value
/// is `0`, then there are no occurrences of the character in question. But if it's not zero
/// then the returned pattern may be checked for non-zero bytes, or input u64 could be directly
/// inspected for the bytes you're looking for.
/// The mask is prepared by using [as_mask] `const fn`.
pub fn detect_mask(w: u64, mask: u64) -> u64 {
    let w = w ^ mask;
    // we cast to i64 and back (having the same representation in bits),
    // to allow subtraction and should be no-op in assembly
    (w as i64 - ONE_EACH_BYTE as i64) as u64 & !w & SIGN_BIT_EACH_BYTE
}

// Here we have some attempts to read in u64/longs. If we do this byte-by-byte
// it would be fast enough, but maybe not that great. Fast unaligned access might be cooler,
// but it requires unsafe etc. Then there's byte order, which is LE on most relevant archs now.

const USE_READ_UNALIGNED: bool = cfg!(feature = "unsafe_optimizations");

/// Reads from a slice of bytes one u64 in little endian, if it oversteps the end,
/// there will be 0 bytes, if it at the end or past the end, whole value will be 0.
/// Output pointer should be in theory (LLM says so) as good as returning `-> (u64, usize)`
pub fn read_u64_le(source: &[u8], offset: usize, read: &mut usize) -> u64 {
    if offset + 8 <= source.len() {
        *read = 8;
        if USE_READ_UNALIGNED {
            unsafe {
                // This unsafe block is inline and not extracted because
                // we have a check for source len boundary above.
                // Paired with the check it is considered safe
                let ptr = source.as_ptr().add(offset) as *const u64;
                // from_le should be noop on LE architectures
                u64::from_le(ptr.read_unaligned())
            }
        } else {
            // Manually constructing, should not be that bad either if somehow optimized
            source[offset] as u64
                | (source[offset + 1] as u64) << (1 * 8)
                | (source[offset + 2] as u64) << (2 * 8)
                | (source[offset + 3] as u64) << (3 * 8)
                | (source[offset + 4] as u64) << (4 * 8)
                | (source[offset + 5] as u64) << (5 * 8)
                | (source[offset + 6] as u64) << (6 * 8)
                | (source[offset + 7] as u64) << (7 * 8)
        }
    } else {
        // This slower, but should be only in the end of input,
        // and will be less likely branch
        read_le_boundary(source, offset, read)
    }
}

fn read_le_boundary(source: &[u8], offset: usize, read: &mut usize) -> u64 {
    // simple subtraction can be negative if we far out of boundary, we clamp it
    let r = (source.len() as i64 - offset as i64).clamp(0, 7) as u8;
    *read = r as usize;
    match r {
        1 => source[offset] as u64,
        2 => source[offset] as u64
            | (source[offset + 1] as u64) << (1 * u8::BITS),
        3 => source[offset] as u64
            | (source[offset + 1] as u64) << (1 * u8::BITS)
            | (source[offset + 2] as u64) << (2 * u8::BITS),
        4 => source[offset] as u64
            | (source[offset + 1] as u64) << (1 * u8::BITS)
            | (source[offset + 2] as u64) << (2 * u8::BITS)
            | (source[offset + 3] as u64) << (3 * u8::BITS),
        5 => source[offset] as u64
            | (source[offset + 1] as u64) << (1 * u8::BITS)
            | (source[offset + 2] as u64) << (2 * u8::BITS)
            | (source[offset + 3] as u64) << (3 * u8::BITS)
            | (source[offset + 4] as u64) << (4 * u8::BITS),
        6 => source[offset] as u64
            | (source[offset + 1] as u64) << (1 * u8::BITS)
            | (source[offset + 2] as u64) << (2 * u8::BITS)
            | (source[offset + 3] as u64) << (3 * u8::BITS)
            | (source[offset + 4] as u64) << (4 * u8::BITS)
            | (source[offset + 5] as u64) << (5 * u8::BITS),
        7 => source[offset] as u64
            | (source[offset + 1] as u64) << (1 * u8::BITS)
            | (source[offset + 2] as u64) << (2 * u8::BITS)
            | (source[offset + 3] as u64) << (3 * u8::BITS)
            | (source[offset + 4] as u64) << (4 * u8::BITS)
            | (source[offset + 5] as u64) << (5 * u8::BITS)
            | (source[offset + 6] as u64) << (6 * u8::BITS),
        _ => 0,
    }
}

// The match functions below expect that
// 1) would be inlined
// 2) that most char arguments will be constant
// 3) most will be precalculated at compile time, casts would be no-op etc.

#[inline(always)]
pub const fn match2(w: u64, c0: char, c1: char) -> bool {
    w & 0xFF_FF == (c0 as u64
        | ((c1 as u64) << u8::BITS)
    )
}

#[inline(always)]
pub const fn match3(w: u64, c0: char, c1: char, c2: char) -> bool {
    w & 0xFF_FF_FF == (c0 as u64
        | ((c1 as u64) << u8::BITS)
        | ((c2 as u64) << (u8::BITS * 2))
    )
}

#[inline(always)]
pub const fn match4(w: u64, c0: char, c1: char, c2: char, c3: char) -> bool {
    w & 0xFF_FF_FF_FF == (c0 as u64
        | ((c1 as u64) << u8::BITS)
        | ((c2 as u64) << (u8::BITS * 2))
        | ((c3 as u64) << (u8::BITS * 3))
    )
}

#[inline(always)]
pub const fn match5(w: u64, c0: char, c1: char, c2: char, c3: char, c4: char) -> bool {
    w & 0xFF_FF_FF_FF_FF == (c0 as u64
        | ((c1 as u64) << u8::BITS)
        | ((c2 as u64) << (u8::BITS * 2))
        | ((c3 as u64) << (u8::BITS * 3))
        | ((c4 as u64) << (u8::BITS * 4))
    )
}

#[inline(always)]
pub const fn match6(w: u64, c0: char, c1: char, c2: char, c3: char, c4: char, c5: char) -> bool {
    w & 0xFF_FF_FF_FF_FF_FF == (c0 as u64
        | ((c1 as u64) << u8::BITS)
        | ((c2 as u64) << (u8::BITS * 2))
        | ((c3 as u64) << (u8::BITS * 3))
        | ((c4 as u64) << (u8::BITS * 4))
        | ((c5 as u64) << (u8::BITS * 5))
    )
}

#[inline(always)]
pub const fn match7(w: u64, c0: char, c1: char, c2: char, c3: char, c4: char, c5: char, c6: char) -> bool {
    w & 0xFF_FF_FF_FF_FF_FF_FF == (c0 as u64
        | ((c1 as u64) << u8::BITS)
        | ((c2 as u64) << (u8::BITS * 2))
        | ((c3 as u64) << (u8::BITS * 3))
        | ((c4 as u64) << (u8::BITS * 4))
        | ((c5 as u64) << (u8::BITS * 5))
        | ((c6 as u64) << (u8::BITS * 6))
    )
}

#[inline(always)]
pub const fn match8(w: u64, c0: char, c1: char, c2: char, c3: char, c4: char, c5: char, c6: char, c7: char) -> bool {
    w == (c0 as u64
        | ((c1 as u64) << u8::BITS)
        | ((c2 as u64) << (u8::BITS * 2))
        | ((c3 as u64) << (u8::BITS * 3))
        | ((c4 as u64) << (u8::BITS * 4))
        | ((c5 as u64) << (u8::BITS * 5))
        | ((c6 as u64) << (u8::BITS * 6))
        | ((c7 as u64) << (u8::BITS * 7))
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_u64() {
        let source: &[u8] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
            0xA, 0xB, 0xC, 0xD, 0xE, 0xF, 0x1A, 0x2B, 0x3C, 0x4D, 0x5E, 0x6F];

        let mut read = 0usize;
        assert_eq!(read_u64_le(source, 0, &mut read), 0x07_06_05_04_03_02_01_00);
        assert_eq!(read, 8);
        assert_eq!(read_u64_le(source, 8, &mut read), 0x0F_0E_0D_0C_0B_0A_09_08);
        assert_eq!(read, 8);
        assert_eq!(read_u64_le(source, 4, &mut read), 0x0B_0A_09_08_07_06_05_04);
        assert_eq!(read, 8);
        assert_eq!(read_u64_le(source, 16, &mut read), 0x00_00_6F_5E_4D_3C_2B_1A);
        assert_eq!(read, 6);
        assert_eq!(read_u64_le(source, 17, &mut read), 0x00_00_00_6F_5E_4D_3C_2B);
        assert_eq!(read, 5);
        assert_eq!(read_u64_le(source, 20, &mut read), 0x00_00_00_00_00_00_6F_5E);
        assert_eq!(read, 2);
        assert_eq!(read_u64_le(source, 22, &mut read), 0x00_00_00_00_00_00_00_00);
        assert_eq!(read, 0);
        assert_eq!(read_u64_le(source, 100, &mut read), 0x00_00_00_00_00_00_00_00);
        assert_eq!(read, 0);
    }

    const MASK_LBRACKET: u64 = as_mask(b'[');

    #[test]
    fn char_detection() {
        let mut read = 0usize;
        assert_eq!(detect_mask(read_u64_le("WHATEVER".as_bytes(), 0, &mut read), MASK_LBRACKET), 0);
        assert_eq!(detect_mask(read_u64_le("        ".as_bytes(), 0, &mut read), MASK_LBRACKET), 0);
        assert_eq!(detect_mask(read_u64_le("---$%^#$".as_bytes(), 0, &mut read), MASK_LBRACKET), 0);
        assert_eq!(detect_mask(read_u64_le("  ] ]  ]".as_bytes(), 0, &mut read), MASK_LBRACKET), 0);

        assert_ne!(detect_mask(read_u64_le("    [   ".as_bytes(), 0, &mut read), MASK_LBRACKET), 0);
        assert_ne!(detect_mask(read_u64_le(" [  [  [ ".as_bytes(), 0, &mut read), MASK_LBRACKET), 0);
    }
}
