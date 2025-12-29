use parsevm::term::{EitherTerms, HasInfo, Term, TermInfo};
use std::ops::{BitOr, Not};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
#[repr(i16)]
pub enum Token {
    #[default]
    EOF = -1,
    Unrecognized = 0,
    Name = 1,
    Def = 10,
    IntNumber = 20,
    Whitespace = 100,
    LineComment = 108,
    BraceL = 110,
    BraceR = 111,
    BrackL = 112,
    BrackR = 113,
    ParenL = 114,
    ParenR = 115,
    Comma = 116,
    Dot = 117,
    Pipe = 130,
    Lower = 131,
    Greater = 132,
    LowerEquals = 133,
    GreaterEquals = 134,
    Underscore = 138,
    Assign = 139,
    Plus = 140,
    Minus = 141,
    Multiply = 142,
    Divide = 143,
    Modulo = 144,
    Colon = 145,
    Question = 146,
    Tilde = 147,
    Ampersand = 148,
    Exclaim = 149,
    Quadot = 150,
    LogicOr = 151,
    LogicAnd = 152,
    OrElse = 153,
    Increment = 154,
    Decrement = 155,
    Equals = 156,
    NotEquals = 157,
    ArrowLeft = 158,
    ArrowRight = 159,
    RangeInclusive = 170,
    RangeExclusiveBegin = 171,
    RangeExclusiveEnd = 172,
    PlusAssign = 190,
    MinusAssign = 191,
    MultiplyAssign = 192,
    DivideAssign = 193,
    ModuloAssign = 194,
    ColonAssign = 195,
}

impl Term for Token {
    fn should_skip(&self) -> bool {
        *self == Token::Whitespace || *self == Token::LineComment
    }
}

#[derive(Debug, Clone)]
pub struct NeitherToken {
    tokens: Vec<Token>
}

#[derive(Debug, Clone)]
pub struct EitherToken {
    tokens: Vec<Token>
}

impl Not for Token {
    type Output = NeitherToken;

    fn not(self) -> Self::Output {
        NeitherToken { tokens: vec![self] }
    }
}

impl Not for EitherToken {
    type Output = NeitherToken;

    fn not(self) -> Self::Output {
        NeitherToken { tokens: self.tokens }
    }
}

impl EitherTerms for Token {
    type Term = Token;

    fn iter_and_not(self) -> (impl Iterator<Item=Self::Term>, bool) {
        (std::iter::once(self), false)
    }
}

impl EitherTerms for EitherToken {
    type Term = Token;

    fn iter_and_not(self) -> (impl Iterator<Item=Self::Term>, bool) {
        (self.tokens.into_iter(), false)
    }
}

impl EitherTerms for NeitherToken {
    type Term = Token;

    fn iter_and_not(self) -> (impl Iterator<Item=Self::Term>, bool) {
        (self.tokens.into_iter(), true)
    }
}

impl BitOr<Token> for Token {
    type Output = EitherToken;

    fn bitor(self, rhs: Token) -> Self::Output {
        EitherToken {
            tokens: vec![self, rhs]
        }
    }
}

impl BitOr<Token> for EitherToken {
    type Output = EitherToken;

    fn bitor(self, rhs: Token) -> Self::Output {
        let mut ts = self;
        ts.tokens.push(rhs);
        ts
    }
}

pub enum Kind {
    Keyword,
    Identifier,
    Delimiter,
    Operator,
    DelimiterOrOperator,
    Comment,
    Other,
}

fn as_info(code: i16, kind: Kind, name: &'static str, symbol: &'static str) -> TermInfo {
    use Kind::*;
    let kind = match kind {
        Keyword => "Keyword",
        Identifier => "Identifier",
        Delimiter => "Delimiter",
        Operator => "Operator",
        DelimiterOrOperator => "DelimiterOrOperator",
        Comment => "Comment",
        Other => "Other"
    };
    TermInfo { code, kind, name, symbol }
}

impl HasInfo for Token {
    fn info(&self) -> TermInfo {
        use Token::*;
        use Kind::*;

        let code = *self as i16;
        match self {
            EOF => as_info(code, Other, "EOF", "EOF"),
            Unrecognized => as_info(code, Other, "Unrecognized", "_?_"),

            Name => as_info(code, Identifier, "Name", "<name>"),
            Def => as_info(code, Keyword, "Def", "def"),

            IntNumber => as_info(code, Other, "IntNumber", "<number>"),

            Whitespace => as_info(code, Other, "Whitespace", " "),
            LineComment => as_info(code, Comment, "LineComment", "//"),
            BraceL => as_info(code, Delimiter, "BraceL", "{"),
            BraceR => as_info(code, Delimiter, "BraceR", "}"),
            BrackL => as_info(code, DelimiterOrOperator, "BrackL", "["),
            BrackR => as_info(code, DelimiterOrOperator, "BrackR", "]"),
            ParenL => as_info(code, Delimiter, "ParenL", "("),
            ParenR => as_info(code, Delimiter, "ParenR", ")"),
            Comma => as_info(code, Delimiter, "Comma", ","),
            Dot => as_info(code, DelimiterOrOperator, "Dot", "."),
            Pipe => as_info(code, DelimiterOrOperator, "Pipe", "|"),
            Lower => as_info(code, DelimiterOrOperator, "Lower", "<"),
            Greater => as_info(code, DelimiterOrOperator, "Greater", ">"),
            LowerEquals => as_info(code, Operator, "LowerEquals", "<="),
            GreaterEquals => as_info(code, Operator, "GreaterEquals", ">="),
            Underscore => as_info(code, Operator, "Underscore", "_"),
            Assign => as_info(code, Operator, "Assign", "="),
            Plus => as_info(code, Operator, "Plus", "+"),
            Minus => as_info(code, Operator, "Minus", "-"),
            Multiply => as_info(code, Operator, "Multiply", "*"),
            Divide => as_info(code, Operator, "Divide", "/"),
            Modulo => as_info(code, Operator, "Modulo", "%"),
            Colon => as_info(code, DelimiterOrOperator, "Colon", ":"),
            Question => as_info(code, Operator, "Question", "?"),
            Tilde => as_info(code, Operator, "Tilde", "~"),
            Ampersand => as_info(code, Operator, "Ampersand", "&"),
            Exclaim => as_info(code, Operator, "Exclaim", "!"),
            Quadot => as_info(code, DelimiterOrOperator, "Quadot", "::"),
            LogicOr => as_info(code, Operator, "LogicOr", "||"),
            LogicAnd => as_info(code, Operator, "LogicAnd", "&&"),
            OrElse => as_info(code, Operator, "OrElse", "??"),
            Increment => as_info(code, Operator, "Increment", "++"),
            Decrement => as_info(code, Operator, "Decrement", "--"),
            Equals => as_info(code, Operator, "Equals", "=="),
            NotEquals => as_info(code, Operator, "NotEquals", "!="),
            ArrowLeft => as_info(code, DelimiterOrOperator, "ArrowLeft", "<-"),
            ArrowRight => as_info(code, DelimiterOrOperator, "ArrowRight", "->"),
            RangeInclusive => as_info(code, Operator, "RangeInclusive", "..="),
            RangeExclusiveBegin => as_info(code, Operator, "RangeExclusiveBegin", ">.."),
            RangeExclusiveEnd => as_info(code, Operator, "RangeExclusiveEnd", ".."),
            PlusAssign => as_info(code, Operator, "PlusAssign", "+="),
            MinusAssign => as_info(code, Operator, "MinusAssign", "-="),
            MultiplyAssign => as_info(code, Operator, "MultiplyAssign", "*="),
            DivideAssign => as_info(code, Operator, "DivideAssign", "/="),
            ModuloAssign => as_info(code, Operator, "ModuloAssign", "%="),
            ColonAssign => as_info(code, Operator, "ColonAssign", ":="),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ops() {
        assert_eq!(Token::LogicAnd, Token::LogicAnd);
        assert_ne!(Token::LogicAnd, Token::LogicOr);
    }

    #[test]
    fn either_tokens() {
        let (iter, not) = Token::LogicOr.iter_and_not();
        assert_eq!(iter.collect::<Vec<_>>(), &[Token::LogicOr]);
        assert!(!not);

        let (iter, not) = (!Token::NotEquals).iter_and_not();
        assert_eq!(iter.collect::<Vec<_>>(), &[Token::NotEquals]);
        assert!(not);

        let (iter, not) = (Token::LogicAnd | Token::LogicOr | Token::Tilde).iter_and_not();
        assert_eq!(iter.collect::<Vec<_>>(), &[Token::LogicAnd, Token::LogicOr, Token::Tilde]);
        assert!(!not);

        let (iter, not) = (!(Token::LogicAnd | Token::LogicOr)).iter_and_not();
        assert_eq!(iter.collect::<Vec<_>>(), &[Token::LogicAnd, Token::LogicOr]);
        assert!(not);
    }
}
