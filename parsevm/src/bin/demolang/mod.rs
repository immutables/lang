//! Sample language for trying out debugging our parser grammar framework

#![allow(non_upper_case_globals)]

use node::*;
use parsevm::grammar::{alias, Grammar};
use parsevm::term::HasInfo;
use parsevm::{choice, group, part, production, transform};
use token::Token;

pub mod lex;
pub mod node;
pub mod token;

production!(declaration, Def, |_| Def::default());
part!(decl_name, Def, Name, |decl, n| decl.name = n.name);
part!(decl_array, Def, Array, |decl, a| decl.array = a);
part!(decl_mark, Def, Mark, |decl, m| decl.mark = m);

production!(name, Name, |_| Name::default());

choice!(mark, Mark);
production!(exclaim, Mark, |_| Mark::Exclaim);
production!(question, Mark, |_| Mark::Question);

production!(array, Array, |_| Array::default());
part!(array_element, Array, Value, |arr, v| arr.values.push(v));

choice!(value, Value);

transform!(to_value_reference, Value, Reference, |r| Value::Reference(r));
production!(reference, Reference, |_| Reference::default());

transform!(to_value_number, Value, Number, |n| Value::Number(n));
production!(number, Number, |_| Number::default());

group!(nls);

#[allow(unused_must_use)]
fn rules() {
    {
        use Token::*;
        // Grammar will panic on any non-mapped alias, aliases for tokens are those &'static str
        // we use in definition interchangeably with Token
        // `enum-iterator` can help here
        for t in [LineComment, Def, Name, IntNumber, Assign, Question, Comma,
            Exclaim, BrackL, BrackR, BraceL, BraceR] {
            alias(t.info().symbol, t);
        }
    }

    declaration
        & ("def", decl_name - name, "{", decl_mark - mark, "=", decl_array - array, "}");

    name
        & Token::Name;

    mark
        | question
        | exclaim;

    question & "?";

    exclaim & "!";

    array
        | ("[", "]")
        | ("[", array_element - value, [..(",", array_element - value)], "]",);

    value | to_value_reference - reference | to_value_number - number;

    reference
        & Token::Name;

    number
        & Token::IntNumber;
}

pub fn grammar() -> Grammar<Token> {
    Grammar::<Token>::with_rules(rules)
}
