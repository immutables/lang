#![allow(non_upper_case_globals)]

use parsevm::term::Terms;
use demolang::lex::tokenize;

mod demolang;

fn main() {
    let source = r#"
    def some_name {
        ! = [1, identifier, 333]
    }
    "#.as_bytes();

    let grammar = demolang::grammar();

    println!("PRINT GRAMMAR OPCODES");
    println!("{}", grammar.show_ops());

    let mut terms = Terms::with_position(0);

    tokenize(&mut terms, source, &mut 0);

    println!("PRINT TERMS");
    println!("{}", terms.show(source));

    match grammar.parse(demolang::declaration, &mut terms) {
        Ok(prods) => {
            println!("PRINT PRODUCTIONS");
            println!("{}", grammar.show_parsed(&prods, &terms, source));
            println!("OK!");

            let ctx = demolang::node::Ctx {};
            let decl = grammar.instantiate(ctx, &prods, &terms, source);

            println!("PRINT PRINT AST");
            println!("{:#?}", decl);
        }
        Err(mismatch) => {
            println!("BAD");

            println!(
                "{}",
                grammar.format_mismatch(&mismatch, None, &terms, source)
            );
        }
    }
}
