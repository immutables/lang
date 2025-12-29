# ParseVM

A PEG-like parser framework with virtual machine-style grammar matching, designed for handwritten lexers.

## Features

- Declarative grammar DSL using Rust operator overloading
- Type-safe AST construction via the `Node` trait
- Virtual machine -like grammar matching
- SWAR (SIMD Within A Register) optimized lexer just as a demo
- Error reporting with annotate-snippets
- Can print grammar opcodes, raw terms and raw parsed production codes.

## Quick Example

```rust
use parsevm::grammar::{Grammar, Node};
use parsevm::term::Terms;
use parsevm::{production, choice, part, transform};

// Define your tokens
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Token {
    Name,
    Number,
    // ... more tokens
}

// Define AST nodes
#[derive(Debug, Default)]
pub struct Declaration {
    pub name: String,
    pub value: i64,
}

impl Node for Declaration {
    type Context = ();
}

// Declare productions
production!(declaration, Declaration, |_| Declaration::default());

// Define grammar rules
fn rules() {
    alias("=", Token::Assign);
  
    declaration
        & (Token::Name, "=", Token::Number);
}

// Create and use grammar
let grammar = Grammar::<Token>::with_rules(rules);
let mut terms = Terms::with_position(0);

// Tokenize it "manually"
tokenize(&mut terms, source, &mut 0);

let result = grammar.parse(declaration, &terms);


```

## Grammar Syntax Cheatsheet

### Production Declarations

```rust
production!(name);                           // Unit production (no node type)
production!(name, MyNode, |ctx| MyNode::new()); // Typed production with constructor

choice!(name, MyNode);                       // Abstract choice production
part!(field_name, Parent, Child, |p, c| p.child = c); // Attach "labeled" part
transform!(name, ToNode, FromNode, |f| ToNode::from(f)); // Transform node
group!(name);                                // Reusable sequence group
```

### Grammar Rules

#### Alternatives

```rust
name                    // Single alternative
    & sequence;              
name                    // Multiple alternatives
    | alternative1
    | alternative2;  
```

#### Sequences

```rust
production
    & ("keyword", Token::Name);       // Tuple sequence
production
    & single_item;                    // Single item
```

#### Quantifiers

```rust
item                                         // Exactly one, 1..1
item..                                       // One or more, 1..N
..item                                       // One or more (prefix), 1..N
[item]                                       // Optional wraps single, 0..1
[..item]                                     // Optional wraps many, 0..N
!production                                  // Not match, works on productions, groups, choices
```

#### Labeled Parts

```rust
part_label - production                      // Attach sub-production to parent
```

#### Transforms

```rust
transform_label - production                 // Transform production result
```

#### Token Matching

```rust
Token::Name                                  // Match specific token
"keyword"                                    // Match by string alias (registered via `alias(&str, term)`)
(or | "," | ":")                             // Either token
(!or | "," | ":")                            // Neither token

// These are working only if you implement stuff akin to demolang::token::Token, manual
Token::A | Token::B                          // Match any of these tokens
!Token::Forbidden                            // NOT this token
```

#### Special Keywords

```rust
sure                                         // No backtrack point (commit)
or                                           // Prefix for inline alternatives
```

### Inline Alternatives

```rust
production & (or | "option1" | "option2")    // when alias is used or when prod
production & (or | "option1" | "option2")    // when alias is used or when prod
production & (Token::A | Token::B)           // Token alternatives, only
```

## Complete Example

See `parsevm/src/bin/demolang/` for a complete example language that parses:

```
def some_name {
    ! = [1, identifier, 333]
}
```

### Key Files

- `token.rs` - Token enum with `Term` trait implementation
- `lex.rs` - demo lexer, kinda "SWAR-optimized", but only as a showcase
- `node.rs` - AST node definitions with `Node` trait
- `mod.rs` - Grammar rules definition
- `demo.rs` - binary, with usage example

### Grammar Rules Example

```rust
// Register token aliases, this makes grammar more 
alias("def", Token::Def);
alias("=", Token::Assign);
// You can use `enum_iterator`, for example, to register all by TermInfo::symbol
for t in all::<Token>() {
    alias(t.info().symbol, t);
}

// Define production rules
declaration & ("def", name, "{", mark, "=", array, "}");

// Same as above, but with labeled parts being attached to parent
declaration
    & ("def", decl_name - name, "{", decl_mark - mark, "=", decl_array - array, "}");

// Choice with alternatives
mark
    | question
    | exclaim;

question & "?";

exclaim & "!";

// Arrays with optional elements
array
    | ("[", "]")
    | ("[", value, [..(",", value)], "]");

// Transforms convert node types
value | to_value_reference - reference
      | to_value_number - number;
```

## API Usage

### 1. Define Tokens

```rust
impl Term for MyToken {
    fn should_skip(&self) -> bool {
        // Return true for whitespace/comments
    }
}

impl HasInfo for MyToken {
    fn info(&self) -> TermInfo {
        // Provide token metadata for error messages
    }
}
```

### 2. Tokenize Source

```rust
let mut terms = Terms::with_position(0);
// tokenize is DIY function
tokenize(&mut terms, source, &mut 0);
```

### 3. Parse

```rust
match grammar.parse(my_production, &mut terms) {
    Ok(productions) => {
        // Success - instantiate AST
        let ast = grammar.instantiate(context, &productions, &terms, source);
    }
    Err(mismatch) => {
        // Error - format and display
        let error = grammar.format_mismatch(&mismatch, None, &terms, source);
        eprintln!("{}", error);
    }
}
```

### 4. Inspect Results

```rust
// Show compiled grammar operations
println!("{}", grammar.show_ops());

// Show parsed production tree
println!("{}", grammar.show_parsed(&productions, &terms, source));
```

See example printouts at the end of this readme.

## Node Trait

Implement `Node` for your AST types:

```rust
impl Node for MyNode {
    type Context = MyContext; // Need to define and use some context struct, same for all nodes

    // Optional fn
    fn complete(&mut self, context: &mut Self::Context, source_range: Range<usize>, source: &[u8]) {
        // Extract data from source when node is complete
        self.text = String::from_utf8_lossy(&source[source_range]).into();
    }
}
```

## Grammar Construction Flow

1. Call `Grammar::with_rules(rules_fn)` - initializes thread-local grammar setup
2. `rules_fn` executes - registers productions, parts, transforms via operator overloading
3. Grammar compiles definitions into VM operations
4. Returns ready-to-use `Grammar<Token>` instance

This uses hidden thread-local during rules_fn execution,
but after grammar created it is fully on its own.

## Error Reporting

ParseVM uses `annotate-snippets` for nice error messages:

```
error[syntax]: stumbled on unexpected terms, while expecting '{' (BraceL) in `declaration`
   |
 3 |     def some_name !
   |                   ^ Not expected
```

## License

Apache-2.0

## Repository

https://github.com/immutables/lang


## Appendix A

Example printout including debug information. For grammar, terms, productions and the result

```
PRINT GRAMMAR OPCODES

   0| head
   1| defn 'declaration' #3 Production
   2| next --
   3| term 'def' Def[10]
   4| prod :13 >> 'name' #4 Production
   5| term '{' BraceL[110]
   6| part 'decl_mark' #1
   7| prod :17 >> 'mark' #5 Choice
   8| term '=' Assign[139]
   9| part 'decl_array' #2
  10| prod :32 >> 'array' #8 Production
  11| term '}' BraceR[111]
  12| done
  13| defn 'name' #4 Production
  14| next --
  15| term '<name>' Name[1]
  16| done
  17| defn 'mark' #5 Choice
  18| next :21
  19| prod :24 >> 'question' #6 Production
  20| done
  21| next --
  22| prod :28 >> 'exclaim' #7 Production
  23| done
  24| defn 'question' #6 Production
  25| next --
  26| term '?' Question[146]
  27| done
  28| defn 'exclaim' #7 Production
  29| next --
  30| term '!' Exclaim[149]
  31| done
  32| defn 'array' #8 Production
  33| next :37
  34| term '[' BrackL[112]
  35| term ']' BrackR[113]
  36| done
  37| next --
  38| term '[' BrackL[112]
  39| part 'array_element' #9
  40| prod :44 >> 'value' #11 Choice
  41| prod* :61 >> '<uninlined>' #15 Uninlined
  42| term ']' BrackR[113]
  43| done
  44| defn 'value' #11 Choice
  45| next :49
  46| tfrm 'to_value_reference' #10
  47| prod :53 >> 'reference' #13 Production
  48| done
  49| next --
  50| tfrm 'to_value_number' #12
  51| prod :57 >> 'number' #14 Production
  52| done
  53| defn 'reference' #13 Production
  54| next --
  55| term '<name>' Name[1]
  56| done
  57| defn 'number' #14 Production
  58| next --
  59| term '<number>' IntNumber[20]
  60| done
  61| defn '<uninlined>' #15 Uninlined
  62| next --
  63| term ',' Comma[116]
  64| part 'array_element' #9
  65| prod :44 >> 'value' #11 Choice
  66| done

PRINT TERMS

0000–0005 +0005| Whitespace <Other>                       `⏎
               |                                              `
0005–0008 +0003| Def <Keyword>                            `def`
0008–0009 +0001| Whitespace <Other>                       ` `
0009–0012 +0009| Name <Identifier>                        `some_name`
0012–0013 +0001| Whitespace <Other>                       ` `
0013–0014 +0001| BraceL <Delimiter>                       `{`
0014–001d +0009| Whitespace <Other>                       `⏎
               |                                                  `
001d–001e +0001| Exclaim <Operator>                       `!`
001e–001f +0001| Whitespace <Other>                       ` `
001f–0020 +0001| Assign <Operator>                        `=`
0020–0021 +0001| Whitespace <Other>                       ` `
0021–0022 +0001| BrackL <DelimiterOrOperator>             `[`
0022–0023 +0001| IntNumber <Other>                        `1`
0023–0024 +0001| Comma <Delimiter>                        `,`
0024–0025 +0001| Whitespace <Other>                       ` `
0025–002f +000a| Name <Identifier>                        `identifier`
002f–0030 +0001| Comma <Delimiter>                        `,`
0030–0031 +0001| Whitespace <Other>                       ` `
0031–0034 +0003| IntNumber <Other>                        `333`
0034–0035 +0001| BrackR <DelimiterOrOperator>             `]`
0035–003a +0005| Whitespace <Other>                       `⏎
               |                                              `
003a–003b +0001| BraceR <Delimiter>                       `}`
003b–0040 +0005| Whitespace <Other>                       `⏎
               |                                              `

PRINT PRODUCTIONS

0000–0007 +0007| declaration                              |def some_name {⏎
               |                                          |        ! = [1, identifier, 333]⏎
               |                                          |    }⌜
0001–0002 +0001| decl_name: name                          |some_name
0002–0003 +0001| decl_mark: exclaim                       |!
0003–0007 +0004| decl_array: array                        |[1, identifier, 333]
0004–0005 +0001| array_element: to_value_number - number  |1
0005–0006 +0001| array_element: to_value_reference - reference |identifier
0006–0007 +0001| array_element: to_value_number - number  |333

OK!

PRINT AST

Def {
    name: "some_name",
    array: Array {
        values: [
            Number(
                Number {
                    number: 1,
                },
            ),
            Reference(
                Reference {
                    name: "identifier",
                },
            ),
            Number(
                Number {
                    number: 333,
                },
            ),
        ],
    },
    mark: Exclaim,
}

```
