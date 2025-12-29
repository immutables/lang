#![allow(unused)]

use parsevm::grammar::Node;
use std::ops::Range;
use std::str::FromStr;

pub struct Ctx {}

#[derive(Debug, Default)]
pub struct Def {
    pub name: String,
    pub array: Array,
    pub mark: Mark,
}

#[derive(Debug, Default)]
pub struct Name {
    pub name: String
}

impl Node for Name {
    type Context = Ctx;

    fn complete(&mut self, _: &mut Self::Context, source_range: Range<usize>, source: &[u8]) {
        self.name = String::from_utf8_lossy(&source[source_range]).into();
    }
}

impl Node for Def { type Context = Ctx; }

#[derive(Debug, Default)]
pub struct Array {
    pub values: Vec<Value>,
}

impl Node for Array { type Context = Ctx; }

#[derive(Debug, Default)]
pub enum Mark {
    #[default]
    None,
    Exclaim,
    Question,
}

impl Node for Mark { type Context = Ctx; }

#[derive(Debug)]
pub enum Value {
    Reference(Reference),
    Number(Number),
}

impl Node for Value {
    type Context = Ctx;
}

#[derive(Debug, Default)]
pub struct Reference {
    pub name: String,
}

impl Node for Reference {
    type Context = Ctx;

    fn complete(&mut self, _: &mut Self::Context, source_range: Range<usize>, source: &[u8]) {
        self.name = String::from_utf8_lossy(&source[source_range]).into();
    }
}

#[derive(Debug, Default)]
pub struct Number {
    pub number: i64,
}

impl Node for Number {
    type Context = Ctx;

    fn complete(&mut self, _: &mut Self::Context, source_range: Range<usize>, source: &[u8]) {
        let cow = String::from_utf8_lossy(&source[source_range]);
        self.number = i64::from_str(&cow).unwrap();
    }
}
