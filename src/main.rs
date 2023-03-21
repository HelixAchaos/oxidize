use std::collections::HashMap;

use crate::interpreter::{eval, Context, Memory};
use crate::typechecker::Gamma;
use chumsky::{Parser, Stream};

mod ast;
mod interpreter;
mod lexer;
mod parser;
mod typechecker;
mod types;

// use chumsky::prelude::*;

fn main() -> Result<(), String> {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    println!("{}", src);

    let (tokens, lex_errs) = lexer::lex().parse_recovery(src.clone());
    let tokens = if lex_errs.is_empty() {
        if let Some(tokens) = tokens {
            tokens
        } else {
            todo!("What should empty file evaluate to? Unit? Error?")
        }
    } else {
        lex_errs.into_iter().fold("".to_string(), |acc, e| {
            format!("{}\nLex error: {}", acc, e)
        });
        Err("".to_string())?
    };

    let len = src.chars().count();

    let (ast, parse_errs) =
        parser::expr_parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

    let ast = if parse_errs.is_empty() {
        if let Some(ast) = ast {
            ast
        } else {
            todo!("I don't know how this would be possible.")
        }
    } else {
        Err(parse_errs.into_iter().fold("".to_string(), |acc, e| {
            format!("{}\nParse error: {:?}", acc, e)
        }))?
    };

    println!("AST:\n    {:?}", ast);

    let mut gamma: Gamma = Gamma { vars: [].to_vec() };
    let tast = typechecker::type_expr(&ast, &mut gamma)?;

    println!("TAST:\n    {:?}", tast);
    let mut vars: Vec<Context> = [HashMap::new()].to_vec();
    let mut memory: Memory = HashMap::new();
    let (c, v) = eval(&tast, &mut vars, &mut memory)?;

    println!("Execution:\n({:?}, {:?})", c, v);
    // println!("Memory:\n{:?}", memory);
    Ok(())
}
