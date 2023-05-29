use crate::typechecker::{Eta, File, Gamma, Mu};
use chumsky::Parser;
use std::collections::VecDeque;

mod ast;
mod interpreter;
mod lexer;
mod parser;
mod typechecker;
mod types;

// use chumsky::prelude::*;

fn main() -> Result<(), String> {
    let file_name = std::env::args().nth(1).unwrap();
    let src = std::fs::read_to_string(file_name.clone()).unwrap();

    // println!("{}", src);

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

    let mut tokens = VecDeque::from(tokens);

    println!("token_count = {}", tokens.len());
    typechecker::pause();

    let spast = match parser::expr_parser(&mut tokens) {
        Ok(spast) => spast,
        Err(e) => Err(format!(
            "{}
            contents: {}",
            e.prettify(),
            src.get(e.span).unwrap_or("")
        ))?,
    };

    println!("AST:\n    {:?}", spast);

    typechecker::pause();

    println!("\n\n\n\n\n");

    let mut gamma: Gamma = Gamma::new();
    let mut eta: Eta = Eta::new();
    let mut mu: Mu = Mu::new();

    let tast = typechecker::type_expr(
        &mut File::new(file_name, tokens),
        spast,
        &mut gamma,
        &mut eta,
        &mut mu,
    )?
    .extract_tast();

    println!("TAST:\n    {:?}", tast);

    // let mut vars: Vec<Context> = vec![HashMap::new()];
    // let mut delta: Delta = Delta {
    //     memory: HashMap::new(),
    // };
    // let (c, v) = eval(tast, &mut vars, &mut delta)?;

    // println!("Execution:\n({:?}, {:?})", c, v);
    // println!("Memory:\n{:?}", memory);
    Ok(())
}
