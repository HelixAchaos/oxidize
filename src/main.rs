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
        Err(lex_errs.into_iter().fold("".to_string(), |acc, e| {
            format!("{}\nLex error: {}", acc, e)
        }))?
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
    let file = &mut File::new(file_name, tokens);

    match typechecker::type_expr(file, spast, &mut gamma, &mut eta, &mut mu, true) {
        Ok(_stast) => {
            // let tast = stast.extract_tast();
            // println!("TAST:\n    {:?}", tast);
            typechecker::cls();
            typechecker::diagnostics(&gamma, &eta, &mu);
            file.reveal(src.len());
            println!("{}", "-".to_string().repeat(80));
            println!("\nType checked! Yipee.");
        }
        Err(e) => {
            typechecker::cls();
            typechecker::diagnostics(&gamma, &eta, &mu);
            file.reveal(e.span.end);
            println!("{}", "-".to_string().repeat(80));
            println!("\n{}", e.prettify(src));
        }
    };

    // let mut vars: Vec<Context> = vec![HashMap::new()];
    // let mut delta: Delta = Delta {
    //     memory: HashMap::new(),
    // };
    // let (c, v) = eval(tast, &mut vars, &mut delta)?;

    // println!("Execution:\n({:?}, {:?})", c, v);
    // println!("Memory:\n{:?}", memory);
    Ok(())
}
