use chumsky::{Parser, Stream};

mod ast;
mod interpreter;
mod lexer;
mod parser;

// use chumsky::prelude::*;

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let (tokens, lex_errs) = lexer::lex().parse_recovery(src.clone());
    if lex_errs.is_empty() {
        if let Some(tokens) = tokens {
            let len = src.chars().count();

            let (ast, parse_errs) = parser::expr_parser()
                .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

            if !parse_errs.is_empty() {
                parse_errs
                    .into_iter()
                    .for_each(|e| println!("Parse error: {:?}", e))
            } else {
                println!("{:#?}", ast)
            }
        } else {
        }
    } else {
        lex_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {}", e))
    }

    // match interpreter::eval(&ast, &mut Vec::new()) {
    //     Ok(output) => println!("{}", output),
    //     Err(eval_err) => println!("Evaluation error: {}", eval_err),
    // }
}
