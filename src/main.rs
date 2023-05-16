use std::collections::HashMap;

// use crate::interpreter::{eval, Context, Delta};
use crate::typechecker::{Eta, File, Gamma, Mu};
// use crate::types::Type;
use chumsky::{Parser, Stream};

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

    // let len = src.chars().count();

    // let (ast, parse_errs) =
    //     parser::expr_parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

    // let ast = if parse_errs.is_empty() {
    //     if let Some(ast) = ast {
    //         ast
    //     } else {
    //         todo!("I don't know how this would be possible.")
    //     }
    // } else {
    //     Err(parse_errs.into_iter().fold("".to_string(), |acc, e| {
    //         format!("{}\nParse error: {:?}", acc, e)
    //     }))?
    // };
    // let ast = ast.0;

    let ast = ast::EExpr::Let {
        name: "a".to_string(),
        rhs: Box::new((
            ast::EExpr::Tuple(vec![
                (ast::EExpr::Num(1), 9..10),
                (ast::EExpr::Num(2), 12..13),
            ]),
            8..14,
        )),
        then: Box::new((
            ast::EExpr::MutLet {
                name: "b".to_string(),
                rhs: Box::new((
                    ast::EExpr::Tuple(vec![
                        (ast::EExpr::Num(1), 35..36),
                        (ast::EExpr::Num(4), 38..39),
                    ]),
                    34..40,
                )),
                then: Box::new((
                    ast::EExpr::Let {
                        name: "z".to_string(),
                        rhs: Box::new((
                            ast::EExpr::Lvalue(ast::ELhs::Index(
                                Box::new((ast::ELhs::Var("a".to_string()), 60..63)),
                                0,
                            )),
                            60..63,
                        )),
                        then: Box::new((
                            ast::EExpr::Seq(
                                Box::new((
                                    ast::EExpr::Assign(
                                        ast::ELhs::Var("b".to_string()),
                                        Box::new((
                                            ast::EExpr::Lvalue(ast::ELhs::Var("b".to_string())),
                                            83..84,
                                        )),
                                    ),
                                    79..84,
                                )),
                                Box::new((
                                    ast::EExpr::Lvalue(ast::ELhs::Var("b".to_string())),
                                    86..87,
                                )),
                            ),
                            79..87,
                        )),
                    },
                    52..87,
                )),
            },
            22..87,
        )),
    };

    // let a = [1, 2] in
    // let mut b = [1, 4] in
    //     let z = a.0 in
    //         b = a; b

    println!("AST:\n    {:?}", ast);

    println!("\n\n\n\n\n");

    let mut gamma: Gamma = Gamma { vars: Vec::new() };
    let mut eta: Eta = Eta {
        loans: HashMap::new(),
    };
    let mut mu: Mu = Mu::new();

    let tast = typechecker::type_expr(
        &mut File::new(file_name, tokens),
        (ast, 0..87),
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
