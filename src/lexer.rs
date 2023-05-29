use std::error::Error;
use std::fmt;

use chumsky::prelude::*;

use crate::ast::Span;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Num(String),
    Op(String),
    Var(String),
    Let,
    Mut,
    In,
    Unit,
    If,
    Then,
    Else,
}

impl Token {
    pub fn expect(self, tok: Token, span: Span) -> Result<Self, ParseError> {
        if self == tok {
            Ok(self)
        } else {
            Err(ParseError {
                msg: format!("Expected {:?} but instead found {:?}", tok, self),
                span,
            })
        }
    }

    pub fn posh_expect(self, tok: Token, span: Span, message: String) -> Result<Self, ParseError> {
        if self == tok {
            Ok(self)
        } else {
            Err(ParseError { msg: message, span })
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
    pub span: Span,
}

impl ParseError {
    pub fn prettify(&self) -> String {
        format!("At {:?}, ParseError was raised: {}", self.span, self.msg)
    }
    pub fn wrap(&self, burrito: Self) -> Self {
        let innie: String = self
            .prettify()
            .split("\n")
            .into_iter()
            .map(|s| format!("\t{}", s))
            .collect();
        let msg = format!("{}\n{}", burrito.msg, innie);
        ParseError {
            msg,
            span: burrito.span,
        }
    }
}

pub fn lex() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> + Clone {
    let integer = text::int(10).map(Token::Num);

    // let newline = just("\n").or(just("\r\n")).to(Token::Eol);

    let op = one_of("=+-*/()&;<>[],.")
        .repeated()
        .exactly(1)
        .collect::<String>()
        .map(Token::Op);

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "let" => Token::Let,
        "mut" => Token::Mut,
        "in" => Token::In,
        "unit" => Token::Unit,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        _ => Token::Var(ident),
    });

    let token = integer
        // .or(newline)
        .or(op)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let whitespace = just(" ").or(just("\t")).or(just("\n").or(just("\r\n")));

    token
        .map_with_span(|tok, span| (tok, span))
        // .padded_by(comment.repeated())
        .padded_by(whitespace.repeated())
        .repeated()
        .then_ignore(end())
}
