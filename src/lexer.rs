use chumsky::prelude::*;

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Num(String),
    Op(String),
    Var(String),
    Let,
    Mut,
    In,
    Unit,
}

pub fn lex() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> + Clone {
    let integer = text::int(10).map(Token::Num);

    // let newline = just("\n").or(just("\r\n")).to(Token::Eol);

    let op = one_of("=+-*/()&;")
        .repeated()
        .exactly(1)
        .collect::<String>()
        .map(Token::Op);

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "let" => Token::Let,
        "mut" => Token::Mut,
        "in" => Token::In,
        "unit" => Token::Unit,
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
