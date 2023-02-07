use crate::{ast::Expr, lexer::Token};
use chumsky::{chain::Chain, prelude::*};

pub fn expr_parser() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    let expr = recursive(|expr: Recursive<Token, Expr, Simple<Token>>| {
        let atom: chumsky::primitive::FilterMap<_, Simple<Token>> = select! {
            Token::Num(s) => Expr::Num(s.parse().unwrap()),
            Token::Var(s) => Expr::Var(s)
        };

        let oppar = atom.or(expr.clone().delimited_by(
            just(Token::Op("(".to_string())),
            just(Token::Op(")".to_string())),
        ));

        let unary = just(Token::Op("-".to_string()))
            .to(Expr::Neg as fn(_) -> _)
            .or(just(Token::Op("&".to_string()))
                .ignore_then(just(Token::Mut))
                .to(Expr::MutRef as fn(_) -> _))
            .or(just(Token::Op("&".to_string())).to(Expr::Ref as fn(_) -> _))
            .or(just(Token::Op("*".to_string())).to(Expr::DeRef as fn(_) -> _))
            .repeated()
            .then(oppar)
            .foldr(|op, rhs| op(Box::new(rhs)));

        let product = unary
            .clone()
            .then(
                just(Token::Op("*".to_string()))
                    .to(Expr::Mul as fn(_, _) -> _)
                    .or(just(Token::Op("/".to_string())).to(Expr::Div as fn(_, _) -> _))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                just(Token::Op("+".to_string()))
                    .to(Expr::Add as fn(_, _) -> _)
                    .or(just(Token::Op("-".to_string())).to(Expr::Sub as fn(_, _) -> _))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let seq = sum
            .clone()
            .then(
                just(Token::Op(";".to_string()))
                    .to(Expr::Seq as fn(_, _) -> _)
                    .then(sum)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let ident = select! { Token::Var(ident) => ident };
        let r#mutlet = just(Token::Let)
            .ignore_then(just(Token::Mut))
            .ignore_then(ident)
            .then_ignore(just(Token::Op("=".to_string())))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((name, rhs), then)| Expr::MutLet {
                name,
                rhs: Box::new(rhs),
                then: Box::new(then),
            });

        let r#let = just(Token::Let)
            .ignore_then(ident)
            .then_ignore(just(Token::Op("=".to_string())))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((name, rhs), then)| Expr::Let {
                name,
                rhs: Box::new(rhs),
                then: Box::new(then),
            });

        let r#assign = just(Token::Op("*".to_string()))
            .repeated()
            .then(ident)
            .then_ignore(just(Token::Op("=".to_string())))
            .then(expr.clone())
            .map(|((derefs, name), rhs)| Expr::Assign {
                deref_count: derefs.len(),
                name,
                rhs: Box::new(rhs),
            });

        r#mutlet.or(r#let).or(r#assign).or(seq)
    });

    expr.then_ignore(end())
}
