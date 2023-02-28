use crate::{ast::Expr, ast::Lhs, lexer::Token};
use chumsky::prelude::*;

pub fn expr_parser() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    let expr = recursive(|expr: Recursive<Token, Expr, Simple<Token>>| {
        let val = select! {
            Token::Num(s) => Expr::Num(s.parse().unwrap()),
            Token::Unit => Expr::Unit
        };

        let name: chumsky::primitive::FilterMap<_, Simple<Token>> =
            select! { Token::Var(s) => Lhs::Var(s) };

        let lhs = just(Token::Op("*".to_string()))
            .to(Lhs::DeRef as fn(_) -> _)
            .repeated()
            .then(name)
            .foldr(|op, rhs| op(Box::new(rhs)))
            .map(Expr::Lvalue as fn(_) -> _);

        let atom = val.or(lhs);

        let oppar = atom.or(expr.clone().delimited_by(
            just(Token::Op("(".to_string())),
            just(Token::Op(")".to_string())),
        ));

        let immut_ref = just::<Token, Token, Simple<Token>>(Token::Op("&".to_string()))
            .ignore_then(select! { Token::Var(s) => Expr::Ref(s) });

        let mut_ref = just(Token::Op("&".to_string())).ignore_then(
            just(Token::Mut).ignore_then(select! { Token::Var(s) => Expr::MutRef(s) }),
        );

        let refs = mut_ref.or(immut_ref);

        let unary = just(Token::Op("-".to_string()))
            .to(Expr::Neg as fn(_) -> _)
            .repeated()
            .then(oppar)
            .foldr(|op, rhs| op(Box::new(rhs)));

        let trunary = refs.or(unary);

        let product = trunary
            .clone()
            .then(
                just(Token::Op("*".to_string()))
                    .to(Expr::Mul as fn(_, _) -> _)
                    .or(just(Token::Op("/".to_string())).to(Expr::Div as fn(_, _) -> _))
                    .then(trunary)
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

        let assign = sum
            .clone()
            .then(
                just(Token::Op("=".to_string()))
                    .to(Expr::Assign as fn(_, _) -> _)
                    .then(sum)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let seq = assign
            .clone()
            .clone()
            .then(
                just(Token::Op(";".to_string()))
                    .to(Expr::Seq as fn(_, _) -> _)
                    .then(assign)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let ident = select! { Token::Var(ident) => ident };

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

        seq.or(r#let).or(r#mutlet)
    });

    expr.then_ignore(end())
}
