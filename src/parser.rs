use crate::{ast::EExpr, ast::ELhs, lexer::Token};
use chumsky::prelude::*;

pub fn expr_parser() -> impl Parser<Token, EExpr, Error = Simple<Token>> {
    let expr = recursive(|expr: Recursive<Token, EExpr, Simple<Token>>| {
        let val = select! {
            Token::Num(s) => EExpr::Num(s.parse().unwrap()),
            Token::Unit => EExpr::Unit
        };

        let name: chumsky::primitive::FilterMap<_, Simple<Token>> =
            select! { Token::Var(s) => ELhs::Var(s) };

        let lhs = just(Token::Op("*".to_string()))
            .to(ELhs::DeRef as fn(_) -> _)
            .repeated()
            .then(name)
            .foldr(|op, rhs| op(Box::new(rhs)));

        let atom = val.or(lhs.clone().map(EExpr::Lvalue as fn(_) -> _));

        let oppar = atom.or(expr.clone().delimited_by(
            just(Token::Op("(".to_string())),
            just(Token::Op(")".to_string())),
        ));

        let immut_ref = just::<Token, Token, Simple<Token>>(Token::Op("&".to_string()))
            .ignore_then(select! { Token::Var(s) => EExpr::Ref(s) });

        let mut_ref = just(Token::Op("&".to_string())).ignore_then(
            just(Token::Mut).ignore_then(select! { Token::Var(s) => EExpr::MutRef(s) }),
        );

        let refs = mut_ref.or(immut_ref);

        let unary = just(Token::Op("-".to_string()))
            .to(EExpr::Neg as fn(_) -> _)
            .repeated()
            .then(oppar)
            .foldr(|op, rhs| op(Box::new(rhs)));

        let trunary = refs.or(unary);

        let product = trunary
            .clone()
            .then(
                just(Token::Op("*".to_string()))
                    .to(EExpr::Mul as fn(_, _) -> _)
                    .or(just(Token::Op("/".to_string())).to(EExpr::Div as fn(_, _) -> _))
                    .then(trunary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                just(Token::Op("+".to_string()))
                    .to(EExpr::Add as fn(_, _) -> _)
                    .or(just(Token::Op("-".to_string())).to(EExpr::Sub as fn(_, _) -> _))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let cmp = sum
            .clone()
            .then(
                just(Token::Op(">".to_string()))
                    .to(EExpr::Gt as fn(_, _) -> _)
                    .or(just(Token::Op("<".to_string())).to(EExpr::Lt as fn(_, _) -> _))
                    .then(sum)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let ifexpr = cmp.clone().or(just(Token::If)
            .ignore_then(cmp.clone())
            .then_ignore(just(Token::Then))
            .then(cmp.clone())
            .then_ignore(just(Token::Else))
            .then(cmp.clone())
            .map(|((cond, then_expr), else_expr)| {
                EExpr::Cond(Box::new(cond), Box::new(then_expr), Box::new(else_expr))
            }));

        let assign = (lhs
            .then(just(Token::Op("=".to_string())).to(EExpr::Assign as fn(_, _) -> _)))
        .repeated()
        .then(ifexpr)
        .foldr(|(lhs, op), rhs| op(lhs, Box::new(rhs)));

        let seq = assign
            .clone()
            .then(
                just(Token::Op(";".to_string()))
                    .to(EExpr::Seq as fn(_, _) -> _)
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
            .map(|((name, rhs), then)| EExpr::Let {
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
            .map(|((name, rhs), then)| EExpr::MutLet {
                name,
                rhs: Box::new(rhs),
                then: Box::new(then),
            });

        seq.or(r#let).or(r#mutlet)
    });

    expr.then_ignore(end())
}
