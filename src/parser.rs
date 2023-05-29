use std::collections::VecDeque;

use crate::ast::{EExpr, ELhs, Spanned};
use crate::lexer::{ParseError, Token};

fn consume(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<Token>, ParseError> {
    println!("consume-{}", tokens.len());
    if let Some(next) = tokens.pop_front() {
        println!("consumed {:?}", tokens.len());
        Ok(next)
    } else {
        Err(ParseError {
            msg: "Unexpected EOF".to_string(),
            span: 0..0,
        })
    }
}

fn metaparse_or<T>(
    tokens: &mut VecDeque<Spanned<Token>>,
    f1: fn(&mut VecDeque<Spanned<Token>>) -> Result<Spanned<T>, ParseError>,
    f2: fn(&mut VecDeque<Spanned<Token>>) -> Result<Spanned<T>, ParseError>,
) -> Result<Spanned<T>, ParseError> {
    println!("metaparse_or");
    let mut tokens_cop = tokens.clone();
    if let Ok(spanned_expr) = f1(&mut tokens_cop) {
        println!("metaparse_or-fst-succ\n");
        tokens.clear();
        tokens.extend(tokens_cop);
        Ok(spanned_expr)
    } else {
        println!("metaparse_or-fst-fail\n");
        f2(tokens)
    }
}

fn macroparse_operator_unary_prefix_het<I, O>(
    tokens: &mut VecDeque<Spanned<Token>>,
    uop_toks: Vec<Token>,
    parse_inside: fn(&mut VecDeque<Spanned<Token>>) -> Result<Spanned<I>, ParseError>,
    ast_uop: fn(Spanned<I>) -> O,
) -> Result<Spanned<O>, ParseError> {
    let init_tok = consume(tokens)?;
    let span_start = init_tok.1.start;
    tokens.push_front(init_tok);
    for uop_tok in uop_toks {
        let (tok, tok_span) = consume(tokens)?;
        tok.expect(uop_tok, span_start..tok_span.end)?;
    }

    let (inner, inner_span) = parse_inside(tokens)?;
    Ok((ast_uop((inner, inner_span.clone())), span_start..inner_span.end))
}

fn macroparse_operator_unary_prefix_hom<T>(
    tokens: &mut VecDeque<Spanned<Token>>,
    uop_toks: Vec<Token>,
    parse_inside: fn(&mut VecDeque<Spanned<Token>>) -> Result<Spanned<T>, ParseError>,
    ast_uop: fn(Box<Spanned<T>>) -> T,
) -> Result<Spanned<T>, ParseError> {
    let init_tok = consume(tokens)?;
    let span_start = init_tok.1.start;
    tokens.push_front(init_tok);
    for uop_tok in uop_toks {
        let (tok, tok_span) = consume(tokens)?;
        tok.expect(uop_tok, span_start..tok_span.end)?;
    }

    let (inner, inner_span) = parse_inside(tokens)?;
    let span_end = inner_span.end;
    Ok((ast_uop(Box::new((inner, inner_span))), span_start..span_end))
}

fn macroparse_operator_binary_infix_hom<T>(
    tokens: &mut VecDeque<Spanned<Token>>,
    bop_tok_funcs: Vec<(Vec<Token>, fn(Box<Spanned<T>>, Box<Spanned<T>>) -> T)>,
    parse_inside: fn(&mut VecDeque<Spanned<Token>>) -> Result<Spanned<T>, ParseError>,
) -> Result<Spanned<T>, ParseError> {
    println!("binar_infix_hom\n");
    let mut spanned_e = parse_inside(tokens)?;
    let span_start = spanned_e.1.start;
    let mut span_end;
    println!("parsed \n");
    while tokens.len() > 0 {
        println!("binop\n");
        if let Some((toks, f)) = bop_tok_funcs
            .iter()
            .find(|(toks, _)| lookahead_match_tokens(tokens, toks).0)
        {
            for _ in 0..toks.len() {
                consume(tokens)?;
            }

            let (e2, e2_span) = parse_inside(tokens)?;
            span_end = e2_span.end;
            spanned_e = (
                f(Box::new(spanned_e), Box::new((e2, e2_span))),
                (span_start..span_end),
            );
        } else {
            break;
        }
    }
    println!("reaced\n");
    Ok(spanned_e)
}

fn lookahead_is_lhs(tokens: &VecDeque<Spanned<Token>>) -> (bool, usize) {
    let mut toks = tokens.clone();
    (parse_lhs(&mut toks).is_ok(), tokens.len() - toks.len())
}

fn lookahead_match_tokens(tokens: &VecDeque<Spanned<Token>>, toks: &Vec<Token>) -> (bool, usize) {
    let matched = toks.iter().enumerate().all(|(i, tok)| match tokens.get(i) {
        Some((next, _)) => next == tok,
        _ => false,
    });
    (matched, if matched { toks.len() } else { 0 })
}

fn lookahead_match_tokens_conj(
    tokens: &VecDeque<Spanned<Token>>,
    f1: fn(&VecDeque<Spanned<Token>>) -> (bool, usize),
    f2: fn(&VecDeque<Spanned<Token>>) -> (bool, usize),
) -> (bool, usize) {
    let mut tokens = tokens.clone();
    let (matched, consumed_count_1) = f1(&tokens);
    tokens.drain(0..consumed_count_1);
    if matched {
        match f2(&tokens) {
            (true, consumed_count_2) => (true, consumed_count_1 + consumed_count_2),
            (false, _) => (false, 0),
        }
    } else {
        (false, 0)
    }
}

fn parse_lhs_name(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<ELhs>, ParseError> {
    println!("lhs_name{:?}\n", tokens);
    let (tok, span) = consume(tokens)?;
    match tok {
        Token::Var(s) => Ok((ELhs::Var(s), span.to_owned())),
        _ => Err(ParseError {
            msg: "Parser, while parsing a variable, expected a name".to_string(),
            span: span.to_owned(),
        }),
    }
}

fn parse_lhs_deref(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<ELhs>, ParseError> {
    println!("lhs_deref\n");
    let (star, star_span) = consume(tokens)?;
    match star {
        Token::Op(op) if op == "*".to_string() => {
            match parse_lhs_name(tokens) {
                Ok((lhs, lhs_span)) => {
                    let span = (star_span.start)..(lhs_span.end);
                    Ok((ELhs::DeRef(Box::new((lhs, lhs_span))), span))
                }
                Err(err) => Err(err.wrap(ParseError { msg: "Parser, while parsing a deref_lhs, expected an lhs after a dereferencing operator".to_string(), span: star_span }))
            }
        }
        _ => Err(ParseError { msg: "expected a dereference operator".to_string(), span: star_span }),
    }
}

fn parse_lhs_index(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<ELhs>, ParseError> {
    println!("lhs_index\n");
    let mut spanned_lhs = parse_lhs_name(tokens)?;
    while lookahead_match_tokens(tokens, &vec![Token::Op(".".to_string())]).0 {
        let (_, dot_span) = consume(tokens)?;
        let (index, index_span) = consume(tokens)?;
        match index {
            Token::Num(n) => {
                let span = (spanned_lhs.1.start)..(index_span.end);
                spanned_lhs = (ELhs::Index(Box::new(spanned_lhs), n.parse().unwrap()), span)
            }
            _ => Err(
                ParseError {
                    msg: "Parser, while parsing a tuple_index, expected a natural literal after a dot operator, as only natural numeric literals are allowed to index tuples.".to_string(),
                    span: dot_span.start..index_span.end
                })?
        }
    }
    Ok(spanned_lhs)
}

fn parse_lhs(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<ELhs>, ParseError> {
    if lookahead_match_tokens(tokens, &vec![Token::Op("*".to_string())]).0 {
        parse_lhs_deref(tokens)
    } else {
        // `parse_lhs_index` should handle `parse_lhs_name` as well
        parse_lhs_index(tokens)
    }
}

fn parse_expr_val(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("val\n");
    let (val, val_span) = consume(tokens)?;

    match val {
        Token::Unit => Ok((EExpr::Unit, val_span)),
        Token::Num(n) => Ok((EExpr::Num(n.parse().unwrap()), val_span)),
        Token::True => Ok((EExpr::Bool(true), val_span)),
        Token::False => Ok((EExpr::Bool(false), val_span)),
        _ => Err(ParseError {
            msg: "Parser expected a unit literal or a numeric literal.".to_string(),
            span: val_span,
        })?,
    }
}

fn parse_expr_lvalue(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("lvalue\n");
    let (lhs, lhs_span) = parse_lhs(tokens)?;
    Ok((EExpr::Lvalue((lhs, lhs_span.clone())), lhs_span))
}

fn parse_expr_atom(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("atom\n");
    metaparse_or(tokens, parse_expr_val, parse_expr_lvalue)
}

fn parse_expr_oppar(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("oppar\n");
    let (oppar, oppar_span) = consume(tokens)?;
    match oppar {
        Token::Op(op) if op == "(".to_string() => {
            let (expr, _) = parse_expr(tokens)?;
            let (cloppar, cloppar_span) = consume(tokens)?;
            let span = (oppar_span.start)..(cloppar_span.end);
            match cloppar {
                Token::Op(op) if op == ")".to_string() => Ok((expr, span)),
                _ => Err(ParseError {
                    msg: "Expected to find the mate of an open open parenthesis".to_string(),
                    span,
                }),
            }
        }
        // the first token, oppar, should be an atom in the wild case
        _ => Err(ParseError {
            msg: "expected an oppar".to_string(),
            span: oppar_span,
        }),
    }
}

fn parse_expr_immut_ref(
    tokens: &mut VecDeque<Spanned<Token>>,
) -> Result<Spanned<EExpr>, ParseError> {
    println!("immut ref\n");
    macroparse_operator_unary_prefix_het(
        tokens,
        vec![Token::Op("&".to_string())],
        parse_lhs,
        EExpr::Ref,
    )
}

fn parse_expr_mut_ref(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("mut ref\n");
    macroparse_operator_unary_prefix_het(
        tokens,
        vec![Token::Op("&".to_string()), Token::Mut],
        parse_lhs,
        EExpr::MutRef,
    )
}

fn parse_expr_mut_uminus(
    tokens: &mut VecDeque<Spanned<Token>>,
) -> Result<Spanned<EExpr>, ParseError> {
    println!("uminus\n");
    macroparse_operator_unary_prefix_hom(
        tokens,
        vec![Token::Op("-".to_string())],
        |tokens| metaparse_or(tokens, parse_expr_oppar, parse_expr_atom),
        EExpr::Neg,
    )
}

fn parse_expr_unary(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("unary\n");
    if lookahead_match_tokens(tokens, &vec![Token::Op("&".to_string()), Token::Mut]).0 {
        parse_expr_mut_ref(tokens)
    } else if lookahead_match_tokens(tokens, &vec![Token::Op("&".to_string())]).0 {
        parse_expr_immut_ref(tokens)
    } else if lookahead_match_tokens(tokens, &vec![Token::Op("(".to_string())]).0 {
        parse_expr_oppar(tokens)
    } else {
        metaparse_or(tokens, parse_expr_mut_uminus, parse_expr_atom)
    }
}

fn parse_expr_product(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("product\n");
    macroparse_operator_binary_infix_hom(
        tokens,
        vec![
            (vec![Token::Op("*".to_string())], EExpr::Mul),
            (vec![Token::Op("/".to_string())], EExpr::Div),
        ],
        parse_expr_unary,
    )
}

fn parse_expr_sum(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("sum\n");
    macroparse_operator_binary_infix_hom(
        tokens,
        vec![
            (vec![Token::Op("+".to_string())], EExpr::Add),
            (vec![Token::Op("-".to_string())], EExpr::Sub),
        ],
        parse_expr_product,
    )
}

fn parse_expr_cmp(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("cmp\n");
    macroparse_operator_binary_infix_hom(
        tokens,
        vec![
            (vec![Token::Op("<".to_string())], EExpr::Lt),
            (vec![Token::Op(">".to_string())], EExpr::Gt),
        ],
        parse_expr_sum,
    )
}

fn parse_expr_cond(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("cond\n");
    let (if_tok, if_span) = consume(tokens)?;
    match if_tok {
        Token::If => Ok(()),
        _ => Err(ParseError {msg: "While parsing a conditional expression, the parser expected an `if` token to be at the start".to_string(), span: if_span})
    }?;

    let spanned_e1 = parse_expr_cmp(tokens)?;

    let (then_tok, then_span) = consume(tokens)?;
    match then_tok {
        Token::Then => Ok(()),
        _ => Err(ParseError {
            msg: "While parsing a conditional expression, the parser expected a `then` token"
                .to_string(),
            span: then_span,
        }),
    }?;

    let spanned_e2 = parse_expr_cmp(tokens)?;

    let (else_tok, else_span) = consume(tokens)?;
    match else_tok {
        Token::Else => Ok(()),
        _ => Err(ParseError {
            msg: "While parsing a conditional expression, the parser expected an `else` token"
                .to_string(),
            span: else_span,
        }),
    }?;

    let spanned_e3 = parse_expr_cmp(tokens)?;

    let span = (spanned_e1.1.start)..(spanned_e3.1.end);

    Ok((
        EExpr::Cond(
            Box::new(spanned_e1),
            Box::new(spanned_e2),
            Box::new(spanned_e3),
        ),
        span,
    ))
}

fn parse_expr_tuple(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("tuple\n");
    let (obbar_tok, obbar_span) = consume(tokens)?;
    let span_start = obbar_span.start;
    let mut span_end = obbar_span.end;
    obbar_tok.posh_expect(Token::Op("[".to_string()), obbar_span, "While parsing a conditional expression, the parser expected an `[` token to be at the start".to_string())?;

    println!("tuple-1\n");
    let exprs: &mut Vec<Spanned<EExpr>> = &mut vec![];
    while tokens.len() > 0 {
        println!("tuple-element\n");
        let (tok, tok_span) = consume(tokens)?;

        if tok == Token::Op("]".to_string()) {
            span_end = tok_span.end;
            break;
        }

        tokens.push_front((tok, tok_span));

        exprs.push(parse_expr_cmp(tokens)?);

        let (comma, comma_span) = consume(tokens)?;
        // duplicated to allow trailing comma and to allow separation of elements by commas
        if comma == Token::Op("]".to_string()) {
            span_end = comma_span.end;
            break;
        } else if comma != Token::Op(",".to_string()) {
            Err(ParseError {
                msg: "Expected a comma".to_string(),
                span: comma_span.to_owned(),
            })?
        }
    }
    println!("tuple-end\n");
    Ok((EExpr::Tuple(exprs.clone()), span_start..span_end))
}

fn parse_expr_normal(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("normal\n");
    if lookahead_match_tokens(tokens, &vec![Token::Op("[".to_string())]).0 {
        parse_expr_tuple(tokens)
    } else if lookahead_match_tokens(tokens, &vec![Token::If]).0 {
        parse_expr_cond(tokens)
    } else {
        parse_expr_cmp(tokens)
    }
}

fn parse_expr_assign(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    let targets: &mut Vec<Spanned<ELhs>> = &mut vec![];
    println!("assign {:?}\n", tokens);

    println!("lookahead_is_lhs(tokens) = {:?}", lookahead_is_lhs(tokens));
    println!(
        "lookahead_match_tokens(tokens, '=') = {:?}",
        lookahead_match_tokens(tokens, &vec![Token::Op("=".to_string())])
    );

    while lookahead_match_tokens_conj(tokens, lookahead_is_lhs, |tokens| {
        lookahead_match_tokens(tokens, &vec![Token::Op("=".to_string())])
    })
    .0
    {
        println!("lhs assign\n");
        let spanned_lhs = parse_lhs(tokens)?;
        let (eq, eq_span) = consume(tokens)?;
        eq.expect(Token::Op("=".to_string()), eq_span)?;
        targets.push(spanned_lhs);
    }

    println!("assign--------------------------");
    let spanned_e = parse_expr_normal(tokens)?;
    println!("assign--------------spanned_e; {:?}", spanned_e);
    Ok(targets
        .into_iter()
        .rfold(spanned_e, |acc, (lhs, lhs_span)| {
            let span = (lhs_span.start)..(acc.1.end);
            (EExpr::Assign((lhs.to_owned(), lhs_span.to_owned()), Box::new(acc)), span)
        }))
}

fn parse_expr_seq(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("parse_expr_seq\n");
    macroparse_operator_binary_infix_hom(
        tokens,
        vec![(vec![Token::Op(";".to_string())], EExpr::Seq)],
        |tokens| metaparse_or(tokens, parse_expr_assign, parse_expr_normal),
    )
}

fn parse_ident(tokens: &mut VecDeque<Spanned<Token>>) -> Result<String, ParseError> {
    match consume(tokens)? {
        (Token::Var(ident), _) => Ok(ident),
        (_, span) => Err(ParseError {
            msg: "Expected identifier".to_string(),
            span,
        }),
    }
}

fn parse_expr_let(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("let\n");
    let spanned_let = consume(tokens)?;
    let start = spanned_let.1.start;
    spanned_let.0.expect(Token::Let, spanned_let.1)?;
    println!("let{}\n", tokens.len());

    let name = parse_ident(tokens)?;
    println!("let{}\n", tokens.len());

    let spanned_eq = consume(tokens)?;
    spanned_eq
        .0
        .expect(Token::Op("=".to_string()), spanned_eq.1)?;

    let rhs = parse_expr_normal(tokens)?;
    println!("let-then-rhs: {:?}\n", rhs);

    let spanned_in = consume(tokens)?;
    spanned_in.0.expect(Token::In, spanned_in.1)?;

    println!("let-then-expr\n");
    let then = parse_expr(tokens)?;

    let span = (start)..(then.1.end);
    Ok((
        EExpr::Let {
            name,
            rhs: Box::new(rhs),
            then: Box::new(then),
        },
        span,
    ))
}

fn parse_expr_mutlet(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("mutlet\n");
    let spanned_let = consume(tokens)?;
    let start = spanned_let.1.start;
    spanned_let.0.expect(Token::Let, spanned_let.1)?;
    let spanned_mut = consume(tokens)?;
    spanned_mut.0.expect(Token::Mut, spanned_mut.1)?;

    let name = parse_ident(tokens)?;

    let spanned_eq = consume(tokens)?;
    spanned_eq
        .0
        .expect(Token::Op("=".to_string()), spanned_eq.1)?;

    let rhs = parse_expr_normal(tokens)?;

    let spanned_in = consume(tokens)?;
    spanned_in.0.expect(Token::In, spanned_in.1)?;

    let then = parse_expr(tokens)?;

    let span = (start)..(then.1.end);
    Ok((
        EExpr::MutLet {
            name,
            rhs: Box::new(rhs),
            then: Box::new(then),
        },
        span,
    ))
}

pub fn parse_expr(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    println!("parse_expr\n");
    if lookahead_match_tokens(tokens, &vec![Token::Let, Token::Mut]).0 {
        parse_expr_mutlet(tokens)
    } else if lookahead_match_tokens(tokens, &vec![Token::Let]).0 {
        parse_expr_let(tokens)
    } else {
        parse_expr_seq(tokens)
    }
}

pub fn expr_parser(tokens: &mut VecDeque<Spanned<Token>>) -> Result<Spanned<EExpr>, ParseError> {
    let spanned_e = parse_expr(tokens)?;
    if let Some(spanned_end) = tokens.pop_back() {
        Err(ParseError {
            msg: "Expected EOF".to_string(),
            span: (spanned_e.1.end)..(spanned_end.1.end),
        })
    } else {
        Ok(spanned_e)
    }
}
