use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::fs;
use std::iter::{self, once};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{EExpr, ELhs, STExpr, Span, Spanned, TLhs};
use crate::lexer;
use crate::types::{Address, Type, TypeError, Var, S};

type ColoredType = (bool, Type);
type TypeContext = HashMap<Var, ColoredType>;

#[derive(Debug, Clone)]
pub struct Mu {
    location_map: HashMap<Var, Address>,
}

impl Mu {
    pub fn new() -> Mu {
        Mu {
            location_map: HashMap::new(),
        }
    }
    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, String, u64> {
        self.location_map.iter()
    }
    fn get(&self, k: &Var) -> Option<&Address> {
        self.location_map.get(k)
    }
    fn insert(&mut self, k: Var, v: Address) -> Option<Address> {
        self.location_map.insert(k, v)
    }
    fn remove(&mut self, k: &Var) -> Option<Address> {
        self.location_map.remove(k)
    }
    fn diagnostics(&self, gamma: &Gamma) -> String {
        // let loans: BTreeMap<_, _> = self.location_map.iter().map(|(k, v)| (v, k)).collect();
        let mut addrs: Vec<(&Address, &Var)> = self
            .location_map
            .iter()
            .map(|(var, ell)| (ell, var))
            .collect::<Vec<_>>();

        addrs.sort();
        addrs
            .into_iter()
            .map(|(ell, var)| match gamma.get(var) {
                Ok((_, tau)) => match tau {
                    Type::Tuple(taus) => {
                        format!(
                            "\t{var} -> {ell}\n{}",
                            taus.iter()
                                .enumerate()
                                .map(|(i, _)| format!(
                                    "\t{}{var}.{i} -> {}",
                                    " ".repeat(var.len() + 1),
                                    ell + (i as u64) + 1
                                ))
                                .collect::<Vec<String>>()
                                .join("\n")
                        )
                    }
                    _ => format!("\t{var} -> {ell}"),
                },
                Err(_) => panic!(),
            })
            .collect::<Vec<String>>()
            .join("\n")
    }
}

#[derive(Debug, Clone)]
pub struct Eta {
    pub loans: Vec<BTreeMap<Address, S>>,
}

impl Eta {
    pub fn new() -> Eta {
        Eta {
            loans: vec![BTreeMap::new()],
        }
    }

    pub fn get(&self, ell: &Address) -> Option<&S> {
        self.loans.last().unwrap().get(ell)
    }

    fn insert(&mut self, ell: Address, ss: Vec<Spanned<S>>) -> Result<(), TypeError> {
        for (offset, (s, span)) in ss.into_iter().enumerate() {
            match s {
                S::None => {
                    let pos = ell + (offset as u64);
                    self.wipe(&pos);
                    self.loans.last_mut().unwrap().insert(pos, s);
                }
                S::Moved(Some(l)) => {
                    let pos = l + (offset as u64);
                    self.wipe(&pos);
                    self.loans
                        .last_mut()
                        .unwrap()
                        .insert(pos, S::Moved(Some(ell)));
                }
                S::Moved(None) => {
                    let pos = ell;
                    self.wipe(&pos);
                    self.loans.last_mut().unwrap().insert(pos, S::Moved(None));
                }
                S::MutRef(target) => {
                    if let Some(old_s) = self.get(&(target + (offset as u64))) {
                        s.validate(old_s, target, span)?;
                    }
                    let pos = target + (offset as u64);
                    self.wipe(&pos);
                    self.loans.last_mut().unwrap().insert(pos, S::MutRef(ell));
                }
                S::ImmutRef(ref targets) => {
                    for target in targets {
                        if let Some(old_s) = self.get(&(target + (offset as u64))) {
                            s.validate(old_s, target.to_owned(), span.clone())?;
                        }
                        let pos = target + (offset as u64);
                        self.wipe(&pos);
                        self.loans
                            .last_mut()
                            .unwrap()
                            .insert(pos, S::ImmutRef(HashSet::from([ell])));
                    }
                }
                S::Union(onions) => {
                    for onion in onions {
                        self.insert(ell, vec![(onion, span.clone())])?;
                    }
                }
            }
        }
        Ok(())
    }

    fn replace(&mut self, eta: Eta) {
        self.loans = eta.loans
    }

    fn wipe(&mut self, ell: &Address) {
        for (k, s) in self.loans.last().unwrap().clone().iter() {
            let t = s.remove(ell);
            self.loans.last_mut().unwrap().insert(*k, t);
            break;
        }
    }

    fn drop(&mut self, ell: &Address, t: &Type) -> Result<(), String> {
        if let Some(s) = self.get(ell) {
            match s {
                S::Union(ss) => {
                    if ss.iter().all(|s| match s {
                        S::Moved(_) | S::None => true,
                        _ => false,
                    }) {
                        self.wipe(ell);
                    } else {
                        Err(format!(
                            "You can't use/drop a value that has a reference pointing to it at {ell}."
                        ))?
                    }
                }
                S::MutRef(_) => Err(format!(
                    "You can't use/drop a value that has a mutable reference pointing to it at {ell}."
                ))?,
                S::ImmutRef(_) => Err(format!(
                    "You can't use/drop a value that has an immutable reference pointing to it. at {ell}"
                ))?,
                S::Moved(_) => Err(format!(
                    "You can't use/drop a moved value. It's already dropped."
                ))?,
                S::None => {
                    self.wipe(ell);
                }
            }
        }

        match t {
            Type::Tuple(ts) => {
                for (i, t) in ts.into_iter().enumerate() {
                    self.drop(&(ell + (i as u64) + 1), t)?;
                }
            }
            _ => (),
        }

        Ok(())
    }

    fn mov(&mut self, ell: Address, new_home: Option<Vec<Address>>, t: Type) {
        match t {
            Type::Tuple(ts) => {
                if let Some(new_home) = new_home {
                    self.mov(
                        ell,
                        Some(vec![new_home.get(0).unwrap().clone()]),
                        Type::Unit, // to treat it as one-element
                    );
                    for (i, t) in ts.into_iter().enumerate() {
                        self.mov(
                            ell + (i as u64) + 1,
                            Some(new_home.iter().map(|l| l + (i as u64) + 1).collect()),
                            t,
                        );
                    }
                } else {
                    self.mov(
                        ell,
                        None,
                        Type::Unit, // to treat it as one-element
                    );
                    for (i, t) in ts.into_iter().enumerate() {
                        self.mov(ell + (i as u64) + 1, None, t);
                    }
                }
            }
            _ => {
                if let Some(new_home) = new_home {
                    for l in new_home {
                        self.loans
                            .last_mut()
                            .unwrap()
                            .insert(ell, S::Moved(Some(l)));
                    }
                } else {
                    self.loans.last_mut().unwrap().insert(ell, S::Moved(None));
                }
            }
        }
    }

    fn may_move(&self, ell: &Address, t: &Type) -> Result<(), String> {
        if let Some(s) = self.get(ell) {
            s.may_move()?;
        } else {
            panic!()
        }

        match t {
            Type::Tuple(ts) => {
                match ts
                    .into_iter()
                    .enumerate()
                    .map(|(i, t)| self.may_move(&(ell + (i as u64) + 1), t))
                    .collect::<Result<Vec<_>, String>>()
                {
                    Ok(_) => Ok(()),
                    Err(e) => Err(format!("Partial: {}", e)),
                }
            }
            _ => Ok(()),
        }
    }

    fn unify(eta1: &Eta, eta2: &Eta) -> Result<Eta, String> {
        let (eta1_top_loans, loans) = eta1.loans.split_last().unwrap();

        assert!(loans == eta2.loans.split_last().unwrap().1, "Unification only cares about the top of the loans-stack. Thus, the everything else besides the tails should agree.");

        let top_keys = eta1_top_loans
            .keys()
            .chain(eta2.loans.last().unwrap().keys())
            .collect::<HashSet<&Address>>();
        let top_loans = top_keys
            .into_iter()
            .map(|k| {
                let v1 = match eta1_top_loans.get(k) {
                    Some(v1) => v1.to_owned(),
                    None => S::None,
                };
                let v2 = match eta2.loans.last().unwrap().get(k) {
                    Some(v2) => v2.to_owned(),
                    None => S::None,
                };
                match S::join(v1, v2) {
                    Ok(s) => Ok((k.to_owned(), s)),
                    Err(e) => Err(e),
                }
            })
            .collect::<Result<BTreeMap<Address, S>, String>>()?;

        let loans: Vec<BTreeMap<u64, S>> = loans
            .into_iter()
            .map(|l| l.clone())
            .chain([top_loans].into_iter())
            .collect();

        Ok(Eta { loans })
    }
    fn diagnostics(&self, mu: &Mu, gamma: &Gamma) -> String {
        let value_info: &mut Vec<String> = &mut vec![];
        let remaining_indents = &mut Vec::new();
        for (ell, loan) in self.loans.last().unwrap().iter() {
            if let Some(name) = mu
                .iter()
                .map(|(k, v)| (v, k))
                .collect::<HashMap<&u64, &String>>()
                .get(ell)
            {
                if let Ok((_, ref tau)) = gamma.get(name) {
                    if let Some(msg) = loan.stringify(self, ell, Some(tau)) {
                        value_info.push(format!(
                            "\t{}{ell} is {}",
                            " ".repeat(
                                remaining_indents.last().or(Some(&(0 as usize))).unwrap() + 1
                            ),
                            msg
                        ));
                    }
                    match tau {
                        Type::Tuple(taus) => remaining_indents.push(taus.len()),
                        _ => {
                            if let Some(indent_level) = remaining_indents.last() {
                                if indent_level > &1 {
                                    remaining_indents.push(indent_level - 1)
                                }
                            }
                        }
                    }
                } else {
                    panic!("Wat, the provided name (that ell points to) should be in gamma...")
                }
            } else {
                if let Some(msg) = loan.stringify(self, ell, None) {
                    value_info.push(format!(
                        "\t{}{ell} is {}",
                        " ".repeat(remaining_indents.last().or(Some(&(0 as usize))).unwrap() + 1),
                        msg
                    ));
                }
            }
        }
        if value_info.len() > 0 {
            format!("\tthe values at addresses...\n{}", value_info.join("\n"))
        } else {
            format!("")
        }
    }
    fn push_level(&mut self) {
        // self.loans.push(BTreeMap::new())
        self.loans.push(self.loans.last().unwrap().clone())
    }
    fn pop_level(&mut self) {
        self.loans.pop().unwrap();
    }
}

#[derive(Debug, Clone)]
pub struct Gamma {
    pub scopes: Vec<TypeContext>,
}

impl Gamma {
    pub fn new() -> Gamma {
        Gamma { scopes: Vec::new() }
    }
    pub fn get(&self, name: &Var) -> Result<ColoredType, String> {
        if let Some(ct) = self.scopes.iter().rev().find_map(|frame| frame.get(name)) {
            Ok(ct.to_owned())
        } else {
            Err(format!("Name not found"))
        }
    }
    fn assign(&mut self, name: Var, ct: ColoredType) {
        self.scopes.last_mut().unwrap().insert(name, ct);
    }
    fn push_level(&mut self) {
        self.scopes.push(HashMap::new())
    }
    fn pop_level(&mut self, mu: &mut Mu) {
        let scope = self.scopes.pop().unwrap();
        for var in scope.keys() {
            mu.remove(var);
        }
    }
    fn diagnostics(&self) -> String {
        self.scopes
            .iter()
            .map(|scope| {
                scope
                    .iter()
                    .map(|(var, (is_mutable, tau))| {
                        format!(
                            "\t{}: {} {}",
                            var,
                            if *is_mutable { "mutable" } else { "immutable" },
                            tau
                        )
                    })
                    .collect::<Vec<String>>()
                    .join("\n")
            })
            .collect::<Vec<String>>()
            .join("\n")
    }
}

fn generate_address() -> Address {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let result = COUNTER.fetch_add(1, Ordering::Relaxed);
    result as u64
}

pub fn pause() {
    std::io::stdin().read_line(&mut String::new()).unwrap();
}

pub fn branch_choice() -> bool {
    println!("Select which branch you want to go to. Input `true` or `false`.");
    loop {
        let mut s = String::new();
        std::io::stdin().read_line(&mut s).unwrap();
        s = s.to_lowercase();
        if s.trim() == "true" {
            break true;
        } else if s.trim() == "false" {
            break false;
        }
    }
}

pub fn diagnostics(gamma: &Gamma, eta: &Eta, mu: &Mu) {
    println!("Type Context (Gamma): \n{}\n", gamma.diagnostics());
    println!("Memory Map (Mu):\n{}\n", mu.diagnostics(gamma));
    println!("Borrow Layout (Eta):\n{}", eta.diagnostics(mu, gamma));
    println!("{}", "-".to_string().repeat(80));
}

pub struct File {
    contents: Box<String>,
    start: usize,
    pub end: usize,
    tokens: VecDeque<(lexer::Token, Span)>,
}

impl File {
    pub fn new(file_name: String, tokens: VecDeque<(lexer::Token, Span)>) -> File {
        let src = &fs::read_to_string(file_name).expect("Should have been able to read the file");

        File {
            contents: Box::new(src.to_owned()),
            start: 0,
            end: src.len(),
            tokens,
        }
    }
    pub fn seek(&mut self, pos: usize) {
        self.end = pos
    }
    pub fn reveal_until(&mut self, token: &lexer::Token) {
        for (tok, span) in self.tokens.iter() {
            if span.end < self.end {
                continue;
            }
            self.end = span.end;
            if tok == token {
                break;
            }
        }
        self.show();
    }
    pub fn reveal(&mut self, end: usize) {
        self.end = end;
        self.show();
    }
    pub fn show(&self) {
        println!("{}", &self.contents[self.start..self.end]);
    }
}

pub fn cls() {
    print!("\x1B[2J\x1B[1;1H");
}

pub fn type_lhs(
    (lhs, span): Spanned<ELhs>,
    ctx: &Gamma,
    eta: &Eta,
    mu: &mut Mu,
) -> Result<(TLhs, Address), TypeError> {
    match lhs {
        ELhs::Var(name) => {
            let (m, tau) = match ctx.get(&name) {
                Ok(s) => Ok(s),
                Err(e) => Err(TypeError::wrap(e, span)),
            }?;
            Ok((
                TLhs::Var(Type::Ref(m, Box::new(tau)), name.to_string()),
                mu.get(&name).unwrap().to_owned(),
            ))
        }
        ELhs::DeRef(rec_lhs) => {
            let rec_lhs_span = rec_lhs.1.clone();
            let (tlhs, ell) = type_lhs(*rec_lhs, ctx, eta, mu)?;
            match tlhs.extract_type() {
                Type::Ref(_, t) => match *t {
                    Type::Ref(m, tau) => Ok((
                        TLhs::DeRef(Type::Ref(m, tau), Box::new((tlhs, rec_lhs_span))),
                        ell,
                    )),
                    _ => Err(TypeError::wrap(
                        "You can't dereference a non-reference value".to_string(),
                        span,
                    )),
                },
                _ => panic!(),
            }
        }
        ELhs::Index(l, i) => {
            let (typed_l, ell) = type_lhs(*l.clone(), ctx, eta, mu)?;
            match typed_l.extract_type() {
                Type::Ref(b, boxed_t) => {
                    if let Type::Tuple(types) = *boxed_t {
                        if let Some(ti) = types.get(i as usize) {
                            Ok((
                                TLhs::Index(
                                    Type::Ref(b, Box::new(ti.to_owned())),
                                    Box::new((typed_l, l.1)),
                                    i,
                                ),
                                ell + i + 1,
                            ))
                        } else {
                            Err(TypeError::wrap(format!("Index `{}` out of range", i), span))
                        }
                    } else {
                        Err(TypeError::wrap(format!("you can only index a tuple"), span))
                    }
                }
                _ => panic!(),
            }
        }
    }
}

pub fn type_expr(
    file: &mut File,
    spanned: Spanned<EExpr>,
    ctx: &mut Gamma,
    eta: &mut Eta,
    mu: &mut Mu,
    display: bool,
) -> Result<STExpr, TypeError> {
    let (expr, span) = spanned;
    match expr {
        EExpr::Unit => Ok(STExpr::Unit((Type::Unit, S::None))),
        EExpr::Num(x) => Ok(STExpr::Num((Type::Int, S::None), x)),
        EExpr::Bool(b) => Ok(STExpr::Bool((Type::Bool, S::None), b)),
        EExpr::Neg(a) => {
            let span_1 = a.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu, display)?;
            let s = a.extract_s();
            assert!(s == S::None);
            match a.extract_type() {
                Type::Int => Ok(STExpr::Neg((Type::Int, S::None), Box::new((a, span_1)))),
                _ => Err(TypeError::wrap(
                    format!("You can negate only integers."),
                    span,
                )),
            }
        }
        EExpr::Gt(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu, display)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu, display)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Gt(
                    (Type::Bool, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(TypeError::wrap(
                    format!("You can compare only integers together."),
                    span,
                )),
            }
        }
        EExpr::Lt(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu, display)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu, display)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Lt(
                    (Type::Bool, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(TypeError::wrap(
                    format!("You can compare only integers together."),
                    span,
                )),
            }
        }
        EExpr::Add(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu, display)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu, display)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Add(
                    (Type::Int, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(TypeError::wrap(
                    format!("You can add only integers together."),
                    span,
                )),
            }
        }
        EExpr::Sub(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu, display)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu, display)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Sub(
                    (Type::Int, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(TypeError::wrap(
                    format!("You can subtract only integers together."),
                    span,
                )),
            }
        }
        EExpr::Mul(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu, display)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu, display)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Mul(
                    (Type::Int, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(TypeError::wrap(
                    format!("You can multiply only integers together."),
                    span,
                )),
            }
        }
        EExpr::Div(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu, display)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu, display)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Div(
                    (Type::Int, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(TypeError::wrap(
                    format!("You can divide only integers together."),
                    span,
                )),
            }
        }
        EExpr::Cond(cond, then_exp, else_exp) => {
            let span_1 = cond.1.clone();
            let span_2 = then_exp.1.clone();
            let span_3 = else_exp.1.clone();

            let cond = type_expr(file, *cond, ctx, eta, mu, display)?;
            if cond.extract_type() != Type::Bool {
                Err(TypeError::wrap(
                    format!(
                        "If-expr conditions must of type bool not of type {:?}",
                        cond.extract_type()
                    ),
                    span.clone(),
                ))?
            }

            if display {
                cls();
                diagnostics(ctx, eta, mu);
                file.reveal(span_1.end);
                pause();
            }

            let branch = branch_choice();

            let mut then_mu = mu.clone();
            let mut then_eta = eta.clone();
            let then_exp = type_expr(file, *then_exp, ctx, &mut then_eta, &mut then_mu, branch)?;
            let s_1 = then_exp.extract_s();

            if branch {
                cls();
                diagnostics(ctx, eta, mu);
                file.reveal(span_2.end);
                pause();
            }

            let mut else_mu = mu.clone();
            let mut else_eta = eta.clone();
            let else_exp = type_expr(file, *else_exp, ctx, &mut else_eta, &mut else_mu, !branch)?;
            let s_2 = else_exp.extract_s();

            if branch {
                cls();
                diagnostics(ctx, eta, mu);
                file.reveal(span_3.end);
                pause();
            }

            let then_tau = then_exp.extract_type();
            let else_tau = else_exp.extract_type();
            if then_tau != else_tau {
                Err(TypeError::wrap(
                    format!(
                    "If-expr branches must have the same type. They instead are of {:?} and {:?}",
                    then_tau, else_tau
                ),
                    span.clone(),
                ))?
            }

            // Gamma doesn't need to be joined bc of scope-lifetimes.

            // Mu doesn't need to be joined because each variable has one canonical address,
            // and the branches will be joined into the outer scope.
            let new_eta = match Eta::unify(&then_eta, &else_eta) {
                Ok(eta) => Ok(eta),
                Err(msg) => Err(TypeError::wrap(msg, span.clone())),
            }?;
            eta.replace(new_eta);

            cls();
            diagnostics(ctx, eta, mu);
            file.reveal(span_3.end);
            pause();

            let s = match S::join(s_1, s_2) {
                Ok(s) => Ok(s),
                Err(msg) => Err(TypeError { msg, span }),
            }?;
            Ok(STExpr::Cond(
                (then_tau, s),
                Box::new((cond, span_1)),
                Box::new((then_exp, span_2)),
                Box::new((else_exp, span_3)),
            ))
        }
        EExpr::Tuple(exprs) => {
            let stexprs = exprs
                .into_iter()
                .map(
                    |e| match type_expr(file, e.clone(), ctx, eta, mu, display) {
                        Ok(stexpr) => Ok((stexpr, e.1)),
                        Err(e) => Err(e),
                    },
                )
                .collect::<Result<Vec<_>, _>>()?;

            let t = Type::Tuple(
                stexprs
                    .iter()
                    .map(|(stexpr, _)| stexpr.extract_type())
                    .collect(),
            );
            Ok(STExpr::Tuple((t, S::None), stexprs))
        }
        EExpr::Lvalue((lhs, lhs_span)) => {
            let (lhs, ell) = type_lhs((lhs, lhs_span.clone()), ctx, eta, mu)?;
            TypeError::result_wrap(eta.drop(&ell, &lhs.extract_type()), span.clone())?;

            match lhs.extract_type() {
                Type::Ref(_, ref tau) => {
                    TypeError::result_wrap(eta.may_move(&ell, tau), span)?;
                    eta.mov(ell, None, *tau.clone());
                    Ok(STExpr::Lvalue(
                        (*tau.to_owned(), S::Moved(Some(ell))),
                        (lhs, lhs_span),
                    ))
                }
                _ => panic!(),
            }
        }
        EExpr::Seq(e1, e2) => {
            let te1 = type_expr(file, *e1.clone(), ctx, eta, mu, display)?;

            // cls();
            // diagnostics(ctx, eta, mu);
            // file.seek(e1.1.end);
            // file.reveal_until(&lexer::Token::Op(";".to_string()));
            // pause();

            let te2 = type_expr(file, *e2.clone(), ctx, eta, mu, display)?;
            let s2 = te2.extract_s();

            Ok(STExpr::Seq(
                (te2.extract_type(), s2),
                Box::new((te1, e1.1)),
                Box::new((te2, e2.1)),
            ))
        }
        EExpr::Ref((lhs, lhs_span)) => {
            let (tlhs, ell) = type_lhs((lhs, lhs_span.clone()), ctx, eta, mu)?;
            match tlhs.extract_type() {
                Type::Ref(_, boxed_tau) => {
                    let ells = match *boxed_tau.clone() {
                        Type::Tuple(taus) => iter::once(ell)
                            .chain(
                                taus.iter()
                                    .enumerate()
                                    .map(|(i, _stast)| ell + (i as u64) + 1),
                            )
                            .collect(),
                        _ => vec![ell],
                    };

                    for (i, ref ell) in ells.iter().enumerate() {
                        let par = if i == 0 { "" } else { "partially " };
                        match eta.get(ell) {
                            Some(s) => {
                                if s.is_mut_ref() > -1 {
                                    Err(TypeError::wrap(format!(
                                    "cannot borrow `{}` as immutable because it is {}borrowed as mutable",
                                    tlhs.extract_lhs().to_string(),
                                    par
                                ), span.clone()))
                                } else if s.is_move() > -1 {
                                    Err(TypeError::wrap(
                                        format!(
                                            "cannot borrow `{}` as immutable because it is {}moved",
                                            tlhs.extract_lhs().to_string(),
                                            par
                                        ),
                                        span.clone(),
                                    ))
                                } else if s.is_drop() > -1 {
                                    Err(TypeError::wrap(
                                        format!(
                                            "cannot borrow `{}` as immutable because it is {}dropped",
                                            tlhs.extract_lhs().to_string(),
                                            par
                                        ),
                                        span.clone(),
                                    ))
                                } else {
                                    Ok(())
                                }
                            }
                            _ => Ok(()),
                        }?
                    }

                    let t = Type::Ref(false, boxed_tau);
                    let s = S::ImmutRef(HashSet::from_iter(once(ell)));
                    Ok(STExpr::Ref((t, s), (tlhs, lhs_span)))
                }
                _ => panic!(),
            }
        }
        EExpr::MutRef((lhs, lhs_span)) => {
            let (tlhs, ell) = type_lhs((lhs, lhs_span.clone()), ctx, eta, mu)?;
            match tlhs.extract_type() {
                Type::Ref(m, boxed_tau) => {
                    if !m {
                        Err(TypeError::wrap(
                            format!(
                                "cannot borrow `{}` as mutable, as it is not declared as mutable",
                                tlhs.extract_lhs().to_string()
                            ),
                            span.clone(),
                        ))?
                    }

                    let ells = match *boxed_tau.clone() {
                        Type::Tuple(taus) => iter::once(ell)
                            .chain(
                                taus.iter()
                                    .enumerate()
                                    .map(|(i, _stast)| ell + (i as u64) + 1),
                            )
                            .collect(),
                        _ => vec![ell],
                    };
                    for (i, ell) in ells.iter().enumerate() {
                        let par = if i == 0 { "" } else { "partially " };
                        match eta.get(ell) {
                            Some(s) => {
                                if s.is_immut_ref() > -1 {
                                    Err(TypeError::wrap(format!(
                                        "cannot borrow `{}` as mutable because it is {}immutably borrowed",
                                        tlhs.extract_lhs().to_string(),
                                        par
                                    ), span.clone()))
                                } else if s.is_mut_ref() > -1 {
                                    Err(TypeError::wrap(format!(
                                    "cannot borrow `{}` as mutable because it is {}mutably borrowed",
                                    tlhs.extract_lhs().to_string(),
                                    par
                                ), span.clone()))
                                } else if s.is_move() > -1 {
                                    Err(TypeError::wrap(
                                        format!(
                                            "cannot borrow `{}` as mutable because it is {}moved",
                                            tlhs.extract_lhs().to_string(),
                                            par
                                        ),
                                        span.clone(),
                                    ))
                                } else if s.is_drop() > -1 {
                                    Err(TypeError::wrap(
                                        format!(
                                            "cannot borrow `{}` as mutable because it is {}dropped",
                                            tlhs.extract_lhs().to_string(),
                                            par
                                        ),
                                        span.clone(),
                                    ))
                                } else {
                                    Ok(())
                                }
                            }
                            _ => Ok(()),
                        }?
                    }

                    let t = Type::Ref(true, boxed_tau);
                    let s = S::MutRef(ell);
                    Ok(STExpr::MutRef((t, s), (tlhs, lhs_span)))
                }
                _ => panic!(),
            }
        }
        EExpr::Let { name, rhs, then } => {
            let te1 = type_expr(file, *rhs.clone(), ctx, eta, mu, display)?;
            let s1: S = te1.extract_s();
            let span1 = rhs.1;

            eta.push_level();
            ctx.push_level();
            ctx.assign(name.clone(), (false, te1.extract_type()));

            let ell = generate_address();
            let ss = if let STExpr::Tuple(_, stexprs) = te1.clone() {
                for _ in 0..stexprs.len() {
                    let _ = generate_address();
                }
                iter::once((s1, span1.clone()))
                    .chain(
                        stexprs
                            .into_iter()
                            .map(|(stexpr, span)| (stexpr.extract_s(), span)),
                    )
                    .collect()
                // stexprs.iter().map(STExpr::extract_s).collect()
            } else {
                vec![(s1, span1.clone())]
            };

            eta.insert(ell, ss)?;
            mu.insert(name.clone(), ell);

            if display {
                cls();
                diagnostics(ctx, eta, mu);
                file.seek(span1.end);
                file.reveal_until(&lexer::Token::In);
                pause();
            }

            let te2 = type_expr(file, *then.clone(), ctx, eta, mu, display)?;
            let s2 = te2.extract_s();
            eta.pop_level();
            ctx.pop_level(mu);
            TypeError::result_wrap(eta.drop(&ell, &te1.extract_type()), span)?;
            let t = te2.extract_type();
            Ok(STExpr::Let {
                name,
                rhs: Box::new((te1, span1)),
                then: Box::new((te2, then.1)),
                t: (t, s2),
            })
        }
        EExpr::MutLet { name, rhs, then } => {
            let te1 = type_expr(file, *rhs.clone(), ctx, eta, mu, display)?;
            let s1 = te1.extract_s();
            let span1 = rhs.1;

            eta.push_level();
            ctx.push_level();
            ctx.assign(name.clone(), (true, te1.extract_type()));

            let ell = generate_address();

            let ss = if let STExpr::Tuple(_, stexprs) = te1.clone() {
                for _ in 0..stexprs.len() {
                    let _ = generate_address();
                }
                iter::once((s1, span1.clone()))
                    .chain(
                        stexprs
                            .into_iter()
                            .map(|(stexpr, span)| (stexpr.extract_s(), span)),
                    )
                    .collect()
                // stexprs.iter().map(STExpr::extract_s).collect()
            } else {
                vec![(s1, span1.clone())]
            };
            eta.insert(ell, ss)?;
            mu.insert(name.clone(), ell);

            if display {
                cls();
                diagnostics(ctx, eta, mu);
                file.seek(span1.end);
                file.reveal_until(&lexer::Token::In);
                pause();
            }

            let te2 = type_expr(file, *then.clone(), ctx, eta, mu, display)?;
            let s2 = te2.extract_s();
            eta.pop_level();
            ctx.pop_level(mu);
            TypeError::result_wrap(eta.drop(&ell, &te1.extract_type()), span)?;
            let t = te2.extract_type();
            Ok(STExpr::MutLet {
                name,
                rhs: Box::new((te1, span1)),
                then: Box::new((te2, then.1)),
                t: (t, s2),
            })
        }
        EExpr::Assign((lhs, lhs_span), e2) => {
            if display {
                cls();
                diagnostics(ctx, eta, mu);
                file.reveal(e2.1.end);
                pause();
            }

            let (tlhs, ell) = type_lhs((lhs.clone(), lhs_span.clone()), ctx, eta, mu)?;
            let tau_lhs = match tlhs.extract_type() {
                Type::Ref(false, _) => {
                    let msg = match tlhs {
                        TLhs::DeRef(_, _) => {
                            format!("You can't assign to something behind an immutable reference")
                        }
                        TLhs::Var(_, _) => format!("You can't assign to an immutable variable"),
                        TLhs::Index(_, ref in_tlhs, i) => format!(
                            "You can't assign to field {i} of the immutable tuple {}",
                            in_tlhs.0.to_string()
                        ),
                    };
                    Err(TypeError::wrap(msg, span.clone()))
                }
                Type::Ref(true, tau) => Ok(*tau),
                _ => panic!(),
            }?;

            let te2 = type_expr(file, *e2.clone(), ctx, eta, mu, display)?;
            let s = te2.extract_s();

            if tau_lhs == te2.extract_type() {
                let span = e2.1;
                let ss = if let STExpr::Tuple(_, stexprs) = te2.clone() {
                    for _ in 0..stexprs.len() {
                        let _ = generate_address();
                    }
                    iter::once((s, span.clone()))
                        .chain(
                            stexprs
                                .into_iter()
                                .map(|(stexpr, span)| (stexpr.extract_s(), span)),
                        )
                        .collect()
                    // stexprs.iter().map(STExpr::extract_s).collect()
                } else {
                    vec![(s, span.clone())]
                };

                TypeError::result_wrap(eta.drop(&ell, &tau_lhs), span.clone())?;

                eta.insert(ell, ss)?;

                Ok(STExpr::Assign(
                    (Type::Unit, S::None),
                    (tlhs, lhs_span),
                    Box::new((te2, span)),
                ))
            } else {
                Err(TypeError::wrap(
                    format!("You can't assign to {:?} with {:?}", lhs, e2),
                    span,
                ))
            }
        }
    }
}
