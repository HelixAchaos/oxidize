use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::fs;
use std::iter::{self};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{EExpr, ELhs, Span, Spanned, TExpr, TLhs};
use crate::lexer;
use crate::types::{Address, BorrowState, Type, TypeError, Var, S};

type ColoredType = (bool, Type);
type TypeContext = HashMap<Var, ColoredType>;

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn generate_address() -> Address {
    let result = COUNTER.fetch_add(1, Ordering::Relaxed);
    result as u64
}

fn reset_counter(n: usize) {
    COUNTER.store(n, Ordering::Relaxed);
}

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
    fn max_addr(&self) -> Address {
        self.location_map.values().max().unwrap().to_owned()
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
    pub loans: BTreeMap<Address, BorrowState>,
    unified: bool,
}

impl Eta {
    pub fn new() -> Eta {
        Eta {
            loans: BTreeMap::new(),
            unified: false,
        }
    }

    pub fn get(&self, ell: &Address) -> Option<&BorrowState> {
        self.loans.get(ell)
    }

    fn insert(&mut self, ell: Address, ss: Vec<Spanned<S>>) -> Result<(), TypeError> {
        for (offset, (s, span)) in ss.into_iter().enumerate() {
            match s {
                S::None => {
                    let pos = ell + (offset as u64);
                    self.wipe(&pos);
                    self.loans.insert(pos, BorrowState::None);
                }
                S::Moved(l) => {
                    let pos = ell + (offset as u64);
                    self.wipe(&l);
                    self.loans.insert(l, BorrowState::Moved(pos));
                }
                S::MutRef(target) => {
                    let pos = ell + (offset as u64);
                    if let Some(old_s) = self.get(&pos) {
                        old_s.validate(&s, ell, span)?;
                    }
                    self.loans
                        .insert(target, BorrowState::Ref(HashSet::from([(true, pos)])));
                }
                S::ImmutRef(target) => {
                    let pos = ell + (offset as u64);
                    if let Some(old_s) = self.get(&pos) {
                        old_s.validate(&s, ell, span)?
                    }
                    self.loans
                        .insert(target, BorrowState::Ref(HashSet::from([(false, pos)])));
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
        self.loans = eta.loans;
        self.unified = eta.unified;
    }

    fn wipe(&mut self, ell: &Address) {
        for (k, s) in self.loans.clone().iter() {
            let t = s.remove(ell);
            self.loans.insert(*k, t);
        }
    }

    fn drop(&mut self, ell: &Address, t: &Type, span: Span) -> Result<(), TypeError> {
        let ells = match t {
            Type::Tuple(ts) => [
                vec![ell.to_owned()],
                (0..ts.len())
                    .into_iter()
                    .map(|offset| (ell + (offset as u64) + 1))
                    .collect(),
            ]
            .concat(),
            _ => vec![ell.to_owned()],
        };
        for ell in ells.iter() {
            if let Some(_bs) = self.get(ell) {
                TypeError::result_wrap(self.may_drop(ell, t), span.clone())?;
                self.wipe(ell);
            }
        }
        Ok(())
    }

    fn force_drop(&mut self, ell: &Address, t: &Type, span: Span) -> Result<(), TypeError> {
        let ells = match t {
            Type::Tuple(ts) => [
                vec![ell.to_owned()],
                (0..ts.len())
                    .into_iter()
                    .map(|offset| (ell + (offset as u64) + 1))
                    .collect(),
            ]
            .concat(),
            _ => vec![ell.to_owned()],
        };
        for ell in ells.iter() {
            if let Some(_bs) = self.get(ell) {
                TypeError::result_wrap(self.may_force_drop(ell, t), span.clone())?;
                self.wipe(ell);
            }
        }
        Ok(())
    }

    fn sudo_drop(&mut self, ell: &Address, t: &Type) {
        let ells = match t {
            Type::Tuple(ts) => [
                vec![ell.to_owned()],
                (0..ts.len())
                    .into_iter()
                    .map(|offset| (ell + (offset as u64) + 1))
                    .collect(),
            ]
            .concat(),
            _ => vec![ell.to_owned()],
        };
        for ell in ells.iter() {
            if let Some(_bs) = self.get(ell) {
                self.wipe(ell);
            }
        }
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
                        self.loans.insert(ell, BorrowState::Moved(l));
                    }
                } else {
                    self.loans.insert(ell, BorrowState::Dropped);
                }
            }
        }
    }

    fn may_drop(&self, ell: &Address, t: &Type) -> Result<(), String> {
        if let Some(s) = self.get(ell) {
            s.may_drop()?;
        }

        match t {
            Type::Tuple(ts) => {
                match ts
                    .into_iter()
                    .enumerate()
                    .map(|(i, t)| self.may_drop(&(ell + (i as u64) + 1), t))
                    .collect::<Result<Vec<_>, String>>()
                {
                    Ok(_) => Ok(()),
                    Err(e) => Err(format!("Partial: {}", e)),
                }
            }
            _ => Ok(()),
        }
    }

    fn may_force_drop(&self, ell: &Address, t: &Type) -> Result<(), String> {
        if let Some(s) = self.get(ell) {
            s.may_force_drop()?;
        }

        match t {
            Type::Tuple(ts) => {
                match ts
                    .into_iter()
                    .enumerate()
                    .map(|(i, t)| self.may_force_drop(&(ell + (i as u64) + 1), t))
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
        let top_keys = eta1
            .loans
            .keys()
            .chain(eta2.loans.keys())
            .collect::<HashSet<&Address>>();
        let loans = top_keys
            .into_iter()
            .map(|k| {
                let v1 = match eta1.loans.get(k) {
                    Some(v1) => v1.to_owned(),
                    None => BorrowState::None,
                };
                let v2 = match eta2.loans.get(k) {
                    Some(v2) => v2.to_owned(),
                    None => BorrowState::None,
                };
                match BorrowState::join(v1, v2) {
                    Ok(s) => Ok((k.to_owned(), s)),
                    Err(e) => Err(e),
                }
            })
            .collect::<Result<BTreeMap<Address, BorrowState>, String>>()?;

        Ok(Eta {
            loans,
            unified: true,
        })
    }
    fn diagnostics(&self, mu: &Mu, gamma: &Gamma) -> String {
        let value_info: &mut Vec<String> = &mut vec![];
        let remaining_indents = &mut Vec::new();
        for (ell, loan) in self.loans.iter() {
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
                            " ".repeat(remaining_indents.last().unwrap_or(&(0 as usize)) + 1),
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
        if value_info.is_empty() {
            format!("")
        } else {
            format!("\tthe values at addresses...\n{}", value_info.join("\n"))
        }
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
    fn pop_level(&mut self, mu: &mut Mu, eta: &mut Eta) {
        let scope = self.scopes.pop().unwrap();
        for var in scope.keys() {
            if let Some(ell) = mu.remove(var) {
                eta.wipe(&ell)
            }
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

pub fn diagnostics(gamma: &Gamma, eta: &mut Eta, mu: &Mu) {
    println!("Type Context (Gamma): \n{}\n", gamma.diagnostics());
    println!("Memory Map (Mu):\n{}\n", mu.diagnostics(gamma));
    println!(
        "Borrow Layout (Eta) {}:\n{}",
        if eta.unified { "freshly unified" } else { "" },
        eta.diagnostics(mu, gamma)
    );
    println!("{}", "-".to_string().repeat(80));
    eta.unified = false;
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
) -> Result<(TExpr, Vec<Spanned<S>>), TypeError> {
    let (expr, span) = spanned;
    match expr {
        EExpr::Unit => Ok((TExpr::Unit(Type::Unit), vec![(S::None, span)])),
        EExpr::Num(x) => Ok((TExpr::Num(Type::Int, x), vec![(S::None, span)])),
        EExpr::Bool(b) => Ok((TExpr::Bool(Type::Bool, b), vec![(S::None, span)])),
        EExpr::Neg(a) => {
            let span_1 = a.1.clone();
            let (a, _) = type_expr(file, *a, ctx, eta, mu, display)?;
            match a.extract_type() {
                Type::Int => Ok((
                    TExpr::Neg(Type::Int, Box::new((a, span_1))),
                    vec![(S::None, span)],
                )),
                _ => Err(TypeError::wrap(
                    format!("You can negate only integers."),
                    span,
                )),
            }
        }
        EExpr::Gt(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let (a, _) = type_expr(file, *a, ctx, eta, mu, display)?;
            let (b, _) = type_expr(file, *b, ctx, eta, mu, display)?;
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok((
                    TExpr::Gt(Type::Bool, Box::new((a, span_1)), Box::new((b, span_2))),
                    vec![(S::None, span)],
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
            let (a, _) = type_expr(file, *a, ctx, eta, mu, display)?;
            let (b, _) = type_expr(file, *b, ctx, eta, mu, display)?;
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok((
                    TExpr::Lt(Type::Bool, Box::new((a, span_1)), Box::new((b, span_2))),
                    vec![(S::None, span)],
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
            let (a, _) = type_expr(file, *a, ctx, eta, mu, display)?;
            let (b, _) = type_expr(file, *b, ctx, eta, mu, display)?;
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok((
                    TExpr::Add(Type::Int, Box::new((a, span_1)), Box::new((b, span_2))),
                    vec![(S::None, span)],
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
            let (a, _) = type_expr(file, *a, ctx, eta, mu, display)?;
            let (b, _) = type_expr(file, *b, ctx, eta, mu, display)?;
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok((
                    TExpr::Sub(Type::Int, Box::new((a, span_1)), Box::new((b, span_2))),
                    vec![(S::None, span)],
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
            let (a, _) = type_expr(file, *a, ctx, eta, mu, display)?;
            let (b, _) = type_expr(file, *b, ctx, eta, mu, display)?;
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok((
                    TExpr::Mul(Type::Int, Box::new((a, span_1)), Box::new((b, span_2))),
                    vec![(S::None, span)],
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
            let (a, _) = type_expr(file, *a, ctx, eta, mu, display)?;
            let (b, _) = type_expr(file, *b, ctx, eta, mu, display)?;
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok((
                    TExpr::Div(Type::Int, Box::new((a, span_1)), Box::new((b, span_2))),
                    vec![(S::None, span)],
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

            let (cond, _) = type_expr(file, *cond, ctx, eta, mu, display)?;
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
            let (then_exp, s_1) =
                type_expr(file, *then_exp, ctx, &mut then_eta, &mut then_mu, branch)?;

            if branch {
                cls();
                diagnostics(ctx, eta, mu);
                file.reveal(span_2.end);
                pause();
            }

            let mut else_mu = mu.clone();
            let mut else_eta = eta.clone();
            let (else_exp, s_2) =
                type_expr(file, *else_exp, ctx, &mut else_eta, &mut else_mu, !branch)?;

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
            assert!(
                s_1.len() == s_2.len(),
                "If the branches have the same type, the lengths of the Ss must be the same."
            );

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

            let s = s_1
                .into_iter()
                .zip(s_2.into_iter())
                .map(|((s1, _), (s2, _))| match S::join(s1, s2) {
                    Ok(s) => Ok((s, span.clone())),
                    Err(msg) => Err(TypeError {
                        msg,
                        span: span.clone(),
                    }),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok((
                TExpr::Cond(
                    then_tau,
                    Box::new((cond, span_1)),
                    Box::new((then_exp, span_2)),
                    Box::new((else_exp, span_3)),
                ),
                s,
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

            let (texprs, s): (Vec<_>, Vec<_>) = stexprs
                .into_iter()
                .map(|((texpr, s), span)| ((texpr, span), s))
                .unzip();

            let t = Type::Tuple(texprs.iter().map(|(te, _)| te.extract_type()).collect());
            let s = [vec![(S::None, span)], s.concat()].concat();

            Ok((TExpr::Tuple(t, texprs), s))
        }
        EExpr::Lvalue((lhs, lhs_span)) => {
            let (lhs, ell) = type_lhs((lhs, lhs_span.clone()), ctx, eta, mu)?;
            eta.drop(&ell, &lhs.extract_type(), span.clone())?;

            match lhs.extract_type() {
                Type::Ref(_, ref tau) => {
                    TypeError::result_wrap(eta.may_drop(&ell, tau), span.clone())?;
                    eta.mov(ell, None, *tau.clone());
                    Ok((
                        TExpr::Lvalue(*tau.to_owned(), (lhs, lhs_span)),
                        vec![(S::Moved(ell), span)],
                    ))
                }
                _ => panic!(),
            }
        }
        EExpr::Seq(e1, e2) => {
            let (te1, _) = type_expr(file, *e1.clone(), ctx, eta, mu, display)?;

            // cls();
            // diagnostics(ctx, eta, mu);
            // file.seek(e1.1.end);
            // file.reveal_until(&lexer::Token::Op(";".to_string()));
            // pause();

            let (te2, s2) = type_expr(file, *e2.clone(), ctx, eta, mu, display)?;

            Ok((
                TExpr::Seq(
                    te2.extract_type(),
                    Box::new((te1, e1.1)),
                    Box::new((te2, e2.1)),
                ),
                s2,
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
                        if let Some(bs) = eta.get(ell) {
                            if bs.is_move() > -1 {
                                Err(TypeError::wrap(
                                    format!(
                                        "cannot borrow `{}` as immutable because it is {}moved",
                                        tlhs.extract_lhs().to_string(),
                                        par
                                    ),
                                    span.clone(),
                                ))
                            } else if bs.is_drop() > -1 {
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
                            }?
                        }
                    }

                    let t = Type::Ref(false, boxed_tau);
                    let s = S::ImmutRef(ell);
                    Ok((TExpr::Ref(t, (tlhs, lhs_span)), vec![(s, span)]))
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
                        if let Some(bs) = eta.get(ell) {
                            if bs.is_immut_ref() > -1 {
                                Err(TypeError::wrap(format!(
                                    "cannot borrow `{}` as mutable because it is {}immutably borrowed",
                                    tlhs.extract_lhs().to_string(),
                                    par
                                ), span.clone()))
                            } else if bs.is_mut_ref() > -1 {
                                Err(TypeError::wrap(
                                    format!(
                                "cannot borrow `{}` as mutable because it is {}mutably borrowed",
                                tlhs.extract_lhs().to_string(),
                                par
                            ),
                                    span.clone(),
                                ))
                            } else if bs.is_move() > -1 {
                                Err(TypeError::wrap(
                                    format!(
                                        "cannot borrow `{}` as mutable because it is {}moved",
                                        tlhs.extract_lhs().to_string(),
                                        par
                                    ),
                                    span.clone(),
                                ))
                            } else if bs.is_drop() > -1 {
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
                            }?
                        }
                    }

                    let t = Type::Ref(true, boxed_tau);
                    let s = S::MutRef(ell);
                    Ok((TExpr::MutRef(t, (tlhs, lhs_span)), vec![(s, span)]))
                }
                _ => panic!(),
            }
        }
        EExpr::Let { name, rhs, then } => {
            let (te1, ss) = type_expr(file, *rhs.clone(), ctx, eta, mu, display)?;
            let span1 = rhs.1;

            ctx.push_level();
            ctx.assign(name.clone(), (false, te1.extract_type()));

            let ell = generate_address();
            if let TExpr::Tuple(_, texprs) = te1.clone() {
                assert!(texprs.len() == ss.len() - 1);
                for _ in 0..texprs.len() {
                    let _ = generate_address();
                }
            }
            eta.insert(ell, ss)?;
            mu.insert(name.clone(), ell);

            if display {
                cls();
                diagnostics(ctx, eta, mu);
                file.seek(span1.end);
                file.reveal_until(&lexer::Token::In);
                pause();
            }

            let (te2, s2) = type_expr(file, *then.clone(), ctx, eta, mu, display)?;
            reset_counter((mu.max_addr() as usize) + 1);
            ctx.pop_level(mu, eta);

            eta.force_drop(&ell, &te1.extract_type(), span.clone())?;

            for (s, s_span) in s2.iter() {
                for referred in s.extract_referred_addresses() {
                    if referred >= ell {
                        Err(TypeError {msg: format!("Lifetime: A reference at address {referred} (created at {:?}) leaked out.", s_span), span: span.clone()})?
                    }
                }
            }

            let t = te2.extract_type();
            Ok((
                TExpr::Let {
                    name,
                    rhs: Box::new((te1, span1)),
                    then: Box::new((te2, then.1)),
                    t,
                },
                s2,
            ))
        }
        EExpr::MutLet { name, rhs, then } => {
            let (te1, ss) = type_expr(file, *rhs.clone(), ctx, eta, mu, display)?;
            let span1 = rhs.1;

            ctx.push_level();
            ctx.assign(name.clone(), (true, te1.extract_type()));

            let ell = generate_address();
            if let TExpr::Tuple(_, texprs) = te1.clone() {
                assert!(texprs.len() == ss.len() - 1);
                for _ in 0..texprs.len() {
                    let _ = generate_address();
                }
            }
            eta.insert(ell, ss)?;
            mu.insert(name.clone(), ell);

            if display {
                cls();
                diagnostics(ctx, eta, mu);
                file.seek(span1.end);
                file.reveal_until(&lexer::Token::In);
                pause();
            }

            let (te2, s2) = type_expr(file, *then.clone(), ctx, eta, mu, display)?;
            reset_counter((mu.max_addr() as usize) + 1);
            ctx.pop_level(mu, eta);

            eta.force_drop(&ell, &te1.extract_type(), span.clone())?;
            for (s, s_span) in s2.iter() {
                for referred in s.extract_referred_addresses() {
                    if referred >= ell {
                        Err(TypeError {msg: format!("Lifetime: A reference at address {referred} (created at {:?}) leaked out.", s_span), span: span.clone()})?
                    }
                }
            }

            let t = te2.extract_type();
            Ok((
                TExpr::MutLet {
                    name,
                    rhs: Box::new((te1, span1)),
                    then: Box::new((te2, then.1)),
                    t,
                },
                s2,
            ))
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

            let (te2, ss) = type_expr(file, *e2.clone(), ctx, eta, mu, display)?;

            if tau_lhs == te2.extract_type() {
                let e2_span = e2.1;

                if let TExpr::Tuple(_, texprs) = te2.clone() {
                    assert!(texprs.len() == ss.len() - 1);
                }

                eta.sudo_drop(&ell, &tau_lhs);

                eta.insert(ell, ss)?;

                Ok((
                    TExpr::Assign(
                        Type::Unit,
                        (tlhs, lhs_span),
                        Box::new((te2, e2_span.clone())),
                    ),
                    vec![(S::None, span)],
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
