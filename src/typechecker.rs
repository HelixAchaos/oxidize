use std::cmp::max;
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::fs;
use std::iter::{self, once};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{EExpr, ELhs, STExpr, Span, Spanned, TLhs};
use crate::lexer;
use crate::types::{Address, Type, Var, S};

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
        let mut loans: Vec<(&Address, &Var)> = self
            .location_map
            .iter()
            .map(|(var, ell)| (ell, var))
            .collect::<Vec<_>>();

        /*

        Memory Map (Mu):
            a -> 0
                 a.0 -> 1
                 a.1 -> 2
                 a.2 -> 3
            b -> 3
                 b.0 -> 4
                 b.1 -> 5
                 b.2 -> 6
            z -> 6

                */

        loans.sort();
        loans
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

        // format!("{:?}", self.location_map)
    }
}

#[derive(Debug, Clone)]
pub struct Eta {
    pub loans: BTreeMap<Address, S>,
}

impl Eta {
    pub fn new() -> Eta {
        Eta {
            loans: BTreeMap::new(),
        }
    }

    pub fn get(&self, ell: Address) -> Option<S> {
        self.loans.get(&ell).cloned()
    }

    fn insert(&mut self, ell: Address, ss: Vec<S>) -> Result<(), String> {
        for (offset, s) in ss.into_iter().enumerate() {
            match s {
                S::None => {
                    self.loans.insert(ell + (offset as u64), s);
                }
                S::Moved(Some(l)) => {
                    // // is_mov is called elsewhere
                    // if let Some(old_s) = self.loans.get(&ell) {
                    //     s.validate(old_s, l)?;
                    // }
                    self.loans.insert(l + (offset as u64), S::Moved(Some(ell)));
                }
                S::Moved(None) => {
                    // is_mov is called elsewhere
                    self.loans.insert(ell, S::Moved(None));
                }
                S::MutRef(target) => {
                    if let Some(old_s) = self.loans.get(&(target + (offset as u64))) {
                        s.validate(old_s, target)?;
                    } else {
                        self.loans.insert(target + (offset as u64), S::MutRef(ell));
                    }
                }
                S::ImmutRef(ref targets) => {
                    for target in targets {
                        if let Some(old_s) = self.loans.get(&(target + (offset as u64))) {
                            s.validate(old_s, target.to_owned())?;
                        }
                        self.loans.insert(target + (offset as u64), S::MutRef(ell));
                    }
                }
                S::Union(onions) => {
                    for onion in onions {
                        self.insert(ell, vec![onion])?;
                    }
                }
            };
        }
        Ok(())
    }

    fn replace(&mut self, eta: Eta) {
        self.loans = eta.loans
    }

    fn wipe(&mut self, ell: Address) {
        for k in self.loans.keys() {
            if let Some(s) = self.loans.get(k) {
                let t = s.remove(ell);
                if s.clone() != t {
                    self.loans.insert(*k, t);
                    break;
                }
            }
        }
    }

    fn drop(&mut self, ell: Address, t: Type) -> Result<(), String> {
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
                            "You can't drop a value that has a reference pointing to it."
                        ))?
                    }
                }
                S::MutRef(_) => Err(format!(
                    "You can't drop a value that has a mutable reference pointing to it."
                ))?,
                S::ImmutRef(_) => Err(format!(
                    "You can't drop a value that has an immutable reference pointing to it."
                ))?,
                S::Moved(_) => Err(format!(
                    "You can't drop a moved value. It's already dropped."
                ))?,
                S::None => {
                    self.wipe(ell);
                }
            }
        }

        match t {
            Type::Tuple(ts) => {
                for (i, t) in ts.into_iter().enumerate() {
                    self.drop(ell + (i as u64) + 1, t)?;
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
                        self.loans.insert(ell, S::Moved(Some(l)));
                    }
                } else {
                    self.loans.insert(ell, S::Moved(None));
                }
            }
        }
    }

    fn may_move(&self, ell: Address, t: Type) -> Result<(), String> {
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
                    .map(|(i, t)| self.may_move(ell + (i as u64) + 1, t))
                    .collect::<Result<Vec<_>, String>>()
                {
                    Ok(_) => Ok(()),
                    Err(e) => {
                        // todo!("specify where borrowed/moved");
                        Err(format!("Partial: {}", e))
                    }
                }
            }
            _ => Ok(()),
        }
    }

    fn unify(eta1: &Eta, eta2: &Eta) -> Result<Eta, String> {
        let keys = eta1
            .loans
            .keys()
            .chain(eta2.loans.keys())
            .collect::<HashSet<&Address>>();
        let loans = keys
            .into_iter()
            .map(|k| {
                let v1 = match eta1.loans.get(k) {
                    Some(v1) => v1.to_owned(),
                    None => S::None,
                };
                let v2 = match eta2.loans.get(k) {
                    Some(v2) => v2.to_owned(),
                    None => S::None,
                };
                match S::join(v1, v2) {
                    Ok(s) => Ok((k.to_owned(), s)),
                    Err(e) => Err(e),
                }
            })
            .collect::<Result<BTreeMap<Address, S>, String>>()?;
        Ok(Eta { loans })
    }
    fn diagnostics(&self, mu: &Mu, gamma: &Gamma) -> String {
        println!("LOANS = {:?}", self.loans);

        let mut value_info = String::new();
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
                        value_info.push_str(&format!("{}{ell} is {}\n", "\t".repeat(remaining_indents.last().or(Some(&(0 as usize))).unwrap() + 1), msg));
                    }
                    match tau {
                        Type::Tuple(taus) => {
                            remaining_indents.push(taus.len())
                        }
                        _ => {
                            if let Some(indent_level) = remaining_indents.last() {
                                if indent_level > &1 {
                                    remaining_indents.push(indent_level - 1)
                                }
                            }
                        }
                    }
                } else {
                    panic!(
                        "Wat, the provided name (that ell points to) should be in gamma..."
                    )
                }
            }
            else {
                if let Some(msg) = loan.stringify(self, ell, None) {
                    value_info.push_str(&format!("{}{ell} is {}\n", "\t".repeat(remaining_indents.last().or(Some(&(0 as usize))).unwrap() + 1), msg));
                }
            }
        }

        format!(
            "\tthe values at addresses...\n{}",
            value_info
        )
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

fn diagnostics(gamma: &Gamma, eta: &Eta, mu: &Mu) {
    println!("Type Context (Gamma): \n{}\n", gamma.diagnostics());
    println!("Memory Map (Mu):\n{}\n", mu.diagnostics(gamma));
    println!("Borrow Layout (Eta):\n{}", eta.diagnostics(mu, gamma));
    println!("{}", "-".to_string().repeat(80));
}

pub struct File {
    contents: Box<String>,
    start: usize,
    end: usize,
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

fn cls() {
    print!("\x1B[2J\x1B[1;1H");
}

pub fn type_lhs(
    lhs: ELhs,
    ctx: &Gamma,
    eta: &mut Eta,
    mu: &mut Mu,
) -> Result<(TLhs, Address), String> {
    match lhs {
        ELhs::Var(name) => {
            let (m, tau) = ctx.get(&name)?;
            Ok((
                TLhs::Var(Type::Ref(m, Box::new(tau)), name.to_string()),
                mu.get(&name).unwrap().to_owned(),
            ))
        }
        ELhs::DeRef(rec_lhs) => {
            let (tlhs, ell) = type_lhs(rec_lhs.0, ctx, eta, mu)?;
            match tlhs.extract_type() {
                Type::Ref(_, t) => match *t {
                    Type::Ref(m, tau) => Ok((
                        TLhs::DeRef(Type::Ref(m, tau), Box::new((tlhs, rec_lhs.1))),
                        ell,
                    )),
                    _ => Err(format!("You can't dereference a non-reference value")),
                },
                _ => panic!(),
            }
        }
        ELhs::Index(l, i) => {
            let (typed_l, ell) = type_lhs(l.0, ctx, eta, mu)?;
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
                            Err(format!("Index `{}` out of range", i))
                        }
                    } else {
                        Err(format!("you can only index a tuple"))
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
) -> Result<STExpr, String> {
    let (expr, _span) = spanned;
    match expr {
        EExpr::Unit => Ok(STExpr::Unit((Type::Unit, S::None))),
        EExpr::Num(x) => Ok(STExpr::Num((Type::Int, S::None), x)),
        EExpr::Neg(a) => {
            let span_1 = a.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu)?;
            let s = a.extract_s();
            assert!(s == S::None);
            match a.extract_type() {
                Type::Int => Ok(STExpr::Neg((Type::Int, S::None), Box::new((a, span_1)))),
                _ => Err(format!("You can negate only integers.")),
            }
        }
        EExpr::Gt(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Gt(
                    (Type::Bool, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(format!("You can compare only integers together.")),
            }
        }
        EExpr::Lt(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Lt(
                    (Type::Bool, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(format!("You can compare only integers together.")),
            }
        }
        EExpr::Add(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Add(
                    (Type::Int, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(format!("You can add only integers together.")),
            }
        }
        EExpr::Sub(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Sub(
                    (Type::Int, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(format!("You can subtract only integers together.")),
            }
        }
        EExpr::Mul(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Mul(
                    (Type::Int, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(format!("You can multiply only integers together.")),
            }
        }
        EExpr::Div(a, b) => {
            let span_1 = a.1.clone();
            let span_2 = b.1.clone();
            let a = type_expr(file, *a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(file, *b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Div(
                    (Type::Int, S::None),
                    Box::new((a, span_1)),
                    Box::new((b, span_2)),
                )),
                _ => Err(format!("You can divide only integers together.")),
            }
        }
        EExpr::Cond(cond, then_exp, else_exp) => {
            let span_1 = cond.1.clone();
            let span_2 = then_exp.1.clone();
            let span_3 = else_exp.1.clone();

            let cond = type_expr(file, *cond, ctx, eta, mu)?;
            if cond.extract_type() != Type::Bool {
                Err(format!(
                    "If-expr conditions must of type bool not of type {:?}",
                    cond.extract_type()
                ))?
            }

            // todo!("choose branch; at the end of the branch, unify");

            // type_expr flag variable - debug?

            let mut then_mu = mu.clone();
            let mut then_eta = eta.clone();
            let then_exp = type_expr(file, *then_exp, ctx, &mut then_eta, &mut then_mu)?;
            let s_1 = then_exp.extract_s();

            let mut else_mu = mu.clone();
            let mut else_eta = eta.clone();
            let else_exp = type_expr(file, *else_exp, ctx, &mut else_eta, &mut else_mu)?;
            let s_2 = else_exp.extract_s();

            let then_tau = then_exp.extract_type();
            let else_tau = else_exp.extract_type();
            if then_tau != else_tau {
                Err(format!(
                    "If-expr branches must have the same type. They instead are of {:?} and {:?}",
                    then_tau, else_tau
                ))?
            }

            // Gamma doesn't need to be joined bc of scope-lifetimes.

            // Mu doesn't need to be joined because each variable has one canonical address,
            // and the branches will be joined into the outer scope.

            eta.replace(Eta::unify(&then_eta, &else_eta)?);

            let s = S::join(s_1, s_2)?;
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
                .map(|e| match type_expr(file, e.clone(), ctx, eta, mu) {
                    Ok(stexpr) => Ok((stexpr, e.1)),
                    Err(e) => Err(e),
                })
                .collect::<Result<Vec<_>, _>>()?;

            let t = Type::Tuple(
                stexprs
                    .iter()
                    .map(|(stexpr, _)| stexpr.extract_type())
                    .collect(),
            );

            // let s = stexprs
            //     .iter()
            //     .map(|(stexpr, _)| stexpr.extract_s())
            //     .fold(Ok(S::None), |acc, x| S::join(acc?, x))?;
            Ok(STExpr::Tuple((t, S::None), stexprs))
        }
        EExpr::Lvalue(lhs) => {
            let (lhs, ell) = type_lhs(lhs, ctx, eta, mu)?;
            eta.drop(ell, lhs.extract_type())?;

            match lhs.extract_type() {
                Type::Ref(_, tau) => {
                    eta.may_move(ell, *tau.clone())?;
                    eta.mov(ell, None, *tau.clone());
                    Ok(STExpr::Lvalue((*tau, S::Moved(Some(ell))), lhs))
                }
                _ => panic!(),
            }
        }
        EExpr::Seq(e1, e2) => {
            let te1 = type_expr(file, *e1.clone(), ctx, eta, mu)?;

            // cls();
            // diagnostics(ctx, eta, mu);
            // file.seek(e1.1.end);
            // file.reveal_until(&lexer::Token::Op(";".to_string()));
            // pause();

            let te2 = type_expr(file, *e2.clone(), ctx, eta, mu)?;
            let s2 = te2.extract_s();

            Ok(STExpr::Seq(
                (te2.extract_type(), s2),
                Box::new((te1, e1.1)),
                Box::new((te2, e2.1)),
            ))
        }
        EExpr::Ref(lhs) => {
            let (tlhs, ell) = type_lhs(lhs, ctx, eta, mu)?;
            match tlhs.extract_type() {
                Type::Ref(_, boxed_tau) => {
                    if eta.loans.values().any(|s| s.contains(&(true, ell))) {
                        Err(format!(
                            "cannot borrow `{}` as immutable because it is also borrowed as mutable",
                            tlhs.extract_lhs().to_string()
                        ))?
                    }

                    let t = Type::Ref(false, boxed_tau);
                    let s = S::ImmutRef(HashSet::from_iter(once(ell)));

                    Ok(STExpr::Ref((t, s), tlhs))
                }
                _ => panic!(),
            }
        }
        EExpr::MutRef(lhs) => {
            let (tlhs, ell) = type_lhs(lhs, ctx, eta, mu)?;
            match tlhs.extract_type() {
                Type::Ref(m, boxed_tau) => {
                    if !m {
                        Err(format!(
                            "cannot borrow `{}` as mutable, as it is not declared as mutable",
                            tlhs.extract_lhs().to_string()
                        ))?
                    }

                    if eta.loans.values().any(|s| s.contains(&(false, ell))) {
                        Err(format!(
                            "cannot borrow `{}` as mutable because it is also borrowed as immutable",
                            tlhs.extract_lhs().to_string()
                        ))?
                    };
                    if eta.loans.values().any(|s| s.contains(&(true, ell))) {
                        Err(format!(
                            "cannot borrow `{}` as mutable more than once at a time",
                            tlhs.extract_lhs().to_string()
                        ))?
                    };

                    let t = Type::Ref(true, boxed_tau);
                    let s = S::MutRef(ell);

                    Ok(STExpr::MutRef((t, s), tlhs))
                }
                _ => panic!(),
            }
        }
        EExpr::Let { name, rhs, then } => {
            let te1 = type_expr(file, *rhs.clone(), ctx, eta, mu)?;
            let s1: S = te1.extract_s();

            ctx.push_level();
            ctx.assign(name.clone(), (false, te1.extract_type()));

            let ell = generate_address();
            let ss = if let STExpr::Tuple(_, stexprs) = te1.clone() {
                for _ in 0..stexprs.len() {
                    let _ = generate_address();
                }
                iter::once(s1)
                    .chain(stexprs.iter().map(|(stexpr, _)| stexpr.extract_s()))
                    .collect()
                // stexprs.iter().map(STExpr::extract_s).collect()
            } else {
                vec![s1]
            };

            eta.insert(ell, ss);
            mu.insert(name.clone(), ell);

            mu.insert(name.clone(), ell);

            cls();
            diagnostics(ctx, eta, mu);
            file.seek(rhs.1.end);
            file.reveal_until(&lexer::Token::In);
            pause();

            let te2 = type_expr(file, *then.clone(), ctx, eta, mu)?;
            let s2 = te2.extract_s();
            ctx.pop_level(mu);
            let t = te2.extract_type();
            Ok(STExpr::Let {
                name,
                rhs: Box::new((te1, rhs.1)),
                then: Box::new((te2, then.1)),
                t: (t, s2),
            })
        }
        EExpr::MutLet { name, rhs, then } => {
            let te1 = type_expr(file, *rhs.clone(), ctx, eta, mu)?;
            let s1 = te1.extract_s();

            ctx.push_level();
            ctx.assign(name.clone(), (true, te1.extract_type()));

            let ell = generate_address();
            let ss = if let STExpr::Tuple(_, stexprs) = te1.clone() {
                for _ in 0..stexprs.len() {
                    let _ = generate_address();
                }
                iter::once(s1)
                    .chain(stexprs.iter().map(|(stexpr, _)| stexpr.extract_s()))
                    .collect()

                // stexprs.iter().map(STExpr::extract_s).collect()
            } else {
                vec![s1]
            };
            eta.insert(ell, ss);
            mu.insert(name.clone(), ell);

            cls();
            diagnostics(ctx, eta, mu);
            file.seek(rhs.1.end);
            file.reveal_until(&lexer::Token::In);
            pause();

            let te2 = type_expr(file, *then.clone(), ctx, eta, mu)?;
            let s2 = te2.extract_s();
            ctx.pop_level(mu);
            let t = te2.extract_type();
            Ok(STExpr::MutLet {
                name,
                rhs: Box::new((te1, rhs.1)),
                then: Box::new((te2, then.1)),
                t: (t, s2),
            })
        }
        EExpr::Assign(lhs, e2) => {
            cls();
            diagnostics(ctx, eta, mu);
            file.reveal(e2.1.end);
            pause();

            let te2 = type_expr(file, *e2.clone(), ctx, eta, mu)?;
            let s = te2.extract_s();

            let (lhs, ell) = type_lhs(lhs, ctx, eta, mu)?;
            let tau_lhs = match lhs.extract_type() {
                Type::Ref(false, _) => Err(format!("You can't assign to an immutable")),
                Type::Ref(true, tau) => Ok(*tau),
                _ => panic!(),
            }?;

            if tau_lhs == te2.extract_type() {
                let ss = if let STExpr::Tuple(_, stexprs) = te2.clone() {
                    iter::once(s)
                        .chain(stexprs.iter().map(|(stexpr, _)| stexpr.extract_s()))
                        .collect()

                    // stexprs.iter().map(STExpr::extract_s).collect()
                } else {
                    vec![s]
                };
                eta.insert(ell, ss);

                Ok(STExpr::Assign(
                    (Type::Unit, S::None),
                    lhs,
                    Box::new((te2, e2.1)),
                ))
            } else {
                Err(format!("You can't assign to {:?} with {:?}", lhs, e2))
            }
        }
    }
}
