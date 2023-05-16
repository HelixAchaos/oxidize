use std::collections::{HashMap, HashSet};
use std::iter::{self, once};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{EExpr, ELhs, STExpr, TLhs};
use crate::types::{Address, Type, Var, S};

type ColoredType = (bool, Type);
type TypeContext = HashMap<Var, ColoredType>;

pub type Mu = HashMap<Var, Address>;

#[derive(Debug, Clone)]
pub struct Eta {
    pub loans: HashMap<Address, S>,
}

impl Eta {
    fn get(&self, ell: Address) -> Option<S> {
        self.loans.get(&ell).cloned()
    }

    fn insert(&mut self, ell: Address, ss: Vec<S>) {
        for (offset, s) in ss.into_iter().enumerate() {
            self.loans.insert(ell + (offset as u64), s);
        }
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
                    if ss.iter().all(|s| s == &S::Moved || s == &S::None) {
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
                S::Moved => Err(format!(
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

    fn mov(&mut self, ell: Address, t: Type) {
        match t {
            Type::Tuple(ts) => {
                for (i, t) in ts.into_iter().enumerate() {
                    self.mov(ell + (i as u64) + 1, t);
                }
            }
            _ => {
                self.loans.insert(ell, S::Moved);
            }
        }
    }

    fn may_move(&self, ell: Address, t: Type) -> Result<(), String> {
        if let Some(s) = self.get(ell) {
            s.may_move()?;
        } else {
            panic!()
        }

        println!("TYPE    {:?}", t);

        match t {
            Type::Tuple(ts) => {
                match ts.into_iter().enumerate().map(|(i, t)| self.may_move(ell + (i as u64) + 1, t)).collect::<Result<Vec<_>, String>>() {
                    Ok(_) => Ok(()),
                    Err(e) => Err(format!("Partial: {}", e))
                }
            }
            _ => Ok(())
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
            .collect::<Result<HashMap<Address, S>, String>>()?;
        Ok(Eta { loans })
    }
}

#[derive(Debug, Clone)]
pub struct Gamma {
    pub vars: Vec<TypeContext>,
}

impl Gamma {
    fn get(&self, name: &Var) -> Result<ColoredType, String> {
        if let Some(ct) = self.vars.iter().rev().find_map(|frame| frame.get(name)) {
            Ok(ct.to_owned())
        } else {
            Err(format!("Name not found"))
        }
    }
    // fn remove(&mut self, name: &Var) {
    //     if let Some(frame) = self
    //         .vars
    //         .iter_mut()
    //         .rev()
    //         .find(|frame| frame.contains_key(name))
    //     {
    //         frame.remove(name);
    //     } else {
    //         panic!()
    //     }
    // }
    fn assign(&mut self, name: Var, ct: ColoredType) {
        self.vars.last_mut().unwrap().insert(name, ct);
    }
    fn push_level(&mut self) {
        self.vars.push(HashMap::new())
    }
    fn pop_level(&mut self) {
        self.vars.pop().unwrap();
    }
    // fn replace(&mut self, gamma: Gamma) {
    //     self.vars = gamma.vars
    // }
}

fn generate_address() -> Address {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let result = COUNTER.fetch_add(1, Ordering::Relaxed);
    result as u64
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
            let (tlhs, ell) = type_lhs(*rec_lhs, ctx, eta, mu)?;
            match tlhs.extract_type() {
                Type::Ref(_, t) => match *t {
                    Type::Ref(m, tau) => Ok((TLhs::DeRef(Type::Ref(m, tau), Box::new(tlhs)), ell)),
                    _ => Err(format!("You can't dereference a non-reference value")),
                },
                _ => panic!(),
            }
        }
        ELhs::Index(l, i) => {
            let (typed_l, ell) = type_lhs(*l, ctx, eta, mu)?;
            match typed_l.extract_type() {
                Type::Ref(b, boxed_t) => {
                    if let Type::Tuple(types) = *boxed_t {
                        if let Some(ti) = types.get(i as usize) {
                            Ok((
                                TLhs::Index(
                                    Type::Ref(b, Box::new(ti.to_owned())),
                                    Box::new(typed_l),
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
    expr: EExpr,
    ctx: &mut Gamma,
    eta: &mut Eta,
    mu: &mut Mu,
) -> Result<STExpr, String> {
    match expr {
        EExpr::Unit => Ok(STExpr::Unit((Type::Unit, S::None))),
        EExpr::Num(x) => Ok(STExpr::Num((Type::Int, S::None), x)),
        EExpr::Neg(a) => {
            let a = type_expr(*a.to_owned(), ctx, eta, mu)?;
            let s = a.extract_s();
            assert!(s == S::None);
            match a.extract_type() {
                Type::Int => Ok(STExpr::Neg((Type::Int, S::None), Box::new(a))),
                _ => Err(format!("You can negate only integers.")),
            }
        }
        EExpr::Gt(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok(STExpr::Gt((Type::Bool, S::None), Box::new(a), Box::new(b)))
                }
                _ => Err(format!("You can compare only integers together.")),
            }
        }
        EExpr::Lt(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok(STExpr::Lt((Type::Bool, S::None), Box::new(a), Box::new(b)))
                }
                _ => Err(format!("You can compare only integers together.")),
            }
        }
        EExpr::Add(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok(STExpr::Add((Type::Bool, S::None), Box::new(a), Box::new(b)))
                }
                _ => Err(format!("You can add only integers together.")),
            }
        }
        EExpr::Sub(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok(STExpr::Sub((Type::Bool, S::None), Box::new(a), Box::new(b)))
                }
                _ => Err(format!("You can subtract only integers together.")),
            }
        }
        EExpr::Mul(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok(STExpr::Mul((Type::Bool, S::None), Box::new(a), Box::new(b)))
                }
                _ => Err(format!("You can multiply only integers together.")),
            }
        }
        EExpr::Div(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::None);
            assert!(s_2 == S::None);
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok(STExpr::Div((Type::Bool, S::None), Box::new(a), Box::new(b)))
                }
                _ => Err(format!("You can divide only integers together.")),
            }
        }
        EExpr::Cond(cond, then_exp, else_exp) => {
            let cond = type_expr(*cond, ctx, eta, mu)?;
            if cond.extract_type() != Type::Bool {
                Err(format!(
                    "If-expr conditions must of type bool not of type {:?}",
                    cond.extract_type()
                ))?
            }

            let mut then_mu = mu.clone();
            let mut then_eta = eta.clone();
            let then_exp = type_expr(*then_exp, ctx, &mut then_eta, &mut then_mu)?;
            let s_1 = then_exp.extract_s();
            let mut else_mu = mu.clone();
            let mut else_eta = eta.clone();
            let else_exp = type_expr(*else_exp, ctx, &mut else_eta, &mut else_mu)?;
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
                Box::new(cond),
                Box::new(then_exp),
                Box::new(else_exp),
            ))
        }
        EExpr::Tuple(exprs) => {
            let stexprs: Vec<STExpr> = exprs
                .into_iter()
                .map(|e| type_expr(e, ctx, eta, mu))
                .collect::<Result<Vec<_>, _>>()?;
            let t = Type::Tuple(stexprs.iter().map(STExpr::extract_type).collect());

            let s = stexprs
                .iter()
                .map(STExpr::extract_s)
                .fold(Ok(S::None), |acc, x| S::join(acc?, x))?;
            Ok(STExpr::Tuple((t, s), stexprs))
        }
        EExpr::Lvalue(lhs) => {
            let (lhs, ell) = type_lhs(lhs, ctx, eta, mu)?;
            eta.drop(ell, lhs.extract_type())?;

            match lhs.extract_type() {
                Type::Ref(_, tau) => {
                    eta.may_move(ell, *tau.clone())?;
                    eta.mov(ell, *tau.clone());
                    println!("\n\neta: {:?}\n\n", eta);
                    Ok(STExpr::Lvalue((*tau, S::None), lhs))
                }
                _ => panic!(),
            }
        }
        EExpr::Seq(e1, e2) => {
            let te1 = type_expr(*e1, ctx, eta, mu)?;
            let te2 = type_expr(*e2, ctx, eta, mu)?;
            let s2 = te2.extract_s();
            Ok(STExpr::Seq(
                (te2.extract_type(), s2),
                Box::new(te1),
                Box::new(te2),
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
            let te1 = type_expr(*rhs, ctx, eta, mu)?;
            let s1 = te1.extract_s();


            ctx.push_level();
            ctx.assign(name.clone(), (false, te1.extract_type()));

            let ell = generate_address();
            let ss = if let STExpr::Tuple(_, stexprs) = te1.clone() {
                for _ in 0..stexprs.len() {
                    let _ = generate_address();
                }
                iter::once(s1)
                    .chain(stexprs.iter().map(STExpr::extract_s))
                    .collect()
                // stexprs.iter().map(STExpr::extract_s).collect()
            } else {
                vec![s1]
            };
            eta.insert(ell, ss);
            mu.insert(name.clone(), ell);
            let te2 = type_expr(*then, ctx, eta, mu)?;
            let s2 = te2.extract_s();
            ctx.pop_level();
            let t = te2.extract_type();
            Ok(STExpr::Let {
                name,
                rhs: Box::new(te1),
                then: Box::new(te2),
                t: (t, s2),
            })
        }
        EExpr::MutLet { name, rhs, then } => {
            let te1 = type_expr(*rhs, ctx, eta, mu)?;
            let s1 = te1.extract_s();


            ctx.push_level();
            ctx.assign(name.clone(), (true, te1.extract_type()));

            let ell = generate_address();
            let ss = if let STExpr::Tuple(_, stexprs) = te1.clone() {
                for _ in 0..stexprs.len() {
                    let _ = generate_address();
                }
                iter::once(s1)
                    .chain(stexprs.iter().map(STExpr::extract_s))
                    .collect()

                // stexprs.iter().map(STExpr::extract_s).collect()
            } else {
                vec![s1]
            };
            eta.insert(ell, ss);
            mu.insert(name.clone(), ell);
            let te2 = type_expr(*then, ctx, eta, mu)?;
            let s2 = te2.extract_s();
            ctx.pop_level();
            let t = te2.extract_type();
            Ok(STExpr::MutLet {
                name,
                rhs: Box::new(te1),
                then: Box::new(te2),
                t: (t, s2),
            })
        }
        EExpr::Assign(lhs, e2) => {
            let te2 = type_expr(*e2.clone(), ctx, eta, mu)?;
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
                        .chain(stexprs.iter().map(STExpr::extract_s))
                        .collect()

                    // stexprs.iter().map(STExpr::extract_s).collect()
                } else {
                    vec![s]
                };
                eta.insert(ell, ss);
                Ok(STExpr::Assign((Type::Unit, S::None), lhs, Box::new(te2)))
            } else {
                Err(format!("You can't assign to {:?} with {:?}", lhs, e2))
            }
        }
    }
}
