use std::collections::{HashMap, HashSet};
use std::iter::{self, once};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{EExpr, ELhs, STExpr, TLhs, S};
use crate::types::{Address, Type, Var};

type ColoredType = (bool, Type);
type TypeContext = HashMap<Var, ColoredType>;

pub type Mu = HashMap<Var, Address>;

#[derive(Debug, Clone)]
pub struct Eta {
    pub loans: HashMap<Address, S>,
}

impl Eta {
    fn get(&self, ell: Address) -> Option<HashSet<(bool, u64)>> {
        self.loans.get(&ell).cloned()
    }

    fn insert(&mut self, ell: Address, ss: Vec<S>) {
        for (offset, s) in ss.into_iter().enumerate() {
            self.loans.insert(ell + (offset as u64), s);
        }
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
    fn remove(&mut self, name: &Var) {
        if let Some(frame) = self
            .vars
            .iter_mut()
            .rev()
            .find(|frame| frame.contains_key(name))
        {
            frame.remove(name);
        } else {
            panic!()
        }
    }
    fn assign(&mut self, name: Var, ct: ColoredType) {
        self.vars.last_mut().unwrap().insert(name, ct);
    }
    fn push_level(&mut self) {
        self.vars.push(HashMap::new())
    }
    fn pop_level(&mut self) {
        self.vars.pop().unwrap();
    }
}

fn generate_address() -> Address {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let result = COUNTER.fetch_add(1, Ordering::Relaxed);
    result as u64
}

pub fn type_lhs(lhs: ELhs, ctx: &Gamma, eta: &mut Eta, mu: &mut Mu) -> Result<TLhs, String> {
    match lhs {
        ELhs::Var(name) => {
            let (m, tau) = ctx.get(&name)?;
            Ok(TLhs::Var(
                Type::Ref(m, Box::new(tau), mu.get(&name).unwrap().to_owned()),
                name.to_string(),
            ))
        }
        ELhs::DeRef(rec_lhs) => {
            match type_expr(
                EExpr::Lvalue(*rec_lhs.clone()),
                &mut ctx.clone(),
                &mut eta.clone(),
                &mut mu.clone(),
            )?
            .extract_type()
            {
                Type::Ref(m, tau, name) => Ok(TLhs::DeRef(
                    Type::Ref(m, tau, name),
                    Box::new(type_lhs(*rec_lhs, ctx, eta, mu)?),
                )),
                _ => Err(format!("You can't dereference a non-reference value")),
            }
        }
        ELhs::Index(l, i) => {
            let typed_l = type_lhs(*l, ctx, eta, mu)?;
            match typed_l.extract_type() {
                Type::Ref(b, boxed_t, ell) => {
                    if let Type::Tuple(types) = *boxed_t {
                        if let Some(ti) = types.get(i as usize) {
                            Ok(TLhs::Index(
                                Type::Ref(b, Box::new(ti.to_owned()), ell + i + 1),
                                Box::new(typed_l),
                                i,
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
        EExpr::Unit => Ok(STExpr::Unit((Type::Unit, S::new()))),
        EExpr::Num(x) => Ok(STExpr::Num((Type::Int, S::new()), x)),
        EExpr::Neg(a) => {
            let a = type_expr(*a.to_owned(), ctx, eta, mu)?;
            let s = a.extract_s();
            assert!(s == S::new());
            match a.extract_type() {
                Type::Int => Ok(STExpr::Neg((Type::Int, S::new()), Box::new(a))),
                _ => Err(format!("You can negate only integers.")),
            }
        }
        EExpr::Gt(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok(STExpr::Gt((Type::Bool, S::new()), Box::new(a), Box::new(b)))
                }
                _ => Err(format!("You can compare only integers together.")),
            }
        }
        EExpr::Lt(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok(STExpr::Lt((Type::Bool, S::new()), Box::new(a), Box::new(b)))
                }
                _ => Err(format!("You can compare only integers together.")),
            }
        }
        EExpr::Add(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Add(
                    (Type::Bool, S::new()),
                    Box::new(a),
                    Box::new(b),
                )),
                _ => Err(format!("You can add only integers together.")),
            }
        }
        EExpr::Sub(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Sub(
                    (Type::Bool, S::new()),
                    Box::new(a),
                    Box::new(b),
                )),
                _ => Err(format!("You can subtract only integers together.")),
            }
        }
        EExpr::Mul(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Mul(
                    (Type::Bool, S::new()),
                    Box::new(a),
                    Box::new(b),
                )),
                _ => Err(format!("You can multiply only integers together.")),
            }
        }
        EExpr::Div(a, b) => {
            let a = type_expr(*a, ctx, eta, mu)?;
            let s_1 = a.extract_s();
            let b = type_expr(*b, ctx, eta, mu)?;
            let s_2 = a.extract_s();
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(STExpr::Div(
                    (Type::Bool, S::new()),
                    Box::new(a),
                    Box::new(b),
                )),
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

            let then_exp = type_expr(*then_exp, ctx, eta, mu)?;
            let s_1 = then_exp.extract_s();
            let else_exp = type_expr(*else_exp, ctx, eta, mu)?;
            let s_2 = else_exp.extract_s();

            let then_tau = then_exp.extract_type();
            let else_tau = else_exp.extract_type();
            if then_tau != else_tau {
                Err(format!(
                    "If-expr branches must have the same type. They instead are of {:?} and {:?}",
                    then_tau, else_tau
                ))?
            }

            let s = s_1.union(&s_2).map(|a| a.to_owned()).collect::<S>();

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
                .flatten()
                .collect::<HashSet<_>>();
            Ok(STExpr::Tuple((t, s), stexprs))
        }
        EExpr::Lvalue(lhs) => {
            let lhs = type_lhs(lhs, ctx, eta, mu)?;
            match lhs {
                TLhs::Var(_, ref name) => {
                    ctx.remove(name);
                    mu.remove(name);
                }
                _ => (),
            };
            match lhs.extract_type() {
                Type::Ref(_, tau, ell) => {
                    if eta
                        .loans
                        .values()
                        .any(|s| s.contains(&(false, ell)) || s.contains(&(true, ell)))
                    {
                        Err(format!("cannot move out of `lhs` because it is borrowed / behind a shared reference"))?
                    } else {
                        let s = if let Type::Tuple(types) = *tau.clone() {
                            (1..=types.len())
                                .map(|offset| eta.get(ell + offset as u64).unwrap())
                                .flatten()
                                .collect()
                        } else {
                            eta.get(ell).unwrap()
                        };
                        Ok(STExpr::Lvalue((*tau, s.to_owned()), lhs))
                    }
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
        EExpr::Ref(name) => {
            let (_, tau) = ctx.get(&name)?;
            let ell = mu.get(&name).unwrap().to_owned();
            if eta.loans.values().any(|s| s.contains(&(true, ell))) {
                Err(format!(
                    "cannot borrow `{}` as immutable because it is also borrowed as mutable",
                    name
                ))?
            }

            let t = Type::Ref(false, Box::new(tau), ell);
            let s = HashSet::from_iter(once((false, ell)));

            Ok(STExpr::Ref((t, s), name.to_string()))
        }
        EExpr::MutRef(name) => {
            let (m, tau) = ctx.get(&name)?;
            if !m {
                Err(format!(
                    "cannot borrow `{}` as mutable, as it is not declared as mutable",
                    name
                ))?
            }

            let ell = mu.get(&name).unwrap().to_owned();
            if eta.loans.values().any(|s| s.contains(&(false, ell))) {
                Err(format!(
                    "cannot borrow `{}` as mutable because it is also borrowed as immutable",
                    name
                ))?
            };
            if eta.loans.values().any(|s| s.contains(&(true, ell))) {
                Err(format!(
                    "cannot borrow `{}` as mutable more than once at a time",
                    name
                ))?
            };

            let t = Type::Ref(true, Box::new(tau), ell);
            let s = HashSet::from_iter(once((true, ell)));

            Ok(STExpr::MutRef((t, s), name.to_string()))
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
            let lhs = type_lhs(lhs, ctx, eta, mu)?;
            let (tau_lhs, ell) = match lhs.extract_type() {
                Type::Ref(false, _, _) => Err(format!("You can't assign to an immutable")),
                Type::Ref(true, tau, ell) => Ok((*tau, ell)),
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
                Ok(STExpr::Assign((Type::Unit, S::new()), lhs, Box::new(te2)))
            } else {
                Err(format!("You can't assign to {:?} with {:?}", lhs, e2))
            }
        }
    }
}
