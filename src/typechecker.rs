use std::collections::{HashMap, HashSet};
use std::iter::once;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{EExpr, ELhs, TExpr, TLhs};
use crate::types::{Address, Type, Var};

type ColoredType = (bool, Type);
type TypeContext = HashMap<Var, ColoredType>;
type S = HashSet<(bool, Address)>;
pub type Eta = HashMap<Address, S>;
pub type Mu = HashMap<Var, Address>;

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
    result as i64
}

pub fn type_lhs(lhs: &ELhs, ctx: &Gamma, eta: &Eta, mu: &Mu) -> Result<TLhs, String> {
    match lhs {
        ELhs::Var(name) => {
            let (m, tau) = ctx.get(name)?;
            Ok(TLhs::Var(
                Type::Ref(
                    m.to_owned(),
                    Box::new(tau.to_owned()),
                    mu.get(name).unwrap().to_owned(),
                ),
                name.to_string(),
            ))
        }
        ELhs::DeRef(rec_lhs) => {
            match type_expr(
                &EExpr::Lvalue(*rec_lhs.to_owned()),
                &mut ctx.clone(),
                &mut eta.clone(),
                &mut mu.clone(),
            )?
            .0
            .extract_type()
            {
                Type::Ref(m, tau, name) => Ok(TLhs::DeRef(
                    Type::Ref(m, tau, name),
                    Box::new(type_lhs(&*rec_lhs.to_owned(), ctx, eta, mu)?),
                )),
                _ => Err(format!("You can't dereference a non-reference value")),
            }
        }
    }
}

pub fn type_expr(
    expr: &EExpr,
    ctx: &mut Gamma,
    eta: &mut Eta,
    mu: &mut Mu,
) -> Result<(TExpr, S), String> {
    match expr {
        EExpr::Unit => Ok((TExpr::Unit(Type::Unit), S::new())),
        EExpr::Num(x) => Ok((TExpr::Num(Type::Int, x.to_owned()), S::new())),
        EExpr::Neg(a) => {
            let (a, s) = type_expr(&*a.to_owned(), ctx, eta, mu)?;
            assert!(s == S::new());
            match a.extract_type() {
                Type::Int => Ok((TExpr::Neg(Type::Int, Box::new(a)), S::new())),
                _ => Err(format!("You can negate only integers.")),
            }
        }
        EExpr::Gt(a, b) => {
            let (a, s_1) = type_expr(&*a.to_owned(), ctx, eta, mu)?;
            let (b, s_2) = type_expr(&*b.to_owned(), ctx, eta, mu)?;
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok((TExpr::Gt(Type::Bool, Box::new(a), Box::new(b)), S::new()))
                }
                _ => Err(format!("You can compare only integers together.")),
            }
        }
        EExpr::Lt(a, b) => {
            let (a, s_1) = type_expr(&*a.to_owned(), ctx, eta, mu)?;
            let (b, s_2) = type_expr(&*b.to_owned(), ctx, eta, mu)?;
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok((TExpr::Lt(Type::Bool, Box::new(a), Box::new(b)), S::new()))
                }
                _ => Err(format!("You can compare only integers together.")),
            }
        }
        EExpr::Add(a, b) => {
            let (a, s_1) = type_expr(&*a.to_owned(), ctx, eta, mu)?;
            let (b, s_2) = type_expr(&*b.to_owned(), ctx, eta, mu)?;
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok((TExpr::Add(Type::Int, Box::new(a), Box::new(b)), S::new()))
                }
                _ => Err(format!("You can add only integers together.")),
            }
        }
        EExpr::Sub(a, b) => {
            let (a, s_1) = type_expr(&*a.to_owned(), ctx, eta, mu)?;
            let (b, s_2) = type_expr(&*b.to_owned(), ctx, eta, mu)?;
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok((TExpr::Sub(Type::Int, Box::new(a), Box::new(b)), S::new()))
                }
                _ => Err(format!("You can subtract only integers together.")),
            }
        }
        EExpr::Mul(a, b) => {
            let (a, s_1) = type_expr(&*a.to_owned(), ctx, eta, mu)?;
            let (b, s_2) = type_expr(&*b.to_owned(), ctx, eta, mu)?;
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok((TExpr::Mul(Type::Int, Box::new(a), Box::new(b)), S::new()))
                }
                _ => Err(format!("You can multiply only integers together.")),
            }
        }
        EExpr::Div(a, b) => {
            let (a, s_1) = type_expr(&*a.to_owned(), ctx, eta, mu)?;
            let (b, s_2) = type_expr(&*b.to_owned(), ctx, eta, mu)?;
            assert!(s_1 == S::new());
            assert!(s_2 == S::new());
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => {
                    Ok((TExpr::Div(Type::Int, Box::new(a), Box::new(b)), S::new()))
                }
                _ => Err(format!("You can divide only integers together.")),
            }
        }
        EExpr::Cond(cond, then_exp, else_exp) => {
            let (cond, _) = type_expr(&*cond.to_owned(), ctx, eta, mu)?;
            if cond.extract_type() != Type::Bool {
                Err(format!(
                    "If-expr conditions must of type bool not of type {:?}",
                    cond.extract_type()
                ))?
            }

            let (then_exp, s_1) = type_expr(&*then_exp.to_owned(), ctx, eta, mu)?;
            let (else_exp, s_2) = type_expr(&*else_exp.to_owned(), ctx, eta, mu)?;

            let then_tau = then_exp.extract_type();
            let else_tau = else_exp.extract_type();
            if then_tau != else_tau {
                Err(format!(
                    "If-expr branches must have the same type. They instead are of {:?} and {:?}",
                    then_tau, else_tau
                ))?
            }

            Ok((
                TExpr::Cond(
                    then_tau,
                    Box::new(cond),
                    Box::new(then_exp),
                    Box::new(else_exp),
                ),
                s_1.union(&s_2).map(|a| a.to_owned()).collect::<S>(),
            ))
        }
        EExpr::Tuple(exprs) => {
            let (texprs, ss): (Vec<_>, Vec<_>) = exprs
                .into_iter()
                .map(|e| type_expr(e, ctx, eta, mu))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .unzip();
            let t = Type::Tuple(texprs.iter().map(TExpr::extract_type).collect());
            let s = ss.into_iter().flatten().collect::<HashSet<_>>();
            Ok((TExpr::Tuple(t, texprs), s))
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
                        .values()
                        .any(|s| s.contains(&(false, ell)) || s.contains(&(true, ell)))
                    {
                        Err(format!("cannot move out of `lhs` because it is borrowed / behind a shared reference"))?
                    } else {
                        let s = eta.get(&ell).unwrap();
                        Ok((TExpr::Lvalue(*tau, lhs), s.to_owned()))
                    }
                }
                _ => panic!(),
            }
        }
        EExpr::Seq(e1, e2) => {
            let (te1, _) = type_expr(&*e1.to_owned(), ctx, eta, mu)?;
            let (te2, s2) = type_expr(&*e2.to_owned(), ctx, eta, mu)?;
            Ok((
                TExpr::Seq(te2.extract_type(), Box::new(te1), Box::new(te2)),
                s2,
            ))
        }
        EExpr::Ref(name) => {
            let (_, tau) = ctx.get(name)?;
            let ell = mu.get(name).unwrap().to_owned();
            if eta.values().any(|s| s.contains(&(true, ell))) {
                Err(format!(
                    "cannot borrow `{}` as immutable because it is also borrowed as mutable",
                    name
                ))?
            }

            Ok((
                TExpr::Ref(
                    Type::Ref(false, Box::new(tau.to_owned()), ell),
                    name.to_string(),
                ),
                HashSet::from_iter(once((false, ell))),
            ))
        }
        EExpr::MutRef(name) => {
            let (m, tau) = ctx.get(name)?;
            if !m {
                Err(format!(
                    "cannot borrow `{}` as mutable, as it is not declared as mutable",
                    name
                ))?
            }

            let ell = mu.get(name).unwrap().to_owned();
            if eta.values().any(|s| s.contains(&(false, ell))) {
                Err(format!(
                    "cannot borrow `{}` as mutable because it is also borrowed as immutable",
                    name
                ))?
            };
            if eta.values().any(|s| s.contains(&(true, ell))) {
                Err(format!(
                    "cannot borrow `{}` as mutable more than once at a time",
                    name
                ))?
            };

            Ok((
                TExpr::MutRef(
                    Type::Ref(true, Box::new(tau.to_owned()), ell),
                    name.to_string(),
                ),
                HashSet::from_iter(once((true, ell))),
            ))
        }
        EExpr::Let { name, rhs, then } => {
            let (te1, s1) = type_expr(rhs, ctx, eta, mu)?;
            ctx.push_level();
            ctx.assign(name.to_owned(), (false, te1.extract_type()));

            let ell = generate_address();
            eta.insert(ell, s1);
            mu.insert(name.to_owned(), ell);
            let (te2, s2) = type_expr(then, ctx, eta, mu)?;
            ctx.pop_level();
            let t = te2.extract_type();
            Ok((
                TExpr::Let {
                    name: name.to_owned(),
                    rhs: Box::new(te1),
                    then: Box::new(te2),
                    t,
                },
                s2,
            ))
        }
        EExpr::MutLet { name, rhs, then } => {
            let (te1, s1) = type_expr(rhs, ctx, eta, mu)?;
            ctx.push_level();
            ctx.assign(name.to_owned(), (true, te1.extract_type()));

            let ell = generate_address();
            eta.insert(ell, s1);
            mu.insert(name.to_owned(), ell);
            let (te2, s2) = type_expr(then, ctx, eta, mu)?;
            ctx.pop_level();
            let t = te2.extract_type();
            Ok((
                TExpr::MutLet {
                    name: name.to_owned(),
                    rhs: Box::new(te1),
                    then: Box::new(te2),
                    t,
                },
                s2,
            ))
        }
        EExpr::Assign(lhs, e2) => {
            let (e2, s) = type_expr(e2, ctx, eta, mu)?;
            let lhs = type_lhs(lhs, ctx, eta, mu)?;
            let (tau_lhs, ell) = match lhs.extract_type() {
                Type::Ref(false, _, _) => Err(format!("You can't assign to an immutable")),
                Type::Ref(true, tau, ell) => Ok((*tau, ell)),
                _ => panic!(),
            }?;

            if tau_lhs == e2.extract_type() {
                eta.insert(ell, s);
                Ok((TExpr::Assign(Type::Unit, lhs, Box::new(e2)), S::new()))
            } else {
                Err(format!("You can't assign to {:?} with {:?}", lhs, e2))
            }
        }
    }
}
