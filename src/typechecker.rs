use std::collections::HashMap;

use crate::ast::{EExpr, ELhs, TExpr, TLhs};
use crate::types::Type;

type Var = String;
type ColoredType = (bool, Type);
type TypeContext = HashMap<Var, ColoredType>;

#[derive(Debug, Clone)]
pub struct Gamma {
    pub vars: Vec<TypeContext>,
}

impl Gamma {
    fn get(&self, name: &Var) -> Result<ColoredType, String> {
        if let Some(ct) = self.vars.iter().rev().find_map(|frame| frame.get(name)) {
            Ok(ct.to_owned())
        } else {
            println!("vars = {:?}", self.vars);
            println!("Name = {:?}", name);
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
        println!("reac");
        self.vars.last_mut().unwrap().insert(name, ct);
        println!("{:?}", self.vars);
    }
    fn push_level(&mut self) {
        self.vars.push(HashMap::new())
    }
    fn pop_level(&mut self) {
        self.vars.pop().unwrap();
    }
}

pub fn type_lhs(lhs: &ELhs, ctx: &mut Gamma) -> Result<TLhs, String> {
    match lhs {
        ELhs::Var(name) => {
            let (m, tau) = ctx.get(name)?;
            Ok(TLhs::Var(
                Type::Ref(m.to_owned(), Box::new(tau.to_owned())),
                name.to_string(),
            ))
        }
        ELhs::DeRef(rec_lhs) => {
            let mut immut_ctx = ctx.clone();
            match type_expr(&EExpr::Lvalue(*rec_lhs.to_owned()), &mut immut_ctx)?.extract_type() {
                Type::Ref(m, tau) => Ok(TLhs::DeRef(
                    Type::Ref(m, tau),
                    Box::new(type_lhs(&*rec_lhs.to_owned(), ctx)?),
                )),
                _ => Err(format!("You can't dereference a non-reference value")),
            }
        }
    }
}

pub fn type_expr(expr: &EExpr, ctx: &mut Gamma) -> Result<TExpr, String> {
    match expr {
        EExpr::Unit => Ok(TExpr::Unit(Type::Unit)),
        EExpr::Num(x) => Ok(TExpr::Num(Type::Int, x.to_owned())),
        EExpr::Neg(a) => {
            let a = type_expr(&*a.to_owned(), ctx)?;
            match a.extract_type() {
                Type::Int => Ok(TExpr::Neg(Type::Int, Box::new(a))),
                _ => Err(format!("You can negate only integers.")),
            }
        }
        EExpr::Add(a, b) => {
            let a = type_expr(&*a.to_owned(), ctx)?;
            let b = type_expr(&*b.to_owned(), ctx)?;
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(TExpr::Add(Type::Int, Box::new(a), Box::new(b))),
                _ => Err(format!("You can add only integers together.")),
            }
        }
        EExpr::Sub(a, b) => {
            let a = type_expr(&*a.to_owned(), ctx)?;
            let b = type_expr(&*b.to_owned(), ctx)?;
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(TExpr::Sub(Type::Int, Box::new(a), Box::new(b))),
                _ => Err(format!("You can subtract only integers together.")),
            }
        }
        EExpr::Mul(a, b) => {
            let a = type_expr(&*a.to_owned(), ctx)?;
            let b = type_expr(&*b.to_owned(), ctx)?;
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(TExpr::Mul(Type::Int, Box::new(a), Box::new(b))),
                _ => Err(format!("You can multiply only integers together.")),
            }
        }
        EExpr::Div(a, b) => {
            let a = type_expr(&*a.to_owned(), ctx)?;
            let b = type_expr(&*b.to_owned(), ctx)?;
            match (a.extract_type(), b.extract_type()) {
                (Type::Int, Type::Int) => Ok(TExpr::Div(Type::Int, Box::new(a), Box::new(b))),
                _ => Err(format!("You can divide only integers together.")),
            }
        }
        EExpr::Lvalue(lhs) => {
            let lhs = type_lhs(lhs, ctx)?;
            match lhs {
                TLhs::Var(_, ref name) => ctx.remove(&name),
                _ => (),
            };
            match lhs.extract_type() {
                Type::Ref(_, tau) => Ok(TExpr::Lvalue(*tau, lhs)),
                _ => panic!(),
            }
        }
        EExpr::Seq(e1, e2) => {
            let te1 = type_expr(&*e1.to_owned(), ctx)?;
            let te2 = type_expr(&*e2.to_owned(), ctx)?;
            Ok(TExpr::Seq(te2.extract_type(), Box::new(te1), Box::new(te2)))
        }
        EExpr::Ref(name) => {
            let (_, tau) = ctx.get(name)?;
            Ok(TExpr::Ref(
                Type::Ref(false, Box::new(tau.to_owned())),
                name.to_string(),
            ))
        }
        EExpr::MutRef(name) => {
            let (m, tau) = ctx.get(name)?;
            if m.to_owned() {
                // ctx.remove(name);
                Ok(TExpr::MutRef(
                    Type::Ref(true, Box::new(tau.to_owned())),
                    name.to_string(),
                ))
            } else {
                Err(format!("You can't mutably reference an immutable."))
            }
        }
        EExpr::Let { name, rhs, then } => {
            let rhs = type_expr(rhs, ctx)?;
            ctx.push_level();
            ctx.assign(name.to_owned(), (false, rhs.extract_type()));
            let then = type_expr(then, ctx)?;
            ctx.pop_level();
            Ok(TExpr::Let {
                name: name.to_owned(),
                rhs: Box::new(rhs),
                then: Box::new(then.clone()),
                t: then.extract_type(),
            })
        }
        EExpr::MutLet { name, rhs, then } => {
            let rhs = type_expr(rhs, ctx)?;
            ctx.push_level();
            ctx.assign(name.to_owned(), (true, rhs.extract_type()));
            println!("ctxAsd: {:?}", ctx.vars);
            let then = type_expr(then, ctx)?;
            ctx.pop_level();
            Ok(TExpr::MutLet {
                name: name.to_owned(),
                rhs: Box::new(rhs),
                then: Box::new(then.clone()),
                t: then.extract_type(),
            })
        }
        EExpr::Assign(lhs, e2) => {
            let e2 = type_expr(e2, ctx)?;
            let lhs = type_lhs(lhs, ctx)?;

            let tau_lhs = *match lhs.extract_type() {
                Type::Ref(true, tau) => Ok(tau),
                Type::Ref(false, _) => Err(format!("You can't assign to an immutable")),
                _ => panic!(),
            }?;

            if tau_lhs == e2.extract_type() {
                Ok(TExpr::Assign(Type::Unit, lhs, Box::new(e2)))
            } else {
                Err(format!("You can't assign to {:?} with {:?}", lhs, e2))
            }
        }
    }
}
