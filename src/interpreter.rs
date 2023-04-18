use std::collections::HashMap;
use std::iter;

use crate::ast::{TExpr, TLhs};
use crate::types::{Address, Var};

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Tuple(Vec<Value>),
    Unit,
    Ref(Address),
}

type ColoredValue = (bool, Value); // false -> pointed memory cannot be modified

#[derive(Debug, Clone)]
pub struct Delta {
    pub memory: HashMap<Address, ColoredValue>,
}
impl Delta {
    fn get(&self, ell: &Address) -> Option<ColoredValue> {
        let val = self.memory.get(&ell).cloned();
        if let Some((b, Value::Tuple(values))) = val.clone() {
            let t: Vec<_> = (1..=values.len())
                .map(|offset| self.get(&(ell + offset as u64)).unwrap().1)
                .collect();
            Some((b, Value::Tuple(t)))
        } else {
            val
        }
    }

    fn insert(&mut self, ell: Address, cvs: Vec<ColoredValue>) {
        for (offset, cv) in cvs.into_iter().enumerate() {
            self.memory.insert(ell + (offset as u64), cv);
        }
    }
}

pub type Context = HashMap<Var, Address>; // Mu

use std::sync::atomic::{AtomicUsize, Ordering};

fn generate_address() -> Address {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let result = COUNTER.fetch_add(1, Ordering::Relaxed);
    result as u64
}

fn ctx_lookup(vars: &Vec<Context>, name: &String, error: String) -> Result<Address, String> {
    if let Some(ell) = vars.into_iter().rev().find_map(|scope| scope.get(name)) {
        Ok(ell.to_owned())
    } else {
        Err(error)
    }
}

fn ctx_new_scope(vars: Vec<Context>) -> Vec<Context> {
    vars.into_iter().chain(iter::once(HashMap::new())).collect()
}

// fn ctx_pop_scope(vars: Vec<Context>) -> Vec<Context> {
//     let len = vars.len();
//     vars.into_iter().enumerate().filter(|(i, _)| *i != len - 1).map(|(_, e)| e).collect()
// }

fn ctx_write(vars: &mut Vec<Context>, name: String, ell: Address) -> Result<(), String> {
    if let Some(ctx) = vars.last_mut() {
        ctx.insert(name, ell);
        Ok(())
    } else {
        Err(format!(
            "Attempted to write to empty context. How did this happen?"
        ))?
    }
}

fn mem_lookup(delta: &Delta, ell: &Address, error: String) -> Result<ColoredValue, String> {
    if let Some(cv) = delta.get(ell) {
        Ok(cv)
    } else {
        Err(error)
    }
}

pub fn eval_lhs(lhs: TLhs, vars: &mut Vec<Context>, delta: &mut Delta) -> Result<Address, String> {
    match lhs {
        TLhs::Var(_t, name) => ctx_lookup(
            vars,
            &name,
            format!("Failed to find name {} in context", name),
        ),
        TLhs::DeRef(t, rec_lhs) => {
            let (_, v) = eval(
                TExpr::Lvalue(t.to_owned(), *(rec_lhs.to_owned())),
                vars,
                delta,
            )?;
            match v {
                Value::Ref(ell) => Ok(ell),
                _ => Err(format!("You can't dereference a non-reference")),
            }
        }
        TLhs::Index(_t, l, i) => {
            let ell = eval_lhs(*l, vars, delta)?;
            Ok(ell + i)
        }
    }
}

pub fn eval(
    expr: TExpr,
    vars: &mut Vec<Context>,
    delta: &mut Delta,
) -> Result<ColoredValue, String> {
    match expr {
        TExpr::Unit(_t) => Ok((false, Value::Unit)),
        TExpr::Num(_t, x) => Ok((false, Value::Int(x.to_owned()))),
        TExpr::Neg(_t, a) => match eval(*a, vars, delta)?.1 {
            Value::Int(i) => Ok((false, Value::Int(-i))),
            _ => panic!("Typechecker should've prevented negation of non-integer"),
        },
        TExpr::Gt(_t, a, b) => match (eval(*a, vars, delta)?.1, eval(*b, vars, delta)?.1) {
            (Value::Int(i1), Value::Int(i2)) => Ok((false, Value::Bool(i1 > i2))),
            _ => panic!("Typechecker should've prevented comparison involving non-integers"),
        },
        TExpr::Lt(_t, a, b) => match (eval(*a, vars, delta)?.1, eval(*b, vars, delta)?.1) {
            (Value::Int(i1), Value::Int(i2)) => Ok((false, Value::Bool(i1 < i2))),
            _ => panic!("Typechecker should've prevented comparison involving non-integers"),
        },
        TExpr::Add(_t, a, b) => match (eval(*a, vars, delta)?.1, eval(*b, vars, delta)?.1) {
            (Value::Int(i1), Value::Int(i2)) => Ok((false, Value::Int(i1 + i2))),
            _ => panic!("Typechecker should've prevented addition involving non-integers"),
        },
        TExpr::Sub(_t, a, b) => match (eval(*a, vars, delta)?.1, eval(*b, vars, delta)?.1) {
            (Value::Int(i1), Value::Int(i2)) => Ok((false, Value::Int(i1 - i2))),
            _ => panic!("Typechecker should've prevented subtraction involving non-integers"),
        },
        TExpr::Mul(_t, a, b) => match (eval(*a, vars, delta)?.1, eval(*b, vars, delta)?.1) {
            (Value::Int(i1), Value::Int(i2)) => Ok((false, Value::Int(i1 + i2))),
            _ => panic!("Typechecker should've prevented multiplication involving non-integers"),
        },
        TExpr::Div(_t, a, b) => match (eval(*a, vars, delta)?.1, eval(*b, vars, delta)?.1) {
            (Value::Int(i1), Value::Int(i2)) => Ok((false, Value::Int(i1 / i2))),
            _ => panic!("Typechecker should've prevented division involving non-integers"),
        },
        TExpr::Cond(_t, cond, then_expr, else_expr) => match eval(*cond, vars, delta)?.1 {
            Value::Bool(b) => {
                if b {
                    eval(*then_expr, vars, delta)
                } else {
                    eval(*else_expr, vars, delta)
                }
            }
            _ => panic!("Typechecker should've prevented if-condition not being of type bool"),
        },
        TExpr::Tuple(_t, texprs) => Ok((
            true,
            Value::Tuple(
                texprs
                    .into_iter()
                    .map(|texpr| eval(texpr, vars, delta))
                    .collect::<Result<Vec<(bool, Value)>, String>>()?
                    .into_iter()
                    .map(|t| t.1)
                    .collect(),
            ),
        )),
        TExpr::Lvalue(_t, lhs) => {
            let ell = eval_lhs(lhs, vars, delta)?;
            mem_lookup(
                delta,
                &ell,
                format!("Attempted to access an address that cannot be found"),
            )
        }
        TExpr::Seq(_t, e1, e2) => {
            eval(*e1, vars, delta)?;
            Ok(eval(*e2, vars, delta)?)
        }
        TExpr::Ref(_t, name) => {
            let ell = ctx_lookup(
                vars,
                &name,
                format!("Attempted to reference a variable that is not bound"),
            )?;
            Ok((false, Value::Ref(ell.to_owned())))
        }
        TExpr::MutRef(_t, name) => {
            let ell = ctx_lookup(
                vars,
                &name,
                format!("Attempted to reference a variable that is not bound"),
            )?;
            Ok((true, Value::Ref(ell.to_owned())))
        }
        TExpr::Let {
            name,
            rhs,
            then,
            t: _,
        } => interpret_let(name, rhs, then, vars, delta),
        TExpr::MutLet {
            name,
            rhs,
            then,
            t: _,
        } => interpret_mutlet(name, rhs, then, vars, delta),
        TExpr::Assign(_t, lhs, e2) => interpret_assign(lhs, e2, vars, delta),
    }
}

pub fn interpret_let(
    name: String,
    rhs: Box<TExpr>,
    then: Box<TExpr>,
    vars: &mut Vec<Context>,
    delta: &mut Delta,
) -> Result<ColoredValue, String> {
    if vars.into_iter().any(|scope| scope.contains_key(&name)) {
        Err(format!("Attempted to bind to already bound name: {}", name))?
    }

    let (_, val) = eval(*rhs, vars, delta)?;
    let vars_prime = &mut ctx_new_scope(vars.to_vec());
    let ell = generate_address();
    ctx_write(vars_prime, name.to_owned(), ell)?;

    let cvals: Vec<ColoredValue> = if let Value::Tuple(values) = val.clone() {
        for _ in 0..values.len() {
            let _ = generate_address();
        }
        iter::once(val.clone())
            .chain(values)
            .map(|v| (false, v))
            .collect()
    } else {
        vec![(false, val.clone())]
    };

    delta.insert(ell, cvals);
    let output = eval(*then, vars_prime, delta);

    output
}
pub fn interpret_mutlet(
    name: String,
    rhs: Box<TExpr>,
    then: Box<TExpr>,
    vars: &mut Vec<Context>,
    delta: &mut Delta,
) -> Result<ColoredValue, String> {
    if vars.into_iter().any(|scope| scope.contains_key(&name)) {
        Err(format!("Attempted to bind to already bound name: {}", name))?
    }

    let (_, val) = eval(*rhs, vars, delta)?;
    let vars_prime = &mut ctx_new_scope(vars.to_vec());
    let ell = generate_address();
    ctx_write(vars_prime, name.to_owned(), ell)?;

    let cvals: Vec<ColoredValue> = if let Value::Tuple(values) = val.clone() {
        for _ in 0..values.len() {
            let _ = generate_address();
        }
        iter::once((true, val.clone()))
            .chain(values.into_iter().map(|v| (true, v)))
            .collect()
    } else {
        vec![(true, val.clone())]
    };

    delta.insert(ell, cvals);
    let output = eval(*then, vars_prime, delta);

    output
}

pub fn interpret_assign(
    lhs: TLhs,
    e2: Box<TExpr>,
    vars: &mut Vec<Context>,
    delta: &mut Delta,
) -> Result<ColoredValue, String> {
    let (_, val) = eval(*e2, vars, delta)?;
    let ell = eval_lhs(lhs, vars, delta)?;

    let (c_1, _) = mem_lookup(
        delta,
        &ell,
        format!("Attempted to access an address that cannot be found"),
    )?;

    if !c_1 {
        Err(format!(
            "Attempted to assign to the pointed value of an immutably bound reference"
        ))?
    }

    let cvals: Vec<ColoredValue> = if let Value::Tuple(values) = val.clone() {
        iter::once((true, val.clone()))
            .chain(values.into_iter().map(|v| (true, v)))
            .collect()
    } else {
        vec![(true, val.clone())]
    };

    delta.insert(ell, cvals);
    Ok((false, Value::Unit))
}

// have an Interpreter struct. impl Interpreter has all the eval_texpr_type methods.
// for the debugger, have a boolean flag field in Interpreter that controls whether "breakpoints" in major texpr_type_eval_steps actually suspend

// consider storing lexical information (starting/ending line_number/column_number )
