use std::collections::HashMap;
use std::iter;

use crate::ast::{TExpr, TLhs};

type Var = String;
type Address = i64;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Unit,
    Ref(Address),
}
type ColoredValue = (bool, Value); // false -> pointed memory cannot be modified
pub type Memory = HashMap<Address, ColoredValue>; // Delta
pub type Context = HashMap<Var, Address>; // Mu

use std::sync::atomic::{AtomicUsize, Ordering};

fn generate_address() -> Address {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let result = COUNTER.fetch_add(1, Ordering::Relaxed);
    result as i64
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

fn mem_lookup(memory: &Memory, ell: &Address, error: String) -> Result<ColoredValue, String> {
    if let Some(cv) = memory.get(ell) {
        Ok(cv.to_owned())
    } else {
        Err(error)
    }
}

pub fn eval_lhs(
    lhs: &TLhs,
    vars: &mut Vec<Context>,
    memory: &mut Memory,
) -> Result<Address, String> {
    match lhs {
        TLhs::Var(_t, name) => ctx_lookup(
            vars,
            name,
            format!("Failed to find name {} in context", name),
        ),
        TLhs::DeRef(t, rec_lhs) => {
            let (_, v) = eval(
                &TExpr::Lvalue(t.to_owned(), *(rec_lhs.to_owned())),
                vars,
                memory,
            )?;
            match v {
                Value::Ref(ell) => Ok(ell),
                _ => Err(format!("You can't dereference a non-reference")),
            }
        }
    }
}

pub fn eval(
    expr: &TExpr,
    vars: &mut Vec<Context>,
    memory: &mut Memory,
) -> Result<ColoredValue, String> {
    match expr {
        TExpr::Unit(_t) => Ok((false, Value::Unit)),
        TExpr::Num(_t, x) => Ok((false, Value::Int(x.to_owned()))),
        TExpr::Neg(_t, a) => match eval(a, vars, memory)?.1 {
            Value::Int(i) => Ok((false, Value::Int(-i))),
            _ => panic!("Typechecker should've prevented negation of non-integer"),
        },
        TExpr::Add(_t, a, b) => match (eval(a, vars, memory)?.1, eval(b, vars, memory)?.1) {
            (Value::Int(i1), Value::Int(i2)) => Ok((false, Value::Int(i1 + i2))),
            _ => panic!("Typechecker should've prevented addition involving non-integers"),
        },
        TExpr::Sub(_t, a, b) => match (eval(a, vars, memory)?.1, eval(b, vars, memory)?.1) {
            (Value::Int(i1), Value::Int(i2)) => Ok((false, Value::Int(i1 - i2))),
            _ => panic!("Typechecker should've prevented subtraction involving non-integers"),
        },
        TExpr::Mul(_t, a, b) => match (eval(a, vars, memory)?.1, eval(b, vars, memory)?.1) {
            (Value::Int(i1), Value::Int(i2)) => Ok((false, Value::Int(i1 + i2))),
            _ => panic!("Typechecker should've prevented multiplication involving non-integers"),
        },
        TExpr::Div(_t, a, b) => match (eval(a, vars, memory)?.1, eval(b, vars, memory)?.1) {
            (Value::Int(i1), Value::Int(i2)) => Ok((false, Value::Int(i1 / i2))),
            _ => panic!("Typechecker should've prevented division involving non-integers"),
        },
        TExpr::Lvalue(_t, lhs) => {
            let ell = eval_lhs(lhs, vars, memory)?;
            mem_lookup(
                memory,
                &ell,
                format!("Attempted to access an address that cannot be found"),
            )
        }
        TExpr::Seq(_t, e1, e2) => {
            eval(e1, vars, memory)?;
            Ok(eval(e2, vars, memory)?)
        }
        TExpr::Ref(_t, name) => {
            let ell = ctx_lookup(
                vars,
                name,
                format!("Attempted to reference a variable that is not bound"),
            )?;
            Ok((false, Value::Ref(ell.to_owned())))
        }
        TExpr::MutRef(_t, name) => {
            let ell = ctx_lookup(
                vars,
                name,
                format!("Attempted to reference a variable that is not bound"),
            )?;
            Ok((true, Value::Ref(ell.to_owned())))
        }
        TExpr::Let {
            name,
            rhs,
            then,
            t: _,
        } => {
            if vars.into_iter().any(|scope| scope.contains_key(name)) {
                Err(format!("Attempted to bind to already bound name: {}", name))?
            }

            let ell = generate_address();

            let (_, val) = eval(rhs, vars, memory)?;

            let vars_prime = &mut ctx_new_scope(vars.to_vec());
            ctx_write(vars_prime, name.to_owned(), ell)?;

            memory.insert(ell, (false, val));
            let output = eval(then, vars_prime, memory);

            output
        }
        TExpr::MutLet {
            name,
            rhs,
            then,
            t: _,
        } => {
            if vars.into_iter().any(|scope| scope.contains_key(name)) {
                Err(format!("Attempted to bind to already bound name: {}", name))?
            }

            let ell = generate_address();

            let (_, val) = eval(rhs, vars, memory)?;

            let vars_prime = &mut ctx_new_scope(vars.to_vec());
            ctx_write(vars_prime, name.to_owned(), ell)?;

            memory.insert(ell, (true, val));
            let output = eval(then, vars_prime, memory);

            output
        }
        TExpr::Assign(_t, lhs, e2) => {
            let (_, val) = eval(e2, vars, memory)?;
            let ell = eval_lhs(lhs, vars, memory)?;

            let (c_1, _) = mem_lookup(
                memory,
                &ell,
                format!("Attempted to access an address that cannot be found"),
            )?;

            if !c_1 {
                Err(format!(
                    "Attempted to assign to the pointed value of an immutably bound reference"
                ))?
            }
            memory.insert(ell, (true, val.clone()));

            // todo: return `(false, unit)` rather than `(false, val)`
            // println!("Write: l={:?};   v={:?};   m={:?}", ell, val, memory);
            Ok((false, val))
        }
    }
}
