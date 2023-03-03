use std::collections::HashMap;
use std::iter;

use crate::ast::{Expr, Lhs};

type Value = i64;
type ColoredValue = (bool, Value); // false -> pointed memory cannot be modified
pub type Memory = HashMap<Address, ColoredValue>; // Delta
type Var = String;
type Address = Value;
pub type Context = HashMap<Var, Value>; // Mu

use std::sync::atomic::{AtomicUsize, Ordering};

fn generate_address() -> Address {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let result = COUNTER.fetch_add(1, Ordering::Relaxed);
    result as i64
}

fn cxt_lookup(vars: &Vec<Context>, name: &String, error: String) -> Result<Address, String> {
    if let Some(ell) = vars.into_iter().rev().find_map(|scope| scope.get(name)) {
        Ok(ell.to_owned())
    } else {
        Err(error)
    }
}

fn cxt_new_scope(vars: Vec<Context>) -> Vec<Context> {
    vars.into_iter().chain(iter::once(HashMap::new())).collect()
}

// fn cxt_pop_scope(vars: Vec<Context>) -> Vec<Context> {
//     let len = vars.len();
//     vars.into_iter().enumerate().filter(|(i, _)| *i != len - 1).map(|(_, e)| e).collect()
// }

fn cxt_write(vars: &mut Vec<Context>, name: String, ell: Address) -> Result<(), String> {
    if let Some(cxt) = vars.last_mut() {
        cxt.insert(name, ell);
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
    lhs: &Lhs,
    vars: &mut Vec<Context>,
    memory: &mut Memory,
) -> Result<Address, String> {
    match lhs {
        Lhs::Var(name) => cxt_lookup(
            vars,
            name,
            format!("Failed to find name {} in context", name),
        ),
        Lhs::DeRef(rec_lhs) => {
            let (_, ell) = eval(&Expr::Lvalue(*(rec_lhs.to_owned())), vars, memory)?;
            Ok(ell)
        }
    }
}

pub fn eval(
    expr: &Expr,
    vars: &mut Vec<Context>,
    memory: &mut Memory,
) -> Result<ColoredValue, String> {
    println!("Expr {:?}\nVars: {:?}\nMem: {:?}\n", expr, vars, memory);
    match expr {
        Expr::Unit => todo!("type checking"),
        Expr::Num(x) => Ok((false, x.to_owned())),
        Expr::Neg(a) => Ok((false, -eval(a, vars, memory)?.1)),
        Expr::Add(a, b) => Ok((false, eval(a, vars, memory)?.1 + eval(b, vars, memory)?.1)),
        Expr::Sub(a, b) => Ok((false, eval(a, vars, memory)?.1 - eval(b, vars, memory)?.1)),
        Expr::Mul(a, b) => Ok((false, eval(a, vars, memory)?.1 * eval(b, vars, memory)?.1)),
        Expr::Div(a, b) => Ok((false, eval(a, vars, memory)?.1 / eval(b, vars, memory)?.1)),
        Expr::Lvalue(lhs) => {
            let ell = eval_lhs(lhs, vars, memory)?;
            mem_lookup(
                memory,
                &ell,
                format!("Attempted to access an address that cannot be found"),
            )
        }
        Expr::Seq(e1, e2) => {
            eval(e1, vars, memory)?;
            Ok(eval(e2, vars, memory)?)
        }
        Expr::Ref(name) => {
            let ell = cxt_lookup(
                vars,
                name,
                format!("Attempted to reference a variable that is not bound"),
            )?;
            Ok((false, ell.to_owned()))
        }
        Expr::MutRef(name) => {
            let ell = cxt_lookup(
                vars,
                name,
                format!("Attempted to reference a variable that is not bound"),
            )?;
            Ok((true, ell.to_owned()))
        }
        Expr::Let { name, rhs, then } => {
            if vars.into_iter().any(|scope| scope.contains_key(name)) {
                Err(format!("Attempted to bind to already bound name: {}", name))?
            }

            let ell = generate_address();

            let (_, val) = eval(rhs, vars, memory)?;

            let vars_prime = &mut cxt_new_scope(vars.to_vec());
            cxt_write(vars_prime, name.to_owned(), ell)?;

            memory.insert(ell, (false, val));
            let output = eval(then, vars_prime, memory);

            output
        }
        Expr::MutLet { name, rhs, then } => {
            if vars.into_iter().any(|scope| scope.contains_key(name)) {
                Err(format!("Attempted to bind to already bound name: {}", name))?
            }

            let ell = generate_address();

            let (_, val) = eval(rhs, vars, memory)?;

            let vars_prime = &mut cxt_new_scope(vars.to_vec());
            cxt_write(vars_prime, name.to_owned(), ell)?;

            memory.insert(ell, (true, val));
            let output = eval(then, vars_prime, memory);

            output
        }
        Expr::Assign(lhs, e2) => {
            println!(
                "ASSSIGN {:?}   {:?} = {:?}",
                lhs,
                eval_lhs(lhs, vars, memory)?,
                eval(e2, vars, memory)?
            );

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
            memory.insert(ell, (true, val));

            // todo: return `(false, unit)` rather than `(false, val)`
            println!("Write: l={};   v={};   m={:?}", ell, val, memory);
            Ok((false, val))
        }
    }
}
