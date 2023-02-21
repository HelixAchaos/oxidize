use std::{collections::HashMap, hash::Hash};

use crate::ast::Expr;

type Value = i64;
type ColoredValue = (bool, Value); // false -> pointed memory cannot be modified
type Memory = HashMap<Address, ColoredValue>; // Delta
type Var = String;
type Address = Value;
type ColoredAddress = (bool, Address);
type Context = HashMap<Var, ColoredAddress>; // Mu

use std::sync::atomic::{AtomicUsize, Ordering};

fn generate_address() -> Address {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let result = COUNTER.fetch_add(1, Ordering::Relaxed);
    result as i64
}

pub fn eval<'a>(
    expr: &'a Expr,
    vars: &mut Vec<Context>,
    memory: &mut Memory,
) -> Result<ColoredValue, String> {
    match expr {
        Expr::Num(x) => Ok((true, x.to_owned())),
        Expr::Neg(a) => Ok((true, -eval(a, vars, memory)?.1)),
        Expr::Add(a, b) => Ok((true, eval(a, vars, memory)?.1 + eval(b, vars, memory)?.1)),
        Expr::Sub(a, b) => Ok((true, eval(a, vars, memory)?.1 - eval(b, vars, memory)?.1)),
        Expr::Mul(a, b) => Ok((true, eval(a, vars, memory)?.1 * eval(b, vars, memory)?.1)),
        Expr::Div(a, b) => Ok((true, eval(a, vars, memory)?.1 / eval(b, vars, memory)?.1)),
        Expr::Var(name) => {
            if let Some((is_mut, ell)) = vars.into_iter().rev().find_map(|scope| scope.get(name)) {
                if let Some((_, val)) = memory.get(ell) {
                    Ok((is_mut.to_owned(), val.to_owned()))
                } else {
                    Err(format!(
                        "Attempted to access an address that cannot be found"
                    ))
                }
            } else {
                Err(format!("Failed to find name {} in context", name))
            }
        }
        Expr::Seq(e1, e2) => {
            eval(e1, vars, memory)?;
            Ok(eval(e2, vars, memory)?)
        }
        Expr::Ref(e) => {
            let (_, val) = eval(e, vars, memory)?;
            let ell = generate_address();
            memory.insert(ell, (false, val));
            Ok((false, ell))
        }
        Expr::MutRef(e) => {
            let (is_mut, val) = eval(e, vars, memory)?;
            if !is_mut {
                Err(format!(
                    "Cannot borrow expression {:?} as mutable, as it's not declared as mutable'",
                    e
                ))?
            }
            let ell = generate_address();
            memory.insert(ell, (true, val));
            Ok((true, ell))
        } // (true,) <- memory
        Expr::DeRef(e) => {
            let (_, address) = eval(e, vars, memory)?;
            if let Some(value) = memory.get(&address) {
                Ok(*value)
            } else {
                Err(format!(
                    "Attempted to dereference a pointer which points to some unkown value"
                ))
            }
        }
        Expr::Let { name, rhs, then } => {
            if vars.into_iter().any(|scope| scope.contains_key(name)) {
                Err(format!("Attempted to bind to already bound name: {}", name))?
            }

            let (_, val) = eval(rhs, vars, memory)?;
            let ell = generate_address();
            memory.insert(ell, (false, val));

            vars.push(
                [(name.clone(), (false, ell))]
                    .iter()
                    .cloned()
                    .collect::<HashMap<_, _>>(),
            );
            let output = eval(then, vars, memory);
            vars.pop();
            output
        }
        Expr::MutLet { name, rhs, then } => {
            if vars.into_iter().any(|scope| scope.contains_key(name)) {
                Err(format!("Attempted to bind to already bound name: {}", name))?
            }

            let (_, val) = eval(rhs, vars, memory)?;
            let ell = generate_address();
            memory.insert(ell, (true, val));

            vars.push(
                [(name.clone(), (true, ell))]
                    .iter()
                    .cloned()
                    .collect::<HashMap<_, _>>(),
            );
            let output = eval(then, vars, memory);
            vars.pop();
            output
        }
        Expr::Assign {
            deref_count,
            name,
            rhs,
        } => {
            let val = eval(rhs, vars, memory)?;
            if let Some((_, mut ell)) = vars.iter().rev().find_map(|scope| scope.get(name)) {
                while *deref_count > 0 {
                    if let Some((c, addr)) = memory.get(&ell) {
                        if !c {
                            Err(format!(
                                "Attempted to assign to the pointed value of an immutably bound reference"
                            ))?
                        }
                        ell = *addr;
                    } else {
                        Err(format!(""))?
                    }
                }
                memory.insert(ell, val);
            } else {
                Err(format!("Attempted to assign to an unbound name {}", name))?
            }
            Ok(val)
        }
    }
}
