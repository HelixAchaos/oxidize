use std::{collections::HashMap, hash::Hash};

use crate::ast::Expr;


type Var = String;
type Value = i64;
type Address = i64;
type ColoredValue = (bool, Value);
type Memory = HashMap<Address, ColoredValue>; // true -> pointed memory can be modified
type Context = HashMap<Var, ColoredValue>; // true -> variable name can be reassigned to

// fn assign_var (context: Vec<Context>, val: Value) -> Result<unit, String> {

// }

fn get_var (vars: &Vec<Context>, name: &Var) -> Result<ColoredValue, String> {
    for scope in vars.iter().rev() {
        if let Some(val) = scope.get(name) {
            return Ok(*val);
        }
    }
    return Err(format!("Failed to find name {} in context", name))
}

// fn assign_var (vars: Vec<Context>, name: &Var, val: Value) -> Result<Vec<Context>, String> {

// }

pub fn eval<'a>(expr: &'a Expr, vars: &mut Vec<Context>, memory: &mut Memory) -> Result<ColoredValue, String> {
    match expr {
        Expr::Num(x) => Ok((false, *x)),
        Expr::Neg(a) => Ok((false, -eval(a, vars, memory)?.1)),
        Expr::Add(a, b) => Ok((false, eval(a, vars, memory)?.1 + eval(b, vars, memory)?.1)),
        Expr::Sub(a, b) => Ok((false, eval(a, vars, memory)?.1 - eval(b, vars, memory)?.1)),
        Expr::Mul(a, b) => Ok((false, eval(a, vars, memory)?.1 * eval(b, vars, memory)?.1)),
        Expr::Div(a, b) => Ok((false, eval(a, vars, memory)?.1 / eval(b, vars, memory)?.1)),
        Expr::Var(name) => get_var(vars, name),
        Expr::Seq(e1, e2) => {
            eval(e1, vars, memory)?;
            Ok(eval(e2, vars, memory)?)
        }
        Expr::Ref(_e) => {
            todo!("How to make this work? Should I store the reference in the value? That would make the values much fatter tho.")
        },
        Expr::MutRef(_e) => {
            todo!("same code pretty much as expr::ref")
        }, // (true,) <- memory
        Expr::DeRef(e) => {
            let (_, address) = eval(e, vars, memory)?;
            if let Some(value) = memory.get(&address) {
                Ok(*value)
            } else {
                Err(format!("Attempted to dereference a pointer which points to some unkown value"))
            }
        },
        Expr::Let { name, rhs, then } => {
            let (_, val) = eval(rhs, vars, memory)?;
            let is_mut = vars.iter().rev().all(|scope| if let Some((is_mut, _)) = scope.get(name) { *is_mut } else { true });
            let is_found = vars.iter().any(|scope| scope.get(name).is_some());
            if !is_mut && is_found {
                Err(format!("Attempted to resassign to immutable binding"))?
            }

            vars.push(
                [(name.clone(), (false, val))]
                .iter()
                .cloned()
                .collect::<HashMap<_, _>>()
            );
            
            let output = eval(then, vars, memory);
            vars.pop();
            output
        }
        Expr::MutLet { name, rhs, then } => {
            let (_, val) = eval(rhs, vars, memory)?;
            let is_mut = vars.iter().rev().all(|scope| if let Some((is_mut, _)) = scope.get(name) { *is_mut } else { true });
            if !is_mut {
                Err(format!("Attempted to make mutable binding on an immutably bound variable"))?
            }

            vars.push(
                [(name.clone(), (true, val))]
                .iter()
                .cloned()
                .collect::<HashMap<_, _>>()
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

            // type check to see if the rhs has gte ref/mutrefs than deref_counts
            let mut ref_mut_ref_count = 0;
            while true {
                match **rhs {
                    Expr::Ref(_) | Expr::MutRef(_) => {
                        ref_mut_ref_count += 1;
                    }
                    _ => {
                        break;
                    }
                };
            }
            if ref_mut_ref_count < *deref_count {
                Err(format!("You can only assign valid references for, well, references"))?
            }

            let (is_mut, mut addr) = get_var(vars, name)?;
            if !is_mut {
                Err(format!("Attempted to assign to the pointed value of an immutably bound reference"))?
            }
            while *deref_count > 0 {
                if let Some((is_mut, a)) = memory.get(&addr) {
                    if !is_mut {
                        Err(format!("Attempted to assign to the pointed value of an immutably bound reference"))?
                    }
                    addr = *a;
                }
            }

            let val = eval(rhs, vars, memory)?;
            memory.insert(addr, val);

            Ok(val)
        }
    }
}
