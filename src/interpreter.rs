use crate::ast::Expr;

pub fn eval<'a>(expr: &'a Expr, vars: &mut Vec<(&'a String, f64)>) -> Result<f64, String> {
    match expr {
        Expr::Num(x) => Ok(*x),
        Expr::DeRef(_e) => todo!("ree"),
        Expr::Ref(_e) => todo!("ree"),
        Expr::MutRef(_e) => todo!("ree"),
        Expr::Neg(a) => Ok(-eval(a, vars)?),
        Expr::Add(a, b) => Ok(eval(a, vars)? + eval(b, vars)?),
        Expr::Sub(a, b) => Ok(eval(a, vars)? - eval(b, vars)?),
        Expr::Mul(a, b) => Ok(eval(a, vars)? * eval(b, vars)?),
        Expr::Div(a, b) => Ok(eval(a, vars)? / eval(b, vars)?),
        Expr::Var(name) => {
            if let Some((_, val)) = vars.iter().rev().find(|(var, _)| *var == name) {
                Ok(*val)
            } else {
                Err(format!("Cannot find variable `{}` in scope", name))
            }
        }
        Expr::Seq(e1, e2) => {
            eval(e1, vars)?;
            Ok(eval(e2, vars)?)
        }
        Expr::Let { name, rhs, then } => {
            // todo: need to block immutable reassignment
            let rhs = eval(rhs, vars)?;
            vars.push((name, rhs));
            let output = eval(then, vars);
            vars.pop();
            output
        }
        Expr::MutLet { name, rhs, then } => {
            let rhs = eval(rhs, vars)?;
            vars.push((name, rhs));
            let output = eval(then, vars);
            vars.pop();
            output
        }
        Expr::Assign {
            deref_count,
            name,
            rhs,
        } => {
            todo!("ree")
        }
    }
}
