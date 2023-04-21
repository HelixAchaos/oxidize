use std::collections::HashSet;

use crate::types::{Address, Type};

#[derive(Debug, Clone)]
pub enum ELhs {
    Var(String),
    DeRef(Box<Self>),
    Index(Box<Self>, u64),
}

impl ELhs {
    pub fn to_string(&self) -> String {
        use ELhs::*;
        match self {
            Var(name) => name.to_string(),
            DeRef(lhs) => format!("*{}", lhs.to_string()),
            Index(lhs, i) => format!("{}.{}", lhs.to_string(), i),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EExpr {
    Unit,
    Num(i64),
    Lvalue(ELhs),
    Ref(ELhs),
    MutRef(ELhs),
    Neg(Box<Self>),
    Gt(Box<Self>, Box<Self>),
    Lt(Box<Self>, Box<Self>),
    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
    Cond(Box<Self>, Box<Self>, Box<Self>),
    Tuple(Vec<Self>),
    Assign(ELhs, Box<Self>),
    Seq(Box<Self>, Box<Self>),
    Let {
        name: String,
        rhs: Box<Self>,
        then: Box<Self>,
    },
    MutLet {
        name: String,
        rhs: Box<Self>,
        then: Box<Self>,
    },
}

impl EExpr {
    pub fn to_string(&self) -> String {
        use EExpr::*;
        match self {
            Unit => "unit".to_string(),
            Num(n) => format!("{}", n),
            Lvalue(lhs) => lhs.to_string(),
            Ref(lhs) => format!("&{}", lhs.to_string()),
            MutRef(lhs) => format!("&mut {}", lhs.to_string()),
            Neg(e) => format!("(-{})", e.to_string()),
            Gt(e1, e2) => format!("({} > {})", e1.to_string(), e2.to_string()),
            Lt(e1, e2) => format!("({} < {})", e1.to_string(), e2.to_string()),
            Add(e1, e2) => format!("({} + {})", e1.to_string(), e2.to_string()),
            Sub(e1, e2) => format!("({} - {})", e1.to_string(), e2.to_string()),
            Mul(e1, e2) => format!("({} * {})", e1.to_string(), e2.to_string()),
            Div(e1, e2) => format!("({} / {})", e1.to_string(), e2.to_string()),
            Cond(b, e1, e2) => format!(
                "(if {} then {} else {})",
                b.to_string(),
                e1.to_string(),
                e2.to_string()
            ),
            Tuple(exprs) => format!(
                "[{}]",
                exprs
                    .iter()
                    .map(EExpr::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Assign(lhs, e) => format!("{} = {}", lhs.to_string(), e.to_string()),
            Seq(e1, e2) => format!("{}; {}", e1.to_string(), e2.to_string()),
            Let { name, rhs, then } => {
                format!("let {} = {} in {}", name, rhs.to_string(), then.to_string())
            }
            MutLet { name, rhs, then } => format!(
                "let mut {} = {} in {}",
                name,
                rhs.to_string(),
                then.to_string()
            ),
        }
    }
}

// pub enum S {
//     MutRef(Address),
//     ImmutRef(HashSet<Address>),
//     Moved,
//     None,
// }

pub type S = HashSet<(bool, Address)>;

type SType = (Type, S);

#[derive(Debug, Clone)]
pub enum TLhs {
    Var(Type, String),
    DeRef(Type, Box<Self>),
    Index(Type, Box<Self>, u64),
}

#[derive(Debug, Clone)]
pub enum STExpr {
    Unit(SType),
    Num(SType, i64),
    Lvalue(SType, TLhs),
    Ref(SType, TLhs),
    MutRef(SType, TLhs),
    Neg(SType, Box<Self>),
    Gt(SType, Box<Self>, Box<Self>),
    Lt(SType, Box<Self>, Box<Self>),
    Add(SType, Box<Self>, Box<Self>),
    Sub(SType, Box<Self>, Box<Self>),
    Mul(SType, Box<Self>, Box<Self>),
    Div(SType, Box<Self>, Box<Self>),
    Cond(SType, Box<Self>, Box<Self>, Box<Self>),
    Tuple(SType, Vec<Self>),
    Assign(SType, TLhs, Box<Self>),
    Seq(SType, Box<Self>, Box<Self>),
    Let {
        name: String,
        rhs: Box<Self>,
        then: Box<Self>,
        t: SType,
    },
    MutLet {
        name: String,
        rhs: Box<Self>,
        then: Box<Self>,
        t: SType,
    },
}

impl TLhs {
    pub fn extract_type(&self) -> Type {
        use TLhs::*;
        match self {
            Var(t, _) => t,
            DeRef(t, _) => t,
            Index(t, _, _) => t,
        }
        .to_owned()
    }

    pub fn extract_lhs(&self) -> ELhs {
        use TLhs::*;
        match self.to_owned() {
            Var(_, name) => ELhs::Var(name),
            DeRef(_, lhs) => ELhs::DeRef(Box::new(lhs.extract_lhs())),
            Index(_, lhs, i) => ELhs::Index(Box::new(lhs.extract_lhs()), i),
        }
    }
}

impl STExpr {
    pub fn extract_type(&self) -> Type {
        self.extract_stype().0
    }
    pub fn extract_s(&self) -> S {
        self.extract_stype().1
    }
    pub fn extract_stype(&self) -> SType {
        use STExpr::*;
        match self {
            Unit(t) => t,
            Num(t, _) => t,
            Lvalue(t, _) => t,
            Ref(t, _) => t,
            MutRef(t, _) => t,
            Neg(t, _) => t,
            Gt(t, _, _) => t,
            Lt(t, _, _) => t,
            Add(t, _, _) => t,
            Sub(t, _, _) => t,
            Mul(t, _, _) => t,
            Div(t, _, _) => t,
            Cond(t, _, _, _) => t,
            Tuple(t, _) => t,
            Assign(t, _, _) => t,
            Seq(t, _, _) => t,
            Let {
                name: _,
                rhs: _,
                then: _,
                t,
            } => t,
            MutLet {
                name: _,
                rhs: _,
                then: _,
                t,
            } => t,
        }
        .to_owned()
    }
    pub fn extract_tast(&self) -> TExpr {
        use STExpr::*;
        match self.to_owned() {
            Unit((t, _)) => TExpr::Unit(t),
            Num((t, _), n) => TExpr::Num(t, n),
            Lvalue((t, _), lhs) => TExpr::Lvalue(t, lhs),
            Ref((t, _), lhs) => TExpr::Ref(t, lhs),
            MutRef((t, _), lhs) => TExpr::MutRef(t, lhs),
            Neg((t, _), e) => TExpr::Neg(t, Box::new(e.extract_tast())),
            Gt((t, _), e1, e2) => {
                TExpr::Gt(t, Box::new(e1.extract_tast()), Box::new(e2.extract_tast()))
            }
            Lt((t, _), e1, e2) => {
                TExpr::Lt(t, Box::new(e1.extract_tast()), Box::new(e2.extract_tast()))
            }
            Add((t, _), e1, e2) => {
                TExpr::Add(t, Box::new(e1.extract_tast()), Box::new(e2.extract_tast()))
            }
            Sub((t, _), e1, e2) => {
                TExpr::Sub(t, Box::new(e1.extract_tast()), Box::new(e2.extract_tast()))
            }
            Mul((t, _), e1, e2) => {
                TExpr::Mul(t, Box::new(e1.extract_tast()), Box::new(e2.extract_tast()))
            }
            Div((t, _), e1, e2) => {
                TExpr::Div(t, Box::new(e1.extract_tast()), Box::new(e2.extract_tast()))
            }
            Cond((t, _), b, e1, e2) => TExpr::Cond(
                t,
                Box::new(b.extract_tast()),
                Box::new(e1.extract_tast()),
                Box::new(e2.extract_tast()),
            ),
            Tuple((t, _), stexprs) => {
                TExpr::Tuple(t, stexprs.iter().map(STExpr::extract_tast).collect())
            }
            Assign((t, _), lhs, e) => TExpr::Assign(t, lhs, Box::new(e.extract_tast())),
            Seq((t, _), e1, e2) => {
                TExpr::Seq(t, Box::new(e1.extract_tast()), Box::new(e2.extract_tast()))
            }
            Let {
                name,
                rhs,
                then,
                t: (t, _),
            } => TExpr::Let {
                name,
                rhs: Box::new(rhs.extract_tast()),
                then: Box::new(then.extract_tast()),
                t,
            },
            MutLet {
                name,
                rhs,
                then,
                t: (t, _),
            } => TExpr::MutLet {
                name,
                rhs: Box::new(rhs.extract_tast()),
                then: Box::new(then.extract_tast()),
                t,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum TExpr {
    Unit(Type),
    Num(Type, i64),
    Lvalue(Type, TLhs),
    Ref(Type, TLhs),
    MutRef(Type, TLhs),
    Neg(Type, Box<Self>),
    Gt(Type, Box<Self>, Box<Self>),
    Lt(Type, Box<Self>, Box<Self>),
    Add(Type, Box<Self>, Box<Self>),
    Sub(Type, Box<Self>, Box<Self>),
    Mul(Type, Box<Self>, Box<Self>),
    Div(Type, Box<Self>, Box<Self>),
    Cond(Type, Box<Self>, Box<Self>, Box<Self>),
    Tuple(Type, Vec<Self>),
    Assign(Type, TLhs, Box<Self>),
    Seq(Type, Box<Self>, Box<Self>),
    Let {
        name: String,
        rhs: Box<Self>,
        then: Box<Self>,
        t: Type,
    },
    MutLet {
        name: String,
        rhs: Box<Self>,
        then: Box<Self>,
        t: Type,
    },
}

impl TExpr {
    pub fn extract_ast(&self) -> EExpr {
        use TExpr::*;
        match self.to_owned() {
            Unit(_) => EExpr::Unit,
            Num(_, n) => EExpr::Num(n),
            Lvalue(_, lhs) => EExpr::Lvalue(lhs.extract_lhs()),
            Ref(_, lhs) => EExpr::Ref(lhs.extract_lhs()),
            MutRef(_, lhs) => EExpr::MutRef(lhs.extract_lhs()),
            Neg(_, e) => EExpr::Neg(Box::new(e.extract_ast())),
            Gt(_, e1, e2) => EExpr::Gt(Box::new(e1.extract_ast()), Box::new(e2.extract_ast())),
            Lt(_, e1, e2) => EExpr::Lt(Box::new(e1.extract_ast()), Box::new(e2.extract_ast())),
            Add(_, e1, e2) => EExpr::Add(Box::new(e1.extract_ast()), Box::new(e2.extract_ast())),
            Sub(_, e1, e2) => EExpr::Sub(Box::new(e1.extract_ast()), Box::new(e2.extract_ast())),
            Mul(_, e1, e2) => EExpr::Mul(Box::new(e1.extract_ast()), Box::new(e2.extract_ast())),
            Div(_, e1, e2) => EExpr::Div(Box::new(e1.extract_ast()), Box::new(e2.extract_ast())),
            Cond(_, b, e1, e2) => EExpr::Cond(
                Box::new(b.extract_ast()),
                Box::new(e1.extract_ast()),
                Box::new(e2.extract_ast()),
            ),
            Tuple(_, texprs) => EExpr::Tuple(texprs.iter().map(TExpr::extract_ast).collect()),
            Assign(_, lhs, e) => EExpr::Assign(lhs.extract_lhs(), Box::new(e.extract_ast())),
            Seq(_, e1, e2) => EExpr::Seq(Box::new(e1.extract_ast()), Box::new(e2.extract_ast())),
            Let {
                name,
                rhs,
                then,
                t: _,
            } => EExpr::Let {
                name,
                rhs: Box::new(rhs.extract_ast()),
                then: Box::new(then.extract_ast()),
            },
            MutLet {
                name,
                rhs,
                then,
                t: _,
            } => EExpr::MutLet {
                name,
                rhs: Box::new(rhs.extract_ast()),
                then: Box::new(then.extract_ast()),
            },
        }
    }
}
