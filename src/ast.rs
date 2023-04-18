use std::collections::HashSet;

use crate::types::{Address, Type};

#[derive(Debug, Clone)]
pub enum ELhs {
    Var(String),
    DeRef(Box<Self>),
    Index(Box<Self>, u64),
}

#[derive(Debug, Clone)]
pub enum EExpr {
    Unit,
    Num(i64),
    Lvalue(ELhs),
    Ref(String),
    MutRef(String),
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
    Ref(SType, String),
    MutRef(SType, String),
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
            Ref((t, _), name) => TExpr::Ref(t, name),
            MutRef((t, _), name) => TExpr::MutRef(t, name),
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
    Ref(Type, String),
    MutRef(Type, String),
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
