use crate::types::Type;

#[derive(Debug, Clone)]
pub enum ELhs {
    Var(String),
    DeRef(Box<Self>),
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

#[derive(Debug, Clone)]
pub enum TLhs {
    Var(Type, String),
    DeRef(Type, Box<Self>),
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

impl TLhs {
    pub fn extract_type(&self) -> Type {
        use TLhs::*;
        match self {
            Var(t, _) => t.to_owned(),
            DeRef(t, _) => t.to_owned(),
        }
    }
}

impl TExpr {
    pub fn extract_type(&self) -> Type {
        use TExpr::*;
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
}
