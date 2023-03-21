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
    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
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
    Add(Type, Box<Self>, Box<Self>),
    Sub(Type, Box<Self>, Box<Self>),
    Mul(Type, Box<Self>, Box<Self>),
    Div(Type, Box<Self>, Box<Self>),
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
            Unit(t) => t.to_owned(),
            Num(t, _) => t.to_owned(),
            Lvalue(t, _) => t.to_owned(),
            Ref(t, _) => t.to_owned(),
            MutRef(t, _) => t.to_owned(),
            Neg(t, _) => t.to_owned(),
            Add(t, _, _) => t.to_owned(),
            Sub(t, _, _) => t.to_owned(),
            Mul(t, _, _) => t.to_owned(),
            Div(t, _, _) => t.to_owned(),
            Assign(t, _, _) => t.to_owned(),
            Seq(t, _, _) => t.to_owned(),
            Let {
                name: _,
                rhs: _,
                then: _,
                t,
            } => t.to_owned(),
            MutLet {
                name: _,
                rhs: _,
                then: _,
                t,
            } => t.to_owned(),
        }
    }
}
