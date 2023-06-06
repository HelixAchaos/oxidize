use crate::types::{Type, S};

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone)]
pub enum ELhs {
    Var(String),
    DeRef(Box<Spanned<Self>>),
    Index(Box<Spanned<Self>>, u64),
}

impl ELhs {
    pub fn to_string(&self) -> String {
        use ELhs::*;
        match self {
            Var(name) => name.to_string(),
            DeRef(lhs) => format!("*{}", lhs.0.to_string()),
            Index(lhs, i) => format!("{}.{}", lhs.0.to_string(), i),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EExpr {
    Unit,
    Num(i64),
    Bool(bool),
    Lvalue(Spanned<ELhs>),
    Ref(Spanned<ELhs>),
    MutRef(Spanned<ELhs>),
    Neg(Box<Spanned<Self>>),
    Gt(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Lt(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Add(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Sub(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Mul(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Div(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Cond(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Tuple(Vec<Spanned<Self>>),
    Assign(Spanned<ELhs>, Box<Spanned<Self>>),
    Seq(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let {
        name: String,
        rhs: Box<Spanned<Self>>,
        then: Box<Spanned<Self>>,
    },
    MutLet {
        name: String,
        rhs: Box<Spanned<Self>>,
        then: Box<Spanned<Self>>,
    },
}

type SType = (Type, S);

#[derive(Debug, Clone)]
pub enum TLhs {
    Var(Type, String),
    DeRef(Type, Box<Spanned<Self>>),
    Index(Type, Box<Spanned<Self>>, u64),
}

impl TLhs {
    pub fn to_string(&self) -> String {
        match self {
            TLhs::Var(_, v) => v.clone(),
            TLhs::DeRef(_, tlhs) => format!("*{}", tlhs.0.to_string()),
            TLhs::Index(_, tlhs, i) => format!("{}.{i}", tlhs.0.to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum STExpr {
    Unit(SType),
    Num(SType, i64),
    Bool(SType, bool),
    Lvalue(SType, Spanned<TLhs>),
    Ref(SType, Spanned<TLhs>),
    MutRef(SType, Spanned<TLhs>),
    Neg(SType, Box<Spanned<Self>>),
    Gt(SType, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Lt(SType, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Add(SType, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Sub(SType, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Mul(SType, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Div(SType, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Cond(
        SType,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
    Tuple(SType, Vec<Spanned<Self>>),
    Assign(SType, Spanned<TLhs>, Box<Spanned<Self>>),
    Seq(SType, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let {
        name: String,
        rhs: Box<Spanned<Self>>,
        then: Box<Spanned<Self>>,
        t: SType,
    },
    MutLet {
        name: String,
        rhs: Box<Spanned<Self>>,
        then: Box<Spanned<Self>>,
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
            DeRef(_, lhs) => ELhs::DeRef(Box::new((lhs.0.extract_lhs(), lhs.1))),
            Index(_, lhs, i) => ELhs::Index(Box::new((lhs.0.extract_lhs(), lhs.1)), i),
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
            Bool(t, _) => t,
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
