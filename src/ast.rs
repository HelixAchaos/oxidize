use crate::types::Type;

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

#[derive(Debug, Clone)]
pub enum TExpr {
    Unit(Type),
    Num(Type, i64),
    Bool(Type, bool),
    Lvalue(Type, Spanned<TLhs>),
    Ref(Type, Spanned<TLhs>),
    MutRef(Type, Spanned<TLhs>),
    Neg(Type, Box<Spanned<Self>>),
    Gt(Type, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Lt(Type, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Add(Type, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Sub(Type, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Mul(Type, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Div(Type, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Cond(
        Type,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
    Tuple(Type, Vec<Spanned<Self>>),
    Assign(Type, Spanned<TLhs>, Box<Spanned<Self>>),
    Seq(Type, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let {
        name: String,
        rhs: Box<Spanned<Self>>,
        then: Box<Spanned<Self>>,
        t: Type,
    },
    MutLet {
        name: String,
        rhs: Box<Spanned<Self>>,
        then: Box<Spanned<Self>>,
        t: Type,
    },
}

impl TExpr {
    pub fn extract_type(&self) -> Type {
        use TExpr::*;
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
