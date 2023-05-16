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
    Lvalue(ELhs),
    Ref(ELhs),
    MutRef(ELhs),
    Neg(Box<Spanned<Self>>),
    Gt(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Lt(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Add(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Sub(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Mul(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Div(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Cond(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Tuple(Vec<Spanned<Self>>),
    Assign(ELhs, Box<Spanned<Self>>),
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

impl EExpr {
    pub fn to_string(&self) -> String {
        use EExpr::*;
        match self {
            Unit => "unit".to_string(),
            Num(n) => format!("{}", n),
            Lvalue(lhs) => lhs.to_string(),
            Ref(lhs) => format!("&{}", lhs.to_string()),
            MutRef(lhs) => format!("&mut {}", lhs.to_string()),
            Neg(e) => format!("(-{})", e.0.to_string()),
            Gt(e1, e2) => format!("({} > {})", e1.0.to_string(), e2.0.to_string()),
            Lt(e1, e2) => format!("({} < {})", e1.0.to_string(), e2.0.to_string()),
            Add(e1, e2) => format!("({} + {})", e1.0.to_string(), e2.0.to_string()),
            Sub(e1, e2) => format!("({} - {})", e1.0.to_string(), e2.0.to_string()),
            Mul(e1, e2) => format!("({} * {})", e1.0.to_string(), e2.0.to_string()),
            Div(e1, e2) => format!("({} / {})", e1.0.to_string(), e2.0.to_string()),
            Cond(b, e1, e2) => format!(
                "(if {} then {} else {})",
                b.0.to_string(),
                e1.0.to_string(),
                e2.0.to_string()
            ),
            Tuple(exprs) => format!(
                "[{}]",
                exprs
                    .iter()
                    .map(|e| e.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Assign(lhs, e) => format!("{} = {}", lhs.to_string(), e.0.to_string()),
            Seq(e1, e2) => format!("{}; {}", e1.0.to_string(), e2.0.to_string()),
            Let { name, rhs, then } => {
                format!(
                    "let {} = {} in {}",
                    name,
                    rhs.0.to_string(),
                    then.0.to_string()
                )
            }
            MutLet { name, rhs, then } => format!(
                "let mut {} = {} in {}",
                name,
                rhs.0.to_string(),
                then.0.to_string()
            ),
        }
    }
}

type SType = (Type, S);

#[derive(Debug, Clone)]
pub enum TLhs {
    Var(Type, String),
    DeRef(Type, Box<Spanned<Self>>),
    Index(Type, Box<Spanned<Self>>, u64),
}

#[derive(Debug, Clone)]
pub enum STExpr {
    Unit(SType),
    Num(SType, i64),
    Lvalue(SType, TLhs),
    Ref(SType, TLhs),
    MutRef(SType, TLhs),
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
    Assign(SType, TLhs, Box<Spanned<Self>>),
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
            Neg((t, _), e) => TExpr::Neg(t, Box::new((e.0.extract_tast(), e.1))),
            Gt((t, _), e1, e2) => TExpr::Gt(
                t,
                Box::new((e1.0.extract_tast(), e1.1)),
                Box::new((e2.0.extract_tast(), e2.1)),
            ),
            Lt((t, _), e1, e2) => TExpr::Lt(
                t,
                Box::new((e1.0.extract_tast(), e1.1)),
                Box::new((e2.0.extract_tast(), e2.1)),
            ),
            Add((t, _), e1, e2) => TExpr::Add(
                t,
                Box::new((e1.0.extract_tast(), e1.1)),
                Box::new((e2.0.extract_tast(), e2.1)),
            ),
            Sub((t, _), e1, e2) => TExpr::Sub(
                t,
                Box::new((e1.0.extract_tast(), e1.1)),
                Box::new((e2.0.extract_tast(), e2.1)),
            ),
            Mul((t, _), e1, e2) => TExpr::Mul(
                t,
                Box::new((e1.0.extract_tast(), e1.1)),
                Box::new((e2.0.extract_tast(), e2.1)),
            ),
            Div((t, _), e1, e2) => TExpr::Div(
                t,
                Box::new((e1.0.extract_tast(), e1.1)),
                Box::new((e2.0.extract_tast(), e2.1)),
            ),
            Cond((t, _), b, e1, e2) => TExpr::Cond(
                t,
                Box::new((b.0.extract_tast(), b.1)),
                Box::new((e1.0.extract_tast(), e1.1)),
                Box::new((e2.0.extract_tast(), e2.1)),
            ),
            Tuple((t, _), stexprs) => TExpr::Tuple(
                t,
                stexprs
                    .iter()
                    .map(|stexpr| (stexpr.0.extract_tast(), stexpr.1.clone()))
                    .collect(),
            ),
            Assign((t, _), lhs, e) => TExpr::Assign(t, lhs, Box::new((e.0.extract_tast(), e.1))),
            Seq((t, _), e1, e2) => TExpr::Seq(
                t,
                Box::new((e1.0.extract_tast(), e1.1)),
                Box::new((e2.0.extract_tast(), e2.1)),
            ),
            Let {
                name,
                rhs,
                then,
                t: (t, _),
            } => TExpr::Let {
                name,
                rhs: Box::new((rhs.0.extract_tast(), rhs.1)),
                then: Box::new((then.0.extract_tast(), then.1)),
                t,
            },
            MutLet {
                name,
                rhs,
                then,
                t: (t, _),
            } => TExpr::MutLet {
                name,
                rhs: Box::new((rhs.0.extract_tast(), rhs.1)),
                then: Box::new((then.0.extract_tast(), then.1)),
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
    Assign(Type, TLhs, Box<Spanned<Self>>),
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
    pub fn extract_ast(&self) -> EExpr {
        use TExpr::*;
        match self.to_owned() {
            Unit(_) => EExpr::Unit,
            Num(_, n) => EExpr::Num(n),
            Lvalue(_, lhs) => EExpr::Lvalue(lhs.extract_lhs()),
            Ref(_, lhs) => EExpr::Ref(lhs.extract_lhs()),
            MutRef(_, lhs) => EExpr::MutRef(lhs.extract_lhs()),
            Neg(_, e) => EExpr::Neg(Box::new((e.0.extract_ast(), e.1))),
            Gt(_, e1, e2) => EExpr::Gt(
                Box::new((e1.0.extract_ast(), e1.1)),
                Box::new((e2.0.extract_ast(), e2.1)),
            ),
            Lt(_, e1, e2) => EExpr::Lt(
                Box::new((e1.0.extract_ast(), e1.1)),
                Box::new((e2.0.extract_ast(), e2.1)),
            ),
            Add(_, e1, e2) => EExpr::Add(
                Box::new((e1.0.extract_ast(), e1.1)),
                Box::new((e2.0.extract_ast(), e2.1)),
            ),
            Sub(_, e1, e2) => EExpr::Sub(
                Box::new((e1.0.extract_ast(), e1.1)),
                Box::new((e2.0.extract_ast(), e2.1)),
            ),
            Mul(_, e1, e2) => EExpr::Mul(
                Box::new((e1.0.extract_ast(), e1.1)),
                Box::new((e2.0.extract_ast(), e2.1)),
            ),
            Div(_, e1, e2) => EExpr::Div(
                Box::new((e1.0.extract_ast(), e1.1)),
                Box::new((e2.0.extract_ast(), e2.1)),
            ),
            Cond(_, b, e1, e2) => EExpr::Cond(
                Box::new((b.0.extract_ast(), b.1)),
                Box::new((e1.0.extract_ast(), e1.1)),
                Box::new((e2.0.extract_ast(), e2.1)),
            ),
            Tuple(_, texprs) => EExpr::Tuple(
                texprs
                    .iter()
                    .map(|texpr| (texpr.0.extract_ast(), texpr.1.clone()))
                    .collect(),
            ),
            Assign(_, lhs, e) => {
                EExpr::Assign(lhs.extract_lhs(), Box::new((e.0.extract_ast(), e.1)))
            }
            Seq(_, e1, e2) => EExpr::Seq(
                Box::new((e1.0.extract_ast(), e1.1)),
                Box::new((e2.0.extract_ast(), e2.1)),
            ),
            Let {
                name,
                rhs,
                then,
                t: _,
            } => EExpr::Let {
                name,
                rhs: Box::new((rhs.0.extract_ast(), rhs.1)),
                then: Box::new((then.0.extract_ast(), then.1)),
            },
            MutLet {
                name,
                rhs,
                then,
                t: _,
            } => EExpr::MutLet {
                name,
                rhs: Box::new((rhs.0.extract_ast(), rhs.1)),
                then: Box::new((then.0.extract_ast(), then.1)),
            },
        }
    }
}
