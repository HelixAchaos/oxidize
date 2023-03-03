#[derive(Debug, Clone)]
pub enum Lhs {
    Var(String),
    DeRef(Box<Self>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Num(i64),
    Lvalue(Lhs),
    Ref(String),
    MutRef(String),
    Neg(Box<Self>),
    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
    Assign(Lhs, Box<Self>),
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
