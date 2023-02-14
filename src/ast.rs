#[derive(Debug)]
pub enum Expr {
    Num(i64),
    Var(String),

    DeRef(Box<Self>),
    Ref(Box<Self>),
    MutRef(Box<Self>),
    Neg(Box<Self>),
    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),

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
    Assign {
        deref_count: usize,
        name: String,
        rhs: Box<Self>,
    },
}
