pub type Var = String;
pub type Address = u64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    Unit,
    Ref(bool, Box<Type>, Address),
    Tuple(Vec<Type>),
}
