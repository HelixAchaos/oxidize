#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Unit,
    Ref(bool, Box<Type>),
}
