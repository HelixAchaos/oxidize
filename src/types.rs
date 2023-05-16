use std::collections::HashSet;

pub type Var = String;
pub type Address = u64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int,
    Unit,
    Ref(bool, Box<Type>),
    Tuple(Vec<Type>),
}

// impl Type {
// pub fn unify(t1: Self, t2: Self) -> Result<Self, String> {
//     match (t1.clone(), t2) {
//         (Type::Bool, Type::Bool) | (Type::Int, Type::Int) | (Type::Unit, Type::Unit) => Ok(t1),
//         (Type::Tuple(types1), Type::Tuple(types2)) => {
//             if types1 == types2 {
//                 Ok(t1)
//             } else {
//                 Err(format!("The types of the tuples' elements don't match."))
//             }
//         }
//         (Type::Ref(true, t1), Type::Ref(true, t2)) => {
//             if t1 == t2 {
//                 Ok(*t1)
//             } else {
//                 Err(format!("The types of the tuples' elements don't match."))
//             }
//         }
//         (Type::Ref(false, t1), Type::Ref(false, t2)) => {
//             if t1 == t2 {
//                 Ok(*t1)
//             } else {
//                 Err(format!("The types of the tuples' elements don't match."))
//             }
//         }
//         (Type::Ref(false, _), Type::Ref(true, _))
//         | (Type::Ref(true, _), Type::Ref(false, _)) => Err(format!(
//             "Mixing mutable references and immutable references is not okay."
//         )),
//         _ => Err(format!("The types trivially don't match.")),
//     }
// }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum S {
    Union(Vec<Self>),
    MutRef(Address),
    ImmutRef(HashSet<Address>),
    Moved,
    None,
}

impl S {
    pub fn contains(&self, t: &(bool, Address)) -> bool {
        match self {
            S::Union(ss) => ss.into_iter().any(|s| s.contains(t)),
            S::MutRef(s) => t.0 && (s == &t.1),
            S::ImmutRef(ss) => (!t.0) && (ss.contains(&t.1)),
            _ => false,
        }
    }

    pub fn join(s1: Self, s2: Self) -> Result<Self, String> {
        match (s1.clone(), s2.clone()) {
            (S::None, _) => Ok(s2),
            (S::MutRef(_), S::MutRef(_)) => Ok(S::Union(vec![s1, s2])),
            (S::ImmutRef(addrs1), S::ImmutRef(addrs2)) => Ok(S::ImmutRef(
                addrs1.into_iter().chain(addrs2.into_iter()).collect(),
            )),
            (S::Moved, S::Moved) => Ok(S::Moved),
            (S::Union(ss1), S::Union(ss2)) => Ok(S::Union([ss1, ss2].concat())),
            (S::Union(ss), _) => Ok(S::Union([ss, vec![s2]].concat())),
            (_, S::Union(ss)) => Ok(S::Union([ss, vec![s1]].concat())),
            _ => Ok(S::Union(vec![s1, s2])),
        }
    }

    pub fn remove(&self, addr: Address) -> Self {
        match self {
            S::Union(ss) => S::Union(ss.into_iter().map(|s| s.remove(addr)).collect()),
            S::MutRef(s) => {
                if s.clone() == addr {
                    S::None
                } else {
                    self.clone()
                }
            }
            S::ImmutRef(ss) => {
                let mut ss_prime = ss.clone();

                (&mut ss_prime).remove(&addr);
                if ss_prime.len() > 0 {
                    S::ImmutRef(ss_prime)
                } else {
                    S::None
                }
            }
            _ => self.clone(),
        }
    }

    pub fn may_move(&self) -> Result<(), String> {
        match self {
            S::Moved => Err(format!("You can't move what was moved.")),
            S::ImmutRef(_) | S::MutRef(_) => Err(format!("You can't move something referred to.")),
            S::None => Ok(()),
            S::Union(ss) => {
                let _ = ss
                    .into_iter()
                    .map(S::may_move)
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(())
            }
        }
    }
}
