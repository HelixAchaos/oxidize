use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::typechecker::{Eta, Gamma, Mu};

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

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Ref(mutability, tau) => {
                    if *mutability {
                        format!("&mut {}", tau)
                    } else {
                        format!("&{}", tau)
                    }
                }
                Type::Tuple(taus) => {
                    taus.iter()
                        .map(|tau| format!("{}", tau))
                        .collect::<Vec<_>>()
                        .join(" * ")
                }
                _ => format!("{:?}", self),
            }
        )
    }
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
    Moved(Option<Address>),
    None,
}

impl S {
    pub fn validate(&self, old_s: &Self, target: Address) -> Result<(), String> {
        match (self, old_s) {
            (&S::None, _) | (_, &S::None) => Ok(()),
            (&S::Moved(Some(_)), &S::Moved(Some(_))) => Err(format!("attempted to move a moved value (expected to be at address {target})"))?,
            (&S::Moved(Some(_)), &S::Moved(None)) => Err(format!("attempted to move a mutable reference to a dropped value (expected to be at address {target})"))?,
            (&S::Moved(Some(_)), &S::ImmutRef(_)) => Err(format!("attempted to move a mutable reference to an immutably referenced value at address {target}"))?,
            (&S::Moved(Some(_)), &S::MutRef(_)) => Err(format!("attempted to move a mutable reference to a mutably referenced value at address {target}"))?,
            (&S::Moved(Some(_)), &S::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target)).collect::<Result<_, _>>()?;}),
            (&S::Moved(None), &S::Moved(Some(_))) => Err(format!("attempted to drop a moved value (expected to be at address {target})"))?,
            (&S::Moved(None), &S::Moved(None)) => Err(format!("attempted to drop a mutable reference to a dropped value (expected to be at address {target})"))?,
            (&S::Moved(None), &S::ImmutRef(_)) => Err(format!("attempted to drop a mutable reference to an immutably referenced value at address {target}"))?,
            (&S::Moved(None), &S::MutRef(_)) => Err(format!("attempted to drop a mutable reference to a mutably referenced value at address {target}"))?,
            (&S::Moved(None), &S::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target)).collect::<Result<_, _>>()?;}),
            (&S::MutRef(_), &S::Moved(Some(_))) => Err(format!("attempted to create a mutable reference to a moved value (expected to be at address {target})"))?,
            (&S::MutRef(_), &S::Moved(None)) => Err(format!("attempted to create a mutable reference to a dropped value (expected to be at address {target})"))?,
            (&S::MutRef(_), &S::ImmutRef(_)) => Err(format!("attempted to create a mutable reference to an immutably referenced value at address {target}"))?,
            (&S::MutRef(_), &S::MutRef(_)) => Err(format!("attempted to create a mutable reference to a mutably referenced value at address {target}"))?,
            (&S::MutRef(_), &S::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target)).collect::<Result<_, _>>()?;}),
            (&S::ImmutRef(_), &S::Moved(Some(_))) => Err(format!("attempted to create a immutable reference to a moved value (expected to be at address {target})"))?,
            (&S::ImmutRef(_), &S::Moved(None)) => Err(format!("attempted to create a immutable reference to a mutable reference to a dropped value (expected to be at address {target})"))?,
            (&S::ImmutRef(_), &S::ImmutRef(_)) => Err(format!("attempted to create a immutable reference to a mutable reference to an immutably referenced value at address {target}"))?,
            (&S::ImmutRef(_), &S::MutRef(_)) => Err(format!("attempted to create a immutable reference to a mutable reference to a mutably referenced value at address {target}"))?,
            (&S::ImmutRef(_), &S::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target)).collect::<Result<_, _>>()?;}),
            (&S::Union(ref ss), &S::Moved(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target)).collect::<Result<_, _>>()?;}),
            (&S::Union(ref ss), &S::ImmutRef(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target)).collect::<Result<_, _>>()?;}),
            (&S::Union(ref ss), &S::MutRef(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target)).collect::<Result<_, _>>()?;}),
            (&S::Union(ref ss), &S::Union(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target)).collect::<Result<_, _>>()?;}),
        }
    }
    fn is_move(&self) -> i8 {
        match self {
            S::Moved(Some(_)) => 1,
            S::Union(ss) => {
                if ss.iter().any(|s| s.is_move() == 1) {
                    0
                } else {
                    -1
                }
            }
            _ => -1,
        }
    }
    fn is_drop(&self) -> i8 {
        match self {
            S::Moved(None) => 1,
            S::Union(ss) => {
                if ss.iter().any(|s| s.is_drop() == 1) {
                    0
                } else {
                    -1
                }
            }
            _ => -1,
        }
    }
    fn is_immut_ref(&self) -> i8 {
        match self {
            S::ImmutRef(_) => 1,
            S::Union(ss) => {
                if ss.iter().any(|s| s.is_immut_ref() == 1) {
                    0
                } else {
                    -1
                }
            }
            _ => -1,
        }
    }
    fn is_mut_ref(&self) -> i8 {
        match self {
            S::MutRef(_) => 1,
            S::Union(ss) => {
                if ss.iter().any(|s| s.is_mut_ref() == 1) {
                    0
                } else {
                    -1
                }
            }
            _ => -1,
        }
    }
    pub fn stringify(&self, eta: &Eta, ell: &Address, tau: Option<&Type>) -> Option<String> {
        match self {
            S::None => match tau {
                Some(Type::Tuple(taus)) => {
                    let ss = (1..=taus.len())
                        .map(|l| eta.get(ell + (l as u64)).unwrap())
                        .collect::<Vec<S>>();

                    let mut string = String::new();
                    if ss.iter().all(|s| s.is_move() == 1) {
                        string.push_str("completely-but-perhaps-discontiguously moved")
                    } else if ss.iter().any(|s| s.is_move() == 1) {
                        string.push_str("partially moved")
                    } else if ss.iter().all(|s| s.is_move() == 0) {
                        string.push_str("maybe completely-but-perhaps-discontiguously moved")
                    } else if ss.iter().any(|s| s.is_move() == 0) {
                        string.push_str("maybe partially moved")
                    }

                    if ss.iter().all(|s| s.is_drop() == 1) {
                        string.push_str("completely immutably_referenced")
                    } else if ss.iter().any(|s| s.is_drop() == 1) {
                        string.push_str("partially immutably_referenced")
                    } else if ss.iter().all(|s| s.is_drop() == 0) {
                        string.push_str("maybe completely immutably_referenced")
                    } else if ss.iter().any(|s| s.is_drop() == 0) {
                        string.push_str("maybe partially immutably_referenced")
                    }

                    if ss.iter().all(|s| s.is_mut_ref() == 1) {
                        string.push_str("completely mutably_referenced")
                    } else if ss.iter().any(|s| s.is_mut_ref() == 1) {
                        string.push_str("partially mutably_referenced")
                    } else if ss.iter().all(|s| s.is_mut_ref() == 0) {
                        string.push_str("maybe completely mutably_referenced")
                    } else if ss.iter().any(|s| s.is_mut_ref() == 0) {
                        string.push_str("maybe partially mutably_referenced")
                    }

                    if string == "" {
                        None
                    } else {
                        Some(string)
                    }
                }
                _ => None,
            },
            S::Moved(Some(new_home)) => Some(format!("moved to address {new_home}")),
            S::Moved(None) => Some("dropped".to_string()),
            S::ImmutRef(referencers) => Some(format!(
                "immutably referenced by addresses [{}]",
                referencers
                    .iter()
                    .map(u64::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            )),
            S::MutRef(referencer) => Some(format!("mutably referenced by address {}", referencer)),
            S::Union(loans) => Some(
                loans
                    .iter()
                    .filter_map(|loan| S::stringify(loan, eta, ell, tau))
                    .collect::<Vec<String>>()
                    .join(" | "),
            ),
        }
    }
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
            (_, S::None) => Ok(s1),
            (S::MutRef(_), S::MutRef(_)) => Ok(S::Union(vec![s1, s2])),
            (S::ImmutRef(addrs1), S::ImmutRef(addrs2)) => Ok(S::ImmutRef(
                addrs1.into_iter().chain(addrs2.into_iter()).collect(),
            )),
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
            S::Moved(Some(addr)) => Err(format!(
                "You're trying to move a value that was already moved to address {}.",
                addr
            )),
            S::Moved(None) => Err(format!("You're trying to move a value that was dropped.")),
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
