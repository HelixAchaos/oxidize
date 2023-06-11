use std::collections::HashSet;
use std::fmt;

use crate::ast::Span;
use crate::typechecker::Eta;

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

#[derive(Debug)]
pub struct TypeError {
    pub msg: String,
    pub span: Span,
}

impl TypeError {
    pub fn prettify(&self, src: String) -> String {
        format!(
            "At {:?}, TypeCheckError was raised: {}\n\nCode snippet:\n```\n{}\n```",
            self.span,
            self.msg,
            &src[self.span.clone()]
        )
    }
    pub fn wrap(msg: String, span: Span) -> TypeError {
        TypeError { msg, span }
    }
    pub fn result_wrap<T>(result: Result<T, String>, span: Span) -> Result<T, TypeError> {
        match result {
            Ok(t) => Ok(t),
            Err(msg) => Err(TypeError::wrap(msg, span)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum S {
    Union(Vec<Self>),
    MutRef(Address),
    ImmutRef(Address),
    Moved(Address),
    None,
}

impl S {
    pub fn join(s1: Self, s2: Self) -> Result<Self, String> {
        match (s1.clone(), s2.clone()) {
            (S::None, _) => Ok(s2),
            (_, S::None) => Ok(s1),
            (S::Union(ss1), S::Union(ss2)) => Ok(S::Union([ss1, ss2].concat())),
            (S::Union(ss), _) => Ok(S::Union([ss, vec![s2]].concat())),
            (_, S::Union(ss)) => Ok(S::Union([ss, vec![s1]].concat())),
            _ => Ok(S::Union(vec![s1, s2])),
        }
    }

    pub fn extract_referred_addresses(&self) -> Vec<Address> {
        match self {
            S::Union(v) => v
                .into_iter()
                .map(S::extract_referred_addresses)
                .collect::<Vec<Vec<Address>>>()
                .concat(),
            S::MutRef(addr) | S::ImmutRef(addr) => vec![addr.to_owned()],
            S::Moved(_) | S::None => vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BorrowState {
    Union(Vec<Self>),
    Ref(HashSet<(bool, Address)>),
    Moved(Address),
    Dropped,
    None,
}

impl BorrowState {
    pub fn validate(&self, new_s: &S, target: Address, span: Span) -> Result<(), TypeError> {
        match (self, new_s) {
            (&BorrowState::None, _) | (_, &S::None) => Ok(()),
            (&BorrowState::Moved(_), &S::Moved(_)) => Err(
                TypeError::wrap(format!("attempted to move a moved value (expected to be at address {target})"), span)
            )?,
            (&BorrowState::Moved(_), &S::MutRef(_)) => {
                Err(TypeError::wrap(format!("attempted to create a mutable reference to a moved value (expected to be at address {target})"), span))
            }
            (&BorrowState::Moved(_), &S::ImmutRef(_)) => {
                Err(TypeError::wrap(format!("attempted to create an immutable reference to a moved value (value at address {target})"), span))
            }
            (&BorrowState::Dropped, &S::Moved(_)) => Err(
                TypeError::wrap(format!("attempted to move a dropped value (expected to be at address {target})"), span)
            )?,
            (&BorrowState::Dropped, &S::MutRef(_)) => {
                Err(TypeError::wrap(format!("attempted to create a mutable reference to a dropped value (expected to be at address {target})"), span))
            }
            (&BorrowState::Dropped, &S::ImmutRef(_)) => {
                Err(TypeError::wrap(format!("attempted to create an immutable reference to a dropped value (value at address {target})"), span))
            }
            (&BorrowState::Ref(ref refs), &S::Moved(_)) => {
                assert!(!refs.is_empty());
                if refs.into_iter().any(|(b, _)| b.to_owned()) {
                    Err(
                        TypeError::wrap(format!("attempted to move a mutably referenced value at address {target}"), span))
                } else {
                    Err(
                        TypeError::wrap(format!("attempted to move an immutably referenced value at address {target}"), span))
                }
            },
            (&BorrowState::Ref(ref refs), &S::MutRef(_)) if refs.into_iter().any(|(b, _)| b.to_owned()) => Err(
                        TypeError::wrap(format!("attempted to create a mutable reference to an already-mutably-referenced value at address {target}"), span)),
            (&BorrowState::Ref(_), &S::MutRef(_)) => Err(
                        TypeError::wrap(format!("attempted to create a mutable reference to an immutably referenced value at address {target}"), span)),

            (&BorrowState::Ref(ref refs), &S::ImmutRef(_)) => {
                assert!(!refs.is_empty());
                Ok(())
            },
            (&BorrowState::Union(ref states) , _) => {
                for state in states.iter() {
                    state.validate(new_s, target, span.clone())?
                };
                Ok(())
            }
            (_, &S::Union(ref ss)) => {
                for s in ss.iter() {
                    self.validate(s, target, span.clone(), )?
                };
                Ok(())
            }
        }
    }
    pub fn is_move(&self) -> i8 {
        match self {
            BorrowState::Moved(_) => 1,
            BorrowState::Union(ss) => {
                if ss.iter().any(|s| s.is_move() == 1) {
                    0
                } else {
                    -1
                }
            }
            _ => -1,
        }
    }
    pub fn is_drop(&self) -> i8 {
        match self {
            BorrowState::Dropped => 1,
            BorrowState::Union(ss) => {
                if ss.iter().any(|s| s.is_drop() == 1) {
                    0
                } else {
                    -1
                }
            }
            _ => -1,
        }
    }
    pub fn is_immut_ref(&self) -> i8 {
        match self {
            BorrowState::Ref(refs) if refs.into_iter().any(|(b, _)| !b) => 1,
            BorrowState::Union(ss) => {
                if ss.iter().any(|s| s.is_immut_ref() == 1) {
                    0
                } else {
                    -1
                }
            }
            _ => -1,
        }
    }
    pub fn is_mut_ref(&self) -> i8 {
        match self {
            BorrowState::Ref(refs) if refs.into_iter().any(|(b, _)| b.to_owned()) => 1,
            BorrowState::Union(ss) => {
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
            BorrowState::None => match tau {
                Some(Type::Tuple(taus)) => {
                    let ss = (1..=taus.len())
                        .map(|l| {
                            eta.get(&(ell + (l as u64)))
                                .unwrap_or_else(|| &BorrowState::None)
                        })
                        .collect::<Vec<&BorrowState>>();

                    let errs = &mut vec![];
                    if ss.iter().all(|s| s.is_move() == 1) {
                        errs.push("completely-but-perhaps-discontiguously moved")
                    } else if ss.iter().any(|s| s.is_move() == 1) {
                        errs.push("partially moved")
                    } else if ss.iter().all(|s| s.is_move() == 0) {
                        errs.push("maybe completely-but-perhaps-discontiguously moved")
                    } else if ss.iter().any(|s| s.is_move() == 0) {
                        errs.push("maybe partially moved")
                    }

                    if ss.iter().all(|s| s.is_drop() == 1) {
                        errs.push("completely-but-perhaps-discontiguously dropped")
                    } else if ss.iter().any(|s| s.is_drop() == 1) {
                        errs.push("partially dropped")
                    } else if ss.iter().all(|s| s.is_drop() == 0) {
                        errs.push("maybe completely-but-perhaps-discontiguously dropped")
                    } else if ss.iter().any(|s| s.is_drop() == 0) {
                        errs.push("maybe partially dropped")
                    }

                    if ss.iter().all(|s| s.is_immut_ref() == 1) {
                        errs.push("completely immutably_referenced")
                    } else if ss.iter().any(|s| s.is_immut_ref() == 1) {
                        errs.push("partially immutably_referenced")
                    } else if ss.iter().all(|s| s.is_immut_ref() == 0) {
                        errs.push("maybe completely immutably_referenced")
                    } else if ss.iter().any(|s| s.is_immut_ref() == 0) {
                        errs.push("maybe partially immutably_referenced")
                    }

                    if ss.iter().all(|s| s.is_mut_ref() == 1) {
                        errs.push("completely mutably_referenced")
                    } else if ss.iter().any(|s| s.is_mut_ref() == 1) {
                        errs.push("partially mutably_referenced")
                    } else if ss.iter().all(|s| s.is_mut_ref() == 0) {
                        errs.push("maybe completely mutably_referenced")
                    } else if ss.iter().any(|s| s.is_mut_ref() == 0) {
                        errs.push("maybe partially mutably_referenced")
                    }

                    if errs.is_empty() {
                        None
                    } else {
                        Some(errs.join(" & "))
                    }
                }
                _ => None,
            },
            BorrowState::Moved(new_home) => Some(format!("moved to address {new_home}")),
            BorrowState::Dropped => Some("dropped".to_string()),
            BorrowState::Ref(referencers) => {
                let mut s: Vec<String> = Vec::new();
                let (mutref, immutrefs): (Vec<_>, Vec<_>) =
                    referencers.into_iter().partition(|(b, _)| b.to_owned());

                if !immutrefs.is_empty() {
                    s.push(format!(
                        "immutably referenced by addresses [{}]",
                        referencers
                            .iter()
                            .map(|(_, a)| a.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    ))
                }
                if !mutref.is_empty() {
                    assert!(mutref.len() == 1);
                    let (_, referencer) = mutref.get(0).unwrap();
                    s.push(format!("mutably referenced by address {}", referencer))
                }
                Some(s.join(" and "))
            }
            BorrowState::Union(loans) => Some(
                loans
                    .iter()
                    .filter_map(|loan| BorrowState::stringify(loan, eta, ell, tau))
                    .collect::<Vec<String>>()
                    .join(" | "),
            ),
        }
    }

    pub fn join(s1: Self, s2: Self) -> Result<Self, String> {
        match (s1.clone(), s2.clone()) {
            (BorrowState::None, _) => Ok(s2),
            (_, BorrowState::None) => Ok(s1),
            (BorrowState::Union(ss1), BorrowState::Union(ss2)) => {
                Ok(BorrowState::Union([ss1, ss2].concat()))
            }
            (BorrowState::Union(ss), _) => Ok(BorrowState::Union([ss, vec![s2]].concat())),
            (_, BorrowState::Union(ss)) => Ok(BorrowState::Union([ss, vec![s1]].concat())),
            _ => Ok(BorrowState::Union(vec![s1, s2])),
        }
    }

    pub fn remove(&self, addr: &Address) -> Self {
        match self {
            BorrowState::Union(ss) => {
                BorrowState::Union(ss.into_iter().map(|s| s.remove(addr)).collect())
            }
            BorrowState::Ref(referencers) => {
                let filtered: HashSet<_> = referencers
                    .into_iter()
                    .filter_map(|(b, a)| {
                        if a != addr {
                            Some((b.to_owned(), a.to_owned()))
                        } else {
                            None
                        }
                    })
                    .collect();

                if filtered.is_empty() {
                    BorrowState::None
                } else {
                    BorrowState::Ref(filtered)
                }
            }
            BorrowState::Moved(ell) => {
                if addr == ell {
                    BorrowState::Dropped
                } else {
                    BorrowState::Moved(*ell)
                }
            }
            _ => self.clone(),
        }
    }

    pub fn may_drop(&self) -> Result<(), String> {
        match self {
            BorrowState::Moved(addr) => Err(format!(
                "You're trying to use/drop a value that was already moved to address {}.",
                addr
            )),
            BorrowState::Dropped => Err(format!(
                "You're trying to use/drop a value that was already dropped."
            )),
            BorrowState::Ref(_) => Err(format!("You can't use/drop something referred to.")),
            BorrowState::None => Ok(()),
            BorrowState::Union(ss) => {
                let _ = ss
                    .into_iter()
                    .map(BorrowState::may_drop)
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(())
            }
        }
    }

    pub fn may_force_drop(&self) -> Result<(), String> {
        match self {
            BorrowState::Moved(addr) => Err(format!(
                "You're trying to use/drop a value that was already moved to address {}.",
                addr
            )),
            BorrowState::Dropped => Ok(()),
            BorrowState::Ref(_) => Err(format!("You can't use/drop something referred to.")),
            BorrowState::None => Ok(()),
            BorrowState::Union(ss) => {
                let _ = ss
                    .into_iter()
                    .map(BorrowState::may_force_drop)
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(())
            }
        }
    }
}
