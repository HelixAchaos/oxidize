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
    MutRef(Address),
    ImmutRef(HashSet<Address>),
    Moved(Option<Address>),
    None,
}

impl BorrowState {
    pub fn validate(&self, old_s: &Self, target: Address, span: Span) -> Result<(), TypeError> {
        match (self, old_s) {
            (&BorrowState::None, _) | (_, &BorrowState::None) => Ok(()),
            (&BorrowState::Moved(Some(_)), &BorrowState::Moved(Some(_))) => Err(
                TypeError::wrap(format!("attempted to move a moved value (expected to be at address {target})"), span)
            )?,
            (&BorrowState::Moved(Some(_)), &BorrowState::Moved(None)) => Err(
                TypeError::wrap(format!("attempted to move a dropped value (expected to be at address {target})"), span)
            )?,
            (&BorrowState::Moved(Some(_)), &BorrowState::ImmutRef(_)) => Err(
                TypeError::wrap(format!("attempted to move an immutably referenced value at address {target}"), span)
            )?,
            (&BorrowState::Moved(Some(_)), &BorrowState::MutRef(_)) => Err(
                TypeError::wrap(format!("attempted to move a mutably referenced value at address {target}"), span)
            )?,
            (&BorrowState::Moved(Some(_)), &BorrowState::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&BorrowState::Moved(None), &BorrowState::Moved(Some(_))) => Err(
                TypeError::wrap(format!("attempted to drop a moved value (expected to be at address {target})"), span)
            )?,
            (&BorrowState::Moved(None), &BorrowState::Moved(None)) => Err(
                TypeError::wrap(format!("attempted to drop a dropped value (expected to be at address {target})"), span)
            )?,
            (&BorrowState::Moved(None), &BorrowState::ImmutRef(_)) => Err(
                TypeError::wrap(format!("attempted to drop a an immutably referenced value at address {target}"), span)
            )?,
            (&BorrowState::Moved(None), &BorrowState::MutRef(_)) => Err(
                TypeError::wrap(format!("attempted to drop a a mutably referenced value at address {target}"), span)
            )?,
            (&BorrowState::Moved(None), &BorrowState::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&BorrowState::MutRef(_), &BorrowState::Moved(Some(_))) => Err(
                TypeError::wrap(format!("attempted to create a mutable reference to a moved value (expected to be at address {target})"), span)
            )?,
            (&BorrowState::MutRef(_), &BorrowState::Moved(None)) => Err(
                TypeError::wrap(format!("attempted to create a mutable reference to a dropped value (expected to be at address {target})"), span)
            )?,
            (&BorrowState::MutRef(_), &BorrowState::ImmutRef(_)) => Err(
                TypeError::wrap(format!("attempted to create a mutable reference to an immutably referenced value at address {target}"), span)
            )?,
            (&BorrowState::MutRef(_), &BorrowState::MutRef(_)) => Err(
                TypeError::wrap(format!("attempted to create a mutable reference to a mutably referenced value at address {target}"), span)
            )?,
            (&BorrowState::MutRef(_), &BorrowState::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&BorrowState::ImmutRef(_), &BorrowState::Moved(Some(_))) => Err(
                TypeError::wrap(format!("attempted to create an immutable reference to a moved value (expected to be at address {target})"), span)
            )?,
            (&BorrowState::ImmutRef(_), &BorrowState::Moved(None)) => Err(
                TypeError::wrap(format!("attempted to create an immutable reference to a dropped value (expected to be at address {target})"), span)
            )?,
            (&BorrowState::ImmutRef(_), &BorrowState::ImmutRef(_)) => Ok(()),
            (&BorrowState::ImmutRef(_), &BorrowState::MutRef(_)) => Err(
                TypeError::wrap(format!("attempted to create an immutable reference to a mutably referenced value at address {target}"), span)
            )?,
            (&BorrowState::ImmutRef(_), &BorrowState::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&BorrowState::Union(ref ss), &BorrowState::Moved(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&BorrowState::Union(ref ss), &BorrowState::ImmutRef(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&BorrowState::Union(ref ss), &BorrowState::MutRef(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&BorrowState::Union(ref ss), &BorrowState::Union(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target, span.clone())).collect::<Result<_, _>>()?;}),
        }
    }
    pub fn is_move(&self) -> i8 {
        match self {
            BorrowState::Moved(Some(_)) => 1,
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
            BorrowState::Moved(None) => 1,
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
            BorrowState::ImmutRef(_) => 1,
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
            BorrowState::MutRef(_) => 1,
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
                        .map(|l| eta.get(&(ell + (l as u64))).unwrap())
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

                    if errs.len() == 0 {
                        None
                    } else {
                        Some(errs.join(" & "))
                    }
                }
                _ => None,
            },
            BorrowState::Moved(Some(new_home)) => Some(format!("moved to address {new_home}")),
            BorrowState::Moved(None) => Some("dropped".to_string()),
            BorrowState::ImmutRef(referencers) => Some(format!(
                "immutably referenced by addresses [{}]",
                referencers
                    .iter()
                    .map(u64::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            )),
            BorrowState::MutRef(referencer) => {
                Some(format!("mutably referenced by address {}", referencer))
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
            (BorrowState::MutRef(_), BorrowState::MutRef(_)) => {
                Ok(BorrowState::Union(vec![s1, s2]))
            }
            (BorrowState::ImmutRef(addrs1), BorrowState::ImmutRef(addrs2)) => Ok(
                BorrowState::ImmutRef(addrs1.into_iter().chain(addrs2.into_iter()).collect()),
            ),
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
            BorrowState::MutRef(s) => {
                if s == addr {
                    BorrowState::None
                } else {
                    self.clone()
                }
            }
            BorrowState::ImmutRef(ss) => {
                let mut ss_prime = ss.clone();

                (&mut ss_prime).remove(&addr);
                if ss_prime.len() > 0 {
                    BorrowState::ImmutRef(ss_prime)
                } else {
                    BorrowState::None
                }
            }
            BorrowState::Moved(Some(ell)) => {
                if addr == ell {
                    BorrowState::Moved(None)
                } else {
                    BorrowState::Moved(Some(*ell))
                }
            }
            _ => self.clone(),
        }
    }

    pub fn may_move(&self) -> Result<(), String> {
        match self {
            BorrowState::Moved(Some(addr)) => Err(format!(
                "You're trying to move a value that was already moved to address {}.",
                addr
            )),
            BorrowState::Moved(None) => {
                Err(format!("You're trying to move a value that was dropped."))
            }
            BorrowState::ImmutRef(_) | BorrowState::MutRef(_) => {
                Err(format!("You can't move something referred to."))
            }
            BorrowState::None => Ok(()),
            BorrowState::Union(ss) => {
                let _ = ss
                    .into_iter()
                    .map(BorrowState::may_move)
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(())
            }
        }
    }
}
