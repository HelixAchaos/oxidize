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
    ImmutRef(HashSet<Address>),
    Moved(Option<Address>),
    None,
}

impl S {
    pub fn validate(&self, old_s: &Self, target: Address, span: Span) -> Result<(), TypeError> {
        match (self, old_s) {
            (&S::None, _) | (_, &S::None) => Ok(()),
            (&S::Moved(Some(_)), &S::Moved(Some(_))) => Err(
                TypeError::wrap(format!("attempted to move a moved value (expected to be at address {target})"), span)
            )?,
            (&S::Moved(Some(_)), &S::Moved(None)) => Err(
                TypeError::wrap(format!("attempted to move a dropped value (expected to be at address {target})"), span)
            )?,
            (&S::Moved(Some(_)), &S::ImmutRef(_)) => Err(
                TypeError::wrap(format!("attempted to move an immutably referenced value at address {target}"), span)
            )?,
            (&S::Moved(Some(_)), &S::MutRef(_)) => Err(
                TypeError::wrap(format!("attempted to move a mutably referenced value at address {target}"), span)
            )?,
            (&S::Moved(Some(_)), &S::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&S::Moved(None), &S::Moved(Some(_))) => Err(
                TypeError::wrap(format!("attempted to drop a moved value (expected to be at address {target})"), span)
            )?,
            (&S::Moved(None), &S::Moved(None)) => Err(
                TypeError::wrap(format!("attempted to drop a dropped value (expected to be at address {target})"), span)
            )?,
            (&S::Moved(None), &S::ImmutRef(_)) => Err(
                TypeError::wrap(format!("attempted to drop a an immutably referenced value at address {target}"), span)
            )?,
            (&S::Moved(None), &S::MutRef(_)) => Err(
                TypeError::wrap(format!("attempted to drop a a mutably referenced value at address {target}"), span)
            )?,
            (&S::Moved(None), &S::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&S::MutRef(_), &S::Moved(Some(_))) => Err(
                TypeError::wrap(format!("attempted to create a mutable reference to a moved value (expected to be at address {target})"), span)
            )?,
            (&S::MutRef(_), &S::Moved(None)) => Err(
                TypeError::wrap(format!("attempted to create a mutable reference to a dropped value (expected to be at address {target})"), span)
            )?,
            (&S::MutRef(_), &S::ImmutRef(_)) => Err(
                TypeError::wrap(format!("attempted to create a mutable reference to an immutably referenced value at address {target}"), span)
            )?,
            (&S::MutRef(_), &S::MutRef(_)) => Err(
                TypeError::wrap(format!("attempted to create a mutable reference to a mutably referenced value at address {target}"), span)
            )?,
            (&S::MutRef(_), &S::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&S::ImmutRef(_), &S::Moved(Some(_))) => Err(
                TypeError::wrap(format!("attempted to create an immutable reference to a moved value (expected to be at address {target})"), span)
            )?,
            (&S::ImmutRef(_), &S::Moved(None)) => Err(
                TypeError::wrap(format!("attempted to create an immutable reference to a dropped value (expected to be at address {target})"), span)
            )?,
            (&S::ImmutRef(_), &S::ImmutRef(_)) => Ok(()),
            (&S::ImmutRef(_), &S::MutRef(_)) => Err(
                TypeError::wrap(format!("attempted to create an immutable reference to a mutably referenced value at address {target}"), span)
            )?,
            (&S::ImmutRef(_), &S::Union(ref ss)) => Ok({ss.iter().map(|s| self.validate(s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&S::Union(ref ss), &S::Moved(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&S::Union(ref ss), &S::ImmutRef(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&S::Union(ref ss), &S::MutRef(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target, span.clone())).collect::<Result<_, _>>()?;}),
            (&S::Union(ref ss), &S::Union(_)) => Ok({ss.iter().map(|s| s.validate(old_s, target, span.clone())).collect::<Result<_, _>>()?;}),
        }
    }
    pub fn is_move(&self) -> i8 {
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
    pub fn is_drop(&self) -> i8 {
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
    pub fn is_immut_ref(&self) -> i8 {
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
    pub fn is_mut_ref(&self) -> i8 {
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
                        .map(|l| eta.get(&(ell + (l as u64))).unwrap())
                        .collect::<Vec<&S>>();

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

    pub fn remove(&self, addr: &Address) -> Self {
        match self {
            S::Union(ss) => S::Union(ss.into_iter().map(|s| s.remove(addr)).collect()),
            S::MutRef(s) => {
                if s == addr {
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
            S::Moved(Some(ell)) => {
                if addr == ell {
                    S::Moved(None)
                } else {
                    S::Moved(Some(*ell))
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
