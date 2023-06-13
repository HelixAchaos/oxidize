use std::collections::{HashMap, HashSet};
use std::{fmt, iter};

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

impl Type {
    pub fn get_typed_addresses(&self, ell: Address) -> Vec<(Address, Type)> {
        match self {
            &Type::Tuple(ref types) => iter::once((ell, self.clone()))
                .chain(
                    types
                        .iter()
                        .enumerate()
                        .map(|(i, tau)| tau.get_typed_addresses(ell + (i as u64) + 1))
                        .flatten(),
                )
                .collect(),
            _ => vec![(ell, self.clone())],
        }
    }
    pub fn get_addresses(&self, ell: Address) -> Vec<Address> {
        match self {
            &Type::Tuple(ref types) => iter::once(ell)
                .chain(
                    types
                        .iter()
                        .enumerate()
                        .map(|(i, tau)| tau.get_addresses(ell + (i as u64) + 1))
                        .flatten(),
                )
                .collect(),
            _ => vec![ell],
        }
    }
    fn is_tuple(&self) -> bool {
        match self {
            &Type::Tuple(_) => true,
            _ => false,
        }
    }
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

    pub fn flip(&self, pos: Address) -> Vec<(Address, BorrowState)> {
        match self {
            S::None => {
                vec![(pos, BorrowState::None)]
            }
            S::Moved(target) => {
                vec![(target.to_owned(), BorrowState::Moved(pos))]
            }
            S::MutRef(target) => {
                vec![(
                    target.to_owned(),
                    BorrowState::Ref(HashSet::from([(true, pos)])),
                )]
            }
            S::ImmutRef(target) => {
                vec![(
                    target.to_owned(),
                    BorrowState::Ref(HashSet::from([(false, pos)])),
                )]
            }
            S::Union(onions) => {
                let mut smap = HashMap::new();
                for (pos, bstate) in onions.iter().map(|s| s.flip(pos)).flatten() {
                    if let Some(old_bstate) = smap.remove(&pos) {
                        smap.insert(pos, [old_bstate, vec![bstate]].concat());
                    } else {
                        smap.insert(pos, vec![bstate]);
                    }
                }

                smap.into_iter()
                    .map(|(k, v)| (k, BorrowState::Union(v).trim()))
                    .collect()
            }
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
                    } else if errs.len() == 1 {
                        Some(format!("{}", errs.remove(0)))
                    } else {
                        Some(format!("({})", errs.join(" & ")))
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

                let tau = tau.unwrap_or(&Type::Unit);
                let binding = tau.get_addresses(ell.to_owned());
                let (_, ells) = binding.split_first().unwrap();
                if immutrefs.is_empty() {
                    if tau.is_tuple() {
                        let ss = ells
                            .iter()
                            .map(|ell| eta.get(ell).unwrap_or_else(|| &BorrowState::None))
                            .collect::<Vec<&BorrowState>>();

                        if ss.iter().all(|s| s.is_immut_ref() == 1) {
                            s.push("completely immutably_referenced".to_string())
                        } else if ss.iter().any(|s| s.is_immut_ref() == 1) {
                            s.push("partially immutably_referenced".to_string())
                        } else if ss.iter().all(|s| s.is_immut_ref() == 0) {
                            s.push("maybe completely immutably_referenced".to_string())
                        } else if ss.iter().any(|s| s.is_immut_ref() == 0) {
                            s.push("maybe partially immutably_referenced".to_string())
                        }
                    }
                } else {
                    s.push(format!(
                        "immutably referenced by addresses [{}]",
                        referencers
                            .iter()
                            .filter_map(|(b, a)| if b.to_owned() {
                                None
                            } else {
                                Some(a.to_string())
                            })
                            .collect::<Vec<String>>()
                            .join(", ")
                    ))
                }

                if mutref.is_empty() {
                    if tau.is_tuple() {
                        let ss = ells
                            .iter()
                            .map(|ell| eta.get(ell).unwrap_or_else(|| &BorrowState::None))
                            .collect::<Vec<&BorrowState>>();

                        if ss.iter().all(|s| s.is_mut_ref() == 1) {
                            s.push("completely mutably_referenced".to_string())
                        } else if ss.iter().any(|s| s.is_mut_ref() == 1) {
                            s.push("partially mutably_referenced".to_string())
                        } else if ss.iter().all(|s| s.is_mut_ref() == 0) {
                            s.push("maybe completely mutably_referenced".to_string())
                        } else if ss.iter().any(|s| s.is_mut_ref() == 0) {
                            s.push("maybe partially mutably_referenced".to_string())
                        }
                    }
                } else {
                    assert!(mutref.len() == 1);
                    let (_, referencer) = mutref.get(0).unwrap();
                    s.push(format!("mutably referenced by address {}", referencer))
                }
                Some(s.join(" and "))
            }
            BorrowState::Union(loans) => {
                let mut strings = loans
                    .iter()
                    .filter_map(|loan| BorrowState::stringify(loan, eta, ell, tau))
                    .collect::<Vec<String>>();

                if strings.is_empty() {
                    None
                } else if strings.len() == 1 {
                    Some(strings.remove(0))
                } else {
                    Some(format!("({})", strings.join(" | ")))
                }
            }
        }
    }

    pub fn join(s1: Self, s2: Self) -> Self {
        match (s1.clone(), s2.clone()) {
            (BorrowState::None, _) => s2,
            (_, BorrowState::None) => s1,
            (BorrowState::Union(ss1), BorrowState::Union(ss2)) => {
                BorrowState::Union([ss1, ss2].concat())
            }
            (BorrowState::Union(ss), _) => BorrowState::Union([ss, vec![s2]].concat()),
            (_, BorrowState::Union(ss)) => BorrowState::Union([ss, vec![s1]].concat()),
            _ => BorrowState::Union(vec![s1, s2]),
        }
        .trim()
    }

    pub fn join_refs(bstate1: Self, bstate2: Self) -> Self {
        match (bstate1.clone(), bstate2.clone()) {
            (BorrowState::Ref(refs1), BorrowState::Ref(refs2)) => {
                BorrowState::Ref(refs1.into_iter().chain(refs2.into_iter()).collect())
            }
            (BorrowState::Union(ss), _) => BorrowState::Union(
                ss.into_iter()
                    .map(|x| BorrowState::join_refs(x, bstate2.clone()))
                    .collect(),
            ),
            (_, BorrowState::Union(ss)) => BorrowState::Union(
                ss.into_iter()
                    .map(|x| BorrowState::join_refs(bstate1.clone(), x))
                    .collect(),
            ),
            _ => bstate1,
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
        .trim()
    }

    pub fn may_drop(&self) -> Result<(), String> {
        match self {
            BorrowState::Moved(addr) => Err(format!(
                "You're trying to use a value that was already moved to address {}.",
                addr
            )),
            BorrowState::Dropped => Err(format!(
                "You're trying to use a value that was already dropped."
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

    pub fn may_let_go(&self) -> Result<(), String> {
        match self {
            BorrowState::Moved(addr) => Err(format!(
                "You're trying to drop a value that was already moved to address {}.",
                addr
            )),
            BorrowState::Dropped => Ok(()),
            BorrowState::Ref(_) => Err(format!("You can't drop something referred to.")),
            BorrowState::None => Ok(()),
            BorrowState::Union(ss) => {
                let _ = ss
                    .into_iter()
                    .map(BorrowState::may_let_go)
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(())
            }
        }
    }

    pub fn may_overwrite(&self) -> Result<(), String> {
        match self {
            BorrowState::Moved(_) => Ok(()),
            BorrowState::Dropped => Ok(()),
            BorrowState::Ref(_) => Err(format!(
                "You can't assign to an address that is currently referred to."
            )),
            BorrowState::None => Ok(()),
            BorrowState::Union(ss) => {
                let _ = ss
                    .into_iter()
                    .map(BorrowState::may_overwrite)
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(())
            }
        }
    }

    pub fn trim(self) -> Self {
        match self {
            BorrowState::Union(onions) => {
                let mut sonions = vec![];
                for (i, bstate_1) in onions
                    .clone()
                    .into_iter()
                    .map(BorrowState::trim)
                    .enumerate()
                {
                    if ((i + 1)..onions.len()).all(|j| {
                        if let Some(bstate_2) = onions.get(j) {
                            bstate_2 != &bstate_1
                        } else {
                            false
                        }
                    }) {
                        sonions.push(bstate_1)
                    }
                }
                if sonions.len() == 1 {
                    sonions.remove(0)
                } else {
                    BorrowState::Union(sonions)
                }
            }
            _ => self,
        }
    }
}
