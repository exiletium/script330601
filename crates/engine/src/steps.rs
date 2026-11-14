//! Optional recording of solution steps.
//!
//! Algorithms call [`step`] at the points a human would write down a line of
//! working.  Recording is off unless a caller wraps the computation in
//! [`record`], so the bookkeeping costs nothing in normal use.

use crate::expr::Expr;
use std::cell::RefCell;

#[derive(Clone, Debug)]
pub struct Step {
    pub text: String,
    pub math: Option<Expr>,
    pub depth: usize,
}

thread_local! {
    static LOG: RefCell<Option<Vec<Step>>> = const { RefCell::new(None) };
    static DEPTH: RefCell<usize> = const { RefCell::new(0) };
}

pub fn enabled() -> bool {
    LOG.with(|l| l.borrow().is_some())
}

/// Record a line of working.
pub fn step(text: impl Into<String>, math: Option<&Expr>) {
    LOG.with(|l| {
        if let Some(v) = l.borrow_mut().as_mut() {
            if v.len() < 400 {
                let depth = DEPTH.with(|d| *d.borrow());
                v.push(Step { text: text.into(), math: math.cloned(), depth });
            }
        }
    });
}

/// Run a sub-derivation one level deeper (indented in the output).
pub fn nested<T>(f: impl FnOnce() -> T) -> T {
    DEPTH.with(|d| *d.borrow_mut() += 1);
    let r = f();
    DEPTH.with(|d| *d.borrow_mut() -= 1);
    r
}

/// Run `f` while collecting steps.
pub fn record<T>(f: impl FnOnce() -> T) -> (T, Vec<Step>) {
    let prev = LOG.with(|l| l.borrow_mut().replace(Vec::new()));
    let prev_depth = DEPTH.with(|d| std::mem::replace(&mut *d.borrow_mut(), 0));
    let r = f();
    DEPTH.with(|d| *d.borrow_mut() = prev_depth);
    let steps = LOG.with(|l| std::mem::replace(&mut *l.borrow_mut(), prev)).unwrap_or_default();
    (r, steps)
}

/// Run `f` with step recording suspended (for internal helper computations).
pub fn quiet<T>(f: impl FnOnce() -> T) -> T {
    let prev = LOG.with(|l| l.borrow_mut().take());
    let r = f();
    LOG.with(|l| *l.borrow_mut() = prev);
    r
}

/// Discard steps recorded by a failed attempt.
pub fn mark() -> usize {
    LOG.with(|l| l.borrow().as_ref().map_or(0, |v| v.len()))
}

pub fn rollback(mark: usize) {
    LOG.with(|l| {
        if let Some(v) = l.borrow_mut().as_mut() {
            v.truncate(mark);
        }
    });
}

/// Insert a step at a position obtained from [`mark`] (used to put a rule's
/// description before the sub-steps it produced).
pub fn insert_at(mark: usize, text: impl Into<String>, math: Option<&Expr>) {
    LOG.with(|l| {
        if let Some(v) = l.borrow_mut().as_mut() {
            if v.len() < 400 && mark <= v.len() {
                let depth = DEPTH.with(|d| *d.borrow());
                v.insert(mark, Step { text: text.into(), math: math.cloned(), depth });
            }
        }
    });
}
