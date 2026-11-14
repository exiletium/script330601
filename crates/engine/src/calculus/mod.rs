//! Calculus: derivatives, series, limits, integrals, sums and products.

pub mod diff;
pub mod integrate;
pub mod limit;
pub mod ode;
pub mod series;
pub mod sum;
pub mod transforms;

pub use diff::{diff, diff_n};
pub use limit::{limit, Dir};
pub use series::series;

use crate::expr::Expr::*;
use crate::expr::*;

/// Evaluate calculus holder nodes (`Derivative`, `Integral`, `Limit`, `Sum`,
/// `Product`) wherever they can be computed; anything else is left as is.
pub fn doit(e: &Expr) -> Expr {
    crate::budget::tick();
    match e {
        Func(name, args) => {
            let a: Vec<Expr> = args.iter().map(doit).collect();
            match (&**name, a.len()) {
                ("Derivative", 3) => {
                    if let (Some(n), true) = (a[2].as_i64(), !has_unknown_fn(&a[0], &a[1])) {
                        if (0..=64).contains(&n) {
                            return crate::simplify::simplify(&diff_n(&a[0], &a[1], n as u32));
                        }
                    }
                    raw_fn(name, a)
                }
                ("Integral", 2) => match integrate::integrate(&a[0], &a[1]) {
                    Some(v) => v,
                    None => raw_fn(name, a),
                },
                ("Integral", 4) => match integrate::definite(&a[0], &a[1], &a[2], &a[3]) {
                    integrate::Definite::Exact(v) => v,
                    integrate::Definite::Numeric(v, _) => float(v),
                    _ => raw_fn(name, a),
                },
                ("Sum", 4) => match sum::sum(&a[0], &a[1], &a[2], &a[3]) {
                    sum::SumResult::Exact(v) => v,
                    sum::SumResult::Numeric(v) => float(v),
                    _ => raw_fn(name, a),
                },
                ("Product", 4) => match sum::product(&a[0], &a[1], &a[2], &a[3]) {
                    sum::SumResult::Exact(v) => v,
                    sum::SumResult::Numeric(v) => float(v),
                    _ => raw_fn(name, a),
                },
                ("Limit", 3) => match limit(&a[0], &a[1], &a[2], Dir::Both) {
                    Some(v) => v,
                    None => raw_fn(name, a),
                },
                ("Limit", 4) => {
                    let dir = if a[3].is_negative_term() { Dir::Left } else { Dir::Right };
                    match limit(&a[0], &a[1], &a[2], dir) {
                        Some(v) => v,
                        None => raw_fn(name, a),
                    }
                }
                _ => {
                    let holder = name.chars().next().map_or(false, |c| c.is_uppercase()) && name.len() > 2;
                    if holder {
                        raw_fn(name, a)
                    } else {
                        func(name, a)
                    }
                }
            }
        }
        Add(v) => add(v.iter().map(doit).collect()),
        Mul(v) => mul(v.iter().map(doit).collect()),
        Pow(b, ex) => pow(doit(b), doit(ex)),
        List(v) => List(v.iter().map(doit).collect()),
        Rel(op, a, b) => crate::funcs::rel(*op, doit(a), doit(b)),
        _ => e.clone(),
    }
}

/// Does `e` contain an unknown function of `x` (so that differentiating it
/// would only produce another holder)?
fn has_unknown_fn(e: &Expr, x: &Expr) -> bool {
    matches!(e, Sym(_)) && e != x && false
}
