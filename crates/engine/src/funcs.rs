//! Automatic evaluation of named functions (special values, parity,
//! exact arithmetic on numbers, floating-point evaluation).

use crate::eval::{self, Fun};
use crate::expr::Expr::*;
use crate::expr::*;
use crate::numtheory as nt;
use num_bigint::BigInt;
use num_complex::Complex64;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::sync::Arc;

/// Functions that are applied element-wise to lists.
const THREADING: &[&str] = &[
    "sin", "cos", "tan", "cot", "sec", "csc", "asin", "acos", "atan", "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", "ln", "exp", "sqrt", "abs", "sign", "floor", "ceil", "round", "factorial", "gamma", "re", "im", "conj", "arg", "erf", "isprime", "fibonacci", "prime", "totient", "cbrt", "log10", "log2", "zeta",
];

pub fn apply(name: &str, args: Vec<Expr>) -> Expr {
    if args.len() == 1 && THREADING.contains(&name) {
        if let List(v) = &args[0] {
            return List(v.iter().map(|a| apply(name, vec![a.clone()])).collect());
        }
    }
    let holder = name.chars().next().map_or(false, |c| c.is_uppercase()) && name.len() > 2;
    if !holder && args.iter().any(|a| a.is_undef()) {
        return undef();
    }
    match evaluate(name, &args) {
        Some(v) => v,
        None => Func(Arc::from(name), args),
    }
}

fn sq(n: i64) -> Expr {
    sqrt(int(n))
}

fn half() -> Expr {
    rat(1, 2)
}

/// `x = q π` for a rational q.
pub fn pi_coeff(x: &Expr) -> Option<Rat> {
    match x {
        Const(K::Pi) => Some(Rat::one()),
        Num(r) if r.is_zero() => Some(Rat::zero()),
        Mul(v) if v.len() == 2 && matches!(v[1], Const(K::Pi)) => v[0].as_rat().cloned(),
        _ => None,
    }
}

fn rat_i(n: i64, d: i64) -> Rat {
    Rat::new(BigInt::from(n), BigInt::from(d))
}

fn sin_table(n: i64, d: i64) -> Option<Expr> {
    Some(match (n, d) {
        (0, 1) => zero(),
        (1, 12) => (sq(6) - sq(2)) / int(4),
        (1, 10) => (sq(5) - one()) / int(4),
        (1, 8) => sqrt(int(2) - sq(2)) / int(2),
        (1, 6) => half(),
        (1, 5) => sqrt(int(10) - int(2) * sq(5)) / int(4),
        (1, 4) => sq(2) / int(2),
        (3, 10) => (one() + sq(5)) / int(4),
        (1, 3) => sq(3) / int(2),
        (3, 8) => sqrt(int(2) + sq(2)) / int(2),
        (2, 5) => sqrt(int(10) + int(2) * sq(5)) / int(4),
        (5, 12) => (sq(6) + sq(2)) / int(4),
        (1, 2) => one(),
        _ => return None,
    })
}

fn tan_table(n: i64, d: i64) -> Option<Expr> {
    Some(match (n, d) {
        (0, 1) => zero(),
        (1, 12) => int(2) - sq(3),
        (1, 10) => sqrt(int(25) - int(10) * sq(5)) / int(5),
        (1, 8) => sq(2) - one(),
        (1, 6) => sq(3) / int(3),
        (1, 5) => sqrt(int(5) - int(2) * sq(5)),
        (1, 4) => one(),
        (3, 10) => sqrt(int(25) + int(10) * sq(5)) / int(5),
        (1, 3) => sq(3),
        (3, 8) => sq(2) + one(),
        (2, 5) => sqrt(int(5) + int(2) * sq(5)),
        (5, 12) => int(2) + sq(3),
        _ => return None,
    })
}

const TABLE_KEYS: [(i64, i64); 13] = [(0, 1), (1, 12), (1, 10), (1, 8), (1, 6), (1, 5), (1, 4), (3, 10), (1, 3), (3, 8), (2, 5), (5, 12), (1, 2)];

/// sin(q π) for rational q, when a closed form is known.
pub fn sin_pi(q: &Rat) -> Option<Expr> {
    let two = Rat::from_integer(BigInt::from(2));
    let mut q = q - (q / &two).floor() * &two; // [0, 2)
    let mut sign = 1;
    if q >= Rat::one() {
        q -= Rat::one();
        sign = -1;
    }
    if q > rat_i(1, 2) {
        q = Rat::one() - q;
    }
    let v = sin_table(q.numer().to_i64()?, q.denom().to_i64()?)?;
    Some(if sign < 0 { neg(v) } else { v })
}

pub fn cos_pi(q: &Rat) -> Option<Expr> {
    sin_pi(&(rat_i(1, 2) - q))
}

pub fn tan_pi(q: &Rat) -> Option<Expr> {
    let mut q = q - q.floor(); // [0, 1)
    let mut sign = 1;
    if q > rat_i(1, 2) {
        q = Rat::one() - q;
        sign = -1;
    }
    if q == rat_i(1, 2) {
        return Some(Const(K::CInf));
    }
    let v = tan_table(q.numer().to_i64()?, q.denom().to_i64()?)?;
    Some(if sign < 0 { neg(v) } else { v })
}

/// Whether `-x` is the canonical representative of the pair `{x, -x}`.
pub fn is_negated(x: &Expr) -> bool {
    match x {
        Num(_) | Float(_) | Mul(_) => x.is_negative_term(),
        // decide by the term whose non-numeric part is smallest, which does
        // not depend on the signs of the coefficients
        Add(v) => v.iter().filter(|t| !t.is_numeric()).min_by(|a, b| a.coeff_split().1.cmp(&b.coeff_split().1)).map_or(false, |t| t.is_negative_term()),
        _ => false,
    }
}

/// Split `x = rest + k π/2` with integer k (mod 4), when such a term exists.
fn quarter_shift(x: &Expr) -> Option<(Expr, i64)> {
    let Add(terms) = x else { return None };
    for (idx, t) in terms.iter().enumerate() {
        if let Some(q) = pi_coeff(t) {
            let twice = &q * Rat::from_integer(BigInt::from(2));
            if twice.is_integer() && !twice.is_zero() {
                let k = twice.numer().mod_floor(&BigInt::from(4)).to_i64()?;
                let mut rest = terms.clone();
                rest.remove(idx);
                return Some((add(rest), k));
            }
        }
    }
    None
}

fn is_fn<'a>(x: &'a Expr, name: &str) -> Option<&'a Expr> {
    match x {
        Func(n, a) if &**n == name && a.len() == 1 => Some(&a[0]),
        _ => None,
    }
}

fn trig(name: &str, x: &Expr) -> Option<Expr> {
    if let Some(q) = pi_coeff(x) {
        let v = match name {
            "sin" => sin_pi(&q),
            "cos" => cos_pi(&q),
            _ => tan_pi(&q),
        };
        if v.is_some() {
            return v;
        }
        // reduce the angle into (-1, 1] turns
        let two = Rat::from_integer(BigInt::from(2));
        let period = if name == "tan" { Rat::one() } else { two };
        let r = &q - (&q / &period).floor() * &period;
        let r = if r > &period / Rat::from_integer(BigInt::from(2)) { r - &period } else { r };
        if r != q {
            return Some(func(name, vec![mul(vec![Num(r), pi()])]));
        }
    }
    if is_negated(x) {
        let inner = func(name, vec![neg(x.clone())]);
        return Some(if name == "cos" { inner } else { neg(inner) });
    }
    if let Some((rest, k)) = quarter_shift(x) {
        let s = || func("sin", vec![rest.clone()]);
        let c = || func("cos", vec![rest.clone()]);
        return Some(match (name, k) {
            ("sin", 0) => s(),
            ("sin", 1) => c(),
            ("sin", 2) => neg(s()),
            ("sin", _) => neg(c()),
            ("cos", 0) => c(),
            ("cos", 1) => neg(s()),
            ("cos", 2) => neg(c()),
            ("cos", _) => s(),
            (_, 0) | (_, 2) => func("tan", vec![rest.clone()]),
            _ => neg(pow(func("tan", vec![rest.clone()]), int(-1))),
        });
    }
    // compositions with inverse functions
    let one_minus_sq = |a: &Expr| sqrt(one() - square(a.clone()));
    let one_plus_sq = |a: &Expr| sqrt(one() + square(a.clone()));
    if let Some(a) = is_fn(x, "asin") {
        return Some(match name {
            "sin" => a.clone(),
            "cos" => one_minus_sq(a),
            _ => a.clone() / one_minus_sq(a),
        });
    }
    if let Some(a) = is_fn(x, "acos") {
        return Some(match name {
            "sin" => one_minus_sq(a),
            "cos" => a.clone(),
            _ => one_minus_sq(a) / a.clone(),
        });
    }
    if let Some(a) = is_fn(x, "atan") {
        return Some(match name {
            "sin" => a.clone() / one_plus_sq(a),
            "cos" => one() / one_plus_sq(a),
            _ => a.clone(),
        });
    }
    // imaginary arguments: sin(i x) = i sinh(x)
    if let Some(r) = imaginary_part_only(x) {
        return Some(match name {
            "sin" => i() * func("sinh", vec![r]),
            "cos" => func("cosh", vec![r]),
            _ => i() * func("tanh", vec![r]),
        });
    }
    None
}

/// `x = i * r` with r free of `i`.
fn imaginary_part_only(x: &Expr) -> Option<Expr> {
    match x {
        Const(K::I) => Some(one()),
        Mul(v) if v.iter().filter(|f| matches!(f, Const(K::I))).count() == 1 => {
            let rest: Vec<Expr> = v.iter().filter(|f| !matches!(f, Const(K::I))).cloned().collect();
            let r = mul(rest);
            if r.has_const(K::I) {
                None
            } else {
                Some(r)
            }
        }
        _ => None,
    }
}

fn hyper(name: &str, x: &Expr) -> Option<Expr> {
    if x.is_zero() {
        return Some(if name == "cosh" { one() } else { zero() });
    }
    if matches!(x, Const(K::Inf)) {
        return Some(if name == "tanh" { one() } else { inf() });
    }
    if x.is_neg_inf() {
        return Some(match name {
            "tanh" => int(-1),
            "cosh" => inf(),
            _ => neg(inf()),
        });
    }
    if is_negated(x) {
        let inner = func(name, vec![neg(x.clone())]);
        return Some(if name == "cosh" { inner } else { neg(inner) });
    }
    let inv = match name {
        "sinh" => "asinh",
        "cosh" => "acosh",
        _ => "atanh",
    };
    if let Some(a) = is_fn(x, inv) {
        return Some(a.clone());
    }
    if let Some(r) = imaginary_part_only(x) {
        return Some(match name {
            "sinh" => i() * func("sin", vec![r]),
            "cosh" => func("cos", vec![r]),
            _ => i() * func("tan", vec![r]),
        });
    }
    None
}

fn inverse_trig(name: &str, x: &Expr) -> Option<Expr> {
    match name {
        "asin" | "acos" => {
            if is_negated(x) {
                let p = func(name, vec![neg(x.clone())]);
                if name == "asin" {
                    return Some(neg(p));
                }
                if !x.free_symbols().is_empty() {
                    return None;
                }
                return Some(pi() - p);
            }
            for (n, d) in TABLE_KEYS {
                if sin_table(n, d).as_ref() == Some(x) {
                    let ang = mul(vec![rat(n, d), pi()]);
                    return Some(if name == "asin" { ang } else { pi() / int(2) - ang });
                }
            }
            None
        }
        "atan" => {
            if x.is_zero() {
                return Some(zero());
            }
            if matches!(x, Const(K::Inf)) {
                return Some(pi() / int(2));
            }
            if x.is_neg_inf() {
                return Some(neg(pi() / int(2)));
            }
            if is_negated(x) {
                return Some(neg(func("atan", vec![neg(x.clone())])));
            }
            for (n, d) in TABLE_KEYS {
                if (n, d) != (1, 2) && tan_table(n, d).as_ref() == Some(x) {
                    return Some(mul(vec![rat(n, d), pi()]));
                }
            }
            if let Some(a) = is_fn(x, "tan") {
                // principal range only for small numeric arguments
                if let Some(v) = a.to_f64_const() {
                    if v.abs() < std::f64::consts::FRAC_PI_2 {
                        return Some(a.clone());
                    }
                }
            }
            None
        }
        "asinh" | "atanh" => {
            if x.is_zero() {
                return Some(zero());
            }
            if name == "atanh" && x.is_one() {
                return Some(inf());
            }
            if name == "asinh" && matches!(x, Const(K::Inf)) {
                return Some(inf());
            }
            if is_negated(x) {
                return Some(neg(func(name, vec![neg(x.clone())])));
            }
            None
        }
        "acosh" => {
            if x.is_one() {
                return Some(zero());
            }
            if matches!(x, Const(K::Inf)) {
                return Some(inf());
            }
            None
        }
        _ => None,
    }
}

/// Smallest base b and exponent k >= 1 with n = b^k.
fn perfect_power(n: &BigInt) -> (BigInt, u32) {
    let bits = n.bits() as u32;
    for k in (2..=bits.max(2)).rev() {
        let r = n.nth_root(k);
        if r > BigInt::one() && r.pow(k) == *n {
            let (b, k2) = perfect_power(&r);
            return (b, k * k2);
        }
    }
    (n.clone(), 1)
}

fn ln_eval(x: &Expr) -> Option<Expr> {
    match x {
        Num(r) => {
            if r.is_zero() {
                return Some(neg(inf()));
            }
            if r.is_one() {
                return Some(zero());
            }
            if r.is_negative() {
                return Some(ln(Num(-r.clone())) + i() * pi());
            }
            if !r.is_integer() {
                if r.numer().is_one() {
                    return Some(neg(ln(big(r.denom().clone()))));
                }
                return Some(ln(big(r.numer().clone())) - ln(big(r.denom().clone())));
            }
            if r.numer().bits() > 4000 {
                return None;
            }
            let (b, k) = perfect_power(r.numer());
            if k > 1 {
                return Some(int(k as i64) * ln(big(b)));
            }
            None
        }
        Float(v) => Some(if v.0 > 0.0 {
            float(v.0.ln())
        } else if v.0 == 0.0 {
            neg(inf())
        } else {
            float((-v.0).ln()) + i() * pi()
        }),
        Const(K::E) => Some(one()),
        Const(K::I) => Some(i() * pi() / int(2)),
        Const(K::Inf) => Some(inf()),
        Pow(b, ex) => {
            let positive_base = matches!(&**b, Num(r) if r.is_positive()) || matches!(&**b, Const(K::E | K::Pi | K::Golden));
            if positive_base && !ex.has_const(K::I) {
                return Some((**ex).clone() * ln((**b).clone()));
            }
            None
        }
        Mul(v) => {
            // ln(-c) for negative constants, ln(i c)
            if v.len() == 2 && v[0].is_minus_one() && matches!(v[1], Const(K::Pi | K::E | K::Golden)) {
                return Some(ln(v[1].clone()) + i() * pi());
            }
            None
        }
        _ => None,
    }
}

fn log_base(b: &Expr, x: &Expr) -> Expr {
    // exact when x is a rational power of b
    if let (Some(bi), Some(xi)) = (b.as_int(), x.as_int()) {
        if bi > &BigInt::one() && xi.is_positive() {
            let mut k = 0i64;
            let mut t = xi.clone();
            while (&t % bi).is_zero() {
                t /= bi;
                k += 1;
            }
            if t.is_one() {
                return int(k);
            }
        }
    }
    ln(x.clone()) / ln(b.clone())
}

/// Numeric value of a constant expression (no free symbols).
fn cval(x: &Expr) -> Option<Complex64> {
    eval::const_c64(x)
}

fn rval(x: &Expr) -> Option<f64> {
    if matches!(x, Sym(_)) {
        return None;
    }
    let z = cval(x)?;
    if z.im.abs() <= 1e-13 * (1.0 + z.re.abs()) {
        Some(z.re)
    } else {
        None
    }
}

fn cfloat(z: Complex64) -> Expr {
    if z.im == 0.0 || (z.im.abs() < 1e-15 * z.re.abs()) {
        float(z.re)
    } else if z.re.abs() < 1e-15 * z.im.abs() {
        float(z.im) * i()
    } else {
        float(z.re) + float(z.im) * i()
    }
}

/// Split into real and imaginary parts, assuming symbols are real.
pub fn re_im(x: &Expr) -> Option<(Expr, Expr)> {
    if !x.has_const(K::I) {
        // real-valued unless a root/log of a negative constant is involved
        if x.free_symbols().is_empty() {
            let z = cval(x)?;
            if z.im.abs() > 1e-13 * (1.0 + z.re.abs()) {
                return match x {
                    Add(v) => {
                        let parts: Option<Vec<_>> = v.iter().map(re_im).collect();
                        let parts = parts?;
                        Some((add(parts.iter().map(|p| p.0.clone()).collect()), add(parts.iter().map(|p| p.1.clone()).collect())))
                    }
                    _ => Some((float(z.re), float(z.im))),
                };
            }
        }
        return Some((x.clone(), zero()));
    }
    match x {
        Const(K::I) => Some((zero(), one())),
        Add(v) => {
            let (mut re, mut im) = (vec![], vec![]);
            for t in v {
                let (a, b) = re_im(t)?;
                re.push(a);
                im.push(b);
            }
            Some((add(re), add(im)))
        }
        Mul(v) => {
            let (mut re, mut im) = (one(), zero());
            for f in v {
                let (c, d) = re_im(f)?;
                let nre = re.clone() * c.clone() - im.clone() * d.clone();
                let nim = re * d + im * c;
                re = crate::simplify::expand(&nre);
                im = crate::simplify::expand(&nim);
            }
            Some((re, im))
        }
        Pow(b, ex) => {
            if let Some(n) = ex.as_i64() {
                if n.abs() <= 64 {
                    let (a, bb) = re_im(b)?;
                    let (mut re, mut im) = (one(), zero());
                    for _ in 0..n.abs() {
                        let nre = crate::simplify::expand(&(re.clone() * a.clone() - im.clone() * bb.clone()));
                        let nim = crate::simplify::expand(&(re * bb.clone() + im * a.clone()));
                        re = nre;
                        im = nim;
                    }
                    if n < 0 {
                        let den = crate::simplify::expand(&(square(re.clone()) + square(im.clone())));
                        return Some((re / den.clone(), neg(im) / den));
                    }
                    return Some((re, im));
                }
            }
            let (c, d) = re_im(ex)?;
            let base_ok = matches!(&**b, Const(K::E)) || matches!(&**b, Num(r) if r.is_positive());
            if base_ok {
                let lnb = ln((**b).clone());
                let mag = pow((**b).clone(), c);
                let ang = d * lnb;
                return Some((mag.clone() * func("cos", vec![ang.clone()]), mag * func("sin", vec![ang])));
            }
            // general case through polar form when numeric
            let z = cval(x)?;
            Some((float(z.re), float(z.im)))
        }
        Func(n, a) if a.len() == 1 => {
            let (c, d) = re_im(&a[0])?;
            let f = |name: &str, v: &Expr| func(name, vec![v.clone()]);
            match &**n {
                "sin" => Some((f("sin", &c) * f("cosh", &d), f("cos", &c) * f("sinh", &d))),
                "cos" => Some((f("cos", &c) * f("cosh", &d), neg(f("sin", &c) * f("sinh", &d)))),
                "sinh" => Some((f("sinh", &c) * f("cos", &d), f("cosh", &c) * f("sin", &d))),
                "cosh" => Some((f("cosh", &c) * f("cos", &d), f("sinh", &c) * f("sin", &d))),
                "ln" => Some((ln(sqrt(square(c.clone()) + square(d.clone()))), func("arg", vec![a[0].clone()]))),
                _ => {
                    let z = cval(x)?;
                    Some((float(z.re), float(z.im)))
                }
            }
        }
        _ => {
            let z = cval(x)?;
            Some((float(z.re), float(z.im)))
        }
    }
}

fn floor_like(name: &str, x: &Expr) -> Option<Expr> {
    if let Num(r) = x {
        return Some(match name {
            "floor" => big(r.floor().to_integer()),
            "ceil" => big(r.ceil().to_integer()),
            "frac" => Num(r - r.floor()),
            _ => {
                // round half to even
                let two = BigInt::from(2);
                let fl = r.floor().to_integer();
                let diff = r - Rat::from_integer(fl.clone());
                let h = rat_i(1, 2);
                if diff > h || (diff == h && fl.is_odd()) {
                    big(fl + 1)
                } else {
                    let _ = two;
                    big(fl)
                }
            }
        });
    }
    if matches!(x, Const(K::Inf)) || x.is_neg_inf() {
        return Some(x.clone());
    }
    if !x.free_symbols().is_empty() {
        return None;
    }
    let v = rval(x)?;
    if !v.is_finite() || v.abs() > 9e15 {
        return None;
    }
    if (v - v.round()).abs() < 1e-9 && name != "round" {
        return None; // too close to an integer to decide numerically
    }
    if name == "round" && ((v - v.floor()) - 0.5).abs() < 1e-9 {
        return None;
    }
    Some(match name {
        "floor" => int(v.floor() as i64),
        "ceil" => int(v.ceil() as i64),
        "frac" => x.clone() - int(v.floor() as i64),
        _ => int(v.round() as i64),
    })
}

fn abs_eval(x: &Expr) -> Option<Expr> {
    match x {
        Num(r) => return Some(Num(r.abs())),
        Float(v) => return Some(float(v.0.abs())),
        Const(K::I) => return Some(one()),
        Const(K::Inf | K::CInf) => return Some(inf()),
        Const(_) => return Some(x.clone()),
        Func(n, _) if &**n == "abs" => return Some(x.clone()),
        Pow(b, ex) => {
            if matches!(&**b, Const(K::E)) && !ex.has_const(K::I) {
                return Some(x.clone());
            }
            if let Some(n) = ex.as_i64() {
                if n % 2 == 0 && !b.has_const(K::I) {
                    return Some(x.clone());
                }
                return Some(pow(func("abs", vec![(**b).clone()]), (**ex).clone()));
            }
            if ex.as_rat().is_some() && !b.has_const(K::I) && b.free_symbols().is_empty() {
                if let Some(v) = rval(b) {
                    if v >= 0.0 {
                        return Some(x.clone());
                    }
                }
            }
        }
        Mul(v) => {
            // |c * rest| = |c| |rest| for numeric and constant factors
            let (mut known, mut rest) = (vec![], vec![]);
            for f in v {
                if f.is_numeric() || matches!(f, Const(_)) {
                    known.push(abs_eval(f)?);
                } else if let Some(a) = abs_eval(f).filter(|a| !matches!(a, Func(n, _) if &**n == "abs")) {
                    known.push(a);
                } else {
                    rest.push(f.clone());
                }
            }
            if !known.is_empty() {
                let r = mul(rest);
                let r = if r.is_one() { one() } else { func("abs", vec![r]) };
                known.push(r);
                return Some(mul(known));
            }
        }
        _ => {}
    }
    if x.free_symbols().is_empty() {
        if x.has_const(K::I) {
            let (a, b) = re_im(x)?;
            return Some(sqrt(crate::simplify::expand(&(square(a) + square(b)))));
        }
        let v = rval(x)?;
        if v > 1e-12 {
            return Some(x.clone());
        }
        if v < -1e-12 {
            return Some(neg(x.clone()));
        }
        return None;
    }
    if is_negated(x) {
        return Some(func("abs", vec![neg(x.clone())]));
    }
    None
}

fn sign_eval(x: &Expr) -> Option<Expr> {
    match x {
        Num(r) => return Some(int(if r.is_positive() { 1 } else if r.is_negative() { -1 } else { 0 })),
        Float(v) => return Some(int(if v.0 > 0.0 { 1 } else if v.0 < 0.0 { -1 } else { 0 })),
        Const(K::Inf) => return Some(one()),
        _ => {}
    }
    if x.free_symbols().is_empty() && !x.has_const(K::I) {
        let v = rval(x)?;
        if v > 1e-12 {
            return Some(one());
        }
        if v < -1e-12 {
            return Some(int(-1));
        }
        return None;
    }
    if is_negated(x) {
        return Some(neg(func("sign", vec![neg(x.clone())])));
    }
    None
}

fn gamma_eval(x: &Expr) -> Option<Expr> {
    match x {
        Num(r) => {
            if r.is_integer() {
                let n = r.numer();
                if !n.is_positive() {
                    return Some(Const(K::CInf));
                }
                let n = n.to_u64()?;
                if n > 20_001 {
                    return None;
                }
                return Some(big(nt::factorial(n - 1)));
            }
            if *r.denom() == BigInt::from(2) {
                // Γ(1/2 + k)
                let k = (r - rat_i(1, 2)).to_integer().to_i64()?;
                if k.abs() > 200 {
                    return None;
                }
                let sqrt_pi = sqrt(pi());
                if k >= 0 {
                    let df = if k == 0 { BigInt::one() } else { nt::double_factorial((2 * k - 1) as u64) };
                    return Some(Num(Rat::new(df, BigInt::from(2).pow(k as u32))) * sqrt_pi);
                }
                let k = -k;
                let df = nt::double_factorial((2 * k - 1) as u64);
                return Some(Num(Rat::new(BigInt::from(-2).pow(k as u32), df)) * sqrt_pi);
            }
            None
        }
        Const(K::Inf) => Some(inf()),
        _ => None,
    }
}

fn zeta_eval(x: &Expr) -> Option<Expr> {
    let n = x.as_i64()?;
    if n == 1 {
        return Some(Const(K::CInf));
    }
    if n == 0 {
        return Some(rat(-1, 2));
    }
    if n < 0 {
        if n < -400 {
            return None;
        }
        let m = (-n) as usize;
        let b = nt::bernoulli(m + 1);
        return Some(Num(-b / Rat::from_integer(BigInt::from(m as i64 + 1))));
    }
    if n % 2 == 0 && n <= 200 {
        let k = (n / 2) as u32;
        let b = nt::bernoulli(n as usize);
        let sign = if k % 2 == 1 { Rat::one() } else { -Rat::one() };
        let coef = sign * b * Rat::from_integer(BigInt::from(2).pow(2 * k)) / Rat::from_integer(nt::factorial(n as u64) * 2);
        return Some(Num(coef) * pow(pi(), int(n)));
    }
    None
}

fn int_args(args: &[Expr]) -> Option<Vec<BigInt>> {
    args.iter().map(|a| a.as_int().cloned()).collect()
}

fn bool_fold(name: &str, args: &[Expr]) -> Option<Expr> {
    let t = Const(K::True);
    let f = Const(K::False);
    match name {
        "not" => {
            let a = args.first()?;
            if *a == t {
                return Some(f);
            }
            if *a == f {
                return Some(t);
            }
            if let Some(inner) = is_fn(a, "not") {
                return Some(inner.clone());
            }
            if let Rel(r, l, rr) = a {
                let nr = match r {
                    crate::expr::RelOp::Eq => crate::expr::RelOp::Ne,
                    crate::expr::RelOp::Ne => crate::expr::RelOp::Eq,
                    crate::expr::RelOp::Lt => crate::expr::RelOp::Ge,
                    crate::expr::RelOp::Le => crate::expr::RelOp::Gt,
                    crate::expr::RelOp::Gt => crate::expr::RelOp::Le,
                    crate::expr::RelOp::Ge => crate::expr::RelOp::Lt,
                };
                return Some(Rel(nr, l.clone(), rr.clone()));
            }
            None
        }
        "and" | "or" => {
            let (unit, absorb) = if name == "and" { (&t, &f) } else { (&f, &t) };
            let mut out: Vec<Expr> = Vec::new();
            let mut changed = false;
            for a in args {
                if a == absorb {
                    return Some(absorb.clone());
                }
                if a == unit {
                    changed = true;
                    continue;
                }
                match a {
                    Func(n, inner) if &**n == name => {
                        changed = true;
                        for x in inner {
                            if !out.contains(x) {
                                out.push(x.clone());
                            }
                        }
                    }
                    _ => {
                        if out.contains(a) {
                            changed = true;
                        } else {
                            out.push(a.clone());
                        }
                    }
                }
            }
            // x and not x
            for a in &out {
                let na = func("not", vec![a.clone()]);
                if out.contains(&na) {
                    return Some(absorb.clone());
                }
            }
            let mut sorted = out.clone();
            sorted.sort();
            if sorted != out {
                changed = true;
            }
            match sorted.len() {
                0 => Some(unit.clone()),
                1 => Some(sorted.pop().unwrap()),
                _ if changed => Some(Func(Arc::from(name), sorted)),
                _ => None,
            }
        }
        "xor" => {
            if args.len() == 2 {
                let (a, b) = (&args[0], &args[1]);
                if a == b {
                    return Some(f);
                }
                if *a == f {
                    return Some(b.clone());
                }
                if *b == f {
                    return Some(a.clone());
                }
                if *a == t {
                    return Some(func("not", vec![b.clone()]));
                }
                if *b == t {
                    return Some(func("not", vec![a.clone()]));
                }
            }
            None
        }
        "implies" => {
            if args.len() == 2 {
                return Some(func("or", vec![func("not", vec![args[0].clone()]), args[1].clone()]));
            }
            None
        }
        "iff" => {
            if args.len() == 2 {
                return Some(func("not", vec![func("xor", vec![args[0].clone(), args[1].clone()])]));
            }
            None
        }
        "nand" => Some(func("not", vec![func("and", args.to_vec())])),
        "nor" => Some(func("not", vec![func("or", args.to_vec())])),
        _ => None,
    }
}

fn evaluate(name: &str, args: &[Expr]) -> Option<Expr> {
    let n = args.len();
    // ---- rewrites to canonical primitives --------------------------------
    match (name, n) {
        ("exp", 1) => return Some(exp(args[0].clone())),
        ("sqrt", 1) => return Some(sqrt(args[0].clone())),
        ("cbrt", 1) => {
            let x = &args[0];
            if x.is_numeric() && x.is_negative_term() {
                return Some(neg(pow(neg(x.clone()), rat(1, 3))));
            }
            return Some(pow(x.clone(), rat(1, 3)));
        }
        ("surd" | "root", 2) => {
            let (x, k) = (&args[0], &args[1]);
            if let Some(kk) = k.as_i64() {
                if kk % 2 != 0 && x.is_numeric() && x.is_negative_term() {
                    return Some(neg(pow(neg(x.clone()), pow(k.clone(), int(-1)))));
                }
            }
            return Some(pow(x.clone(), pow(k.clone(), int(-1))));
        }
        ("log", 1) => return Some(ln(args[0].clone())),
        ("log", 2) => return Some(log_base(&args[0], &args[1])),
        ("log10" | "lg", 1) => return Some(log_base(&int(10), &args[0])),
        ("log2" | "lb", 1) => return Some(log_base(&int(2), &args[0])),
        ("sec", 1) => return Some(pow(func("cos", args.to_vec()), int(-1))),
        ("csc", 1) => return Some(pow(func("sin", args.to_vec()), int(-1))),
        ("cot", 1) => {
            if let Some(q) = pi_coeff(&args[0]) {
                if let Some(v) = tan_pi(&(rat_i(1, 2) - q)) {
                    return Some(v);
                }
            }
            return Some(pow(func("tan", args.to_vec()), int(-1)));
        }
        ("sech", 1) => return Some(pow(func("cosh", args.to_vec()), int(-1))),
        ("csch", 1) => return Some(pow(func("sinh", args.to_vec()), int(-1))),
        ("coth", 1) => return Some(pow(func("tanh", args.to_vec()), int(-1))),
        ("asec", 1) => return Some(func("acos", vec![pow(args[0].clone(), int(-1))])),
        ("acsc", 1) => return Some(func("asin", vec![pow(args[0].clone(), int(-1))])),
        ("acot", 1) => {
            if args[0].is_zero() {
                return Some(pi() / int(2));
            }
            return Some(func("atan", vec![pow(args[0].clone(), int(-1))]));
        }
        ("asech", 1) => return Some(func("acosh", vec![pow(args[0].clone(), int(-1))])),
        ("acsch", 1) => return Some(func("asinh", vec![pow(args[0].clone(), int(-1))])),
        ("acoth", 1) => return Some(func("atanh", vec![pow(args[0].clone(), int(-1))])),
        ("sgn", 1) => return Some(func("sign", args.to_vec())),
        ("erfc", 1) => return Some(one() - func("erf", args.to_vec())),
        ("sinc", 1) => {
            if args[0].is_zero() {
                return Some(one());
            }
            return Some(func("sin", args.to_vec()) / args[0].clone());
        }
        ("factorial2", 1) => {
            let k = args[0].as_i64()?;
            if (0..=20000).contains(&k) {
                return Some(big(nt::double_factorial(k as u64)));
            }
            return None;
        }
        ("phi", 1) => return Some(func("totient", args.to_vec())),
        ("min" | "max" | "gcd" | "lcm", 1) => {
            if let List(v) = &args[0] {
                return Some(func(name, v.clone()));
            }
        }
        _ => {}
    }

    // ---- floating point --------------------------------------------------
    if n > 0 && args.iter().any(|a| a.has_float()) && args.iter().all(|a| a.free_symbols().is_empty() && !a.is_list()) {
        if let Some(f) = Fun::from_name(name) {
            let vals: Option<Vec<Complex64>> = args.iter().map(cval).collect();
            if let Some(vals) = vals {
                let z = if vals.iter().all(|v| v.im == 0.0) {
                    let r: Vec<f64> = vals.iter().map(|v| v.re).collect();
                    let v = eval::real_fn(f, &r);
                    if v.is_nan() {
                        eval::complex_fn(f, &vals)
                    } else {
                        Complex64::new(v, 0.0)
                    }
                } else {
                    eval::complex_fn(f, &vals)
                };
                if !z.re.is_nan() && !z.im.is_nan() {
                    return Some(cfloat(z));
                }
            }
        }
    }

    let x = args.first();
    match (name, n) {
        ("sin" | "cos" | "tan", 1) => trig(name, x?),
        ("sinh" | "cosh" | "tanh", 1) => hyper(name, x?),
        ("asin" | "acos" | "atan" | "asinh" | "acosh" | "atanh", 1) => inverse_trig(name, x?),
        ("ln", 1) => ln_eval(x?),
        ("abs", 1) => abs_eval(x?),
        ("sign", 1) => sign_eval(x?),
        ("floor" | "ceil" | "round" | "frac", 1) => floor_like(name, x?),
        ("round", 2) => {
            // round(x, digits)
            let d = args[1].as_i64()?;
            let scale = pow(int(10), int(d));
            let r = func("round", vec![args[0].clone() * scale.clone()]);
            if r.has_fn("round") {
                None
            } else {
                Some(r / scale)
            }
        }
        ("re" | "im" | "conj" | "arg", 1) => {
            let x = x?;
            if name == "arg" {
                if x.free_symbols().is_empty() {
                    let (a, b) = re_im(x)?;
                    let (av, bv) = (rval(&a)?, rval(&b)?);
                    if bv.abs() < 1e-14 {
                        return if av > 0.0 {
                            Some(zero())
                        } else if av < 0.0 {
                            Some(pi())
                        } else {
                            None
                        };
                    }
                    if av.abs() < 1e-14 {
                        return Some(if bv > 0.0 { pi() / int(2) } else { neg(pi()) / int(2) });
                    }
                    let base = func("atan", vec![b / a]);
                    return Some(if av > 0.0 {
                        base
                    } else if bv > 0.0 {
                        base + pi()
                    } else {
                        base - pi()
                    });
                }
                return None;
            }
            let (a, b) = re_im(x)?;
            Some(match name {
                "re" => a,
                "im" => b,
                _ => a - i() * b,
            })
        }
        ("factorial", 1) => {
            let x = x?;
            match x {
                Num(r) if r.is_integer() => {
                    if r.is_negative() {
                        return Some(Const(K::CInf));
                    }
                    let k = r.numer().to_u64()?;
                    if k > 20_000 {
                        return None;
                    }
                    Some(big(nt::factorial(k)))
                }
                Num(_) => {
                    let g = func("gamma", vec![x.clone() + one()]);
                    if g.has_fn("gamma") {
                        None
                    } else {
                        Some(g)
                    }
                }
                Const(K::Inf) => Some(inf()),
                _ => None,
            }
        }
        ("gamma", 1) => gamma_eval(x?),
        ("binomial", 2) => {
            let (a, b) = (&args[0], &args[1]);
            if let (Some(nn), Some(k)) = (a.as_int(), b.as_i64()) {
                if k < 0 {
                    return Some(zero());
                }
                if nn.is_positive() && BigInt::from(k) > *nn {
                    return Some(zero());
                }
                let k = if nn.is_positive() { k.min((nn - k).to_i64().unwrap_or(k)) } else { k };
                if k > 200_000 {
                    return None;
                }
                return Some(big(nt::binomial(nn, k as u64)));
            }
            if let (Num(r), Some(k)) = (a, b.as_i64()) {
                if (0..=500).contains(&k) {
                    let mut acc = Rat::one();
                    for j in 0..k {
                        acc = acc * (r - Rat::from_integer(BigInt::from(j))) / Rat::from_integer(BigInt::from(j + 1));
                    }
                    return Some(Num(acc));
                }
            }
            if let Some(k) = b.as_i64() {
                if k == 0 {
                    return Some(one());
                }
                if k == 1 {
                    return Some(a.clone());
                }
            }
            None
        }
        ("permutations", 2) => {
            let (nn, k) = (args[0].as_int()?, args[1].as_i64()?);
            if k < 0 || k > 100_000 {
                return None;
            }
            let mut acc = BigInt::one();
            for j in 0..k {
                acc *= nn - j;
            }
            Some(big(acc))
        }
        ("mod", 2) => {
            let (a, b) = (args[0].as_rat()?, args[1].as_rat()?);
            if b.is_zero() {
                return Some(undef());
            }
            Some(Num(a - b * (a / b).floor()))
        }
        ("quotient", 2) => {
            let (a, b) = (args[0].as_rat()?, args[1].as_rat()?);
            if b.is_zero() {
                return Some(undef());
            }
            Some(Num((a / b).floor()))
        }
        ("powmod", 3) => {
            let v = int_args(args)?;
            if v[2].is_zero() {
                return Some(undef());
            }
            if v[1].is_negative() {
                let inv = nt::mod_inv(&v[0], &v[2])?;
                return Some(big(inv.modpow(&(-&v[1]), &v[2])));
            }
            Some(big(v[0].modpow(&v[1], &v[2]).mod_floor(&v[2])))
        }
        ("modinv", 2) => {
            let v = int_args(args)?;
            Some(nt::mod_inv(&v[0], &v[1]).map(big).unwrap_or_else(undef))
        }
        ("gcd", _) if n >= 1 => {
            let rs: Option<Vec<Rat>> = args.iter().map(|a| a.as_rat().cloned()).collect();
            let rs = rs?;
            // gcd of rationals: gcd(numerators) / lcm(denominators)
            let g = rs.iter().fold(BigInt::zero(), |g, r| g.gcd(r.numer()));
            let l = rs.iter().fold(BigInt::one(), |l, r| l.lcm(r.denom()));
            Some(Num(Rat::new(g, l)))
        }
        ("lcm", _) if n >= 1 => {
            let rs: Option<Vec<Rat>> = args.iter().map(|a| a.as_rat().cloned()).collect();
            let rs = rs?;
            let l = rs.iter().fold(BigInt::one(), |l, r| l.lcm(r.numer()));
            let g = rs.iter().fold(BigInt::zero(), |g, r| g.gcd(r.denom()));
            Some(Num(Rat::new(l, g)))
        }
        ("min" | "max", _) if n >= 1 => {
            if args.iter().any(|a| a.is_list() || !a.free_symbols().is_empty()) {
                return None;
            }
            let mut best: Option<(f64, &Expr)> = None;
            for a in args {
                let v = match a {
                    Const(K::Inf) => f64::INFINITY,
                    _ if a.is_neg_inf() => f64::NEG_INFINITY,
                    _ => rval(a)?,
                };
                let better = match best {
                    None => true,
                    Some((b, _)) => {
                        if name == "min" {
                            v < b
                        } else {
                            v > b
                        }
                    }
                };
                if better {
                    best = Some((v, a));
                }
            }
            best.map(|b| b.1.clone())
        }
        ("fibonacci", 1) => {
            let k = x?.as_i64()?;
            if k.abs() > 200_000 {
                return None;
            }
            let v = nt::fib(k.unsigned_abs());
            Some(big(if k < 0 && k % 2 == 0 { -v } else { v }))
        }
        ("lucas", 1) => {
            let k = x?.as_i64()?;
            if !(0..=200_000).contains(&k) {
                return None;
            }
            Some(big(nt::lucas(k as u64)))
        }
        ("prime", 1) => {
            let k = x?.as_i64()?;
            if k < 1 {
                return None;
            }
            nt::nth_prime(k as u64).map(|p| big(BigInt::from(p)))
        }
        ("primepi", 1) => {
            let v = rval(x?)?;
            if v < 2.0 {
                return Some(zero());
            }
            nt::prime_pi(v.floor() as u64).map(|c| big(BigInt::from(c)))
        }
        ("isprime", 1) => {
            let k = x?.as_int()?;
            Some(boolean(nt::is_prime(k)))
        }
        ("nextprime", 1) => {
            let v = x?.as_rat()?.floor().to_integer();
            Some(big(nt::next_prime(&v)))
        }
        ("prevprime", 1) => {
            let v = x?.as_rat()?.ceil().to_integer();
            nt::prev_prime(&v).map(big)
        }
        ("totient", 1) => {
            let k = x?.as_int()?;
            if !k.is_positive() {
                return None;
            }
            Some(big(nt::totient(k)))
        }
        ("sigma", 1) => {
            let k = x?.as_int()?;
            if !k.is_positive() {
                return None;
            }
            Some(big(nt::sigma(1, k)))
        }
        ("sigma", 2) => {
            let (k, m) = (args[0].as_i64()?, args[1].as_int()?);
            if !m.is_positive() || !(0..=64).contains(&k) {
                return None;
            }
            Some(big(nt::sigma(k as u32, m)))
        }
        ("divisors", 1) => {
            let k = x?.as_int()?;
            if !k.is_positive() {
                return None;
            }
            Some(List(nt::divisors(k).into_iter().map(big).collect()))
        }
        ("mobius", 1) => {
            let k = x?.as_int()?;
            if !k.is_positive() {
                return None;
            }
            Some(int(nt::mobius(k) as i64))
        }
        ("jacobi", 2) => {
            let v = int_args(args)?;
            if v[1].is_even() || !v[1].is_positive() {
                return None;
            }
            Some(int(nt::jacobi(&v[0], &v[1]) as i64))
        }
        ("bernoulli", 1) => {
            let k = x?.as_i64()?;
            if !(0..=600).contains(&k) {
                return None;
            }
            Some(Num(nt::bernoulli(k as usize)))
        }
        ("harmonic", 1) => {
            let k = x?.as_i64()?;
            if !(0..=3000).contains(&k) {
                return None;
            }
            let mut s = Rat::zero();
            for j in 1..=k {
                s += Rat::new(BigInt::one(), BigInt::from(j));
            }
            Some(Num(s))
        }
        ("partitions", 1) => {
            let k = x?.as_i64()?;
            if !(0..=20_000).contains(&k) {
                return None;
            }
            Some(big(nt::partitions(k as usize)))
        }
        ("catalan", 1) => {
            let k = x?.as_i64()?;
            if !(0..=5000).contains(&k) {
                return None;
            }
            Some(big(nt::binomial(&BigInt::from(2 * k), k as u64) / (k + 1)))
        }
        ("zeta", 1) => zeta_eval(x?),
        ("erf", 1) => {
            let x = x?;
            if x.is_zero() {
                return Some(zero());
            }
            if matches!(x, Const(K::Inf)) {
                return Some(one());
            }
            if x.is_neg_inf() {
                return Some(int(-1));
            }
            if is_negated(x) {
                return Some(neg(func("erf", vec![neg(x.clone())])));
            }
            None
        }
        ("erfi", 1) => {
            let x = x?;
            if x.is_zero() {
                return Some(zero());
            }
            if is_negated(x) {
                return Some(neg(func("erfi", vec![neg(x.clone())])));
            }
            None
        }
        ("heaviside", 1) => {
            let x = x?;
            if !x.free_symbols().is_empty() {
                return None;
            }
            let v = rval(x)?;
            Some(if v > 0.0 {
                one()
            } else if v < 0.0 {
                zero()
            } else {
                half()
            })
        }
        ("dirac", 1) => {
            let x = x?;
            if !x.free_symbols().is_empty() {
                return None;
            }
            let v = rval(x)?;
            if v != 0.0 {
                Some(zero())
            } else {
                None
            }
        }
        ("W", 1) => {
            let x = x?;
            if x.is_zero() {
                return Some(zero());
            }
            if matches!(x, Const(K::E)) {
                return Some(one());
            }
            if *x == neg(pow(e(), int(-1))) {
                return Some(int(-1));
            }
            // W(a e^a) = a for a >= -1
            if let Mul(v) = x {
                if v.len() == 2 {
                    if let Pow(b, ex) = &v[1] {
                        if matches!(**b, Const(K::E)) && **ex == v[0] {
                            if let Some(a) = rval(&v[0]) {
                                if a >= -1.0 {
                                    return Some(v[0].clone());
                                }
                            }
                        }
                    }
                }
            }
            None
        }
        ("Si", 1) => {
            let x = x?;
            if x.is_zero() {
                return Some(zero());
            }
            if matches!(x, Const(K::Inf)) {
                return Some(pi() / int(2));
            }
            if is_negated(x) {
                return Some(neg(func("Si", vec![neg(x.clone())])));
            }
            None
        }
        ("li", 1) => {
            if x?.is_zero() {
                Some(zero())
            } else {
                None
            }
        }
        ("besselj" | "besseli", 2) => {
            if args[1].is_zero() {
                if args[0].is_zero() {
                    return Some(one());
                }
                if let Some(v) = rval(&args[0]) {
                    if v > 0.0 {
                        return Some(zero());
                    }
                }
            }
            None
        }
        ("atan2", 2) => {
            // atan2(y, x)
            let (y, xx) = (&args[0], &args[1]);
            if !y.free_symbols().is_empty() || !xx.free_symbols().is_empty() {
                return None;
            }
            let r = func("arg", vec![xx.clone() + i() * y.clone()]);
            if r.has_fn("arg") {
                None
            } else {
                Some(r)
            }
        }
        ("not" | "and" | "or" | "xor" | "implies" | "iff" | "nand" | "nor", _) => bool_fold(name, args),
        ("ellipticK", 1) => {
            if x?.is_zero() {
                Some(pi() / int(2))
            } else {
                None
            }
        }
        ("ellipticE", 1) => {
            let x = x?;
            if x.is_zero() {
                Some(pi() / int(2))
            } else if x.is_one() {
                Some(one())
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Evaluate a relation between constants to `True`/`False` when decidable.
pub fn rel(op: RelOp, a: Expr, b: Expr) -> Expr {
    use std::cmp::Ordering::*;
    let ord = match (&a, &b) {
        (Num(x), Num(y)) => Some(x.cmp(y)),
        _ => {
            if a == b {
                Some(Equal)
            } else if a.free_symbols().is_empty() && b.free_symbols().is_empty() && !a.is_list() && !b.is_list() {
                let d = crate::simplify::simplify(&(a.clone() - b.clone()));
                if d.is_zero() {
                    Some(Equal)
                } else {
                    match rval(&d) {
                        Some(v) if v > 1e-12 => Some(Greater),
                        Some(v) if v < -1e-12 => Some(Less),
                        _ => None,
                    }
                }
            } else {
                None
            }
        }
    };
    match ord {
        Some(o) => boolean(match op {
            RelOp::Eq => o == Equal,
            RelOp::Ne => o != Equal,
            RelOp::Lt => o == Less,
            RelOp::Le => o != Greater,
            RelOp::Gt => o == Greater,
            RelOp::Ge => o != Less,
        }),
        None => Rel(op, Box::new(a), Box::new(b)),
    }
}
