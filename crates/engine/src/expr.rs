//! Symbolic expressions with automatic simplification.
//!
//! Expressions are immutable trees.  The constructor functions [`add`],
//! [`mul`], [`pow`] and [`func`] keep every tree in a canonical form
//! (flattened, numeric parts folded, like terms collected, sorted), so two
//! equal expressions built in different ways compare equal.

use crate::budget;
use num_bigint::BigInt;
use num_integer::Integer;
use num_rational::BigRational;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::sync::Arc;

pub type Name = Arc<str>;
pub type Int = BigInt;
pub type Rat = BigRational;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum K {
    Pi,
    E,
    I,
    Inf,
    CInf,
    Undef,
    EulerGamma,
    Golden,
    Catalan,
    True,
    False,
}

/// An `f64` with total ordering so that expressions can be hashed and sorted.
#[derive(Clone, Copy, Debug)]
pub struct F(pub f64);
impl PartialEq for F {
    fn eq(&self, o: &Self) -> bool {
        self.0.to_bits() == o.0.to_bits()
    }
}
impl Eq for F {}
impl std::hash::Hash for F {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.0.to_bits().hash(h)
    }
}
impl PartialOrd for F {
    fn partial_cmp(&self, o: &Self) -> Option<Ordering> {
        Some(self.cmp(o))
    }
}
impl Ord for F {
    fn cmp(&self, o: &Self) -> Ordering {
        self.0.total_cmp(&o.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum RelOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl RelOp {
    pub fn symbol(self) -> &'static str {
        match self {
            RelOp::Eq => "=",
            RelOp::Ne => "≠",
            RelOp::Lt => "<",
            RelOp::Le => "≤",
            RelOp::Gt => ">",
            RelOp::Ge => "≥",
        }
    }
    pub fn flip(self) -> RelOp {
        match self {
            RelOp::Lt => RelOp::Gt,
            RelOp::Le => RelOp::Ge,
            RelOp::Gt => RelOp::Lt,
            RelOp::Ge => RelOp::Le,
            r => r,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Expr {
    Num(Rat),
    Float(F),
    Sym(Name),
    Const(K),
    Add(Vec<Expr>),
    Mul(Vec<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Func(Name, Vec<Expr>),
    List(Vec<Expr>),
    Rel(RelOp, Box<Expr>, Box<Expr>),
}

use Expr::*;

// ---------------------------------------------------------------------------
// basic constructors
// ---------------------------------------------------------------------------

pub fn int(n: i64) -> Expr {
    Num(Rat::from_integer(BigInt::from(n)))
}
pub fn big(n: BigInt) -> Expr {
    Num(Rat::from_integer(n))
}
pub fn rat(p: i64, q: i64) -> Expr {
    Num(Rat::new(BigInt::from(p), BigInt::from(q)))
}
pub fn num(r: Rat) -> Expr {
    Num(r)
}
pub fn float(x: f64) -> Expr {
    if x.is_nan() {
        Const(K::Undef)
    } else if x.is_infinite() {
        if x > 0.0 {
            inf()
        } else {
            neg(inf())
        }
    } else {
        Float(F(x))
    }
}
pub fn sym(name: &str) -> Expr {
    Sym(Arc::from(name))
}
pub fn zero() -> Expr {
    int(0)
}
pub fn one() -> Expr {
    int(1)
}
pub fn pi() -> Expr {
    Const(K::Pi)
}
pub fn e() -> Expr {
    Const(K::E)
}
pub fn i() -> Expr {
    Const(K::I)
}
pub fn inf() -> Expr {
    Const(K::Inf)
}
pub fn undef() -> Expr {
    Const(K::Undef)
}
pub fn boolean(b: bool) -> Expr {
    Const(if b { K::True } else { K::False })
}
pub fn neg(a: Expr) -> Expr {
    mul(vec![int(-1), a])
}
pub fn sub(a: Expr, b: Expr) -> Expr {
    add(vec![a, neg(b)])
}
pub fn div(a: Expr, b: Expr) -> Expr {
    mul(vec![a, pow(b, int(-1))])
}
pub fn sqrt(a: Expr) -> Expr {
    pow(a, rat(1, 2))
}
pub fn exp(a: Expr) -> Expr {
    pow(e(), a)
}
pub fn ln(a: Expr) -> Expr {
    func("ln", vec![a])
}
pub fn f1(name: &str, a: Expr) -> Expr {
    func(name, vec![a])
}
pub fn eq(a: Expr, b: Expr) -> Expr {
    Rel(self::RelOp::Eq, Box::new(a), Box::new(b))
}
pub fn list(v: Vec<Expr>) -> Expr {
    List(v)
}
pub fn square(a: Expr) -> Expr {
    pow(a, int(2))
}

impl From<i64> for Expr {
    fn from(n: i64) -> Self {
        int(n)
    }
}
impl From<i32> for Expr {
    fn from(n: i32) -> Self {
        int(n as i64)
    }
}
impl std::ops::Add for Expr {
    type Output = Expr;
    fn add(self, o: Expr) -> Expr {
        add(vec![self, o])
    }
}
impl std::ops::Sub for Expr {
    type Output = Expr;
    fn sub(self, o: Expr) -> Expr {
        sub(self, o)
    }
}
impl std::ops::Mul for Expr {
    type Output = Expr;
    fn mul(self, o: Expr) -> Expr {
        mul(vec![self, o])
    }
}
impl std::ops::Div for Expr {
    type Output = Expr;
    fn div(self, o: Expr) -> Expr {
        div(self, o)
    }
}
impl std::ops::Neg for Expr {
    type Output = Expr;
    fn neg(self) -> Expr {
        neg(self)
    }
}

// ---------------------------------------------------------------------------
// queries
// ---------------------------------------------------------------------------

impl Expr {
    pub fn is_zero(&self) -> bool {
        match self {
            Num(r) => r.is_zero(),
            Float(f) => f.0 == 0.0,
            _ => false,
        }
    }
    pub fn is_one(&self) -> bool {
        matches!(self, Num(r) if r.is_one())
    }
    pub fn is_minus_one(&self) -> bool {
        matches!(self, Num(r) if *r == -Rat::one())
    }
    pub fn as_rat(&self) -> Option<&Rat> {
        match self {
            Num(r) => Some(r),
            _ => None,
        }
    }
    pub fn as_int(&self) -> Option<&Int> {
        match self {
            Num(r) if r.is_integer() => Some(r.numer()),
            _ => None,
        }
    }
    pub fn as_i64(&self) -> Option<i64> {
        self.as_int().and_then(|n| n.to_i64())
    }
    pub fn is_integer(&self) -> bool {
        self.as_int().is_some()
    }
    /// Exact rational or float.
    pub fn is_numeric(&self) -> bool {
        matches!(self, Num(_) | Float(_))
    }
    pub fn is_sym(&self, name: &str) -> bool {
        matches!(self, Sym(s) if &**s == name)
    }
    pub fn is_undef(&self) -> bool {
        matches!(self, Const(K::Undef))
    }
    pub fn is_inf(&self) -> bool {
        match self {
            Const(K::Inf) | Const(K::CInf) => true,
            Mul(v) => v.len() == 2 && v[0].is_minus_one() && matches!(v[1], Const(K::Inf)),
            _ => false,
        }
    }
    pub fn is_neg_inf(&self) -> bool {
        matches!(self, Mul(v) if v.len() == 2 && v[0].is_minus_one() && matches!(v[1], Const(K::Inf)))
    }
    pub fn is_list(&self) -> bool {
        matches!(self, List(_))
    }
    /// Sign of the leading numeric coefficient is negative (`-x`, `-2*y`, `-3`).
    pub fn is_negative_term(&self) -> bool {
        match self {
            Num(r) => r.is_negative(),
            Float(f) => f.0 < 0.0,
            Mul(v) => v.first().map_or(false, |c| c.is_numeric() && c.is_negative_term()),
            _ => false,
        }
    }

    pub fn has(&self, pred: &dyn Fn(&Expr) -> bool) -> bool {
        if pred(self) {
            return true;
        }
        match self {
            Add(v) | Mul(v) | List(v) | Func(_, v) => v.iter().any(|a| a.has(pred)),
            Pow(a, b) | Rel(_, a, b) => a.has(pred) || b.has(pred),
            _ => false,
        }
    }
    pub fn has_sym(&self, s: &Expr) -> bool {
        self.has(&|x| x == s)
    }
    pub fn has_fn(&self, name: &str) -> bool {
        self.has(&|x| matches!(x, Func(n, _) if &**n == name))
    }
    pub fn has_any_fn(&self, names: &[&str]) -> bool {
        self.has(&|x| matches!(x, Func(n, _) if names.contains(&&**n)))
    }
    pub fn has_float(&self) -> bool {
        self.has(&|x| matches!(x, Float(_)))
    }
    pub fn has_const(&self, c: K) -> bool {
        self.has(&|x| matches!(x, Const(k) if *k == c))
    }

    pub fn free_symbols(&self) -> BTreeSet<Name> {
        let mut out = BTreeSet::new();
        self.collect_syms(&mut out);
        out
    }
    fn collect_syms(&self, out: &mut BTreeSet<Name>) {
        match self {
            Sym(s) => {
                out.insert(s.clone());
            }
            Add(v) | Mul(v) | List(v) => v.iter().for_each(|a| a.collect_syms(out)),
            Func(name, v) => {
                // bound variables of integrals, sums, ... are not free
                let bound: Option<&Expr> = match (&**name, v.len()) {
                    ("Integral" | "Sum" | "Product" | "Limit", n) if n >= 2 => Some(&v[1]),
                    _ => None,
                };
                let mut inner = BTreeSet::new();
                v.iter().for_each(|a| a.collect_syms(&mut inner));
                if let Some(Sym(b)) = bound {
                    if v.len() >= 4 || &**name == "Limit" {
                        inner.remove(b);
                        for a in v.iter().skip(2) {
                            a.collect_syms(&mut inner);
                        }
                    }
                }
                out.extend(inner);
            }
            Pow(a, b) | Rel(_, a, b) => {
                a.collect_syms(out);
                b.collect_syms(out);
            }
            _ => {}
        }
    }

    /// Structural size, used to compare candidate simplifications.
    pub fn size(&self) -> usize {
        match self {
            Num(r) => {
                let d = if r.is_integer() { 0 } else { 1 };
                1 + d + (r.numer().bits() / 24) as usize + if r.is_negative() { 1 } else { 0 }
            }
            Add(v) | Mul(v) | List(v) => 1 + v.iter().map(|a| a.size()).sum::<usize>(),
            Func(_, v) => 2 + v.iter().map(|a| a.size()).sum::<usize>(),
            Pow(a, b) | Rel(_, a, b) => 1 + a.size() + b.size(),
            _ => 1,
        }
    }

    /// Apply `f` bottom-up to every sub-expression, rebuilding through the
    /// simplifying constructors.
    pub fn map(&self, f: &dyn Fn(Expr) -> Expr) -> Expr {
        let rebuilt = match self {
            Add(v) => add(v.iter().map(|a| a.map(f)).collect()),
            Mul(v) => mul(v.iter().map(|a| a.map(f)).collect()),
            Pow(a, b) => pow(a.map(f), b.map(f)),
            Func(n, v) => func(n, v.iter().map(|a| a.map(f)).collect()),
            List(v) => List(v.iter().map(|a| a.map(f)).collect()),
            Rel(r, a, b) => Rel(*r, Box::new(a.map(f)), Box::new(b.map(f))),
            other => other.clone(),
        };
        f(rebuilt)
    }

    /// Replace every occurrence of `from` by `to`.
    pub fn subs(&self, from: &Expr, to: &Expr) -> Expr {
        if self == from {
            return to.clone();
        }
        match self {
            Add(v) => add(v.iter().map(|a| a.subs(from, to)).collect()),
            Mul(v) => mul(v.iter().map(|a| a.subs(from, to)).collect()),
            Pow(a, b) => pow(a.subs(from, to), b.subs(from, to)),
            Func(n, v) => func(n, v.iter().map(|a| a.subs(from, to)).collect()),
            List(v) => List(v.iter().map(|a| a.subs(from, to)).collect()),
            Rel(r, a, b) => Rel(*r, Box::new(a.subs(from, to)), Box::new(b.subs(from, to))),
            other => other.clone(),
        }
    }

    pub fn subs_many(&self, pairs: &[(Expr, Expr)]) -> Expr {
        // simultaneous substitution through temporary symbols
        let mut cur = self.clone();
        let tmps: Vec<Expr> = (0..pairs.len()).map(|k| sym(&format!("\u{1}tmp{k}"))).collect();
        for ((from, _), t) in pairs.iter().zip(&tmps) {
            cur = cur.subs(from, t);
        }
        for ((_, to), t) in pairs.iter().zip(&tmps) {
            cur = cur.subs(t, to);
        }
        cur
    }

    /// Split into numeric coefficient and the remaining factor: `3*x*y -> (3, x*y)`.
    pub fn coeff_split(&self) -> (Expr, Expr) {
        match self {
            Num(_) | Float(_) => (self.clone(), one()),
            Mul(v) if v[0].is_numeric() => {
                let rest = if v.len() == 2 { v[1].clone() } else { Mul(v[1..].to_vec()) };
                (v[0].clone(), rest)
            }
            _ => (one(), self.clone()),
        }
    }

    /// `x^n -> (x, n)`, anything else `-> (self, 1)`.
    pub fn base_exp(&self) -> (Expr, Expr) {
        match self {
            Pow(b, e) => ((**b).clone(), (**e).clone()),
            _ => (self.clone(), one()),
        }
    }

    /// Numerator and denominator (splitting on negative exponents).
    pub fn numer_denom(&self) -> (Expr, Expr) {
        match self {
            Num(r) => (big(r.numer().clone()), big(r.denom().clone())),
            Pow(b, e) if e.is_negative_term() => (one(), pow((**b).clone(), neg((**e).clone()))),
            Mul(v) => {
                let (mut n, mut d) = (vec![], vec![]);
                for f in v {
                    let (a, b) = f.numer_denom();
                    n.push(a);
                    d.push(b);
                }
                (mul(n), mul(d))
            }
            _ => (self.clone(), one()),
        }
    }

    pub fn terms(&self) -> Vec<Expr> {
        match self {
            Add(v) => v.clone(),
            _ => vec![self.clone()],
        }
    }
    pub fn factors(&self) -> Vec<Expr> {
        match self {
            Mul(v) => v.clone(),
            _ => vec![self.clone()],
        }
    }
    pub fn to_f64_const(&self) -> Option<f64> {
        crate::eval::eval_f64(self, &[])
    }
}

pub fn rat_to_f64(r: &Rat) -> f64 {
    let (n, d) = (r.numer(), r.denom());
    let (nb, db) = (n.bits(), d.bits());
    if nb < 1000 && db < 1000 {
        return n.to_f64().unwrap_or(f64::NAN) / d.to_f64().unwrap_or(f64::NAN);
    }
    // scale to avoid overflow
    let shift = nb as i64 - db as i64;
    let (n2, d2) = if shift > 0 { (n.clone(), d.clone() << (shift as usize)) } else { (n.clone() << ((-shift) as usize), d.clone()) };
    let q = Rat::new(n2 * BigInt::from(1u64 << 60), d2).to_integer().to_f64().unwrap_or(f64::NAN) / (1u64 << 60) as f64;
    q * 2f64.powi(shift.clamp(-100000, 100000) as i32)
}

pub fn f64_to_rat(x: f64) -> Option<Rat> {
    Rat::from_float(x)
}

// ---------------------------------------------------------------------------
// numeric helpers
// ---------------------------------------------------------------------------

fn num_add(a: &Expr, b: &Expr) -> Expr {
    match (a, b) {
        (Num(x), Num(y)) => Num(x + y),
        (Num(x), Float(y)) | (Float(y), Num(x)) => float(rat_to_f64(x) + y.0),
        (Float(x), Float(y)) => float(x.0 + y.0),
        _ => unreachable!(),
    }
}
fn num_mul(a: &Expr, b: &Expr) -> Expr {
    match (a, b) {
        (Num(x), Num(y)) => Num(x * y),
        (Num(x), Float(y)) | (Float(y), Num(x)) => {
            if x.is_zero() {
                zero()
            } else {
                float(rat_to_f64(x) * y.0)
            }
        }
        (Float(x), Float(y)) => float(x.0 * y.0),
        _ => unreachable!(),
    }
}

/// Largest `s` with `s^d | n`, returned as `(s, n / s^d)`; trial division.
fn extract_power(n: &BigInt, d: u32) -> (BigInt, BigInt) {
    let mut s = BigInt::one();
    let mut t = n.clone();
    let exact = t.nth_root(d);
    if exact.pow(d) == t {
        return (exact, BigInt::one());
    }
    let mut p = 2u32;
    let limit = 20000u32;
    while p <= limit {
        let pb = BigInt::from(p);
        let pd = pb.pow(d);
        if pd > t {
            break;
        }
        while (&t % &pd).is_zero() {
            t /= &pd;
            s *= &pb;
        }
        // strip remaining lower powers to keep the search bound small
        p += if p == 2 { 1 } else { 2 };
    }
    let exact = t.nth_root(d);
    if exact.pow(d) == t && !t.is_one() {
        return (s * exact, BigInt::one());
    }
    (s, t)
}

/// Prime factorisation by trial division; `None` when a large cofactor remains.
fn small_factor(n: &BigInt) -> Option<Vec<(BigInt, u32)>> {
    let mut out = vec![];
    let mut t = n.clone();
    let mut p = 2u64;
    while p < 100_000 {
        let pb = BigInt::from(p);
        if &pb * &pb > t {
            break;
        }
        let mut k = 0;
        while (&t % &pb).is_zero() {
            t /= &pb;
            k += 1;
        }
        if k > 0 {
            out.push((pb, k));
        }
        p += if p == 2 { 1 } else { 2 };
    }
    if !t.is_one() {
        if t.bits() > 34 && p >= 100_000 {
            return None;
        }
        out.push((t, 1));
    }
    Some(out)
}

/// `r^(n/d)` for a rational `r`, exact where possible.
fn rat_pow(r: &Rat, q: &Rat) -> Expr {
    let n = q.numer();
    let d = q.denom();
    if d.is_one() {
        let Some(k) = n.to_i64() else { return Pow(Box::new(Num(r.clone())), Box::new(Num(q.clone()))) };
        if r.is_zero() {
            return if k > 0 { zero() } else { Const(K::CInf) };
        }
        if k.unsigned_abs() > 2_000_000 && !(r.numer().is_one() || *r.numer() == -BigInt::one()) || r.numer().bits() * k.unsigned_abs() > 40_000_000 {
            return Pow(Box::new(Num(r.clone())), Box::new(Num(q.clone())));
        }
        let p = num_traits::Pow::pow(r.clone(), k.unsigned_abs() as u32);
        return Num(if k < 0 { p.recip() } else { p });
    }
    if r.is_zero() {
        return if q.is_positive() { zero() } else { Const(K::CInf) };
    }
    let Some(dd) = d.to_u32() else { return Pow(Box::new(Num(r.clone())), Box::new(Num(q.clone()))) };
    if r.is_negative() {
        // (-a)^(n/d) = (-1)^(n/d) * a^(n/d)
        let unit = if dd == 2 {
            pow(Const(K::I), big(n.clone()))
        } else {
            let nn = n.mod_floor(&(d * 2));
            Pow(Box::new(int(-1)), Box::new(Num(Rat::new(nn, d.clone()))))
        };
        let mag = rat_pow(&-r.clone(), q);
        return mul(vec![unit, mag]);
    }
    if dd > 64 || n.bits() > 20 {
        return Pow(Box::new(Num(r.clone())), Box::new(Num(q.clone())));
    }
    // whole and fractional part of the exponent
    let whole = n.div_floor(d);
    let m = n.mod_floor(d).to_u32().unwrap();
    let mut coef = match whole.to_i64() {
        Some(w) => match rat_pow(r, &Rat::from_integer(BigInt::from(w))) {
            Num(c) => c,
            other => return mul(vec![other, rat_pow(r, &Rat::new(BigInt::from(m), d.clone()))]),
        },
        None => return Pow(Box::new(Num(r.clone())), Box::new(Num(q.clone()))),
    };
    // r^(m/d) = (a^m b^(d-m))^(1/d) / b
    let (a, b) = (r.numer(), r.denom());
    let radicand = a.pow(m) * b.pow(dd - m);
    coef /= Rat::from_integer(b.clone());
    let (s, t) = extract_power(&radicand, dd);
    coef *= Rat::from_integer(s);
    if t.is_one() {
        return Num(coef);
    }
    // canonical radical: product of prime powers with reduced exponents
    let mut parts = vec![Num(coef)];
    match small_factor(&t) {
        Some(fs) => {
            for (p, k) in fs {
                parts.push(Pow(Box::new(big(p)), Box::new(Num(Rat::new(BigInt::from(k), BigInt::from(dd))))));
            }
        }
        None => parts.push(Pow(Box::new(big(t)), Box::new(Num(Rat::new(BigInt::one(), BigInt::from(dd)))))),
    }
    if parts[0].is_one() {
        parts.remove(0);
    }
    parts.sort();
    if parts.len() == 1 {
        parts.pop().unwrap()
    } else {
        Mul(parts)
    }
}

// ---------------------------------------------------------------------------
// simplifying constructors
// ---------------------------------------------------------------------------

pub fn add(terms: Vec<Expr>) -> Expr {
    budget::tick();
    let mut konst = zero();
    let mut map: Vec<(Expr, Expr)> = Vec::new(); // (rest, coefficient)
    let mut index: Option<std::collections::HashMap<Expr, usize>> = None;
    let (mut pinf, mut ninf, mut cinf) = (false, false, false);
    let mut stack: Vec<Expr> = terms.into_iter().rev().collect();
    while let Some(t) = stack.pop() {
        match t {
            Add(v) => stack.extend(v.into_iter().rev()),
            Num(_) | Float(_) => konst = num_add(&konst, &t),
            Const(K::Undef) => return undef(),
            Const(K::Inf) => pinf = true,
            Const(K::CInf) => cinf = true,
            List(a) => {
                // element-wise addition of lists of equal shape with everything else
                let rest = add(stack.drain(..).rev().collect::<Vec<_>>().into_iter().chain(map.drain(..).map(|(r, c)| mul_coef(c, r))).chain(std::iter::once(konst.clone())).collect());
                return match rest {
                    List(b) if b.len() == a.len() => List(a.into_iter().zip(b).map(|(x, y)| add(vec![x, y])).collect()),
                    List(_) => undef(),
                    other => List(a.into_iter().map(|x| add(vec![x, other.clone()])).collect()),
                };
            }
            other => {
                if other.is_neg_inf() {
                    ninf = true;
                    continue;
                }
                let (c, rest) = other.coeff_split();
                if map.len() >= 12 {
                    // large sums: hash index instead of a linear scan
                    let idx = index.get_or_insert_with(|| map.iter().enumerate().map(|(k, (r, _))| (r.clone(), k)).collect());
                    match idx.get(&rest) {
                        Some(&k) => map[k].1 = num_add(&map[k].1, &c),
                        None => {
                            idx.insert(rest.clone(), map.len());
                            map.push((rest, c));
                        }
                    }
                } else if let Some(slot) = map.iter_mut().find(|(r, _)| *r == rest) {
                    slot.1 = num_add(&slot.1, &c);
                } else {
                    map.push((rest, c));
                }
            }
        }
    }
    if cinf || (pinf && ninf) {
        return if cinf && !pinf && !ninf { Const(K::CInf) } else { undef() };
    }
    if pinf {
        return inf();
    }
    if ninf {
        return Mul(vec![int(-1), inf()]);
    }
    let mut out: Vec<Expr> = Vec::with_capacity(map.len() + 1);
    for (rest, c) in map {
        if c.is_zero() && !matches!(c, Float(_)) {
            continue;
        }
        if c.is_zero() {
            continue;
        }
        out.push(mul_coef(c, rest));
    }
    if !konst.is_zero() || (out.is_empty()) {
        if !(konst.is_zero() && !out.is_empty()) {
            out.push(konst);
        }
    }
    out.sort();
    match out.len() {
        0 => zero(),
        1 => out.pop().unwrap(),
        _ => Add(out),
    }
}

/// `c * rest` where `c` is numeric and `rest` has no numeric coefficient.
fn mul_coef(c: Expr, rest: Expr) -> Expr {
    if c.is_one() {
        return rest;
    }
    match rest {
        Mul(mut v) => {
            v.insert(0, c);
            Mul(v)
        }
        Add(v) => Add(v.into_iter().map(|t| mul(vec![c.clone(), t])).collect::<Vec<_>>()).resort(),
        other => Mul(vec![c, other]),
    }
}

impl Expr {
    fn resort(self) -> Expr {
        match self {
            Add(v) => add(v),
            o => o,
        }
    }
}

pub fn mul(factors: Vec<Expr>) -> Expr {
    budget::tick();
    let mut coef = one();
    let mut powers: Vec<(Expr, Vec<Expr>)> = Vec::new();
    let mut infs = 0usize;
    let mut cinf = false;
    let mut stack: Vec<Expr> = factors.into_iter().rev().collect();
    while let Some(f) = stack.pop() {
        match f {
            Mul(v) => stack.extend(v.into_iter().rev()),
            Num(_) | Float(_) => coef = num_mul(&coef, &f),
            Const(K::Undef) => return undef(),
            Const(K::Inf) => infs += 1,
            Const(K::CInf) => cinf = true,
            List(a) => {
                let rest = mul(stack.drain(..).rev().collect::<Vec<_>>().into_iter().chain(powers.drain(..).map(|(b, es)| pow(b, add(es)))).chain(std::iter::once(coef.clone())).collect());
                return match rest {
                    List(b) if b.len() == a.len() => List(a.into_iter().zip(b).map(|(x, y)| mul(vec![x, y])).collect()),
                    List(_) => undef(),
                    other => List(a.into_iter().map(|x| mul(vec![x, other.clone()])).collect()),
                };
            }
            other => {
                let (b, ex) = other.base_exp();
                if let Some(slot) = powers.iter_mut().find(|(pb, _)| *pb == b) {
                    slot.1.push(ex);
                } else {
                    powers.push((b, vec![ex]));
                }
            }
        }
    }
    if cinf {
        return if coef.is_zero() { undef() } else { Const(K::CInf) };
    }
    let mut out: Vec<Expr> = Vec::with_capacity(powers.len() + 1);
    let mut refold = false;
    for (b, es) in powers {
        let merged = es.len() > 1;
        let p = if merged { pow(b, add(es)) } else { pow_raw(b, es.into_iter().next().unwrap()) };
        match p {
            Num(_) | Float(_) => coef = num_mul(&coef, &p),
            Mul(_) => {
                refold = true;
                out.push(p)
            }
            Const(K::Undef) => return undef(),
            Const(K::Inf) => infs += 1,
            other => {
                if merged && !matches!(other, Pow(..)) {
                    refold = true;
                }
                out.push(other)
            }
        }
    }
    if refold {
        out.push(coef);
        for _ in 0..infs {
            out.push(inf());
        }
        return mul(out);
    }
    if infs > 0 {
        if coef.is_zero() {
            return undef();
        }
        if out.is_empty() {
            return if coef.is_negative_term() { Mul(vec![int(-1), inf()]) } else { inf() };
        }
        out.push(inf());
        coef = if coef.is_negative_term() { int(-1) } else { one() };
    }
    if coef.is_zero() {
        return coef;
    }
    if out.is_empty() {
        return coef;
    }
    out.sort();
    if out.len() == 1 {
        let single = out.pop().unwrap();
        return mul_coef(coef, single);
    }
    if !coef.is_one() {
        out.insert(0, coef);
    }
    Mul(out)
}

/// Rebuild a power that is already known to be simplified.
fn pow_raw(b: Expr, ex: Expr) -> Expr {
    if ex.is_one() {
        b
    } else {
        Pow(Box::new(b), Box::new(ex))
    }
}

pub fn pow(b: Expr, ex: Expr) -> Expr {
    budget::tick();
    if b.is_undef() || ex.is_undef() {
        return undef();
    }
    if let Num(q) = &ex {
        if q.is_zero() {
            return one();
        }
        if q.is_one() {
            return b;
        }
    }
    if b.is_one() {
        return one();
    }
    match (&b, &ex) {
        (Num(r), Num(q)) => return rat_pow(r, q),
        (Float(_), Num(_)) | (Float(_), Float(_)) | (Num(_), Float(_)) => {
            let bx = match &b {
                Float(x) => x.0,
                Num(r) => rat_to_f64(r),
                _ => unreachable!(),
            };
            let exx = match &ex {
                Float(y) => y.0,
                Num(q) => rat_to_f64(q),
                _ => unreachable!(),
            };
            if bx < 0.0 && exx.fract() != 0.0 {
                let z = num_complex::Complex64::new(bx, 0.0).powf(exx);
                return add(vec![float(z.re), mul(vec![float(z.im), i()])]);
            }
            return float(bx.powf(exx));
        }
        (Num(r), _) if r.is_zero() => {
            if let Some(v) = ex.to_f64_const() {
                return if v > 0.0 { zero() } else { Const(K::CInf) };
            }
        }
        (Const(K::I), Num(q)) if q.is_integer() => {
            let k = q.numer().mod_floor(&BigInt::from(4)).to_i64().unwrap();
            return match k {
                0 => one(),
                1 => i(),
                2 => int(-1),
                _ => neg(i()),
            };
        }
        (Const(K::Inf), Num(q)) => return if q.is_positive() { inf() } else { zero() },
        (Const(K::E), _) => {
            if let Some(v) = exp_special(&ex) {
                return v;
            }
        }
        (Pow(b2, e2), Num(q)) => {
            // (x^a)^n = x^(a n) for integer n; also for positive numeric bases
            let positive_base = matches!(&**b2, Num(r) if r.is_positive()) || matches!(&**b2, Const(K::E | K::Pi));
            if q.is_integer() || positive_base {
                return pow((**b2).clone(), mul(vec![(**e2).clone(), ex.clone()]));
            }
        }
        (Pow(b2, e2), _) if matches!(&**b2, Const(K::E)) => {
            // (e^a)^b = e^(a b) when a is real-valued symbolic: accept for numeric b
            if ex.is_numeric() {
                return pow((**b2).clone(), mul(vec![(**e2).clone(), ex.clone()]));
            }
        }
        (Mul(fs), Num(q)) => {
            if q.is_integer() {
                return mul(fs.iter().map(|f| pow(f.clone(), ex.clone())).collect());
            }
            // pull a positive numeric coefficient out of a root
            if let Num(c) = &fs[0] {
                let rest = if fs.len() == 2 { fs[1].clone() } else { Mul(fs[1..].to_vec()) };
                if c.is_positive() {
                    return mul(vec![rat_pow(c, q), pow(rest, ex.clone())]);
                }
                if *c != -Rat::one() {
                    return mul(vec![rat_pow(&-c.clone(), q), pow(neg(rest), ex.clone())]);
                }
            }
        }
        (List(v), _) if !ex.is_list() => return List(v.iter().map(|a| pow(a.clone(), ex.clone())).collect()),
        _ => {}
    }
    if b.is_numeric() && ex.is_numeric() {
        unreachable!()
    }
    Pow(Box::new(b), Box::new(ex))
}

/// Special values of `e^x`.
fn exp_special(x: &Expr) -> Option<Expr> {
    match x {
        Float(v) => Some(float(v.0.exp())),
        Func(n, a) if &**n == "ln" => Some(a[0].clone()),
        Const(K::Inf) => Some(inf()),
        Mul(v) => {
            if x.is_neg_inf() {
                return Some(zero());
            }
            // e^(c ln a) = a^c
            if v.len() == 2 {
                if let (Num(_), Func(n, a)) = (&v[0], &v[1]) {
                    if &**n == "ln" {
                        return Some(pow(a[0].clone(), v[0].clone()));
                    }
                }
            }
            // e^(i pi q): Euler's formula at rational multiples of pi
            let has_i = v.iter().any(|f| matches!(f, Const(K::I)));
            let has_pi = v.iter().any(|f| matches!(f, Const(K::Pi)));
            if has_i && has_pi && v.len() <= 3 {
                let q = if v.len() == 2 { Rat::one() } else { v[0].as_rat()?.clone() };
                let ang = mul(vec![Num(q), pi()]);
                let c = func("cos", vec![ang.clone()]);
                let s = func("sin", vec![ang]);
                if !c.has_fn("cos") && !s.has_fn("sin") {
                    return Some(add(vec![c, mul(vec![i(), s])]));
                }
            }
            None
        }
        _ => None,
    }
}

pub fn func(name: &str, args: Vec<Expr>) -> Expr {
    budget::tick();
    crate::funcs::apply(name, args)
}

/// Build a function node without evaluation.
pub fn raw_fn(name: &str, args: Vec<Expr>) -> Expr {
    Func(Arc::from(name), args)
}

// ---------------------------------------------------------------------------
// matrices are lists of equally long lists
// ---------------------------------------------------------------------------

impl Expr {
    pub fn matrix_shape(&self) -> Option<(usize, usize)> {
        if let List(rows) = self {
            if rows.is_empty() {
                return None;
            }
            let mut cols = None;
            for r in rows {
                match r {
                    List(c) if !c.is_empty() && c.iter().all(|x| !x.is_list()) => {
                        if cols.map_or(false, |k| k != c.len()) {
                            return None;
                        }
                        cols = Some(c.len());
                    }
                    _ => return None,
                }
            }
            return cols.map(|c| (rows.len(), c));
        }
        None
    }
    pub fn is_matrix(&self) -> bool {
        self.matrix_shape().is_some()
    }
    pub fn as_list(&self) -> Option<&Vec<Expr>> {
        match self {
            List(v) => Some(v),
            _ => None,
        }
    }
}
