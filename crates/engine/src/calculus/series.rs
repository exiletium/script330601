//! Truncated generalized power series (Laurent / Puiseux with logarithmic
//! coefficients) about h -> 0+.  Used for limits and for series expansions.

use super::diff::diff;
use crate::budget;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::simplify;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};

/// Internal symbol standing for `ln(h)` inside coefficients.
pub const LOGH: &str = "\u{2113}h";

const EXACT: i64 = i64::MAX / 4;

/// `sum_k c[k] h^((val + k)/q) + O(h^(order/q))`.
#[derive(Clone, Debug)]
pub struct Ser {
    pub q: i64,
    pub val: i64,
    pub c: Vec<Expr>,
    pub order: i64,
}

fn sat(a: i64, b: i64) -> i64 {
    if a >= EXACT || b >= EXACT {
        EXACT
    } else {
        a + b
    }
}

pub fn logh() -> Expr {
    sym(LOGH)
}

fn coeff_is_zero(c: &Expr) -> bool {
    if c.is_zero() {
        return true;
    }
    if c.is_numeric() || c.size() > 400 {
        return false;
    }
    matches!(simplify::is_zero(c), Some(true))
}

impl Ser {
    pub fn constant(c: Expr) -> Ser {
        if c.is_zero() {
            return Ser { q: 1, val: 0, c: vec![], order: EXACT };
        }
        Ser { q: 1, val: 0, c: vec![c], order: EXACT }
    }
    pub fn var() -> Ser {
        Ser { q: 1, val: 1, c: vec![one()], order: EXACT }
    }
    pub fn is_exact(&self) -> bool {
        self.order >= EXACT
    }
    /// Identically zero (to all orders).
    pub fn is_exact_zero(&self) -> bool {
        self.is_exact() && self.c.iter().all(|c| c.is_zero())
    }

    fn rescale(&self, q2: i64) -> Ser {
        if q2 == self.q {
            return self.clone();
        }
        let m = q2 / self.q;
        let mut c = Vec::with_capacity(self.c.len() * m as usize);
        for (k, x) in self.c.iter().enumerate() {
            if k > 0 {
                for _ in 1..m {
                    c.push(zero());
                }
            }
            c.push(x.clone());
        }
        Ser { q: q2, val: self.val * m, c, order: if self.is_exact() { EXACT } else { self.order * m } }
    }

    /// Drop leading zero coefficients (cheap test only).
    fn trim(mut self) -> Ser {
        let k = self.c.iter().take_while(|c| c.is_zero()).count();
        if k > 0 {
            self.c.drain(..k);
            self.val += k as i64;
        }
        while self.c.last().map_or(false, |c| c.is_zero()) && self.is_exact() {
            self.c.pop();
        }
        if self.c.is_empty() && !self.is_exact() {
            self.val = self.order;
        }
        self
    }

    /// Drop leading coefficients that are zero after simplification.
    pub fn lead_trim(mut self) -> Ser {
        self = self.trim();
        while let Some(c0) = self.c.first() {
            if coeff_is_zero(c0) {
                self.c.remove(0);
                self.val += 1;
                self = self.trim();
            } else {
                break;
            }
        }
        self
    }

    pub fn add(&self, o: &Ser) -> Ser {
        let q = self.q.lcm(&o.q);
        let (a, b) = (self.rescale(q), o.rescale(q));
        let val = if a.c.is_empty() && a.is_exact() {
            b.val
        } else if b.c.is_empty() && b.is_exact() {
            a.val
        } else {
            a.val.min(b.val)
        };
        let order = a.order.min(b.order);
        let end = (a.val + a.c.len() as i64).max(b.val + b.c.len() as i64).min(order);
        let mut c = Vec::new();
        let mut e = val;
        while e < end {
            let x = if e >= a.val && e < a.val + a.c.len() as i64 { a.c[(e - a.val) as usize].clone() } else { zero() };
            let y = if e >= b.val && e < b.val + b.c.len() as i64 { b.c[(e - b.val) as usize].clone() } else { zero() };
            c.push(crate::expr::add(vec![x, y]));
            e += 1;
        }
        Ser { q, val, c, order }.trim()
    }

    pub fn neg(&self) -> Ser {
        Ser { q: self.q, val: self.val, c: self.c.iter().map(|c| crate::expr::neg(c.clone())).collect(), order: self.order }
    }

    pub fn scale(&self, k: &Expr) -> Ser {
        if k.is_zero() {
            return Ser::constant(zero());
        }
        Ser { q: self.q, val: self.val, c: self.c.iter().map(|c| crate::expr::mul(vec![k.clone(), c.clone()])).collect(), order: self.order }
    }

    pub fn mul(&self, o: &Ser, t: i64) -> Ser {
        budget::tick();
        if self.is_exact_zero() || o.is_exact_zero() {
            return Ser::constant(zero());
        }
        let q = self.q.lcm(&o.q);
        let (a, b) = (self.rescale(q), o.rescale(q));
        let val = a.val + b.val;
        let mut order = sat(a.order, b.val).min(sat(b.order, a.val));
        let natural = (a.c.len() + b.c.len()).saturating_sub(1) as i64;
        let cap = t * q;
        let mut len = natural;
        if a.c.is_empty() || b.c.is_empty() {
            len = 0;
        }
        if !order.eq(&EXACT) {
            len = len.min((order - val).max(0));
        }
        if len > cap {
            len = cap;
            order = order.min(val + cap);
        }
        let mut c = vec![Vec::new(); len as usize];
        for (i, x) in a.c.iter().enumerate() {
            if x.is_zero() {
                continue;
            }
            for (j, y) in b.c.iter().enumerate() {
                if i + j >= len as usize {
                    break;
                }
                if y.is_zero() {
                    continue;
                }
                c[i + j].push(crate::expr::mul(vec![x.clone(), y.clone()]));
            }
        }
        Ser { q, val, c: c.into_iter().map(crate::expr::add).collect(), order }.trim()
    }

    pub fn inv(&self, t: i64) -> Option<Ser> {
        let a = self.clone().lead_trim();
        let c0 = a.c.first()?.clone();
        let c0inv = pow(c0, int(-1));
        if a.is_exact() && a.c.len() == 1 {
            return Some(Ser { q: a.q, val: -a.val, c: vec![c0inv], order: EXACT });
        }
        let r = if a.is_exact() { t * a.q } else { (a.order - a.val).min(t * a.q) };
        let r = r.max(1) as usize;
        let u: Vec<Expr> = a.c.iter().map(|c| crate::expr::mul(vec![c.clone(), c0inv.clone()])).collect();
        let mut b: Vec<Expr> = vec![one()];
        for k in 1..r {
            let mut s = Vec::new();
            for j in 1..=k {
                if j < u.len() && !u[j].is_zero() && !b[k - j].is_zero() {
                    s.push(crate::expr::mul(vec![u[j].clone(), b[k - j].clone()]));
                }
            }
            b.push(crate::expr::neg(crate::expr::add(s)));
        }
        let c = b.into_iter().map(|x| crate::expr::mul(vec![x, c0inv.clone()])).collect();
        Some(Ser { q: a.q, val: -a.val, c, order: -a.val + r as i64 }.trim())
    }

    /// Split into the leading monomial data and `u` with `self = c0 h^(val/q) (1 + u)`.
    fn split_lead(&self) -> Option<(Expr, i64, Ser)> {
        let a = self.clone().lead_trim();
        let c0 = a.c.first()?.clone();
        let c0inv = pow(c0.clone(), int(-1));
        let mut c: Vec<Expr> = a.c.iter().skip(1).map(|c| crate::expr::mul(vec![c.clone(), c0inv.clone()])).collect();
        if c.is_empty() && a.is_exact() {
            c = vec![];
        }
        let u = Ser { q: a.q, val: 1, c, order: if a.is_exact() { EXACT } else { a.order - a.val } }.trim();
        Some((c0, a.val, u))
    }

    pub fn powi(&self, n: i64, t: i64) -> Option<Ser> {
        if n < 0 {
            return self.inv(t)?.powi(-n, t);
        }
        let mut result = Ser::constant(one());
        let mut base = self.clone();
        let mut k = n;
        while k > 0 {
            if k & 1 == 1 {
                result = result.mul(&base, t);
            }
            k >>= 1;
            if k > 0 {
                base = base.mul(&base, t);
            }
        }
        Some(result)
    }

    pub fn powr(&self, p: &Expr, t: i64) -> Option<Ser> {
        if let Some(n) = p.as_i64() {
            if n.abs() <= 64 {
                return self.powi(n, t);
            }
        }
        let (c0, v, u) = self.split_lead()?;
        let q0 = self.clone().lead_trim().q;
        // (1 + u)^p
        let p2 = p.clone();
        let binom = compose_taylor(
            &u,
            &mut |k| {
                let mut fs = vec![];
                for j in 0..k {
                    fs.push(div(sub(p2.clone(), int(j as i64)), int(j as i64 + 1)));
                }
                crate::expr::mul(fs)
            },
            t,
        )?;
        let lead_c = pow(c0, p.clone());
        let (new_q, new_val) = match p.as_rat() {
            Some(r) => {
                let (pa, pb) = (r.numer().to_i64()?, r.denom().to_i64()?);
                if v == 0 {
                    (1, 0)
                } else {
                    // exponent v/q0 * pa/pb
                    let num = v.checked_mul(pa)?;
                    let den = q0.checked_mul(pb)?;
                    let g = num.gcd(&den).max(1);
                    (den / g, num / g)
                }
            }
            None => {
                if v != 0 {
                    return None;
                }
                (1, 0)
            }
        };
        let mono = Ser { q: new_q, val: new_val, c: vec![lead_c], order: EXACT };
        Some(mono.mul(&binom, t))
    }

    /// Coefficient of h^0 and the remainder with positive valuation; `None`
    /// when the series has a pole.
    fn split_const(&self) -> Option<(Expr, Ser)> {
        let a = self.clone().trim();
        if a.val > 0 || a.c.is_empty() {
            return Some((zero(), a));
        }
        let a = if a.val < 0 { a.lead_trim() } else { a };
        if a.val < 0 {
            return None;
        }
        if a.val > 0 || a.c.is_empty() {
            return Some((zero(), a));
        }
        let c0 = a.c[0].clone();
        let rest = Ser { q: a.q, val: 1, c: a.c[1..].to_vec(), order: a.order }.trim();
        Some((c0, rest))
    }

    pub fn exp(&self, t: i64) -> Option<Ser> {
        match self.split_const() {
            Some((a0, u)) => {
                let e = compose_taylor(&u, &mut |k| Num(Rat::new(Int::one(), crate::numtheory::factorial(k as u64))), t)?;
                // a0 = alpha * ln(h) + beta  ->  h^alpha e^beta
                let l = logh();
                if a0.has_sym(&l) {
                    let cs = crate::poly::coeffs(&a0, &l)?;
                    if cs.len() != 2 {
                        return None;
                    }
                    let alpha = cs[1].as_rat()?.clone();
                    let (pa, pb) = (alpha.numer().to_i64()?, alpha.denom().to_i64()?);
                    let mono = Ser { q: pb, val: pa, c: vec![crate::expr::exp(cs[0].clone())], order: EXACT };
                    return Some(mono.mul(&e, t));
                }
                Some(e.scale(&crate::expr::exp(a0)))
            }
            None => {
                // pole in the exponent: e^(-c/h^k) is flat, e^(+c/h^k) blows up
                let a = self.clone().lead_trim();
                let c0 = a.c.first()?;
                let s = sign_at_zero(c0)?;
                if s < 0 {
                    Some(Ser::constant(zero()))
                } else {
                    None
                }
            }
        }
    }

    pub fn ln(&self, t: i64) -> Option<Ser> {
        let (c0, v, u) = self.split_lead()?;
        let q0 = self.clone().lead_trim().q;
        let l1 = compose_taylor(&u, &mut |k| if k == 0 { zero() } else { rat(if k % 2 == 1 { 1 } else { -1 }, k as i64) }, t)?;
        let mut konst = vec![crate::expr::ln(c0)];
        if v != 0 {
            konst.push(crate::expr::mul(vec![Num(Rat::new(Int::from(v), Int::from(q0))), logh()]));
        }
        Some(Ser::constant(crate::expr::add(konst)).add(&l1))
    }

    /// Leading term `(coefficient, exponent)`; `None` when every known
    /// coefficient vanishes.
    pub fn leading(&self) -> Option<(Expr, Rat)> {
        let a = self.clone().lead_trim();
        let c0 = a.c.first()?.clone();
        Some((c0, Rat::new(Int::from(a.val), Int::from(a.q))))
    }

    /// Exponent of the error term.
    pub fn order_exp(&self) -> Option<Rat> {
        if self.is_exact() {
            None
        } else {
            Some(Rat::new(Int::from(self.order), Int::from(self.q)))
        }
    }

    /// Terms as `(coefficient, exponent)` pairs.
    pub fn terms(&self) -> Vec<(Expr, Rat)> {
        self.c.iter().enumerate().filter(|(_, c)| !c.is_zero()).map(|(k, c)| (c.clone(), Rat::new(Int::from(self.val + k as i64), Int::from(self.q)))).collect()
    }
}

/// `sum_k coef(k) u^k` for a series `u` of positive valuation.
fn compose_taylor(u: &Ser, coef: &mut dyn FnMut(usize) -> Expr, t: i64) -> Option<Ser> {
    let c0 = coef(0);
    if u.is_exact_zero() {
        return Some(Ser::constant(c0));
    }
    if u.val <= 0 {
        return None;
    }
    let r = if u.is_exact() { t * u.q } else { u.order.min(t * u.q) };
    let mut result = Ser::constant(c0);
    result.q = u.q;
    let mut power = Ser::constant(one());
    let mut k = 1usize;
    loop {
        if (k as i64) * u.val >= r || k > 200 {
            break;
        }
        power = power.mul(u, t);
        if power.c.is_empty() {
            result = result.add(&power);
            break;
        }
        let ck = coef(k);
        if ck.is_undef() || ck.is_inf() {
            return None;
        }
        result = result.add(&power.scale(&ck));
        k += 1;
    }
    if !result.is_exact() || !u.c.is_empty() {
        let cap = r.max(1);
        if result.order > cap {
            result.order = cap;
            let keep = (cap - result.val).max(0) as usize;
            result.c.truncate(keep);
        }
    }
    Some(result.trim())
}

/// Sign of a coefficient as h -> 0+ (the coefficient may be a polynomial in
/// ln h, which tends to -infinity).
pub fn sign_at_zero(c: &Expr) -> Option<i32> {
    let l = logh();
    if !c.has_sym(&l) {
        let v = c.to_f64_const()?;
        return if v > 0.0 {
            Some(1)
        } else if v < 0.0 {
            Some(-1)
        } else {
            Some(0)
        };
    }
    let cs = crate::poly::coeffs(c, &l)?;
    let d = cs.iter().rposition(|k| !k.is_zero())?;
    let s = sign_at_zero(&cs[d])?;
    Some(if d % 2 == 1 { -s } else { s })
}

/// Series of `e` in `h` about 0+, keeping about `t` terms.
pub fn ser(e: &Expr, h: &Expr, t: i64) -> Option<Ser> {
    budget::tick();
    if !e.has_sym(h) {
        if e.is_undef() || e.is_inf() || matches!(e, Const(K::CInf)) {
            return None;
        }
        return Some(Ser::constant(e.clone()));
    }
    match e {
        Sym(_) => Some(Ser::var()),
        Add(v) => {
            let mut acc = Ser::constant(zero());
            for x in v {
                acc = acc.add(&ser(x, h, t)?);
            }
            Some(acc)
        }
        Mul(v) => {
            let mut acc = Ser::constant(one());
            for x in v {
                acc = acc.mul(&ser(x, h, t)?, t);
            }
            Some(acc)
        }
        Pow(b, ex) => {
            if !ex.has_sym(h) {
                return ser(b, h, t)?.powr(ex, t);
            }
            if matches!(**b, Const(K::E)) {
                return ser(ex, h, t)?.exp(t);
            }
            let lb = ser(b, h, t)?.ln(t)?;
            ser(ex, h, t)?.mul(&lb, t).exp(t)
        }
        Func(name, args) if args.len() == 1 => {
            let a = ser(&args[0], h, t)?;
            match &**name {
                "ln" => a.ln(t),
                "abs" | "sign" => {
                    let (c0, _) = a.leading()?;
                    let s = sign_at_zero(&c0)?;
                    if &**name == "sign" {
                        return Some(Ser::constant(int(s as i64)));
                    }
                    Some(if s < 0 { a.neg() } else { a })
                }
                _ => {
                    match a.split_const() {
                        Some((a0, u)) => taylor_compose_fn(name, &a0, &u, t),
                        None => {
                            // argument tends to +-infinity
                            let lead = a.clone().lead_trim();
                            let s = sign_at_zero(lead.c.first()?)?;
                            match &**name {
                                "atan" => {
                                    let inner = a.inv(t)?;
                                    let (z, u) = inner.split_const()?;
                                    let at = taylor_compose_fn("atan", &z, &u, t)?;
                                    Some(Ser::constant(crate::expr::mul(vec![rat(s as i64, 2), pi()])).add(&at.neg()))
                                }
                                "tanh" | "erf" => Some(Ser::constant(int(s as i64))),
                                "erfc" => Some(Ser::constant(int(if s > 0 { 0 } else { 2 }))),
                                "heaviside" => Some(Ser::constant(int(if s > 0 { 1 } else { 0 }))),
                                _ => None,
                            }
                        }
                    }
                }
            }
        }
        _ => None,
    }
}

/// Taylor expansion of `f(a0 + u)` using derivatives of `f` at `a0`.
fn taylor_compose_fn(name: &str, a0: &Expr, u: &Ser, t: i64) -> Option<Ser> {
    let w = sym("\u{3c9}t");
    let f = func(name, vec![w.clone()]);
    if u.is_exact_zero() {
        let v = func(name, vec![a0.clone()]);
        if v.is_undef() || v.is_inf() {
            return None;
        }
        return Some(Ser::constant(v));
    }
    // a few entire functions that are 0/0 at the origin when differentiated
    if a0.is_zero() {
        match name {
            "Si" => {
                return compose_taylor(
                    u,
                    &mut |k| {
                        if k % 2 == 0 {
                            zero()
                        } else {
                            let s = if (k / 2) % 2 == 0 { 1 } else { -1 };
                            Num(Rat::new(Int::from(s), Int::from(k) * crate::numtheory::factorial(k as u64)))
                        }
                    },
                    t,
                );
            }
            "sinc" => {
                return compose_taylor(
                    u,
                    &mut |k| {
                        if k % 2 == 1 {
                            zero()
                        } else {
                            let s = if (k / 2) % 2 == 0 { 1 } else { -1 };
                            Num(Rat::new(Int::from(s), crate::numtheory::factorial(k as u64 + 1)))
                        }
                    },
                    t,
                );
            }
            _ => {}
        }
    }
    let mut derivs: Vec<Expr> = vec![f];
    let mut failed = false;
    let a0c = a0.clone();
    let r = compose_taylor(
        u,
        &mut |k| {
            while derivs.len() <= k {
                let last = derivs.last().unwrap().clone();
                derivs.push(diff(&last, &w));
            }
            let v = derivs[k].subs(&w, &a0c);
            if v.is_undef() || v.is_inf() || v.has(&|x| matches!(x, Const(K::CInf | K::Undef | K::Inf)) || matches!(x, Func(n, _) if &**n == "Derivative")) {
                failed = true;
                return undef();
            }
            crate::expr::mul(vec![v, Num(Rat::new(Int::one(), crate::numtheory::factorial(k as u64)))])
        },
        t,
    );
    if failed {
        return None;
    }
    r
}

/// Where to expand.
fn shifted(e: &Expr, x: &Expr, a: &Expr, h: &Expr) -> Expr {
    if a.is_inf() {
        e.subs(x, &pow(h.clone(), int(-1)))
    } else if a.is_neg_inf() {
        e.subs(x, &neg(pow(h.clone(), int(-1))))
    } else if a.is_zero() {
        e.subs(x, h)
    } else {
        e.subs(x, &add(vec![a.clone(), h.clone()]))
    }
}

pub struct Expansion {
    /// Sum of the known terms, written in the original variable.
    pub poly: Expr,
    /// Individual terms, lowest order first.
    pub terms: Vec<Expr>,
    /// The big-O remainder (e.g. `x^6`), if the expansion is not exact.
    pub big_o: Option<Expr>,
}

/// Series expansion of `e` about `x = a` including terms up to order `n`.
pub fn series(e: &Expr, x: &Expr, a: &Expr, n: usize) -> Option<Expansion> {
    let h = sym("\u{3b7}h");
    let g = shifted(e, x, a, &h);
    let want = n as i64 + 1;
    let mut t = want + 1;
    let mut best: Option<Ser> = None;
    for _ in 0..4 {
        let s = budget::attempt_opt(budget::remaining().min(6.0) * 0.5, || ser(&g, &h, t))?;
        let s = s.lead_trim();
        let enough = s.is_exact() || s.order >= want * s.q;
        let deficit = if s.is_exact() { 0 } else { want * s.q - s.order };
        best = Some(s);
        if enough {
            break;
        }
        t += (deficit + 1).max(2);
        if t > 40 {
            break;
        }
    }
    let s = best?;
    // the expansion variable back in terms of x
    let back = if a.is_inf() {
        pow(x.clone(), int(-1))
    } else if a.is_neg_inf() {
        neg(pow(x.clone(), int(-1)))
    } else if a.is_zero() {
        x.clone()
    } else {
        sub(x.clone(), a.clone())
    };
    let logback = if a.is_inf() { neg(ln(x.clone())) } else { ln(back.clone()) };
    let l = logh();
    let limit_exp = Rat::from_integer(Int::from(n as i64));
    let mut terms = Vec::new();
    for (c, ex) in s.terms() {
        if ex > limit_exp {
            continue;
        }
        let c = simplify::simplify(&c.subs(&l, &logback));
        if c.is_zero() {
            continue;
        }
        terms.push(mul(vec![c, pow(back.clone(), Num(ex))]));
    }
    let big_o = match s.order_exp() {
        None => None,
        Some(o) => {
            let o = o.min(limit_exp + Rat::one());
            // next representable exponent
            Some(pow(back.clone(), Num(o)))
        }
    };
    if terms.is_empty() && big_o.is_none() {
        terms.push(zero());
    }
    Some(Expansion { poly: add(terms.clone()), terms, big_o })
}

/// Taylor polynomial of degree `n` about `a` by repeated differentiation
/// (works for symbolic expansion points as long as derivatives exist).
pub fn taylor_poly(e: &Expr, x: &Expr, a: &Expr, n: usize) -> Option<Expr> {
    let mut terms = Vec::new();
    let mut dk = e.clone();
    let dx = sub(x.clone(), a.clone());
    for k in 0..=n {
        let v = dk.subs(x, a);
        if v.is_undef() || v.is_inf() || v.has(&|x| matches!(x, Const(K::CInf | K::Undef))) {
            return None;
        }
        let v = simplify::simplify(&v);
        terms.push(mul(vec![v, pow(dx.clone(), int(k as i64)), Num(Rat::new(Int::one(), crate::numtheory::factorial(k as u64)))]));
        if k < n {
            dk = diff(&dk, x);
        }
    }
    Some(add(terms))
}

/// Leading behaviour of `g(h)` as h -> 0+: `Some((coefficient, exponent))`,
/// or `Some((0, 0))` when `g` is smaller than every power of h.
pub fn leading_term(g: &Expr, h: &Expr) -> Option<(Expr, Rat)> {
    let mut t = 4;
    for _ in 0..4 {
        let s = budget::attempt_opt(budget::remaining().min(4.0) * 0.4, || ser(g, h, t))?;
        if s.is_exact_zero() {
            return Some((zero(), Rat::zero()));
        }
        if let Some(l) = s.leading() {
            return Some(l);
        }
        t *= 2;
        if t > 32 {
            break;
        }
    }
    None
}

#[allow(dead_code)]
fn is_neg(r: &Rat) -> bool {
    r.is_negative()
}
