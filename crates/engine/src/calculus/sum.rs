//! Sums and products: closed forms (polynomial, geometric, telescoping,
//! hypergeometric-style known series), convergence tests and numeric values.

use super::limit::{limit, Dir};
use crate::budget;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::poly;
use crate::simplify;
use num_traits::{One, Signed, ToPrimitive, Zero};

#[derive(Clone, Debug)]
pub enum SumResult {
    Exact(Expr),
    Numeric(f64),
    Divergent(&'static str),
    Unknown,
}

fn is_pinf(e: &Expr) -> bool {
    matches!(e, Const(K::Inf))
}

/// Σ_{k=a}^{b} f(k)
pub fn sum(f: &Expr, k: &Expr, a: &Expr, b: &Expr) -> SumResult {
    budget::tick();
    if let (Some(lo), Some(hi)) = (a.as_i64(), b.as_i64()) {
        if hi < lo {
            return SumResult::Exact(zero());
        }
        if hi - lo <= 2000 && (f.free_symbols().len() <= 1 || hi - lo <= 200) {
            let terms: Vec<Expr> = (lo..=hi).map(|j| f.subs(k, &int(j))).collect();
            let s = add(terms);
            if !s.has(&|n| matches!(n, Const(K::Undef | K::CInf))) {
                return SumResult::Exact(simplify::simplify(&s));
            }
        }
    }
    if !f.has_sym(k) {
        if is_pinf(b) {
            return if f.is_zero() { SumResult::Exact(zero()) } else { SumResult::Divergent("the terms do not tend to zero") };
        }
        return SumResult::Exact(simplify::simplify(&mul(vec![f.clone(), add(vec![b.clone(), neg(a.clone()), one()])])));
    }
    // linearity
    let fe = simplify::expand(f);
    if let Add(v) = &fe {
        if !is_pinf(b) {
            let mut parts = Vec::new();
            let mut ok = true;
            for t in v {
                match closed(t, k, a, b) {
                    Some(r) => parts.push(r),
                    None => {
                        ok = false;
                        break;
                    }
                }
            }
            if ok {
                return SumResult::Exact(tidy(&add(parts)));
            }
        }
    }
    if let Some(r) = budget::attempt_opt(budget::remaining().min(8.0) * 0.6, || closed(f, k, a, b)) {
        let r = tidy(&r);
        if !r.has(&|n| matches!(n, Const(K::Undef | K::CInf))) {
            if is_pinf(&r) || r.is_neg_inf() {
                return SumResult::Divergent("the partial sums grow without bound");
            }
            if is_pinf(b) && check_numeric(f, k, a, &r) == Some(false) {
                // a closed form that disagrees numerically is discarded
            } else {
                return SumResult::Exact(r);
            }
        }
    }
    if is_pinf(b) {
        if let Some(reason) = divergence(f, k) {
            return SumResult::Divergent(reason);
        }
        if let Some(v) = numeric_infinite(f, k, a) {
            return SumResult::Numeric(v);
        }
    } else if let (Some(lo), Some(hi)) = (a.as_i64(), b.as_i64()) {
        if hi - lo <= 2_000_000 && f.free_symbols().len() == 1 {
            if let Sym(name) = k {
                if let Some(p) = crate::eval::Program::compile(f, &[name]) {
                    let mut s = 0.0;
                    let mut c = 0.0; // Kahan
                    for j in lo..=hi {
                        let y = p.eval1(j as f64) - c;
                        let t = s + y;
                        c = (t - s) - y;
                        s = t;
                    }
                    return SumResult::Numeric(s);
                }
            }
        }
    }
    SumResult::Unknown
}

fn check_numeric(f: &Expr, k: &Expr, a: &Expr, r: &Expr) -> Option<bool> {
    let v = r.to_f64_const()?;
    let n = numeric_infinite(f, k, a)?;
    Some((v - n).abs() <= 1e-6 * (1.0 + v.abs()))
}

/// Closed form of Σ_{k=a}^{b} f.
fn closed(f: &Expr, k: &Expr, a: &Expr, b: &Expr) -> Option<Expr> {
    let (c, rest) = split_const(f, k);
    if !c.is_one() {
        return Some(mul(vec![c, closed(&rest, k, a, b)?]));
    }
    if let Some(r) = polynomial(f, k, a, b) {
        return Some(r);
    }
    if let Some(r) = geometric(f, k, a, b) {
        return Some(r);
    }
    if let Some(r) = known_series(f, k, a, b) {
        return Some(r);
    }
    if let Some(r) = rational(f, k, a, b) {
        return Some(r);
    }
    None
}

fn split_const(e: &Expr, k: &Expr) -> (Expr, Expr) {
    match e {
        Mul(v) => {
            let (c, r): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|t| !t.has_sym(k));
            (mul(c), mul(r))
        }
        _ => (one(), e.clone()),
    }
}

/// S_p(n) = Σ_{j=0}^{n} j^p (Faulhaber).
fn power_sum(p: usize, n: &Expr) -> Expr {
    if p == 0 {
        return add(vec![n.clone(), one()]);
    }
    // Σ_{j=1}^{n} j^p = 1/(p+1) Σ_{i=0}^{p} C(p+1, i) B_i^+ n^(p+1-i)
    let mut terms = Vec::new();
    for i in 0..=p {
        let mut bi = crate::numtheory::bernoulli(i);
        if i == 1 {
            bi = -bi; // B1+ = +1/2
        }
        if bi.is_zero() {
            continue;
        }
        let c = crate::numtheory::binomial(&Int::from(p + 1), i as u64);
        let coef = Rat::from_integer(c) * bi / Rat::from_integer(Int::from(p + 1));
        terms.push(mul(vec![Num(coef), pow(n.clone(), int((p + 1 - i) as i64))]));
    }
    add(terms)
}

fn polynomial(f: &Expr, k: &Expr, a: &Expr, b: &Expr) -> Option<Expr> {
    if is_pinf(b) {
        return None;
    }
    let cs = poly::coeffs(f, k)?;
    if cs.len() > 40 {
        return None;
    }
    let mut terms = Vec::new();
    let am1 = sub(a.clone(), one());
    for (p, c) in cs.iter().enumerate() {
        if c.is_zero() {
            continue;
        }
        terms.push(mul(vec![c.clone(), sub(power_sum(p, b), power_sum(p, &am1))]));
    }
    Some(simplify::expand(&add(terms)))
}

/// Σ c r^k (times a polynomial in k).
fn geometric(f: &Expr, k: &Expr, a: &Expr, b: &Expr) -> Option<Expr> {
    // collect factors base^(linear in k)
    let mut ratio = Vec::new();
    let mut rest = Vec::new();
    for t in f.factors() {
        match &t {
            Pow(base, ex) if !base.has_sym(k) && ex.has_sym(k) => {
                let cs = poly::coeffs(ex, k)?;
                if cs.len() != 2 {
                    return None;
                }
                ratio.push(pow((**base).clone(), cs[1].clone()));
                rest.push(pow((**base).clone(), cs[0].clone()));
            }
            _ => rest.push(t),
        }
    }
    if ratio.is_empty() {
        return None;
    }
    let r = simplify::simplify(&mul(ratio));
    let p = mul(rest);
    let pc = poly::coeffs(&p, k)?;
    let deg = pc.len() - 1;
    if deg > 6 {
        return None;
    }
    if r.is_one() {
        return polynomial(&p, k, a, b);
    }
    if is_pinf(b) {
        // Σ_{k=a}^∞ k^d r^k requires |r| < 1
        let rv = crate::eval::const_c64(&r)?;
        if rv.norm() >= 1.0 {
            return Some(inf());
        }
        // generating function: Σ_{k>=0} k^d r^k via (r d/dr)^d 1/(1-r)
        let z = sym("\u{3b6}g");
        let mut g = pow(sub(one(), z.clone()), int(-1));
        let mut gs = vec![g.clone()];
        for _ in 1..=deg {
            g = mul(vec![z.clone(), super::diff::diff(&g, &z)]);
            gs.push(g.clone());
        }
        let total = add(pc.iter().enumerate().map(|(d, c)| mul(vec![c.clone(), gs[d].clone()])).collect());
        let total = total.subs(&z, &r);
        // subtract the terms below a
        let lo = a.as_i64()?;
        let mut corr = Vec::new();
        if lo > 0 {
            for j in 0..lo {
                corr.push(mul(vec![p.subs(k, &int(j)), pow(r.clone(), int(j))]));
            }
        } else if lo < 0 {
            return None;
        }
        return Some(sub(total, add(corr)));
    }
    if deg == 0 {
        // p (r^a - r^(b+1)) / (1 - r)
        let num = sub(pow(r.clone(), a.clone()), pow(r.clone(), add(vec![b.clone(), one()])));
        return Some(mul(vec![p, num, pow(sub(one(), r), int(-1))]));
    }
    if deg == 1 {
        // Σ k r^k = r (1 - (n+1) r^n + n r^(n+1)) / (1-r)^2 from 0..n
        let s1 = |n: &Expr| {
            let n1 = add(vec![n.clone(), one()]);
            div(mul(vec![r.clone(), add(vec![one(), neg(mul(vec![n1, pow(r.clone(), n.clone())])), mul(vec![n.clone(), pow(r.clone(), add(vec![n.clone(), one()]))])])]), square(sub(one(), r.clone())))
        };
        let s0 = |n: &Expr| div(sub(one(), pow(r.clone(), add(vec![n.clone(), one()]))), sub(one(), r.clone()));
        let am1 = sub(a.clone(), one());
        let part = |c: &Expr, s: &dyn Fn(&Expr) -> Expr| mul(vec![c.clone(), sub(s(b), s(&am1))]);
        return Some(add(vec![part(&pc[0], &s0), part(&pc[1], &s1)]));
    }
    None
}

/// Rational terms: telescoping via partial fractions; ζ and polygamma for
/// infinite sums.
fn rational(f: &Expr, k: &Expr, a: &Expr, b: &Expr) -> Option<Expr> {
    if f.has(&|n| matches!(n, Func(..))) {
        return None;
    }
    let (num, den) = simplify::numer_denom(f);
    let np = poly::to_qpoly(&num, k)?;
    let dp = poly::to_qpoly(&den, k)?;
    if dp.degree() == 0 {
        return None;
    }
    if is_pinf(b) && np.degree() + 1 >= dp.degree() {
        return Some(inf()); // terms do not decay fast enough (positive terms)
    }
    // pure power: Σ 1/k^s
    if np.degree() == 0 && dp.0.iter().take(dp.degree()).all(|c| c.is_zero()) {
        let s = dp.degree() as i64;
        let c = np.coeff(0) / dp.lc();
        if is_pinf(b) {
            let lo = a.as_i64()?;
            let mut corr = Vec::new();
            for j in 1..lo {
                corr.push(pow(int(j), int(-s)));
            }
            if lo < 1 {
                return None;
            }
            return Some(mul(vec![Num(c), sub(func("zeta", vec![int(s)]), add(corr))]));
        }
        // generalized harmonic numbers
        let h = |n: Expr| if s == 1 { func("harmonic", vec![n]) } else { func("harmonic", vec![n, int(s)]) };
        let am1 = sub(a.clone(), one());
        return Some(mul(vec![Num(c), sub(h(b.clone()), h(am1))]));
    }
    // partial fractions with linear factors: Σ c/(k + r)^m
    let pf = simplify::apart(f, k)?;
    let mut by_power: Vec<(Expr, Rat, i64)> = Vec::new(); // (coeff, shift r, power)
    for t in pf.terms() {
        let (c, rest) = split_const(&t, k);
        let (base, e) = rest.base_exp();
        let m = -e.as_i64()?;
        if m <= 0 {
            return None;
        }
        let bc = poly::to_qpoly(&base, k)?;
        if bc.degree() != 1 {
            return None;
        }
        let lc = bc.coeff(1);
        let r = bc.coeff(0) / lc.clone();
        let cc = mul(vec![c, pow(Num(lc), int(-m))]);
        by_power.push((cc, r, m));
    }
    // all shifts must differ by integers for telescoping
    let mut out = Vec::new();
    let infinite = is_pinf(b);
    for m in 1..=8i64 {
        let group: Vec<&(Expr, Rat, i64)> = by_power.iter().filter(|g| g.2 == m).collect();
        if group.is_empty() {
            continue;
        }
        let r0 = group[0].1.clone();
        if group.iter().any(|g| !(g.1.clone() - r0.clone()).is_integer()) {
            return None;
        }
        if m == 1 {
            // Σ_j c_j / (k + r0 + d_j): telescopes iff Σ c_j = 0
            let total = simplify::simplify(&add(group.iter().map(|g| g.0.clone()).collect()));
            if !total.is_zero() {
                if infinite {
                    return Some(inf());
                }
            }
            for g in &group {
                let d = (g.1.clone() - r0.clone()).to_integer().to_i64()?;
                // Σ_{k=a}^{b} 1/(k + r0 + d) = H(b + r0 + d) - H(a - 1 + r0 + d)
                let shift = add(vec![Num(r0.clone()), int(d)]);
                if infinite {
                    // tail contributions cancel; keep -H(a - 1 + shift)
                    out.push(mul(vec![g.0.clone(), neg(harmonic_shift(&sub(a.clone(), one()), &shift)?)]));
                } else {
                    out.push(mul(vec![g.0.clone(), sub(harmonic_shift(b, &shift)?, harmonic_shift(&sub(a.clone(), one()), &shift)?)]));
                }
            }
        } else {
            for g in &group {
                let shift = Num(g.1.clone());
                if infinite {
                    // Σ_{k=a}^∞ 1/(k + s)^m = ζ(m, a + s)
                    out.push(mul(vec![g.0.clone(), hurwitz(m, &add(vec![a.clone(), shift]))?]));
                } else {
                    let tail = |n: &Expr| hurwitz(m, &add(vec![n.clone(), shift.clone()]));
                    out.push(mul(vec![g.0.clone(), sub(tail(a)?, tail(&add(vec![b.clone(), one()]))?)]));
                }
            }
        }
    }
    Some(add(out))
}

/// H(n + s) for rational s when n + s is an integer offset; otherwise digamma.
fn harmonic_shift(n: &Expr, s: &Expr) -> Option<Expr> {
    let x = add(vec![n.clone(), s.clone()]);
    if s.is_integer() {
        return Some(func("harmonic", vec![x]));
    }
    // H(x) = ψ(x + 1) + γ
    Some(add(vec![func("digamma", vec![add(vec![x, one()])]), Const(K::EulerGamma)]))
}

/// Hurwitz zeta ζ(m, q) for integer q >= 1 in terms of ζ(m).
fn hurwitz(m: i64, q: &Expr) -> Option<Expr> {
    if let Some(qi) = q.as_i64() {
        if qi >= 1 && qi < 2000 {
            let corr: Vec<Expr> = (1..qi).map(|j| pow(int(j), int(-m))).collect();
            return Some(sub(func("zeta", vec![int(m)]), add(corr)));
        }
    }
    if q.has(&|n| matches!(n, Sym(_))) {
        // symbolic upper limit: ζ(m) - H(q - 1, m)
        return Some(sub(func("zeta", vec![int(m)]), func("harmonic", vec![sub(q.clone(), one()), int(m)])));
    }
    None
}

/// Well-known series (Taylor series of elementary functions, alternating
/// series).
fn known_series(f: &Expr, k: &Expr, a: &Expr, b: &Expr) -> Option<Expr> {
    if !is_pinf(b) {
        return None;
    }
    let lo = a.as_i64()?;
    if !(0..=3).contains(&lo) {
        return None;
    }
    let shape = |g: &Expr| simplify::simplify(&g.subs(k, &add(vec![k.clone(), int(lo)])));
    let g = shape(f);
    let fact = |e: &Expr| matches!(e, Func(n, a) if &**n == "factorial" && a.len() == 1);
    // split into: x^(αk+β) parts, (-1)^k, 1/(ck+d)!, polynomial factor
    let mut x_pow: Option<(Expr, Expr, Expr)> = None; // base, alpha, beta
    let mut sign_alt = false;
    let mut fact_den: Option<(i64, i64)> = None;
    let mut poly_den: Option<Expr> = None;
    let mut others = Vec::new();
    for t in g.factors() {
        match &t {
            Pow(b0, ex) if **b0 == int(-1) => {
                let cs = poly::coeffs(ex, k)?;
                if cs.len() != 2 {
                    return None;
                }
                let s = cs[1].as_i64()?;
                if s % 2 != 0 {
                    sign_alt = !sign_alt;
                }
                if let Some(c0) = cs[0].as_i64() {
                    if c0 % 2 != 0 {
                        others.push(int(-1));
                    }
                } else {
                    return None;
                }
            }
            Pow(b0, ex) if fact(b0) && ex.is_minus_one() => {
                let Func(_, args) = &**b0 else { return None };
                let cs = poly::coeffs(&args[0], k)?;
                if cs.len() != 2 {
                    return None;
                }
                fact_den = Some((cs[1].as_i64()?, cs[0].as_i64()?));
            }
            Pow(b0, ex) if !b0.has_sym(k) && ex.has_sym(k) => {
                let cs = poly::coeffs(ex, k)?;
                if cs.len() != 2 || x_pow.is_some() {
                    return None;
                }
                x_pow = Some(((**b0).clone(), cs[1].clone(), cs[0].clone()));
            }
            Pow(b0, ex) if b0.has_sym(k) && ex.is_minus_one() => {
                if poly_den.is_some() {
                    return None;
                }
                poly_den = Some((**b0).clone());
            }
            _ if t.has_sym(k) => return None,
            _ => others.push(t.clone()),
        }
    }
    let c = mul(others);
    let (base, alpha, beta) = x_pow.unwrap_or((one(), one(), zero()));
    let xa = pow(base.clone(), alpha.clone()); // the effective variable^k
    let pre = pow(base.clone(), beta);
    let s = if sign_alt { neg(xa.clone()) } else { xa.clone() };
    let result = match (fact_den, &poly_den) {
        // Σ s^k/k! = e^s ; Σ s^k/(2k)! and (2k+1)! give cosh/sinh-type
        (Some((1, 0)), None) => exp(s),
        (Some((2, 0)), None) => {
            let r = sqrt(xa.clone());
            if sign_alt {
                f1("cos", r)
            } else {
                f1("cosh", r)
            }
        }
        (Some((2, 1)), None) => {
            let r = sqrt(xa.clone());
            if sign_alt {
                div(f1("sin", r.clone()), r)
            } else {
                div(f1("sinh", r.clone()), r)
            }
        }
        // Σ s^k/(k + d) for k >= 0
        (None, Some(pd)) => {
            let cs = poly::coeffs(pd, k)?;
            if cs.len() != 2 || !cs[1].is_one() {
                return None;
            }
            let d = cs[0].as_i64()?;
            if d < 1 || d > 3 {
                return None;
            }
            // Σ_{k>=0} s^k/(k+1) = -ln(1-s)/s ; shift for d > 1
            let sv = crate::eval::const_c64(&s);
            if let Some(z) = sv {
                if z.norm() > 1.0 || (z - num_complex::Complex64::new(1.0, 0.0)).norm() < 1e-12 {
                    return if z.norm() > 1.0 { None } else { Some(inf()) };
                }
            }
            let mut v = div(neg(ln(sub(one(), s.clone()))), s.clone());
            // Σ s^k/(k+d) = s^(1-d) (Σ_{j>=0} s^j/(j+1) - Σ_{j<d-1} s^j/(j+1))
            if d > 1 {
                let corr: Vec<Expr> = (0..d - 1).map(|j| div(pow(s.clone(), int(j)), int(j + 1))).collect();
                v = mul(vec![pow(s.clone(), int(1 - d)), sub(v, add(corr))]);
            }
            v
        }
        _ => return None,
    };
    Some(mul(vec![c, pre, result]))
}

/// Divergence tests for an infinite sum; `Some(reason)` if divergent.
fn divergence(f: &Expr, k: &Expr) -> Option<&'static str> {
    let l = limit(f, k, &inf(), Dir::Both);
    match &l {
        Some(v) if v.is_zero() => {}
        Some(v) if is_pinf(v) || v.is_neg_inf() => return Some("the terms do not tend to zero"),
        Some(v) => {
            if v.to_f64_const().map_or(false, |x| x.abs() > 1e-12) {
                return Some("the terms do not tend to zero");
            }
        }
        None => {
            // oscillating terms such as (-1)^k
            let Sym(name) = k else { return None };
            let p = crate::eval::Program::compile(f, &[name])?;
            let tail: Vec<f64> = (0..20).map(|j| p.eval1((1_000_000 + j) as f64).abs()).collect();
            if tail.iter().all(|v| v.is_finite()) && tail.iter().cloned().fold(0.0, f64::max) > 1e-3 {
                return Some("the terms do not tend to zero");
            }
        }
    }
    // ratio/comparison with p-series: |f| ~ C k^p with p >= -1 and fixed sign
    let h = sym("\u{3b7}s");
    let g = f.subs(k, &pow(h.clone(), int(-1)));
    if let Some((c, ex)) = super::series::leading_term(&g, &h) {
        if !c.has(&|n| matches!(n, Func(..))) && c.to_f64_const().is_some() && ex <= Rat::one() && !f.has(&|n| matches!(n, Pow(b, _) if **b == int(-1))) && !f.has_any_fn(&["sin", "cos"]) {
            return Some("the terms decay no faster than 1/k (comparison with the harmonic series)");
        }
    }
    None
}

/// Numeric value of Σ_{k=a}^∞ f(k).
fn numeric_infinite(f: &Expr, k: &Expr, a: &Expr) -> Option<f64> {
    let Sym(name) = k else { return None };
    let lo = a.as_i64()?;
    let p = crate::eval::Program::compile(f, &[name])?;
    let term = |j: i64| p.eval1(j as f64);
    // alternating? use Euler/Cohen acceleration on the sign-alternating tail
    let t0 = term(lo);
    let t1 = term(lo + 1);
    let alternating = t0 * t1 < 0.0 && term(lo + 2) * t1 < 0.0 && term(lo + 3) * term(lo + 2) < 0.0;
    if alternating {
        // Cohen–Villegas–Zagier
        let n = 60;
        let d0 = (3.0 + 8f64.sqrt()).powi(n);
        let d = (d0 + 1.0 / d0) / 2.0;
        let (mut b, mut c, mut s) = (-1.0, -d, 0.0);
        for j in 0..n {
            c = b - c;
            let aj = term(lo + j as i64).abs();
            s += c * aj;
            b = (j as f64 + n as f64) * (j as f64 - n as f64) * b / ((j as f64 + 0.5) * (j as f64 + 1.0));
        }
        let v = s / d;
        return Some(if t0 > 0.0 { v } else { -v });
    }
    // direct summation plus Euler–Maclaurin style tail estimate via integral
    let n = 20_000i64;
    let mut s = 0.0;
    let mut comp = 0.0;
    for j in lo..lo + n {
        let y = term(j) - comp;
        let t = s + y;
        comp = (t - s) - y;
        s = t;
        if !s.is_finite() {
            return None;
        }
    }
    let end = (lo + n) as f64;
    let tail = crate::numeric::quad(&|t| p.eval1(t), end, f64::INFINITY, 1e-10);
    if !tail.value.is_finite() {
        return None;
    }
    let corr = 0.5 * p.eval1(end) - crate::numeric::nderiv(&|t| p.eval1(t), end) / 12.0;
    let v = s + tail.value + corr;
    // sanity: must have converged
    let last = term(lo + n).abs();
    if last > 1e-6 * (1.0 + v.abs()) {
        return None;
    }
    Some(v)
}

/// Π_{k=a}^{b} f(k)
pub fn product(f: &Expr, k: &Expr, a: &Expr, b: &Expr) -> SumResult {
    if let (Some(lo), Some(hi)) = (a.as_i64(), b.as_i64()) {
        if hi < lo {
            return SumResult::Exact(one());
        }
        if hi - lo <= 1000 {
            let p = mul((lo..=hi).map(|j| f.subs(k, &int(j))).collect());
            if !p.has(&|n| matches!(n, Const(K::Undef | K::CInf))) {
                return SumResult::Exact(simplify::simplify(&p));
            }
        }
    }
    if !f.has_sym(k) {
        return SumResult::Exact(pow(f.clone(), add(vec![b.clone(), neg(a.clone()), one()])));
    }
    // Π (k + c) = Γ(b + c + 1)/Γ(a + c)
    if let Some(cs) = poly::coeffs(f, k) {
        if cs.len() == 2 && !is_pinf(b) {
            let (s, c) = (cs[1].clone(), div(cs[0].clone(), cs[1].clone()));
            let num = func("gamma", vec![add(vec![b.clone(), c.clone(), one()])]);
            let den = func("gamma", vec![add(vec![a.clone(), c])]);
            let r = mul(vec![pow(s, add(vec![b.clone(), neg(a.clone()), one()])), num, pow(den, int(-1))]);
            let r = rewrite_gamma(&simplify::simplify(&r));
            return SumResult::Exact(r);
        }
    }
    // Π c^g(k) = c^(Σ g(k))
    if let Pow(base, ex) = f {
        if !base.has_sym(k) {
            if let SumResult::Exact(s) = sum(ex, k, a, b) {
                return SumResult::Exact(simplify::simplify(&pow((**base).clone(), s)));
            }
        }
    }
    if is_pinf(b) {
        // numeric infinite product through Σ ln f(k)
        if let SumResult::Numeric(v) = sum(&ln(f.clone()), k, a, b) {
            return SumResult::Numeric(v.exp());
        }
        if let SumResult::Exact(v) = sum(&ln(f.clone()), k, a, b) {
            if let Some(x) = v.to_f64_const() {
                return SumResult::Numeric(x.exp());
            }
        }
    }
    SumResult::Unknown
}

/// Γ(n + 1) with symbolic integer n is shown as n!.
fn rewrite_gamma(e: &Expr) -> Expr {
    match e {
        Func(n, a) if &**n == "gamma" && a.len() == 1 => {
            let arg = rewrite_gamma(&a[0]);
            let m1 = simplify::simplify(&sub(arg.clone(), one()));
            if m1.has(&|x| matches!(x, Sym(_))) {
                func("factorial", vec![m1])
            } else {
                func("gamma", vec![arg])
            }
        }
        Add(v) => add(v.iter().map(rewrite_gamma).collect()),
        Mul(v) => mul(v.iter().map(rewrite_gamma).collect()),
        Pow(b, x) => pow(rewrite_gamma(b), (**x).clone()),
        _ => e.clone(),
    }
}

#[allow(dead_code)]
fn positive(r: &Rat) -> bool {
    r.is_positive()
}

/// Simplify, first rewriting `H(n + m)` (integer m) as `H(n) + Σ 1/(n + j)`
/// so that telescoped harmonic numbers cancel.
fn tidy(e: &Expr) -> Expr {
    fn rw(e: &Expr) -> Expr {
        match e {
            Func(n, a) if &**n == "harmonic" && a.len() == 1 => {
                let arg = rw(&a[0]);
                if let Add(ts) = &arg {
                    let (nums, rest): (Vec<Expr>, Vec<Expr>) = ts.iter().cloned().partition(|t| t.is_integer());
                    if let Some(m) = add(nums).as_i64() {
                        if m != 0 && m.abs() <= 50 {
                            let base = add(rest);
                            let mut parts = vec![func("harmonic", vec![base.clone()])];
                            if m > 0 {
                                for j in 1..=m {
                                    parts.push(pow(add(vec![base.clone(), int(j)]), int(-1)));
                                }
                            } else {
                                for j in 0..(-m) {
                                    parts.push(neg(pow(sub(base.clone(), int(j)), int(-1))));
                                }
                            }
                            return add(parts);
                        }
                    }
                }
                func("harmonic", vec![arg])
            }
            Add(v) => add(v.iter().map(rw).collect()),
            Mul(v) => mul(v.iter().map(rw).collect()),
            Pow(b, x) => pow(rw(b), rw(x)),
            Func(n, a) => func(n, a.iter().map(rw).collect()),
            _ => e.clone(),
        }
    }
    let r = rw(e);
    let s = simplify::simplify(&r);
    if s.has_fn("harmonic") || !r.has_fn("harmonic") {
        let s0 = simplify::simplify(e);
        if s0.size() <= s.size() {
            return s0;
        }
    }
    s
}
