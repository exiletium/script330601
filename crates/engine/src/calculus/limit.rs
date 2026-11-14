//! Limits: direct substitution, series expansion, structural rules with
//! L'Hôpital, and a numeric fallback.

use super::diff::diff;
use super::series::{leading_term, logh, sign_at_zero};
use crate::budget;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::simplify;
use num_traits::{Signed, Zero};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Dir {
    Both,
    Left,
    Right,
}

fn ninf() -> Expr {
    neg(inf())
}

fn is_pinf(e: &Expr) -> bool {
    matches!(e, Const(K::Inf))
}

fn infinite(e: &Expr) -> bool {
    is_pinf(e) || e.is_neg_inf()
}

fn bad(e: &Expr) -> bool {
    e.has(&|x| matches!(x, Const(K::Undef | K::CInf | K::Inf)))
}

fn signed_inf(s: i32) -> Option<Expr> {
    match s {
        1 => Some(inf()),
        -1 => Some(ninf()),
        _ => None,
    }
}

fn sign_of(e: &Expr) -> Option<i32> {
    if is_pinf(e) {
        return Some(1);
    }
    if e.is_neg_inf() {
        return Some(-1);
    }
    let v = e.to_f64_const()?;
    Some(if v > 0.0 {
        1
    } else if v < 0.0 {
        -1
    } else {
        0
    })
}

const DISCONTINUOUS: &[&str] = &["floor", "ceil", "round", "sign", "heaviside", "frac", "mod", "dirac", "arg", "gamma", "zeta", "tan", "factorial", "lgamma", "digamma", "atan2"];

/// Limit of `e` as `x -> a` (`a` may be ±infinity).  The result is a finite
/// expression or ±infinity; `None` when the limit does not exist or could not
/// be determined.
pub fn limit(e: &Expr, x: &Expr, a: &Expr, dir: Dir) -> Option<Expr> {
    let h = sym("\u{3b7}l");
    let at = |g: Expr| lim0(&g, &h, 0).map(|v| if infinite(&v) { v } else { simplify::simplify(&v) });
    if is_pinf(a) {
        return at(e.subs(x, &pow(h.clone(), int(-1))));
    }
    if a.is_neg_inf() {
        return at(e.subs(x, &neg(pow(h.clone(), int(-1)))));
    }
    if !e.has_sym(x) {
        return Some(e.clone());
    }
    let right = || at(e.subs(x, &add(vec![a.clone(), h.clone()])));
    let left = || at(e.subs(x, &sub(a.clone(), h.clone())));
    match dir {
        Dir::Right => right(),
        Dir::Left => left(),
        Dir::Both => {
            let r = right()?;
            let l = left()?;
            if same(&l, &r) {
                Some(r)
            } else {
                None
            }
        }
    }
}

/// Both one-sided limits (left, right).
pub fn limit_sides(e: &Expr, x: &Expr, a: &Expr) -> (Option<Expr>, Option<Expr>) {
    (limit(e, x, a, Dir::Left), limit(e, x, a, Dir::Right))
}

pub fn same(l: &Expr, r: &Expr) -> bool {
    if infinite(l) || infinite(r) {
        return l == r;
    }
    if l == r {
        return true;
    }
    if let (Float(a), Float(b)) = (l, r) {
        return (a.0 - b.0).abs() <= 1e-7 * (1.0 + a.0.abs());
    }
    simplify::equal(l, r)
}

fn bounded(e: &Expr) -> bool {
    match e {
        Num(_) | Float(_) => true,
        Const(k) => !matches!(k, K::Inf | K::CInf | K::Undef),
        Func(n, a) => matches!(&**n, "sin" | "cos" | "atan" | "tanh" | "erf" | "sign" | "heaviside" | "frac" | "sinc" | "Si") && a.len() == 1,
        Add(v) | Mul(v) => v.iter().all(bounded),
        Pow(b, ex) => bounded(b) && ex.as_i64().map_or(false, |k| k > 0),
        _ => false,
    }
}

/// Limit of `g(h)` as h -> 0+.
fn lim0(g: &Expr, h: &Expr, depth: usize) -> Option<Expr> {
    budget::tick();
    if !g.has_sym(h) {
        return if g.is_undef() || matches!(g, Const(K::CInf)) { None } else { Some(g.clone()) };
    }
    if depth > 9 {
        return numeric(g, h);
    }
    // 1. direct substitution for expressions continuous at the point
    let var_exponent = g.has(&|x| matches!(x, Pow(_, ex) if ex.has_sym(h)));
    if !var_exponent && !g.has_any_fn(DISCONTINUOUS) {
        let v = g.subs(h, &zero());
        if !bad(&v) && !v.has(&|x| matches!(x, Func(n, _) if &**n == "Derivative")) {
            return Some(v);
        }
    }
    // 2. series
    if let Some((c, ex)) = leading_term(g, h) {
        if c.is_zero() {
            return Some(zero());
        }
        if ex.is_positive() {
            return Some(zero());
        }
        let l = logh();
        if ex.is_zero() {
            if !c.has_sym(&l) {
                return Some(c);
            }
            if let Some(cs) = crate::poly::coeffs(&c, &l) {
                if cs.len() <= 1 {
                    return cs.first().cloned();
                }
            }
            return signed_inf(sign_at_zero(&c)?);
        }
        if let Some(s) = sign_at_zero(&c) {
            return signed_inf(s);
        }
        // sign unknown: fall through to other methods
    }
    // 3. structure
    if let Some(v) = structural(g, h, depth) {
        return Some(v);
    }
    // 4. numeric
    numeric(g, h)
}

fn structural(g: &Expr, h: &Expr, depth: usize) -> Option<Expr> {
    let rec = |e: &Expr| lim0(e, h, depth + 1);
    match g {
        Add(v) => {
            let ls: Vec<Option<Expr>> = v.iter().map(&rec).collect();
            if ls.iter().any(|l| l.is_none()) {
                return None;
            }
            let ls: Vec<Expr> = ls.into_iter().flatten().collect();
            let p = ls.iter().filter(|l| is_pinf(l)).count();
            let n = ls.iter().filter(|l| l.is_neg_inf()).count();
            if p > 0 && n > 0 {
                // infinity - infinity: compare against a dominant term
                for (i, ti) in v.iter().enumerate() {
                    if !infinite(&ls[i]) {
                        continue;
                    }
                    let mut total = Vec::new();
                    let mut ok = true;
                    for tj in v.iter() {
                        let ratio = simplify::cancel(&div(tj.clone(), ti.clone()));
                        match rec(&ratio) {
                            Some(r) if !infinite(&r) => total.push(r),
                            _ => {
                                ok = false;
                                break;
                            }
                        }
                    }
                    if !ok {
                        continue;
                    }
                    let s = sign_of(&simplify::simplify(&add(total)))?;
                    if s == 0 {
                        break;
                    }
                    return signed_inf(s * sign_of(&ls[i])?);
                }
                let t = simplify::together(g);
                if !matches!(t, Add(_)) && t != *g {
                    return rec(&t);
                }
                return None;
            }
            if p > 0 {
                return Some(inf());
            }
            if n > 0 {
                return Some(ninf());
            }
            Some(add(ls))
        }
        Mul(v) => {
            let ls: Vec<Option<Expr>> = v.iter().map(&rec).collect();
            // bounded × 0
            if ls.iter().any(|l| l.is_none()) {
                let known_zero = ls.iter().any(|l| l.as_ref().map_or(false, |x| x.is_zero()));
                let fine = v.iter().zip(&ls).all(|(f, l)| match l {
                    Some(x) => !infinite(x),
                    None => bounded(f),
                });
                return if known_zero && fine { Some(zero()) } else { None };
            }
            let ls: Vec<Expr> = ls.into_iter().flatten().collect();
            let zeros: Vec<usize> = (0..v.len()).filter(|&k| ls[k].is_zero()).collect();
            let infs: Vec<usize> = (0..v.len()).filter(|&k| infinite(&ls[k])).collect();
            if !zeros.is_empty() && !infs.is_empty() {
                // 0 × infinity -> quotient for L'Hôpital
                let num = mul((0..v.len()).filter(|k| !zeros.contains(k)).map(|k| v[k].clone()).collect());
                let den = pow(mul(zeros.iter().map(|&k| v[k].clone()).collect()), int(-1));
                if let Some(r) = lhopital(&num, &den, h, depth) {
                    return Some(r);
                }
                let num = mul((0..v.len()).filter(|k| !infs.contains(k)).map(|k| v[k].clone()).collect());
                let den = pow(mul(infs.iter().map(|&k| v[k].clone()).collect()), int(-1));
                return lhopital(&num, &den, h, depth);
            }
            if !infs.is_empty() {
                let mut s = 1;
                for l in &ls {
                    s *= sign_of(l)?;
                }
                return signed_inf(s);
            }
            Some(mul(ls))
        }
        Pow(b, ex) => {
            if ex.has_sym(h) {
                // b^ex = exp(ex ln b)
                let inner = if matches!(**b, Const(K::E)) { (**ex).clone() } else { mul(vec![(**ex).clone(), ln((**b).clone())]) };
                let l = rec(&inner)?;
                if is_pinf(&l) {
                    return Some(inf());
                }
                if l.is_neg_inf() {
                    return Some(zero());
                }
                return Some(exp(l));
            }
            let lb = rec(b)?;
            let s = sign_of(ex);
            if infinite(&lb) {
                return match s? {
                    1 => {
                        if is_pinf(&lb) {
                            Some(inf())
                        } else {
                            match ex.as_i64() {
                                Some(k) if k % 2 == 0 => Some(inf()),
                                Some(_) => Some(ninf()),
                                None => None,
                            }
                        }
                    }
                    -1 => Some(zero()),
                    _ => Some(one()),
                };
            }
            if lb.is_zero() {
                return match s? {
                    1 => Some(zero()),
                    -1 => {
                        // sign of the base near the point
                        let prog = crate::eval::Program::compile(b, &[sym_name(h)])?;
                        let sb = prog.eval1(1e-7);
                        if sb > 0.0 {
                            Some(inf())
                        } else if sb < 0.0 {
                            match ex.as_i64() {
                                Some(k) if k % 2 == 0 => Some(inf()),
                                Some(_) => Some(ninf()),
                                None => None,
                            }
                        } else {
                            None
                        }
                    }
                    _ => Some(one()),
                };
            }
            let v = pow(lb, (**ex).clone());
            if bad(&v) {
                None
            } else {
                Some(v)
            }
        }
        Func(name, args) if args.len() == 1 => {
            let lu = rec(&args[0])?;
            if infinite(&lu) {
                let s = if is_pinf(&lu) { 1 } else { -1 };
                let half_pi = |k: i64| mul(vec![rat(k, 2), pi()]);
                return match (&**name, s) {
                    ("ln", 1) | ("sinh", 1) | ("cosh", _) | ("asinh", 1) | ("abs", _) | ("gamma", 1) | ("Ei", 1) | ("li", 1) | ("floor", 1) | ("ceil", 1) | ("round", 1) | ("W", 1) | ("acosh", 1) | ("factorial", 1) | ("lgamma", 1) | ("erfi", 1) | ("digamma", 1) => Some(inf()),
                    ("sinh", -1) | ("asinh", -1) | ("floor", -1) | ("ceil", -1) | ("round", -1) | ("erfi", -1) => Some(ninf()),
                    ("atan", _) | ("Si", _) => Some(half_pi(s as i64)),
                    ("tanh", _) | ("erf", _) | ("sign", _) => Some(int(s as i64)),
                    ("erfc", 1) | ("Ci", 1) | ("Ei", -1) | ("heaviside", -1) | ("dirac", _) | ("sinc", _) | ("airyai", 1) => Some(zero()),
                    ("erfc", -1) => Some(int(2)),
                    ("heaviside", 1) | ("zeta", 1) => Some(one()),
                    ("fresnels", _) | ("fresnelc", _) => Some(rat(s as i64, 2)),
                    _ => None,
                };
            }
            if lu.is_zero() {
                match &**name {
                    "ln" => {
                        // assume the argument is positive near the point
                        return Some(ninf());
                    }
                    "Ci" => return Some(ninf()),
                    _ => {}
                }
            }
            if DISCONTINUOUS.contains(&&**name) {
                return None;
            }
            let v = func(name, vec![lu]);
            if bad(&v) {
                None
            } else {
                Some(v)
            }
        }
        _ => None,
    }
    .or_else(|| {
        // quotient: L'Hôpital on numerator / denominator
        let (n, d) = g.numer_denom();
        if d.is_one() {
            return None;
        }
        lhopital(&n, &d, h, depth)
    })
}

fn sym_name(h: &Expr) -> &str {
    match h {
        Sym(n) => n,
        _ => "",
    }
}

fn lhopital(num: &Expr, den: &Expr, h: &Expr, depth: usize) -> Option<Expr> {
    if depth > 7 {
        return None;
    }
    let ln_ = lim0(num, h, depth + 1)?;
    let ld = lim0(den, h, depth + 1)?;
    let indeterminate = (ln_.is_zero() && ld.is_zero()) || (infinite(&ln_) && infinite(&ld));
    if !indeterminate {
        if infinite(&ld) {
            return if infinite(&ln_) { None } else { Some(zero()) };
        }
        if ld.is_zero() {
            // finite / 0: sign from numeric evaluation of the denominator
            let prog = crate::eval::Program::compile(den, &[sym_name(h)])?;
            let (s1, s2) = (prog.eval1(1e-6), prog.eval1(1e-9));
            if s1 * s2 <= 0.0 || s1.is_nan() || s2.is_nan() {
                return None;
            }
            let sd = if s1 > 0.0 { 1 } else { -1 };
            return signed_inf(sign_of(&ln_)? * sd);
        }
        if infinite(&ln_) {
            return signed_inf(sign_of(&ln_)? * sign_of(&ld)?);
        }
        let v = div(ln_, ld);
        return if bad(&v) { None } else { Some(v) };
    }
    let dn = diff(num, h);
    let dd = diff(den, h);
    if dd.is_zero() {
        return None;
    }
    let q = simplify::cancel(&div(dn, dd));
    if q.size() > 600 {
        return None;
    }
    lim0(&q, h, depth + 2)
}

/// Numeric estimate of the limit as h -> 0+.
fn numeric(g: &Expr, h: &Expr) -> Option<Expr> {
    let prog = crate::eval::Program::compile(g, &[sym_name(h)])?;
    let v = crate::numeric::nlimit(&|t| prog.eval1(t), 0.0, 1.0)?;
    if v.is_infinite() {
        return signed_inf(if v > 0.0 { 1 } else { -1 });
    }
    Some(tidy_float(v))
}

/// Replace floats that are obviously simple rationals by exact values.
pub fn tidy_float(v: f64) -> Expr {
    for den in [1i64, 2, 3, 4, 5, 6, 8, 10, 12] {
        let n = v * den as f64;
        if (n - n.round()).abs() < 1e-9 * (1.0 + n.abs()) && n.abs() < 1e9 {
            return rat(n.round() as i64, den);
        }
    }
    float(v)
}

// ---------------------------------------------------------------------------
// limits of functions of several variables
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum MultiLimit {
    /// The limit exists; `proof` explains why.
    Value { value: Expr, proof: String },
    /// Two approaches give different results.
    DoesNotExist { paths: Vec<(String, Option<Expr>)> },
    /// Every tested path agrees, but no proof was found.
    Likely { value: Expr, paths: Vec<(String, Option<Expr>)> },
    Unknown,
}

fn bounded_trig(e: &Expr, th: &Expr) -> bool {
    match e {
        _ if !e.has_sym(th) => e.free_symbols().is_empty() || true,
        Func(n, a) if matches!(&**n, "sin" | "cos") => a.len() == 1,
        Add(v) | Mul(v) => v.iter().all(|t| bounded_trig(t, th)),
        Pow(b, k) => k.as_i64().map_or(false, |k| k >= 0) && bounded_trig(b, th),
        _ => false,
    }
}

/// lim f(x, y) as (x, y) → (a, b).
pub fn limit_multi(f: &Expr, vars: &[Expr], point: &[Expr]) -> MultiLimit {
    if vars.len() != 2 || point.len() != 2 {
        return MultiLimit::Unknown;
    }
    let (x, y) = (&vars[0], &vars[1]);
    let (a, b) = (&point[0], &point[1]);
    // continuity: direct substitution
    let (_, den) = crate::simplify::numer_denom(&crate::simplify::together(f));
    let den_at = crate::simplify::simplify(&den.subs(x, a).subs(y, b));
    let den_ok = !bad(&den_at) && !den_at.is_zero() && den_at.to_f64_const().map_or(true, |v| v.abs() > 1e-300);
    let direct = crate::simplify::simplify(&f.subs(x, a).subs(y, b));
    let no_neg_powers = !f.has(&|n| matches!(n, Pow(base, k) if k.is_negative_term() && base.has_sym(x) | base.has_sym(y) && crate::simplify::simplify(&(**base).subs(x, a).subs(y, b)).is_zero()));
    if den_ok && no_neg_powers && !bad(&direct) && !f.has_any_fn(DISCONTINUOUS) {
        return MultiLimit::Value { value: direct, proof: "The function is continuous at the point, so the limit equals its value there.".into() };
    }
    // shift to the origin
    let (u, v) = (sym("\u{3b7}u"), sym("\u{3b7}v"));
    let g = f.subs(x, &add(vec![a.clone(), u.clone()])).subs(y, &add(vec![b.clone(), v.clone()]));
    let t = sym("\u{3b7}t");
    let xs = crate::print::text(&sub(x.clone(), a.clone()));
    let ys = crate::print::text(&sub(y.clone(), b.clone()));
    let path_lim = |gu: Expr| -> Option<Expr> {
        let l = limit(&gu, &t, &zero(), Dir::Right)?;
        let r = limit(&gu, &t, &zero(), Dir::Left)?;
        if same(&l, &r) {
            Some(l)
        } else {
            None
        }
    };
    let mut paths: Vec<(String, Option<Expr>)> = Vec::new();
    let mut add_path = |label: String, uu: Expr, vv: Expr| {
        let gu = crate::simplify::simplify(&g.subs(&u, &uu).subs(&v, &vv));
        paths.push((label, path_lim(gu)));
    };
    let dx = sub(x.clone(), a.clone());
    let dy = sub(y.clone(), b.clone());
    let lbl = |l: Expr, r: Expr| format!("along {}", crate::print::text(&eq(l, r)));
    add_path(lbl(dy.clone(), zero()), t.clone(), zero());
    add_path(lbl(dx.clone(), zero()), zero(), t.clone());
    for m in [1i64, -1, 2] {
        add_path(lbl(dy.clone(), mul(vec![int(m), dx.clone()])), t.clone(), mul(vec![int(m), t.clone()]));
    }
    add_path(lbl(dy.clone(), square(dx.clone())), t.clone(), square(t.clone()));
    add_path(lbl(dx.clone(), square(dy.clone())), square(t.clone()), t.clone());
    // general line y = m x with symbolic slope
    let mslope = sym("m");
    let line = crate::simplify::simplify(&g.subs(&u, &t).subs(&v, &mul(vec![mslope.clone(), t.clone()])));
    let line_lim = limit(&line, &t, &zero(), Dir::Right);
    let defined: Vec<&Expr> = paths.iter().filter_map(|p| p.1.as_ref()).collect();
    let disagree = paths.iter().any(|p| p.1.is_none()) || defined.windows(2).any(|w| !same(w[0], w[1]));
    if let Some(l) = &line_lim {
        if l.has_sym(&mslope) {
            let mut ps = paths.clone();
            ps.push((format!("along {} (slope m)", crate::print::text(&eq(sub(y.clone(), b.clone()), mul(vec![mslope.clone(), sub(x.clone(), a.clone())])))), Some(l.clone())));
            return MultiLimit::DoesNotExist { paths: ps };
        }
    }
    if disagree {
        return MultiLimit::DoesNotExist { paths };
    }
    let Some(cand) = defined.first().map(|e| (*e).clone()) else { return MultiLimit::Unknown };
    // polar proof: |g - L| ≤ C r^p, p > 0
    let (r, th) = (sym("\u{3b7}r"), sym("θ"));
    let gp = crate::simplify::simplify(&sub(g.subs(&u, &mul(vec![r.clone(), f1("cos", th.clone())])).subs(&v, &mul(vec![r.clone(), f1("sin", th.clone())])), cand.clone()));
    if gp.is_zero() {
        return MultiLimit::Value { value: cand, proof: "In polar coordinates the function is constant.".into() };
    }
    if let Some((c, p)) = super::series::leading_term(&gp, &r) {
        use num_traits::Signed;
        if c.is_zero() || (p.is_positive() && bounded_trig(&c, &th) && !c.has_sym(&sym(super::series::LOGH))) {
            let proof = format!(
                "In polar coordinates ({xs} = r cos θ, {ys} = r sin θ), f − {} behaves like r^{} times a bounded function of θ, which tends to 0 as r → 0 for every θ.",
                crate::print::text(&cand),
                crate::print::text(&Num(p))
            );
            return MultiLimit::Value { value: cand, proof };
        }
    }
    MultiLimit::Likely { value: cand, paths }
}
