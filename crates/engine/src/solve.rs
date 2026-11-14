//! Equation, system and inequality solving.

use crate::budget;
use crate::calculus::diff::diff;
use crate::eval::Program;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::poly::{self, QPoly};
use crate::simplify;
use num_complex::Complex64;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};

/// Solutions of one equation in one unknown.
#[derive(Clone, Debug, Default)]
pub struct RootSet {
    /// Distinct roots (exact where possible).  Roots in a periodic family
    /// contain the integer parameter [`PARAM`].
    pub roots: Vec<Expr>,
    /// Roots were only found numerically (in a scanned range).
    pub numeric: bool,
    /// The solution set involves the integer parameter n.
    pub periodic: bool,
    /// The equation holds for every value.
    pub identity: bool,
    /// The search for further solutions was incomplete.
    pub partial: bool,
}

/// Integer parameter name for periodic families.
pub const PARAM: &str = "n";

pub fn param() -> Expr {
    sym(PARAM)
}

fn is_real_value(e: &Expr) -> bool {
    if e.has_sym(&param()) {
        // a family is real when its n = 0 member is real
        return is_real_value(&e.subs(&param(), &zero()));
    }
    match crate::eval::const_c64(e) {
        Some(z) => z.im.abs() <= 1e-10 * (1.0 + z.re.abs()),
        None => !e.has_const(K::I),
    }
}

impl RootSet {
    pub fn real(&self) -> Vec<Expr> {
        self.roots.iter().filter(|r| is_real_value(r)).cloned().collect()
    }
    pub fn complex(&self) -> Vec<Expr> {
        self.roots.iter().filter(|r| !is_real_value(r)).cloned().collect()
    }
}

fn push_root(out: &mut Vec<Expr>, r: Expr) {
    let r = if r.size() < 400 { simplify::simplify(&r) } else { r };
    if r.is_undef() || r.is_inf() || r.has(&|n| matches!(n, Const(K::CInf | K::Undef))) {
        return;
    }
    for q in out.iter() {
        if *q == r {
            return;
        }
        if let (Some(a), Some(b)) = (crate::eval::const_c64(q), crate::eval::const_c64(&r)) {
            if (a - b).norm() <= 1e-9 * (1.0 + a.norm()) && !q.has_sym(&param()) && !r.has_sym(&param()) {
                return;
            }
        }
    }
    out.push(r);
}

/// Sort roots: real ones ascending, then complex by real part / imaginary part.
fn sort_roots(v: &mut [Expr]) {
    v.sort_by(|a, b| {
        let za = crate::eval::const_c64(&a.subs(&param(), &zero())).unwrap_or(Complex64::new(f64::MAX, 0.0));
        let zb = crate::eval::const_c64(&b.subs(&param(), &zero())).unwrap_or(Complex64::new(f64::MAX, 0.0));
        let ra = za.im.abs() < 1e-10;
        let rb = zb.im.abs() < 1e-10;
        rb.cmp(&ra).then(za.re.partial_cmp(&zb.re).unwrap_or(std::cmp::Ordering::Equal)).then(za.im.partial_cmp(&zb.im).unwrap_or(std::cmp::Ordering::Equal))
    });
}

/// Turn `a = b` (or a bare expression meaning `= 0`) into `a - b`.
pub fn as_zero(eq: &Expr) -> Expr {
    match eq {
        Rel(RelOp::Eq, a, b) => sub((**a).clone(), (**b).clone()),
        _ => eq.clone(),
    }
}

/// Solve `eq` (an equation or expression = 0) for `x`.
pub fn solve(eq: &Expr, x: &Expr) -> RootSet {
    let f = as_zero(eq);
    let mut rs = budget::attempt(budget::remaining().min(15.0) * 0.7, || solve0(&f, x, 0)).unwrap_or_default();
    if rs.roots.is_empty() && !rs.identity {
        // numeric scan as a last resort
        let num = numeric_real_roots(&f, x);
        if !num.is_empty() {
            rs.roots = num;
            rs.numeric = true;
            rs.partial = true;
        }
    }
    if !rs.periodic {
        sort_roots(&mut rs.roots);
    }
    rs
}

fn solve0(f: &Expr, x: &Expr, depth: usize) -> RootSet {
    budget::tick();
    let mut rs = RootSet::default();
    if depth > 6 {
        return rs;
    }
    if !f.has_sym(x) {
        rs.identity = simplify::is_zero(f) == Some(true);
        return rs;
    }
    // f = N/D
    let t = simplify::together(f);
    let (n, d) = simplify::numer_denom(&t);
    let n = simplify::expand(&n);
    if n.is_zero() {
        rs.identity = true;
        return rs;
    }
    let candidates = if let Some(cs) = poly::coeffs(&n, x) {
        polynomial_roots(&cs)
    } else if let Some(r) = by_kernel(&n, x, depth) {
        if r.periodic {
            rs.periodic = true;
        }
        rs.partial |= r.partial;
        r.roots
    } else if let Some(r) = isolate(&n, x, depth) {
        rs.periodic |= r.periodic;
        rs.partial |= r.partial;
        r.roots
    } else if let Some(r) = radical_equation(&n, x, depth) {
        r
    } else {
        // factor and solve each factor
        let fl = simplify::factor(&n);
        if let Mul(fs) = &fl {
            let mut all = Vec::new();
            let mut any = false;
            for g in fs {
                let (b, _) = g.base_exp();
                if b.has_sym(x) {
                    let r = solve0(&b, x, depth + 1);
                    rs.periodic |= r.periodic;
                    rs.partial |= r.partial;
                    rs.numeric |= r.numeric;
                    any |= !r.roots.is_empty();
                    all.extend(r.roots);
                }
            }
            if any {
                all
            } else {
                rs.partial = true;
                vec![]
            }
        } else {
            rs.partial = true;
            vec![]
        }
    };
    for r in candidates {
        // reject roots that make the denominator (or the original) undefined
        if !d.is_one() && !r.has_sym(&param()) {
            let dv = simplify::simplify(&d.subs(x, &r));
            if dv.is_zero() || numerically_zero(&dv) {
                continue;
            }
        }
        if r.has(&|e| matches!(e, Float(_))) {
            rs.numeric = true;
        }
        push_root(&mut rs.roots, r);
    }
    rs
}

fn numerically_zero(e: &Expr) -> bool {
    crate::eval::const_c64(e).map_or(false, |z| z.norm() < 1e-12)
}

// ---------------------------------------------------------------------------
// polynomials
// ---------------------------------------------------------------------------

/// Roots of Σ cs[k] x^k (coefficients may be symbolic).
pub fn polynomial_roots(cs: &[Expr]) -> Vec<Expr> {
    let mut cs: Vec<Expr> = cs.to_vec();
    while cs.len() > 1 && simplify::is_zero(cs.last().unwrap()) == Some(true) {
        cs.pop();
    }
    let deg = cs.len() - 1;
    let mut out = Vec::new();
    if deg == 0 {
        return out;
    }
    // x^k factor
    let low = cs.iter().take_while(|c| c.is_zero()).count();
    if low > 0 {
        out.push(zero());
        let rest = polynomial_roots(&cs[low..]);
        for r in rest {
            push_root(&mut out, r);
        }
        return out;
    }
    let numeric_coeffs = cs.iter().all(|c| c.as_rat().is_some());
    if numeric_coeffs {
        let p = QPoly::new(cs.iter().map(|c| c.as_rat().unwrap().clone()).collect());
        if deg >= 3 && deg <= 24 && (1..deg).all(|k| p.coeff(k).is_zero()) {
            for r in nth_roots(&(-p.coeff(0) / p.coeff(deg)), deg as i64) {
                push_root(&mut out, r);
            }
            return out;
        }
        let (_, factors) = poly::factor(&p);
        for (q, _) in factors {
            for r in qpoly_roots(&q) {
                push_root(&mut out, r);
            }
        }
        return out;
    }
    // symbolic coefficients
    match deg {
        1 => out.push(simplify::simplify(&div(neg(cs[0].clone()), cs[1].clone()))),
        2 => {
            for r in quadratic_formula(&cs[2], &cs[1], &cs[0]) {
                push_root(&mut out, r);
            }
        }
        _ => {
            // binomial a x^n + b
            if cs[1..deg].iter().all(|c| c.is_zero()) {
                let c = div(neg(cs[0].clone()), cs[deg].clone());
                for k in 0..deg as i64 {
                    let w = exp(mul(vec![int(2), pi(), i(), rat(k, deg as i64)]));
                    push_root(&mut out, mul(vec![pow(c.clone(), rat(1, deg as i64)), w]));
                }
            } else if let Some(f) = factor_symbolic(&cs) {
                return f;
            }
        }
    }
    out
}

fn factor_symbolic(cs: &[Expr]) -> Option<Vec<Expr>> {
    let x = sym("\u{3c7}p");
    let p = add(cs.iter().enumerate().map(|(k, c)| mul(vec![c.clone(), pow(x.clone(), int(k as i64))])).collect());
    let (_, fs) = simplify::factor_list(&p);
    if fs.len() < 2 && fs.iter().all(|(_, m)| *m == 1) {
        return None;
    }
    let mut out = Vec::new();
    for (g, _) in fs {
        if let Some(c) = poly::coeffs(&g, &x) {
            if c.len() - 1 >= cs.len() - 1 {
                return None;
            }
            for r in polynomial_roots(&c) {
                push_root(&mut out, r);
            }
        }
    }
    Some(out)
}

pub fn quadratic_formula(a: &Expr, b: &Expr, c: &Expr) -> Vec<Expr> {
    let disc = simplify::expand(&sub(square(b.clone()), mul(vec![int(4), a.clone(), c.clone()])));
    let s = sqrt(disc.clone());
    let two_a = mul(vec![int(2), a.clone()]);
    if disc.is_zero() {
        return vec![div(neg(b.clone()), two_a)];
    }
    vec![div(sub(neg(b.clone()), s.clone()), two_a.clone()), div(add(vec![neg(b.clone()), s]), two_a)]
}

/// Roots of an irreducible rational polynomial.
fn qpoly_roots(p: &QPoly) -> Vec<Expr> {
    let x = sym("x");
    let d = p.degree();
    let c = |k: usize| Num(p.coeff(k));
    match d {
        0 => vec![],
        1 => vec![Num(-p.coeff(0) / p.coeff(1))],
        2 => quadratic_formula(&c(2), &c(1), &c(0)),
        3 => cubic(p),
        _ => {
            // binomial x^n = r
            if (1..d).all(|k| p.coeff(k).is_zero()) {
                return nth_roots(&(-p.coeff(0) / p.coeff(d)), d as i64);
            }
            // even polynomial: substitute y = x^2
            if d % 2 == 0 && (0..=d).all(|k| k % 2 == 0 || p.coeff(k).is_zero()) {
                let q = QPoly::new((0..=d / 2).map(|k| p.coeff(2 * k)).collect());
                let mut out = Vec::new();
                let (_, fs) = poly::factor(&q);
                for (g, _) in fs {
                    for y in qpoly_roots(&g) {
                        let s = sqrt(y);
                        out.push(neg(s.clone()));
                        out.push(s);
                    }
                }
                if out.iter().all(|r| r.size() < 60) {
                    return out;
                }
            }
            let _ = &x;
            p.roots_c().into_iter().map(complex_float).collect()
        }
    }
}

/// All n-th roots of a rational number, written with cos/sin special values.
pub fn nth_roots(r: &Rat, n: i64) -> Vec<Expr> {
    let modulus = pow(Num(r.abs()), rat(1, n));
    let base = if r.is_negative() { Rat::new(Int::one(), Int::from(n)) } else { Rat::zero() };
    (0..n)
        .map(|k| {
            // angle = π (base + 2k/n)
            let q = base.clone() + Rat::new(Int::from(2 * k), Int::from(n));
            let c = crate::funcs::cos_pi(&q).unwrap_or_else(|| f1("cos", mul(vec![Num(q.clone()), pi()])));
            let s = crate::funcs::sin_pi(&q).unwrap_or_else(|| f1("sin", mul(vec![Num(q.clone()), pi()])));
            simplify::simplify(&mul(vec![modulus.clone(), add(vec![c, mul(vec![i(), s])])]))
        })
        .collect()
}

fn complex_float(z: Complex64) -> Expr {
    let re = crate::eval::chop(z.re);
    let im = if z.im.abs() < 1e-12 * (1.0 + z.re.abs()) { 0.0 } else { z.im };
    if im == 0.0 {
        float(re)
    } else {
        add(vec![float(re), mul(vec![float(im), i()])])
    }
}

/// Cardano / trigonometric solution of an irreducible cubic.
fn cubic(p: &QPoly) -> Vec<Expr> {
    let a = p.lc();
    let b = p.coeff(2) / a.clone();
    let c = p.coeff(1) / a.clone();
    let d = p.coeff(0) / a;
    // x = t - b/3 ; t^3 + P t + Q = 0
    let three = Rat::from_integer(Int::from(3));
    let pp = c.clone() - b.clone() * b.clone() / three.clone();
    let qq = Rat::from_integer(Int::from(2)) * b.clone() * b.clone() * b.clone() / Rat::from_integer(Int::from(27)) - b.clone() * c / three.clone() + d;
    let shift = Num(-b / three);
    let disc = qq.clone() * qq.clone() / Rat::from_integer(Int::from(4)) + pp.clone() * pp.clone() * pp.clone() / Rat::from_integer(Int::from(27));
    let pe = Num(pp.clone());
    let qe = Num(qq.clone());
    if disc.is_negative() {
        // three real roots
        let m = mul(vec![int(2), sqrt(div(neg(pe.clone()), int(3)))]);
        let theta = mul(vec![rat(1, 3), f1("acos", mul(vec![div(mul(vec![int(3), qe]), mul(vec![int(2), pe.clone()])), sqrt(div(int(-3), pe))]))]);
        (0..3).map(|k| add(vec![mul(vec![m.clone(), f1("cos", sub(theta.clone(), mul(vec![rat(2 * k, 3), pi()])))]), shift.clone()])).collect()
    } else {
        let sd = sqrt(Num(disc));
        let real_cbrt = |a: Expr| -> Expr {
            let a = simplify::simplify(&a);
            if a.to_f64_const().map_or(false, |v| v < 0.0) {
                neg(pow(simplify::simplify(&neg(a)), rat(1, 3)))
            } else {
                pow(a, rat(1, 3))
            }
        };
        let u = real_cbrt(add(vec![div(neg(qe.clone()), int(2)), sd.clone()]));
        let v = real_cbrt(sub(div(neg(qe), int(2)), sd));
        let w = div(add(vec![int(-1), mul(vec![i(), sqrt(int(3))])]), int(2));
        let w2 = div(sub(int(-1), mul(vec![i(), sqrt(int(3))])), int(2));
        vec![
            add(vec![u.clone(), v.clone(), shift.clone()]),
            add(vec![mul(vec![w.clone(), u.clone()]), mul(vec![w2.clone(), v.clone()]), shift.clone()]),
            add(vec![mul(vec![w2, u]), mul(vec![w, v]), shift]),
        ]
    }
}

// ---------------------------------------------------------------------------
// substitution through a kernel g(x)
// ---------------------------------------------------------------------------

fn kernels(e: &Expr, x: &Expr, out: &mut Vec<Expr>) {
    if !e.has_sym(x) {
        return;
    }
    match e {
        Sym(_) => {
            if !out.contains(e) {
                out.push(e.clone());
            }
        }
        Add(v) | Mul(v) => v.iter().for_each(|t| kernels(t, x, out)),
        Pow(b, ex) => {
            if ex.is_integer() {
                kernels(b, x, out);
            } else if !out.contains(e) {
                out.push(e.clone());
            }
        }
        _ => {
            if !out.contains(e) {
                out.push(e.clone());
            }
        }
    }
}

/// Equations polynomial in a single kernel such as e^x, ln(x), sin(x).
fn by_kernel(n: &Expr, x: &Expr, depth: usize) -> Option<RootSet> {
    let mut ks = Vec::new();
    kernels(n, x, &mut ks);
    if ks.is_empty() {
        return None;
    }
    let t = sym(&format!("\u{3c4}{depth}"));
    // unify exponentials b^(a_i x + c_i)
    let exps: Vec<&Expr> = ks.iter().filter(|k| matches!(k, Pow(b, ex) if !b.has_sym(x) && ex.has_sym(x))).collect();
    if exps.len() == ks.len() && exps.len() >= 1 {
        let base = match exps[0] {
            Pow(b, _) => (**b).clone(),
            _ => unreachable!(),
        };
        let mut lin = Vec::new();
        for k in &exps {
            let Pow(b, ex) = k else { return None };
            if **b != base {
                return None;
            }
            let cs = poly::coeffs(ex, x)?;
            if cs.len() != 2 {
                return None;
            }
            lin.push((cs[1].clone(), cs[0].clone()));
        }
        let a0 = lin[0].0.clone();
        let mut ratios = Vec::new();
        let mut q = Int::one();
        for (a, _) in &lin {
            let r = simplify::simplify(&div(a.clone(), a0.clone()));
            let r = r.as_rat()?.clone();
            q = q.lcm(r.denom());
            ratios.push(r);
        }
        let unit = div(a0.clone(), big(q.clone())); // t = base^(unit x)
        let mut sub_e = n.clone();
        for (k, (r, (_, c))) in exps.iter().zip(ratios.iter().zip(lin.iter())) {
            let m = (r * Rat::from_integer(q.clone())).to_integer();
            sub_e = sub_e.subs(k, &mul(vec![pow(base.clone(), c.clone()), pow(t.clone(), big(m))]));
        }
        if sub_e.has_sym(x) {
            return None;
        }
        let inner = solve0(&sub_e, &t, depth + 1);
        let mut out = RootSet::default();
        for tv in inner.roots {
            // base^(unit x) = tv  ->  x = log(tv)/(unit log(base)) (+ 2πi n)
            let lb = if matches!(base, Const(K::E)) { one() } else { ln(base.clone()) };
            let xv = div(ln(tv.clone()), mul(vec![unit.clone(), lb.clone()]));
            push_root(&mut out.roots, xv);
        }
        return Some(out);
    }
    if ks.len() == 1 {
        let k = ks[0].clone();
        if k == *x {
            return None;
        }
        let sub_e = n.subs(&k, &t);
        if sub_e.has_sym(x) {
            return None;
        }
        // already linear in the kernel: isolation handles it directly
        if poly::degree(&sub_e, &t).map_or(false, |d| d <= 1) {
            return None;
        }
        let inner = solve0(&sub_e, &t, depth + 1);
        let mut out = RootSet { partial: inner.partial, ..Default::default() };
        for tv in inner.roots {
            let r = solve0(&sub(k.clone(), tv), x, depth + 1);
            out.periodic |= r.periodic;
            out.partial |= r.partial;
            for v in r.roots {
                push_root(&mut out.roots, v);
            }
        }
        return Some(out);
    }
    // sin and cos (and tan) of the same argument: tan half-angle substitution
    let mut arg: Option<Expr> = None;
    for k in &ks {
        match k {
            Func(name, a) if matches!(&**name, "sin" | "cos" | "tan") && a.len() == 1 => {
                if arg.as_ref().map_or(false, |g| *g != a[0]) {
                    return None;
                }
                arg = Some(a[0].clone());
            }
            Pow(b, _) if matches!(&**b, Func(name, a) if matches!(&**name, "sin" | "cos" | "tan") && a.len() == 1 && arg.as_ref().map_or(true, |g| *g == a[0])) => {}
            _ => return None,
        }
    }
    let u = arg?;
    let t2 = square(t.clone());
    let den = add(vec![one(), t2.clone()]);
    let sub_e = n
        .subs(&f1("sin", u.clone()), &div(mul(vec![int(2), t.clone()]), den.clone()))
        .subs(&f1("cos", u.clone()), &div(sub(one(), t2.clone()), den.clone()))
        .subs(&f1("tan", u.clone()), &div(mul(vec![int(2), t.clone()]), sub(one(), t2)));
    if sub_e.has_sym(x) {
        return None;
    }
    let inner = solve0(&sub_e, &t, depth + 1);
    let mut out = RootSet { periodic: true, ..Default::default() };
    for tv in inner.roots {
        if !is_real_value(&tv) {
            continue;
        }
        // u/2 = atan(t) + π n
        let r = solve0(&sub(u.clone(), add(vec![mul(vec![int(2), f1("atan", tv)]), mul(vec![int(2), pi(), param()])])), x, depth + 1);
        for v in r.roots {
            push_root(&mut out.roots, v);
        }
    }
    // the substitution misses u = π (+ 2πn)
    let at_pi = n.subs(&u, &pi());
    if simplify::is_zero(&simplify::simplify(&at_pi.subs(x, &x.clone()))) == Some(true) || (!at_pi.has_sym(x) && simplify::simplify(&at_pi).is_zero()) {
        let r = solve0(&sub(u.clone(), add(vec![pi(), mul(vec![int(2), pi(), param()])])), x, depth + 1);
        for v in r.roots {
            push_root(&mut out.roots, v);
        }
    }
    Some(out)
}

// ---------------------------------------------------------------------------
// isolating a single occurrence
// ---------------------------------------------------------------------------

fn occurrences(e: &Expr, x: &Expr) -> usize {
    if e == x {
        return 1;
    }
    match e {
        Add(v) | Mul(v) | List(v) => v.iter().map(|t| occurrences(t, x)).sum(),
        Pow(b, ex) => occurrences(b, x) + occurrences(ex, x),
        Func(_, a) => a.iter().map(|t| occurrences(t, x)).sum(),
        _ => 0,
    }
}

/// Solve `lhs = 0` by peeling off operations around the only occurrence of x.
fn isolate(f: &Expr, x: &Expr, depth: usize) -> Option<RootSet> {
    if occurrences(f, x) != 1 {
        return None;
    }
    let mut out = RootSet::default();
    let mut goals: Vec<(Expr, Expr)> = vec![(f.clone(), zero())];
    let mut steps = 0;
    while let Some((lhs, rhs)) = goals.pop() {
        steps += 1;
        if steps > 200 {
            out.partial = true;
            break;
        }
        if lhs == *x {
            push_root(&mut out.roots, rhs);
            continue;
        }
        match &lhs {
            Add(v) => {
                let (with, without): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|t| t.has_sym(x));
                goals.push((with.into_iter().next()?, sub(rhs, add(without))));
            }
            Mul(v) => {
                let (with, without): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|t| t.has_sym(x));
                let c = mul(without);
                if c.is_zero() {
                    return None;
                }
                goals.push((with.into_iter().next()?, div(rhs, c)));
            }
            Pow(b, ex) => {
                if b.has_sym(x) {
                    if rhs.is_zero() {
                        if ex.to_f64_const().map_or(false, |v| v > 0.0) {
                            goals.push(((**b).clone(), zero()));
                        }
                        continue;
                    }
                    match ex.as_rat() {
                        Some(r) if r.is_integer() => {
                            let n = r.to_integer().to_i64()?;
                            if n.abs() > 64 {
                                return None;
                            }
                            let m = n.abs();
                            let rr = if n < 0 { pow(rhs.clone(), int(-1)) } else { rhs.clone() };
                            for k in 0..m {
                                let w = exp(mul(vec![int(2), pi(), i(), rat(k, m)]));
                                goals.push(((**b).clone(), mul(vec![pow(rr.clone(), rat(1, m)), w])));
                            }
                        }
                        Some(r) => {
                            // b^(p/q) = rhs  -> b = rhs^(q/p), principal branch; check later
                            let inv = Num(r.recip());
                            let cand = pow(rhs.clone(), inv);
                            goals.push(((**b).clone(), cand));
                        }
                        None => {
                            goals.push(((**b).clone(), pow(rhs.clone(), pow((**ex).clone(), int(-1)))));
                        }
                    }
                } else {
                    // b^g = rhs  -> g = log(rhs)/log(b)
                    if rhs.is_zero() {
                        continue;
                    }
                    let lb = if matches!(**b, Const(K::E)) { one() } else { ln((**b).clone()) };
                    goals.push(((**ex).clone(), div(ln(rhs.clone()), lb)));
                }
            }
            Func(name, args) if args.len() == 1 => {
                let g = args[0].clone();
                let n = param();
                let two_pi_n = mul(vec![int(2), pi(), n.clone()]);
                match &**name {
                    "sin" => {
                        out.periodic = true;
                        let a = f1("asin", rhs.clone());
                        goals.push((g.clone(), add(vec![a.clone(), two_pi_n.clone()])));
                        goals.push((g, add(vec![sub(pi(), a), two_pi_n])));
                    }
                    "cos" => {
                        out.periodic = true;
                        let a = f1("acos", rhs.clone());
                        goals.push((g.clone(), add(vec![a.clone(), two_pi_n.clone()])));
                        goals.push((g, add(vec![neg(a), two_pi_n])));
                    }
                    "tan" => {
                        out.periodic = true;
                        goals.push((g, add(vec![f1("atan", rhs), mul(vec![pi(), n])])));
                    }
                    "ln" => goals.push((g, exp(rhs))),
                    "asin" => goals.push((g, f1("sin", rhs))),
                    "acos" => goals.push((g, f1("cos", rhs))),
                    "atan" => goals.push((g, f1("tan", rhs))),
                    "sinh" => goals.push((g, f1("asinh", rhs))),
                    "cosh" => {
                        let a = f1("acosh", rhs);
                        goals.push((g.clone(), a.clone()));
                        goals.push((g, neg(a)));
                    }
                    "tanh" => goals.push((g, f1("atanh", rhs))),
                    "asinh" => goals.push((g, f1("sinh", rhs))),
                    "acosh" => goals.push((g, f1("cosh", rhs))),
                    "atanh" => goals.push((g, f1("tanh", rhs))),
                    "abs" => {
                        if rhs.to_f64_const().map_or(false, |v| v < 0.0) {
                            continue;
                        }
                        goals.push((g.clone(), rhs.clone()));
                        goals.push((g, neg(rhs)));
                    }
                    "W" => goals.push((g, mul(vec![rhs.clone(), exp(rhs)]))),
                    "cbrt" => goals.push((g, pow(rhs, int(3)))),
                    "erf" => goals.push((g, f1("erfinv", rhs))),
                    _ => return None,
                }
            }
            _ => return None,
        }
    }
    // verify (removes extraneous roots from fractional powers)
    out.roots.retain(|r| r.has_sym(&param()) || check_root(f, x, r));
    let _ = depth;
    Some(out)
}

/// Numerically confirm that `r` solves `f = 0`.
fn check_root(f: &Expr, x: &Expr, r: &Expr) -> bool {
    let Some(z) = crate::eval::const_c64(r) else { return true };
    let Sym(name) = x else { return true };
    let Some(p) = Program::compile(f, &[name]) else { return true };
    let v = p.eval_c(&[z]);
    if v.re.is_nan() {
        return false;
    }
    let scale = 1.0 + z.norm();
    v.norm() <= 1e-7 * scale.max(1.0) * 10.0
}

// ---------------------------------------------------------------------------
// radical equations
// ---------------------------------------------------------------------------

fn radical_equation(f: &Expr, x: &Expr, depth: usize) -> Option<Vec<Expr>> {
    if depth > 4 {
        return None;
    }
    let terms = f.terms();
    let is_rad = |t: &Expr| t.has(&|n| matches!(n, Pow(b, e) if b.has_sym(x) && e.as_rat().map_or(false, |r| !r.is_integer())));
    let (rad, rest): (Vec<Expr>, Vec<Expr>) = terms.into_iter().partition(|t| is_rad(t));
    if rad.is_empty() {
        return None;
    }
    // isolate the first radical term and raise to the power of its index
    let first = rad[0].clone();
    let others: Vec<Expr> = rad[1..].iter().cloned().chain(rest).collect();
    let mut q = 2i64;
    first.has(&|n| {
        if let Pow(_, e) = n {
            if let Some(r) = e.as_rat() {
                if !r.is_integer() {
                    // first fractional exponent found decides the index
                }
            }
        }
        false
    });
    if let Some(r) = find_frac(&first) {
        q = r;
    }
    let lhs = pow(first, int(q));
    let rhs = pow(neg(add(others)), int(q));
    let g = simplify::expand(&sub(lhs, rhs));
    let rs = solve0(&g, x, depth + 1);
    let cands: Vec<Expr> = rs.roots.into_iter().filter(|r| check_root(f, x, r)).collect();
    Some(cands)
}

fn find_frac(e: &Expr) -> Option<i64> {
    match e {
        Pow(b, ex) => {
            if let Some(r) = ex.as_rat() {
                if !r.is_integer() {
                    return r.denom().to_i64();
                }
            }
            find_frac(b)
        }
        Mul(v) | Add(v) => v.iter().find_map(find_frac),
        Func(_, a) => a.iter().find_map(find_frac),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// numeric root finding
// ---------------------------------------------------------------------------

/// Real roots found by scanning; tries growing ranges.
pub fn numeric_real_roots(f: &Expr, x: &Expr) -> Vec<Expr> {
    let Sym(name) = x else { return vec![] };
    let free = f.free_symbols();
    if free.len() != 1 {
        return vec![];
    }
    let Some(p) = Program::compile(f, &[name]) else { return vec![] };
    let mut out: Vec<f64> = Vec::new();
    for range in [10.0, 100.0, 1000.0] {
        let roots = crate::numeric::find_roots(&|t| p.eval1(t), -range, range, 4000);
        for r in roots {
            let v = p.eval1(r);
            if v.is_finite() && v.abs() < 1e-6 && !out.iter().any(|q| (q - r).abs() < 1e-8 * (1.0 + r.abs())) {
                out.push(r);
            }
        }
        if !out.is_empty() {
            break;
        }
    }
    out.sort_by(|a, b| a.partial_cmp(b).unwrap());
    out.into_iter().take(50).map(|r| float(crate::eval::chop(r))).collect()
}

/// Real roots of `f` in [a, b] (used by plots and inequalities).
pub fn real_roots_in(f: &Expr, x: &Expr, a: f64, b: f64) -> Vec<f64> {
    let Sym(name) = x else { return vec![] };
    let Some(p) = Program::compile(f, &[name]) else { return vec![] };
    crate::numeric::find_roots(&|t| p.eval1(t), a, b, 2000)
}

// ---------------------------------------------------------------------------
// systems
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, Default)]
pub struct SystemSolution {
    /// Each solution assigns values to (some of) the variables.
    pub sols: Vec<Vec<(Expr, Expr)>>,
    pub numeric: bool,
    /// Infinitely many solutions (some variables are free).
    pub underdetermined: bool,
    pub inconsistent: bool,
}

fn is_linear(e: &Expr, vars: &[Expr]) -> bool {
    let mut gens = poly::Gens(vars.to_vec());
    let r = poly::to_ratfn(e, &mut gens);
    if r.den.as_constant().is_none() {
        // allow symbolic constants in denominators that do not involve vars
        let (_, d) = simplify::numer_denom(e);
        if vars.iter().any(|v| d.has_sym(v)) {
            return false;
        }
    }
    let ex = simplify::expand(e);
    for t in ex.terms() {
        let mut deg = 0;
        for f in t.factors() {
            for v in vars {
                if f == *v {
                    deg += 1;
                } else if f.has_sym(v) {
                    return false;
                }
            }
        }
        if deg > 1 {
            return false;
        }
    }
    true
}

/// Solve a system of equations for `vars`.
pub fn solve_system(eqs: &[Expr], vars: &[Expr]) -> SystemSolution {
    let fs: Vec<Expr> = eqs.iter().map(|e| simplify::expand(&as_zero(e))).filter(|e| !e.is_zero()).collect();
    if fs.iter().all(|f| is_linear(f, vars)) {
        return linear_system(&fs, vars);
    }
    if let Some(s) = budget::attempt_opt(budget::remaining().min(12.0) * 0.6, || substitution_system(&fs, vars, 0)) {
        if !s.sols.is_empty() {
            return s;
        }
    }
    numeric_system(&fs, vars)
}

/// Gaussian elimination with exact (possibly symbolic) coefficients.
pub fn linear_system(fs: &[Expr], vars: &[Expr]) -> SystemSolution {
    let n = vars.len();
    let mut rows: Vec<Vec<Expr>> = Vec::new();
    for f in fs {
        let mut row = vec![zero(); n + 1];
        let ex = simplify::expand(f);
        for t in ex.terms() {
            match vars.iter().position(|v| t.has_sym(v)) {
                Some(j) => row[j] = add(vec![row[j].clone(), div(t.clone(), vars[j].clone())]),
                None => row[n] = add(vec![row[n].clone(), neg(t)]),
            }
        }
        rows.push(row.into_iter().map(|c| simplify::cancel(&c)).collect());
    }
    let (rref, pivots) = rref(rows, n);
    let mut sol = SystemSolution::default();
    for row in &rref {
        if row[..n].iter().all(|c| c.is_zero()) && !row[n].is_zero() {
            sol.inconsistent = true;
            return sol;
        }
    }
    let mut assign = Vec::new();
    for (r, &c) in pivots.iter().enumerate() {
        let mut val = vec![rref[r][n].clone()];
        for j in 0..n {
            if j != c && !pivots.contains(&j) && !rref[r][j].is_zero() {
                val.push(neg(mul(vec![rref[r][j].clone(), vars[j].clone()])));
            }
        }
        assign.push((vars[c].clone(), simplify::simplify(&add(val))));
    }
    sol.underdetermined = pivots.len() < n;
    sol.sols.push(assign);
    sol
}

/// Reduced row echelon form over expressions; returns pivot columns.
pub fn rref(mut m: Vec<Vec<Expr>>, ncols: usize) -> (Vec<Vec<Expr>>, Vec<usize>) {
    let mut pivots = Vec::new();
    let mut r = 0;
    for c in 0..ncols {
        if r >= m.len() {
            break;
        }
        let Some(p) = (r..m.len()).find(|&i| simplify::is_zero(&m[i][c]) != Some(true) && !m[i][c].is_zero()) else { continue };
        m.swap(r, p);
        let piv = m[r][c].clone();
        let inv = pow(piv, int(-1));
        m[r] = m[r].iter().map(|v| simplify::cancel(&mul(vec![v.clone(), inv.clone()]))).collect();
        for i in 0..m.len() {
            if i != r && !m[i][c].is_zero() {
                let fct = m[i][c].clone();
                let pr = m[r].clone();
                m[i] = m[i].iter().zip(pr.iter()).map(|(a, b)| simplify::cancel(&sub(a.clone(), mul(vec![fct.clone(), b.clone()])))).collect();
            }
        }
        pivots.push(c);
        r += 1;
    }
    (m, pivots)
}

/// Eliminate variables one at a time by solving an equation for one of them.
fn substitution_system(fs: &[Expr], vars: &[Expr], depth: usize) -> Option<SystemSolution> {
    if depth > 6 || fs.is_empty() {
        return None;
    }
    if vars.len() == 1 {
        // all equations in one variable: common roots
        let rs = solve(&fs[0], &vars[0]);
        let mut sol = SystemSolution { numeric: rs.numeric, ..Default::default() };
        for r in rs.roots {
            if fs[1..].iter().all(|g| check_root(g, &vars[0], &r)) {
                sol.sols.push(vec![(vars[0].clone(), r)]);
            }
        }
        return Some(sol);
    }
    // choose the equation/variable pair with the lowest degree
    let mut best: Option<(usize, usize, usize)> = None;
    for (i, f) in fs.iter().enumerate() {
        for (j, v) in vars.iter().enumerate() {
            if !f.has_sym(v) {
                continue;
            }
            let d = poly::degree(f, v).unwrap_or(99);
            if best.map_or(true, |b| d < b.2) {
                best = Some((i, j, d));
            }
        }
    }
    let (i, j, d) = best?;
    if d > 4 {
        // try a resultant to eliminate instead
        return resultant_system(fs, vars, depth);
    }
    let v = vars[j].clone();
    let rs = solve(&fs[i], &v);
    if rs.periodic || rs.roots.is_empty() {
        return None;
    }
    let rest_vars: Vec<Expr> = vars.iter().enumerate().filter(|(k, _)| *k != j).map(|(_, e)| e.clone()).collect();
    let mut sol = SystemSolution { numeric: rs.numeric, ..Default::default() };
    for r in rs.roots {
        let sub_fs: Vec<Expr> = fs.iter().enumerate().filter(|(k, _)| *k != i).map(|(_, g)| simplify::cancel(&g.subs(&v, &r))).filter(|g| !g.is_zero()).collect();
        if sub_fs.is_empty() {
            sol.underdetermined = true;
            sol.sols.push(vec![(v.clone(), r)]);
            continue;
        }
        if sub_fs.iter().any(|g| g.free_symbols().iter().all(|s| !rest_vars.iter().any(|rv| matches!(rv, Sym(n) if n == s))) && !numerically_zero(g) && g.free_symbols().is_empty()) {
            continue;
        }
        let inner = if sub_fs.iter().all(|g| is_linear(g, &rest_vars)) { linear_system(&sub_fs, &rest_vars) } else { substitution_system(&sub_fs, &rest_vars, depth + 1)? };
        sol.numeric |= inner.numeric;
        sol.underdetermined |= inner.underdetermined;
        for s in inner.sols {
            let rv = simplify::simplify(&r.subs_many(&s));
            let mut full = s.clone();
            full.push((v.clone(), rv));
            // order as in vars
            full.sort_by_key(|(var, _)| vars.iter().position(|q| q == var).unwrap_or(99));
            if !sol.sols.contains(&full) {
                sol.sols.push(full);
            }
        }
    }
    Some(sol)
}

/// Two polynomial equations: eliminate the last variable with a resultant.
fn resultant_system(fs: &[Expr], vars: &[Expr], depth: usize) -> Option<SystemSolution> {
    if fs.len() < 2 || vars.len() != 2 {
        return None;
    }
    let y = &vars[1];
    let r = resultant(&fs[0], &fs[1], y)?;
    let r = simplify::expand(&r);
    if r.is_zero() {
        return None;
    }
    let xs = solve(&r, &vars[0]);
    let mut sol = SystemSolution { numeric: xs.numeric, ..Default::default() };
    for xv in xs.roots {
        let g0 = simplify::expand(&fs[0].subs(&vars[0], &xv));
        let ys = solve(&g0, y);
        for yv in ys.roots {
            let ok = fs[1..].iter().all(|g| {
                let v = g.subs(&vars[0], &xv).subs(y, &yv);
                crate::eval::const_c64(&v).map_or(false, |z| z.norm() < 1e-7)
            });
            if ok {
                sol.sols.push(vec![(vars[0].clone(), xv.clone()), (y.clone(), yv)]);
            }
        }
    }
    let _ = depth;
    Some(sol)
}

/// Sylvester resultant of two polynomials in `y`.
pub fn resultant(a: &Expr, b: &Expr, y: &Expr) -> Option<Expr> {
    let ca = poly::coeffs(a, y)?;
    let cb = poly::coeffs(b, y)?;
    let (m, n) = (ca.len() - 1, cb.len() - 1);
    if m + n == 0 || m + n > 12 {
        return None;
    }
    let size = m + n;
    let mut mat = vec![vec![zero(); size]; size];
    for i in 0..n {
        for (k, c) in ca.iter().rev().enumerate() {
            mat[i][i + k] = c.clone();
        }
    }
    for i in 0..m {
        for (k, c) in cb.iter().rev().enumerate() {
            mat[n + i][i + k] = c.clone();
        }
    }
    Some(det(&mat))
}

/// Determinant by fraction-free Bareiss elimination.
pub fn det(m: &[Vec<Expr>]) -> Expr {
    let n = m.len();
    if n == 0 {
        return one();
    }
    let mut a: Vec<Vec<Expr>> = m.to_vec();
    let mut sign = 1i64;
    let mut prev = one();
    for k in 0..n - 1 {
        if a[k][k].is_zero() || simplify::is_zero(&a[k][k]) == Some(true) {
            let Some(p) = (k + 1..n).find(|&i| !a[i][k].is_zero()) else { return zero() };
            a.swap(k, p);
            sign = -sign;
        }
        for i in k + 1..n {
            for j in k + 1..n {
                let v = sub(mul(vec![a[i][j].clone(), a[k][k].clone()]), mul(vec![a[i][k].clone(), a[k][j].clone()]));
                a[i][j] = simplify::cancel(&simplify::expand(&div(v, prev.clone())));
            }
            a[i][k] = zero();
        }
        prev = a[k][k].clone();
    }
    simplify::expand(&mul(vec![int(sign), a[n - 1][n - 1].clone()]))
}

/// Newton's method from several starting points.
fn numeric_system(fs: &[Expr], vars: &[Expr]) -> SystemSolution {
    let mut sol = SystemSolution { numeric: true, ..Default::default() };
    let names: Vec<String> = vars
        .iter()
        .filter_map(|v| match v {
            Sym(n) => Some(n.to_string()),
            _ => None,
        })
        .collect();
    if names.len() != vars.len() || fs.len() != vars.len() {
        return sol;
    }
    let refs: Vec<&str> = names.iter().map(|s| s.as_str()).collect();
    let progs: Option<Vec<Program>> = fs.iter().map(|f| Program::compile(f, &refs)).collect();
    let Some(progs) = progs else { return sol };
    let fun = |v: &[f64]| progs.iter().map(|p| p.eval(v)).collect::<Vec<f64>>();
    let mut found: Vec<Vec<f64>> = Vec::new();
    let starts = [-3.1, -1.3, -0.4, 0.6, 1.7, 3.3];
    let n = vars.len();
    let total = starts.len().pow(n.min(3) as u32);
    for idx in 0..total {
        budget::tick();
        let mut x0 = vec![0.0; n];
        let mut k = idx;
        for xi in x0.iter_mut().take(n.min(3)) {
            *xi = starts[k % starts.len()];
            k /= starts.len();
        }
        if let Some(r) = crate::numeric::newton_system(&fun, &x0, 60) {
            let res: f64 = fun(&r).iter().map(|v| v.abs()).sum();
            if res < 1e-9 && !found.iter().any(|q| q.iter().zip(&r).all(|(a, b)| (a - b).abs() < 1e-6)) {
                found.push(r);
            }
        }
    }
    found.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    for r in found {
        sol.sols.push(vars.iter().cloned().zip(r.into_iter().map(|v| float(crate::eval::chop(v)))).collect());
    }
    sol
}

// ---------------------------------------------------------------------------
// inequalities
// ---------------------------------------------------------------------------

/// Solve a real inequality `lhs op rhs` in x.  Returns the solution set as a
/// readable expression (`x < -1 or x > 1`, `-2 ≤ x ≤ 3`), `True` or `False`.
pub fn solve_inequality(op: RelOp, lhs: &Expr, rhs: &Expr, x: &Expr) -> Option<Expr> {
    let f = simplify::together(&sub(lhs.clone(), rhs.clone()));
    let (num, den) = simplify::numer_denom(&f);
    let Sym(name) = x else { return None };
    let prog = Program::compile(&f, &[name])?.with_real_roots();
    // critical points
    let mut crit: Vec<(f64, Expr, bool)> = Vec::new(); // value, exact, is_zero_of_numerator
    for (part, is_num) in [(&num, true), (&den, false)] {
        if !part.has_sym(x) {
            continue;
        }
        let rs = solve(part, x);
        for r in rs.real() {
            if r.has_sym(&param()) {
                return None; // periodic: not handled
            }
            if let Some(v) = r.to_f64_const() {
                if !crit.iter().any(|(c, _, _)| (c - v).abs() < 1e-12) {
                    crit.push((v, r.clone(), is_num));
                } else if !is_num {
                    // a pole overrides a zero at the same place
                    for c in crit.iter_mut() {
                        if (c.0 - v).abs() < 1e-12 {
                            c.2 = false;
                        }
                    }
                }
            }
        }
        if rs.partial && rs.roots.is_empty() {
            for v in real_roots_in(part, x, -1e3, 1e3) {
                crit.push((v, float(v), is_num));
            }
        }
    }
    // domain edges (sqrt, log): sample for NaN boundaries
    let mut edges = Vec::new();
    {
        let mut last: Option<(f64, bool)> = None;
        let n = 4000;
        for k in 0..=n {
            let t = -1000.0 + 2000.0 * (k as f64 / n as f64).powi(1);
            let ok = prog.eval1(t).is_finite();
            if let Some((t0, ok0)) = last {
                if ok0 != ok {
                    // bisect the boundary
                    let (mut a, mut b) = (t0, t);
                    for _ in 0..80 {
                        let m = 0.5 * (a + b);
                        if prog.eval1(m).is_finite() == ok0 {
                            a = m;
                        } else {
                            b = m;
                        }
                    }
                    edges.push(0.5 * (a + b));
                }
            }
            last = Some((t, ok));
        }
    }
    for e in edges {
        if !crit.iter().any(|(c, _, _)| (c - e).abs() < 1e-6) {
            let ex = crate::calculus::limit::tidy_float(e);
            crit.push((e, ex, false));
        }
    }
    crit.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    let holds = |v: f64| -> bool {
        if !v.is_finite() {
            return false;
        }
        match op {
            RelOp::Lt => v < 0.0,
            RelOp::Le => v <= 0.0,
            RelOp::Gt => v > 0.0,
            RelOp::Ge => v >= 0.0,
            RelOp::Eq => v == 0.0,
            RelOp::Ne => v != 0.0,
        }
    };
    let strict = matches!(op, RelOp::Lt | RelOp::Gt | RelOp::Ne);
    // test points
    let mut intervals: Vec<(Option<Expr>, bool, Option<Expr>, bool)> = Vec::new();
    let pts: Vec<f64> = crit.iter().map(|c| c.0).collect();
    let mut segs: Vec<(f64, f64)> = Vec::new();
    if pts.is_empty() {
        segs.push((f64::NEG_INFINITY, f64::INFINITY));
    } else {
        segs.push((f64::NEG_INFINITY, pts[0]));
        for w in pts.windows(2) {
            segs.push((w[0], w[1]));
        }
        segs.push((*pts.last().unwrap(), f64::INFINITY));
    }
    let sample = |a: f64, b: f64| -> f64 {
        if a.is_infinite() && b.is_infinite() {
            0.123
        } else if a.is_infinite() {
            b - 1.0 - b.abs() * 0.1
        } else if b.is_infinite() {
            a + 1.0 + a.abs() * 0.1
        } else {
            0.5 * (a + b)
        }
    };
    let point_ok = |k: usize| -> bool {
        let (v, _, is_num) = &crit[k];
        !strict && *is_num && prog.eval1(*v).abs() < 1e-9
    };
    for (si, (a, b)) in segs.iter().enumerate() {
        let inside = holds(prog.eval1(sample(*a, *b)));
        let lo_idx = if si == 0 { None } else { Some(si - 1) };
        let hi_idx = if si < pts.len() { Some(si) } else { None };
        if inside {
            let lo = lo_idx.map(|k| crit[k].1.clone());
            let lo_closed = lo_idx.map_or(false, point_ok);
            let hi = hi_idx.map(|k| crit[k].1.clone());
            let hi_closed = hi_idx.map_or(false, point_ok);
            // merge with the previous interval when they touch at a closed point
            if let Some(last) = intervals.last_mut() {
                if last.2.is_some() && last.2 == lo && (last.3 || lo_closed) {
                    last.2 = hi;
                    last.3 = hi_closed;
                    continue;
                }
            }
            intervals.push((lo, lo_closed, hi, hi_closed));
        } else if let Some(k) = hi_idx {
            // isolated point solution (e.g. x^2 <= 0)
            if point_ok(k) {
                let next_inside = segs.get(si + 1).map_or(false, |(a2, b2)| holds(prog.eval1(sample(*a2, *b2))));
                if !next_inside {
                    let p = crit[k].1.clone();
                    intervals.push((Some(p.clone()), true, Some(p), true));
                }
            }
        }
    }
    if intervals.is_empty() {
        return Some(boolean(false));
    }
    let mut parts = Vec::new();
    for (lo, lc, hi, hc) in intervals {
        let lop = if lc { RelOp::Le } else { RelOp::Lt };
        let hop = if hc { RelOp::Le } else { RelOp::Lt };
        let p = match (lo, hi) {
            (None, None) => return Some(boolean(true)),
            (Some(l), Some(h)) if l == h => Rel(RelOp::Eq, Box::new(x.clone()), Box::new(l)),
            (None, Some(h)) => Rel(hop, Box::new(x.clone()), Box::new(h)),
            (Some(l), None) => Rel(lop.flip(), Box::new(x.clone()), Box::new(l)),
            (Some(l), Some(h)) => raw_fn("And", vec![Rel(lop, Box::new(l), Box::new(x.clone())), Rel(hop, Box::new(x.clone()), Box::new(h))]),
        };
        parts.push(p);
    }
    Some(if parts.len() == 1 { parts.pop().unwrap() } else { raw_fn("Or", parts) })
}

/// Implicit-function helper: dy/dx for F(x, y) = 0.
pub fn implicit_derivative(eq: &Expr, x: &Expr, y: &Expr) -> Option<Expr> {
    let f = as_zero(eq);
    let fx = diff(&f, x);
    let fy = diff(&f, y);
    if fy.is_zero() {
        return None;
    }
    Some(simplify::simplify(&neg(div(fx, fy))))
}

#[allow(dead_code)]
fn positive(r: &Rat) -> bool {
    r.is_positive()
}
