//! Symbolic integration by rules and heuristics, with numeric verification,
//! plus definite and numeric integration.

use super::diff::diff;
use super::limit::{limit, Dir};
use crate::budget;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::poly::{self, QPoly};
use crate::print::text;
use crate::simplify;
use crate::steps;
use num_complex::Complex64;
use num_integer::Integer;
use num_traits::{One, ToPrimitive, Zero};
use std::cell::RefCell;

thread_local! {
    static STACK: RefCell<Vec<Expr>> = const { RefCell::new(Vec::new()) };
}

struct Guard;
impl Drop for Guard {
    fn drop(&mut self) {
        STACK.with(|s| {
            s.borrow_mut().pop();
        });
    }
}

fn half() -> Expr {
    rat(1, 2)
}

fn f(name: &str, u: &Expr) -> Expr {
    func(name, vec![u.clone()])
}

fn fpow(name: &str, u: &Expr, n: i64) -> Expr {
    pow(f(name, u), int(n))
}

/// `u = a x + b` with `a != 0` free of x.
fn linear(u: &Expr, x: &Expr) -> Option<(Expr, Expr)> {
    if u == x {
        return Some((one(), zero()));
    }
    if !u.has_sym(x) || u.size() > 80 {
        return None;
    }
    let c = poly::coeffs(u, x)?;
    if c.len() == 2 && !c[1].is_zero() {
        Some((c[1].clone(), c[0].clone()))
    } else {
        None
    }
}

/// Coefficients `[c, b, a]` of a genuine quadratic in x.
fn quadratic(u: &Expr, x: &Expr) -> Option<(Expr, Expr, Expr)> {
    if !u.has_sym(x) || u.size() > 120 {
        return None;
    }
    let c = poly::coeffs(u, x)?;
    if c.len() == 3 && !c[2].is_zero() {
        Some((c[2].clone(), c[1].clone(), c[0].clone()))
    } else {
        None
    }
}

fn split_const(e: &Expr, x: &Expr) -> (Expr, Expr) {
    match e {
        Mul(v) => {
            let (c, r): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|t| !t.has_sym(x));
            (mul(c), mul(r))
        }
        _ => (one(), e.clone()),
    }
}

fn sign_num(e: &Expr) -> Option<i32> {
    let v = e.to_f64_const()?;
    Some(if v > 0.0 {
        1
    } else if v < 0.0 {
        -1
    } else {
        0
    })
}

/// Top-down rewriting.
fn rewrite(e: &Expr, r: &dyn Fn(&Expr) -> Option<Expr>) -> Expr {
    if let Some(v) = r(e) {
        return v;
    }
    match e {
        Add(v) => add(v.iter().map(|t| rewrite(t, r)).collect()),
        Mul(v) => mul(v.iter().map(|t| rewrite(t, r)).collect()),
        Pow(b, ex) => pow(rewrite(b, r), rewrite(ex, r)),
        Func(n, a) => func(n, a.iter().map(|t| rewrite(t, r)).collect()),
        _ => e.clone(),
    }
}

// ---------------------------------------------------------------------------
// public entry points
// ---------------------------------------------------------------------------

/// Antiderivative of `e` with respect to `x` (without the constant of
/// integration), or `None` if no closed form was found.  Results are checked
/// numerically against the integrand.
pub fn integrate(e: &Expr, x: &Expr) -> Option<Expr> {
    STACK.with(|s| s.borrow_mut().clear());
    let secs = budget::remaining().min(20.0) * 0.7;
    let raw = budget::attempt_opt(secs, || integ(e, x, 0));
    STACK.with(|s| s.borrow_mut().clear());
    let raw = raw?;
    if raw.has(&|n| matches!(n, Const(K::Undef | K::CInf | K::Inf))) {
        return None;
    }
    let tidy = budget::attempt(budget::remaining().min(6.0) * 0.5, || simplify::simplify(&raw)).unwrap_or_else(|| raw.clone());
    let tidy = drop_constants(&tidy, x);
    if verify(e, &tidy, x) {
        return Some(tidy);
    }
    let raw = drop_constants(&raw, x);
    if verify(e, &raw, x) {
        return Some(raw);
    }
    None
}

/// Remove additive terms that do not depend on x.
fn drop_constants(e: &Expr, x: &Expr) -> Expr {
    match e {
        Add(v) => add(v.iter().filter(|t| t.has_sym(x)).cloned().collect()),
        _ => e.clone(),
    }
}

/// Check `d/dx F = f` at sample points.
pub fn verify(fe: &Expr, big_f: &Expr, x: &Expr) -> bool {
    let df = diff(big_f, x);
    let mut syms: Vec<Name> = fe.free_symbols().into_iter().collect();
    for s in df.free_symbols() {
        if !syms.contains(&s) {
            syms.push(s);
        }
    }
    let names: Vec<&str> = syms.iter().map(|n| &**n).collect();
    let (Some(p1), Some(p2)) = (crate::eval::Program::compile(fe, &names), crate::eval::Program::compile(&df, &names)) else {
        return true;
    };
    let xi = syms.iter().position(|s| matches!(x, Sym(n) if n == s));
    let pts = [0.31, 0.83, 1.37, 2.9, -0.57, -1.9, 5.3, 0.05, 11.2, -3.4];
    let (mut good, mut valid) = (0, 0);
    for (k, &p) in pts.iter().enumerate() {
        let vals: Vec<Complex64> = (0..names.len()).map(|j| if Some(j) == xi { Complex64::new(p, 0.0) } else { Complex64::new(0.7 + 0.31 * j as f64 + 0.01 * k as f64, 0.0) }).collect();
        let a = p1.eval_c(&vals);
        let b = p2.eval_c(&vals);
        if !a.re.is_finite() || !a.im.is_finite() || !b.re.is_finite() || !b.im.is_finite() {
            continue;
        }
        valid += 1;
        if (a - b).norm() <= 1e-6 * (1.0 + a.norm()) {
            good += 1;
        }
    }
    if valid == 0 {
        return true;
    }
    good >= 2.min(valid) && good * 10 >= valid * 6
}

// ---------------------------------------------------------------------------
// the rule engine
// ---------------------------------------------------------------------------

fn integ(e: &Expr, x: &Expr, depth: usize) -> Option<Expr> {
    budget::tick();
    if depth > 14 {
        return None;
    }
    if !e.has_sym(x) {
        let r = mul(vec![e.clone(), x.clone()]);
        if steps::enabled() {
            steps::step(format!("The integral of the constant {} is {}:", text(&pretty(e)), text(&pretty(&r))), Some(&step_eq(e, x, &r)));
        }
        return Some(r);
    }
    let m = steps::mark();
    if let Add(v) = e {
        let mut out = Vec::with_capacity(v.len());
        let mut ok = true;
        steps::nested(|| {
            for t in v {
                match integ(t, x, depth) {
                    Some(r) => out.push(r),
                    None => {
                        ok = false;
                        break;
                    }
                }
            }
        });
        if ok {
            let r = add(out);
            if steps::enabled() {
                steps::insert_at(m, "Integrate the sum term by term:", Some(&step_eq(e, x, &r)));
            }
            return Some(r);
        }
        steps::rollback(m);
    } else {
        let (c, r) = split_const(e, x);
        if !c.is_one() {
            let inner = steps::nested(|| integ(&r, x, depth));
            let Some(inner) = inner else {
                steps::rollback(m);
                return None;
            };
            let res = mul(vec![c.clone(), inner]);
            if steps::enabled() {
                steps::insert_at(m, format!("Factor out the constant {}:", text(&pretty(&c))), Some(&step_eq(e, x, &res)));
            }
            return Some(res);
        }
    }
    if STACK.with(|s| s.borrow().contains(e)) {
        return None;
    }
    STACK.with(|s| s.borrow_mut().push(e.clone()));
    let _g = Guard;
    let m = steps::mark();
    match steps::nested(|| core(e, x, depth)) {
        Some((r, why)) => {
            if steps::enabled() {
                steps::insert_at(m, why, Some(&step_eq(e, x, &r)));
            }
            Some(r)
        }
        None => {
            steps::rollback(m);
            None
        }
    }
}

/// Internal substitution variables are shown as u.
fn pretty(e: &Expr) -> Expr {
    match e {
        Sym(n) if n.starts_with(['\u{3be}', '\u{3c4}', '\u{3c1}', '\u{3b5}']) => sym("u"),
        Add(v) => Add(v.iter().map(pretty).collect()),
        Mul(v) => Mul(v.iter().map(pretty).collect()),
        Pow(b, x) => Pow(Box::new(pretty(b)), Box::new(pretty(x))),
        Func(n, a) => Func(n.clone(), a.iter().map(pretty).collect()),
        Rel(op, a, b) => Rel(*op, Box::new(pretty(a)), Box::new(pretty(b))),
        _ => e.clone(),
    }
}

fn step_eq(e: &Expr, x: &Expr, r: &Expr) -> Expr {
    pretty(&eq(raw_fn("Integral", vec![e.clone(), x.clone()]), r.clone()))
}

/// Antiderivative with a step-by-step explanation.
pub fn integrate_explained(e: &Expr, x: &Expr) -> (Option<Expr>, Vec<steps::Step>) {
    let (r, mut st) = steps::record(|| integrate(e, x));
    if let Some(v) = &r {
        st.push(steps::Step { text: "Add the constant of integration:".into(), math: Some(eq(raw_fn("Integral", vec![e.clone(), x.clone()]), add(vec![v.clone(), sym("constant")]))), depth: 0 });
    }
    (r, st)
}

fn core(e: &Expr, x: &Expr, depth: usize) -> Option<(Expr, String)> {
    let on = steps::enabled();
    let d = |s: String| if on { s } else { String::new() };
    let es = || text(&pretty(e));
    if let Some(r) = table(e, x) {
        return Some((r, d(format!("Use the standard integral of {}:", es()))));
    }
    if let Some(r) = rational(e, x) {
        let pf = if on { simplify::apart(e, x).map(|a| format!(" Its partial fraction decomposition is {}.", text(&a))).unwrap_or_default() } else { String::new() };
        return Some((r, d(format!("Integrate the rational function by partial fractions.{pf}"))));
    }
    if let Some(r) = quad_denominator(e, x) {
        return Some((r, d("Complete the square in the quadratic denominator and use the arctangent / logarithm forms:".into())));
    }
    if let Some(r) = exp_trig(e, x) {
        return Some((r, d("Integrate by parts twice (∫ e^(a x) sin(b x) dx and ∫ e^(a x) cos(b x) dx have closed forms):".into())));
    }
    if let Some(r) = sqrt_quadratic(e, x) {
        return Some((r, d("Write the integrand as P(x)·sqrt(a x^2 + b x + c) and use the reduction formula for such integrals:".into())));
    }
    if let Some(r) = special(e, x, depth) {
        let fname = ["erf", "erfi", "Si", "Ci", "Ei", "li", "fresnels", "fresnelc"].iter().find(|f| r.has_fn(f)).map(|f| match *f {
            "erf" | "erfi" => "the error function",
            "Si" => "the sine integral Si",
            "Ci" => "the cosine integral Ci",
            "Ei" => "the exponential integral Ei",
            "li" => "the logarithmic integral li",
            _ => "the Fresnel integrals",
        });
        return Some((r, d(format!("This integral is not elementary; it is expressed with {}:", fname.unwrap_or("a special function")))));
    }
    if let Some((r, u, dv)) = parts_poly(e, x, depth) {
        return Some((r, d(format!("Integrate by parts, ∫ u dv = u v - ∫ v du, with u = {} and dv = {} dx:", text(&pretty(&u)), text(&pretty(&dv))))));
    }
    if depth <= 7 {
        if let Some((r, u)) = substitution(e, x, depth) {
            return Some((r, d(format!("Substitute u = {}, du = {} dx:", text(&pretty(&u)), text(&pretty(&simplify::simplify(&super::diff::diff(&u, x))))))));
        }
    }
    if let Some((r, u, dv)) = parts_poly(e, x, depth) {
        return Some((r, d(format!("Integrate by parts, ∫ u dv = u v - ∫ v du, with u = {} and dv = {} dx:", text(&pretty(&u)), text(&pretty(&dv))))));
    }
    if let Some((r, u, dv)) = parts_log(e, x, depth) {
        return Some((r, d(format!("Integrate by parts, ∫ u dv = u v - ∫ v du, with u = {} and dv = {} dx:", text(&pretty(&u)), text(&pretty(&dv))))));
    }
    if let Some((r, how)) = trig_methods(e, x, depth) {
        return Some((r, d(how)));
    }
    if let Some(r) = radical_subst(e, x, depth) {
        return Some((r, d("Rationalise the integrand with the substitution u = (a x + b)^(1/n):".into())));
    }
    if let Some(r) = euler_subst(e, x, depth) {
        return Some((r, d("Use an Euler substitution to rationalise the square root:".into())));
    }
    // rewrites
    let ex = simplify::expand(e);
    if ex != *e {
        if let Some(r) = integ(&ex, x, depth + 1) {
            return Some((r, d(format!("Expand the integrand to {}:", text(&pretty(&ex))))));
        }
    }
    if e.has_any_fn(&["sinh", "cosh", "tanh"]) {
        let r = simplify::expand(&simplify::rewrite_exp(e));
        if r != *e {
            if let Some(v) = integ(&r, x, depth + 2) {
                return Some((v, d("Rewrite the hyperbolic functions with exponentials:".into())));
            }
        }
    }
    let c = simplify::cancel(e);
    if c != *e && c != ex {
        if let Some(r) = integ(&c, x, depth + 1) {
            return Some((r, d(format!("Simplify the integrand to {}:", text(&pretty(&c))))));
        }
    }
    let p = simplify::power_simplify(e);
    if p != *e && p != c {
        if let Some(r) = integ(&p, x, depth + 2) {
            return Some((r, d(format!("Simplify the powers in the integrand to {}:", text(&pretty(&p))))));
        }
    }
    None
}

// ---------------------------------------------------------------------------
// table look-ups
// ---------------------------------------------------------------------------

fn table(e: &Expr, x: &Expr) -> Option<Expr> {
    if e == x {
        return Some(mul(vec![half(), square(x.clone())]));
    }
    match e {
        Pow(b, n) if !n.has_sym(x) => {
            if let Some((a, _)) = linear(b, x) {
                if n.is_minus_one() {
                    return Some(div(ln((**b).clone()), a));
                }
                let n1 = add(vec![(**n).clone(), one()]);
                return Some(div(pow((**b).clone(), n1.clone()), mul(vec![a, n1])));
            }
            if let (Func(name, args), Some(k)) = (&**b, n.as_i64()) {
                if args.len() == 1 {
                    if let Some((a, _)) = linear(&args[0], x) {
                        return Some(div(trig_pow(name, &args[0], k)?, a));
                    }
                }
            }
            None
        }
        Pow(c, u) if !c.has_sym(x) => {
            let (a, _) = linear(u, x)?;
            Some(div(e.clone(), mul(vec![a, ln((**c).clone())])))
        }
        Func(name, args) if args.len() == 1 => {
            let (a, _) = linear(&args[0], x)?;
            Some(div(fn_table(name, &args[0])?, a))
        }
        _ => None,
    }
}

/// ∫ f(u) du for elementary and special functions.
fn fn_table(name: &str, u: &Expr) -> Option<Expr> {
    let uu = u.clone();
    let sq = |e: Expr| sqrt(e);
    let u2 = square(uu.clone());
    let inv_sqrt_pi = pow(pi(), rat(-1, 2));
    Some(match name {
        "sin" => neg(f("cos", u)),
        "cos" => f("sin", u),
        "tan" => neg(ln(f("cos", u))),
        "sinh" => f("cosh", u),
        "cosh" => f("sinh", u),
        "tanh" => ln(f("cosh", u)),
        "ln" => sub(mul(vec![uu.clone(), ln(uu)]), u.clone()),
        "asin" => add(vec![mul(vec![uu, f("asin", u)]), sq(sub(one(), u2))]),
        "acos" => sub(mul(vec![uu, f("acos", u)]), sq(sub(one(), u2))),
        "atan" => sub(mul(vec![uu, f("atan", u)]), mul(vec![half(), ln(add(vec![one(), u2]))])),
        "asinh" => sub(mul(vec![uu, f("asinh", u)]), sq(add(vec![u2, one()]))),
        "acosh" => sub(mul(vec![uu.clone(), f("acosh", u)]), mul(vec![sq(sub(uu.clone(), one())), sq(add(vec![uu, one()]))])),
        "atanh" => add(vec![mul(vec![uu, f("atanh", u)]), mul(vec![half(), ln(sub(one(), u2))])]),
        "erf" => add(vec![mul(vec![uu, f("erf", u)]), mul(vec![inv_sqrt_pi, exp(neg(u2))])]),
        "erfc" => sub(mul(vec![uu, f("erfc", u)]), mul(vec![inv_sqrt_pi, exp(neg(u2))])),
        "erfi" => sub(mul(vec![uu, f("erfi", u)]), mul(vec![inv_sqrt_pi, exp(u2)])),
        "abs" => mul(vec![half(), uu, f("abs", u)]),
        "sign" => f("abs", u),
        "heaviside" => mul(vec![uu, f("heaviside", u)]),
        "dirac" => f("heaviside", u),
        "Si" => add(vec![mul(vec![uu, f("Si", u)]), f("cos", u)]),
        "Ci" => sub(mul(vec![uu, f("Ci", u)]), f("sin", u)),
        "Ei" => sub(mul(vec![uu.clone(), f("Ei", u)]), exp(uu)),
        "li" => sub(mul(vec![uu.clone(), f("li", u)]), f("Ei", &mul(vec![int(2), ln(uu)]))),
        "fresnels" => add(vec![mul(vec![uu, f("fresnels", u)]), div(f("cos", &mul(vec![half(), pi(), u2])), pi())]),
        "fresnelc" => sub(mul(vec![uu, f("fresnelc", u)]), div(f("sin", &mul(vec![half(), pi(), u2])), pi())),
        "W" => {
            let w = f("W", u);
            mul(vec![uu, add(vec![w.clone(), int(-1), pow(w, int(-1))])])
        }
        _ => return None,
    })
}

/// ∫ f(u)^n du for integer n.
fn trig_pow(name: &str, u: &Expr, n: i64) -> Option<Expr> {
    if n == 0 {
        return Some(u.clone());
    }
    if n == 1 {
        return fn_table(name, u);
    }
    if n.abs() > 40 {
        return None;
    }
    let s = |k: i64| fpow("sin", u, k);
    let c = |k: i64| fpow("cos", u, k);
    let t = |k: i64| fpow("tan", u, k);
    let sh = |k: i64| fpow("sinh", u, k);
    let ch = |k: i64| fpow("cosh", u, k);
    let th = |k: i64| fpow("tanh", u, k);
    let half_u = mul(vec![half(), u.clone()]);
    Some(match name {
        "sin" if n >= 2 => add(vec![mul(vec![rat(-1, n), s(n - 1), c(1)]), mul(vec![rat(n - 1, n), trig_pow("sin", u, n - 2)?])]),
        "cos" if n >= 2 => add(vec![mul(vec![rat(1, n), c(n - 1), s(1)]), mul(vec![rat(n - 1, n), trig_pow("cos", u, n - 2)?])]),
        "cos" if n == -1 => ln(add(vec![t(1), c(-1)])),
        "cos" if n == -2 => t(1),
        "cos" => {
            let m = -n;
            add(vec![mul(vec![rat(1, m - 1), c(-(m - 2)), t(1)]), mul(vec![rat(m - 2, m - 1), trig_pow("cos", u, -(m - 2))?])])
        }
        "sin" if n == -1 => ln(f("tan", &half_u)),
        "sin" if n == -2 => neg(t(-1)),
        "sin" => {
            let m = -n;
            add(vec![mul(vec![rat(-1, m - 1), s(-(m - 2)), t(-1)]), mul(vec![rat(m - 2, m - 1), trig_pow("sin", u, -(m - 2))?])])
        }
        "tan" if n == -1 => ln(s(1)),
        "tan" if n >= 2 => sub(mul(vec![rat(1, n - 1), t(n - 1)]), trig_pow("tan", u, n - 2)?),
        "tan" => {
            let m = -n;
            sub(mul(vec![rat(-1, m - 1), t(-(m - 1))]), trig_pow("tan", u, -(m - 2))?)
        }
        "sinh" if n >= 2 => sub(mul(vec![rat(1, n), sh(n - 1), ch(1)]), mul(vec![rat(n - 1, n), trig_pow("sinh", u, n - 2)?])),
        "cosh" if n >= 2 => add(vec![mul(vec![rat(1, n), ch(n - 1), sh(1)]), mul(vec![rat(n - 1, n), trig_pow("cosh", u, n - 2)?])]),
        "cosh" if n == -2 => th(1),
        "cosh" if n == -1 => mul(vec![int(2), f("atan", &f("tanh", &half_u))]),
        "sinh" if n == -1 => ln(f("tanh", &half_u)),
        "sinh" if n == -2 => neg(th(-1)),
        "tanh" if n >= 2 => add(vec![mul(vec![rat(-1, n - 1), th(n - 1)]), trig_pow("tanh", u, n - 2)?]),
        "tanh" if n == -1 => ln(sh(1)),
        "tanh" if n == -2 => sub(u.clone(), th(-1)),
        "ln" if n >= 2 => sub(mul(vec![u.clone(), pow(ln(u.clone()), int(n))]), mul(vec![int(n), trig_pow("ln", u, n - 1)?])),
        _ => return None,
    })
}

// ---------------------------------------------------------------------------
// rational functions
// ---------------------------------------------------------------------------

fn rational(e: &Expr, x: &Expr) -> Option<Expr> {
    if e.has(&|n| matches!(n, Func(..) | Float(_))) {
        return None;
    }
    let (n, d) = simplify::numer_denom(e);
    let mut np = poly::to_qpoly(&n, x)?;
    let mut dp = poly::to_qpoly(&d, x)?;
    if dp.is_zero() {
        return None;
    }
    let g = np.gcd(&dp);
    if g.degree() > 0 {
        np = np.div_exact(&g)?;
        dp = dp.div_exact(&g)?;
    }
    let (q, r) = np.divrem(&dp);
    let mut out = Vec::new();
    if !q.is_zero() {
        for k in 0..=q.degree() {
            let c = q.coeff(k);
            if !c.is_zero() {
                out.push(mul(vec![Num(c / Rat::from_integer(Int::from(k + 1))), pow(x.clone(), int(k as i64 + 1))]));
            }
        }
    }
    if r.is_zero() {
        return Some(add(out));
    }
    let (content, factors) = poly::factor(&dp);
    if factors.iter().any(|(p, _)| p.degree() > 2) {
        return None;
    }
    let r = r.scale(&content.recip());
    let full: QPoly = factors.iter().fold(QPoly::one(), |acc, (p, m)| acc.mul(&p.pow(*m)));
    for (p, m) in &factors {
        let pm = p.pow(*m);
        let cof = full.div_exact(&pm)?;
        let (g, s, _) = cof.xgcd(&pm);
        if !g.is_constant() || g.is_zero() {
            return None;
        }
        let s = s.scale(&g.coeff(0).recip());
        let mut ni = r.mul(&s).rem(&pm);
        let mut digits = Vec::new();
        for _ in 0..*m {
            let (qq, rr) = ni.divrem(p);
            digits.push(rr);
            ni = qq;
        }
        for (j, a) in digits.iter().enumerate() {
            if a.is_zero() {
                continue;
            }
            let k = (*m as usize - j) as i64;
            if p.degree() == 1 {
                let pe = p.to_expr(x);
                let c = a.coeff(0) / p.coeff(1);
                out.push(if k == 1 { mul(vec![Num(c), ln(pe)]) } else { mul(vec![Num(c / Rat::from_integer(Int::from(1 - k))), pow(pe, int(1 - k))]) });
            } else {
                out.push(quad_int(&Num(a.coeff(1)), &Num(a.coeff(0)), &Num(p.coeff(2)), &Num(p.coeff(1)), &Num(p.coeff(0)), k, x)?);
            }
        }
    }
    Some(add(out))
}

/// ∫ (A x + B) / (a x² + b x + c)^k dx.
fn quad_int(ca: &Expr, cb: &Expr, a: &Expr, b: &Expr, c: &Expr, k: i64, x: &Expr) -> Option<Expr> {
    let p = add(vec![mul(vec![a.clone(), square(x.clone())]), mul(vec![b.clone(), x.clone()]), c.clone()]);
    let lin = add(vec![mul(vec![int(2), a.clone(), x.clone()]), b.clone()]);
    let d = simplify::expand(&sub(mul(vec![int(4), a.clone(), c.clone()]), square(b.clone())));
    let alpha = div(ca.clone(), mul(vec![int(2), a.clone()]));
    let beta = simplify::cancel(&sub(cb.clone(), mul(vec![alpha.clone(), b.clone()])));
    let mut out = Vec::new();
    if !alpha.is_zero() {
        out.push(mul(vec![alpha, if k == 1 { ln(p.clone()) } else { div(pow(p.clone(), int(1 - k)), int(1 - k)) }]));
    }
    if !beta.is_zero() {
        if d.is_zero() {
            return None;
        }
        let mut j = match sign_num(&d) {
            Some(-1) => {
                let s = sqrt(neg(d.clone()));
                div(ln(div(sub(lin.clone(), s.clone()), add(vec![lin.clone(), s.clone()]))), s)
            }
            _ => {
                let s = sqrt(d.clone());
                div(mul(vec![int(2), f("atan", &div(lin.clone(), s.clone()))]), s)
            }
        };
        for m in 2..=k {
            let t1 = div(lin.clone(), mul(vec![int(m - 1), d.clone(), pow(p.clone(), int(m - 1))]));
            let t2 = mul(vec![div(mul(vec![int(2 * (2 * m - 3)), a.clone()]), mul(vec![int(m - 1), d.clone()])), j]);
            j = add(vec![t1, t2]);
        }
        out.push(mul(vec![beta, j]));
    }
    Some(add(out))
}

/// (A x + B)/(quadratic)^k with symbolic coefficients.
fn quad_denominator(e: &Expr, x: &Expr) -> Option<Expr> {
    let (n, d) = e.numer_denom();
    if d.is_one() {
        return None;
    }
    let (q, k) = match &d {
        Pow(b, k) => ((**b).clone(), k.as_i64()?),
        _ => (d.clone(), 1),
    };
    if !(1..=12).contains(&k) {
        return None;
    }
    let (a, b, c) = quadratic(&q, x)?;
    let nc = poly::coeffs(&n, x)?;
    if nc.len() > 2 {
        return None;
    }
    let ca = nc.get(1).cloned().unwrap_or_else(zero);
    let cb = nc.first().cloned().unwrap_or_else(zero);
    quad_int(&ca, &cb, &a, &b, &c, k, x)
}

// ---------------------------------------------------------------------------
// exponential × trigonometric
// ---------------------------------------------------------------------------

fn exp_trig(e: &Expr, x: &Expr) -> Option<Expr> {
    let Mul(v) = e else { return None };
    if v.len() != 2 {
        return None;
    }
    let (ex, tr) = match (&v[0], &v[1]) {
        (Pow(..), Func(..)) => (&v[0], &v[1]),
        (Func(..), Pow(..)) => (&v[1], &v[0]),
        _ => return None,
    };
    let (Pow(base, w), Func(name, args)) = (ex, tr) else { return None };
    if base.has_sym(x) || args.len() != 1 {
        return None;
    }
    let (a, _) = linear(w, x)?;
    let (b, _) = linear(&args[0], x)?;
    let a = mul(vec![a, ln((**base).clone())]);
    let den = add(vec![square(a.clone()), square(b.clone())]);
    let (s, c) = (f("sin", &args[0]), f("cos", &args[0]));
    let num = match &**name {
        "sin" => sub(mul(vec![a, s]), mul(vec![b, c])),
        "cos" => add(vec![mul(vec![a, c]), mul(vec![b, s])]),
        _ => return None,
    };
    Some(div(mul(vec![ex.clone(), num]), den))
}

// ---------------------------------------------------------------------------
// P(x) (a x² + b x + c)^(±1/2)
// ---------------------------------------------------------------------------

/// ∫ dx / sqrt(a x² + b x + c)
fn j0(a: &Expr, b: &Expr, c: &Expr, x: &Expr) -> Option<Expr> {
    let q = add(vec![mul(vec![a.clone(), square(x.clone())]), mul(vec![b.clone(), x.clone()]), c.clone()]);
    let lin = add(vec![mul(vec![int(2), a.clone(), x.clone()]), b.clone()]);
    let d = simplify::expand(&sub(mul(vec![int(4), a.clone(), c.clone()]), square(b.clone())));
    match sign_num(a) {
        Some(-1) => {
            let na = neg(a.clone());
            let nd = neg(d);
            if sign_num(&nd) == Some(-1) {
                return None;
            }
            // lin has negative leading coefficient; asin is odd
            Some(div(f("asin", &div(neg(lin), sqrt(nd))), sqrt(na)))
        }
        _ => {
            let sa = sqrt(a.clone());
            if sign_num(&d) == Some(1) {
                Some(div(f("asinh", &div(lin, sqrt(d))), sa))
            } else {
                Some(div(ln(add(vec![mul(vec![int(2), sa.clone(), sqrt(q)]), lin])), sa))
            }
        }
    }
}

fn sqrt_quadratic(e: &Expr, x: &Expr) -> Option<Expr> {
    let fs = e.factors();
    let mut rad: Option<(Expr, bool)> = None;
    let mut rest = Vec::new();
    for t in &fs {
        match t {
            Pow(q, r) if (**r == half() || **r == rat(-1, 2) || **r == rat(-3, 2)) && quadratic(q, x).is_some() => {
                if rad.is_some() {
                    return None;
                }
                if **r == rat(-3, 2) {
                    if fs.len() != 1 {
                        return None;
                    }
                    let (a, b, c) = quadratic(q, x)?;
                    let d = simplify::expand(&sub(mul(vec![int(4), a.clone(), c]), square(b.clone())));
                    if d.is_zero() {
                        return None;
                    }
                    let lin = add(vec![mul(vec![int(2), a, x.clone()]), b]);
                    return Some(div(mul(vec![int(2), lin]), mul(vec![d, sqrt((**q).clone())])));
                }
                rad = Some(((**q).clone(), **r == half()));
            }
            _ => rest.push(t.clone()),
        }
    }
    let (q, positive) = rad?;
    let (a, b, c) = quadratic(&q, x)?;
    let mut p = mul(rest);
    if positive {
        p = mul(vec![p, q.clone()]);
    }
    let pc = poly::coeffs(&p, x)?;
    let n = pc.len() - 1;
    if n > 40 {
        return None;
    }
    // P = Q' q + Q q'/2 + lambda
    let mut cq = vec![zero(); n + 2];
    for j in (1..=n).rev() {
        let jj = j as i64;
        let v = sub(pc[j].clone(), add(vec![mul(vec![b.clone(), rat(2 * jj + 1, 2), cq[j].clone()]), mul(vec![int(jj + 1), c.clone(), cq[j + 1].clone()])]));
        cq[j - 1] = simplify::cancel(&div(v, mul(vec![a.clone(), int(jj)])));
    }
    let lambda = simplify::cancel(&sub(pc[0].clone(), add(vec![mul(vec![half(), b.clone(), cq[0].clone()]), mul(vec![c.clone(), cq[1].clone()])])));
    let qpoly = add((0..n).map(|k| mul(vec![cq[k].clone(), pow(x.clone(), int(k as i64))])).collect());
    let mut out = vec![mul(vec![qpoly, sqrt(q)])];
    if !lambda.is_zero() {
        out.push(mul(vec![lambda, j0(&a, &b, &c, x)?]));
    }
    Some(add(out))
}

// ---------------------------------------------------------------------------
// non-elementary integrals
// ---------------------------------------------------------------------------

fn special(e: &Expr, x: &Expr, depth: usize) -> Option<Expr> {
    let sqrt_pi = sqrt(pi());
    match e {
        // e^(a x² + b x + c)
        Pow(base, w) if matches!(**base, Const(K::E)) => {
            let (a, b, c) = quadratic(w, x)?;
            let shift = exp(sub(c, div(square(b.clone()), mul(vec![int(4), a.clone()]))));
            return Some(match sign_num(&a) {
                Some(1) => {
                    let sa = sqrt(a.clone());
                    mul(vec![half(), sqrt_pi, pow(sa.clone(), int(-1)), shift, f("erfi", &add(vec![mul(vec![sa.clone(), x.clone()]), div(b, mul(vec![int(2), sa]))]))])
                }
                _ => {
                    let sa = sqrt(neg(a.clone()));
                    mul(vec![half(), sqrt_pi, pow(sa.clone(), int(-1)), shift, f("erf", &sub(mul(vec![sa.clone(), x.clone()]), div(b, mul(vec![int(2), sa]))))])
                }
            });
        }
        Pow(b, n) if n.is_minus_one() => {
            if let Func(name, args) = &**b {
                if &**name == "ln" && args.len() == 1 && args[0] == *x {
                    return Some(f("li", x));
                }
            }
            return None;
        }
        Func(name, args) if args.len() == 1 && matches!(&**name, "sin" | "cos") => {
            let cs = poly::coeffs(&args[0], x)?;
            if cs.len() != 3 || !cs[1].is_zero() || !cs[0].is_zero() {
                return None;
            }
            let a = cs[2].clone();
            if sign_num(&a) == Some(-1) {
                return None;
            }
            let k = sqrt(div(mul(vec![int(2), a.clone()]), pi()));
            let fname = if &**name == "sin" { "fresnels" } else { "fresnelc" };
            return Some(div(f(fname, &mul(vec![k.clone(), x.clone()])), k));
        }
        Mul(v) if v.len() == 2 => {
            // g(a x)/x
            let (inv, g) = if v[0] == pow(x.clone(), int(-1)) {
                (true, &v[1])
            } else if v[1] == pow(x.clone(), int(-1)) {
                (true, &v[0])
            } else {
                (false, &v[0])
            };
            if inv {
                match g {
                    Func(name, args) if args.len() == 1 => {
                        let (a, b) = linear(&args[0], x)?;
                        if !b.is_zero() {
                            return None;
                        }
                        let ax = mul(vec![a, x.clone()]);
                        return match &**name {
                            "sin" => Some(f("Si", &ax)),
                            "cos" => Some(f("Ci", &ax)),
                            "sinh" => Some(mul(vec![half(), sub(f("Ei", &ax), f("Ei", &neg(ax.clone())))])),
                            "cosh" => Some(mul(vec![half(), add(vec![f("Ei", &ax), f("Ei", &neg(ax.clone()))])])),
                            _ => None,
                        };
                    }
                    Pow(base, w) if !base.has_sym(x) => {
                        let (a, b) = linear(w, x)?;
                        let k = mul(vec![a, ln((**base).clone())]);
                        return Some(mul(vec![pow((**base).clone(), b), f("Ei", &mul(vec![k, x.clone()]))]));
                    }
                    _ => return None,
                }
            }
            // x^n e^(a x²), n even
            let (xp, ex) = match (&v[0], &v[1]) {
                (Pow(b1, _), Pow(b2, _)) if **b1 == *x && matches!(**b2, Const(K::E)) => (&v[0], &v[1]),
                (Pow(b1, _), Pow(b2, _)) if **b2 == *x && matches!(**b1, Const(K::E)) => (&v[1], &v[0]),
                _ => return None,
            };
            let (Pow(_, n), Pow(_, w)) = (xp, ex) else { return None };
            let n = n.as_i64()?;
            if n < 2 || n % 2 != 0 || n > 40 {
                return None;
            }
            let (a, b, _) = quadratic(w, x)?;
            if !b.is_zero() {
                return None;
            }
            let two_a = mul(vec![int(2), a]);
            let lower = integ(&mul(vec![pow(x.clone(), int(n - 2)), ex.clone()]), x, depth + 1)?;
            return Some(sub(div(mul(vec![pow(x.clone(), int(n - 1)), ex.clone()]), two_a.clone()), mul(vec![div(int(n - 1), two_a), lower])));
        }
        _ => {}
    }
    None
}

// ---------------------------------------------------------------------------
// substitution
// ---------------------------------------------------------------------------

fn candidates(e: &Expr, x: &Expr, out: &mut Vec<Expr>) {
    fn push(u: &Expr, x: &Expr, out: &mut Vec<Expr>) {
        if u != x && u.has_sym(x) && !out.contains(u) && !matches!(u, Mul(v) if v.len() == 2 && v[0].is_numeric() && v[1] == *x) {
            out.push(u.clone());
        }
    }
    if !e.has_sym(x) {
        return;
    }
    match e {
        Add(v) | Mul(v) => {
            for t in v {
                candidates(t, x, out);
            }
        }
        Pow(b, ex) => {
            if ex.has_sym(x) {
                push(ex, x, out);
                push(e, x, out);
                candidates(ex, x, out);
            } else if !ex.is_integer() {
                push(e, x, out);
            } else if **b == *x {
                if let Some(k) = ex.as_i64() {
                    for g in 2..k {
                        if k % g == 0 {
                            push(&pow(x.clone(), int(g)), x, out);
                        }
                    }
                }
            }
            push(b, x, out);
            candidates(b, x, out);
        }
        Func(_, args) => {
            push(e, x, out);
            for a in args {
                push(a, x, out);
                candidates(a, x, out);
            }
        }
        _ => {}
    }
}

fn substitution(e: &Expr, x: &Expr, depth: usize) -> Option<(Expr, Expr)> {
    let mut cands = Vec::new();
    candidates(e, x, &mut cands);
    let t = sym(&format!("\u{3be}{depth}"));
    for u in cands.iter().take(16) {
        budget::tick();
        let du = diff(u, x);
        if du.is_zero() {
            continue;
        }
        let ratio = simplify::cancel(&div(e.clone(), du));
        let r = subs_smart(&ratio, u, &t, x);
        if r.has_sym(x) || r.has(&|n| matches!(n, Const(K::Undef | K::CInf))) {
            continue;
        }
        if r.size() > 3 * e.size() + 40 {
            continue;
        }
        if let Some(big) = integ(&r, &t, depth + 1) {
            return Some((big.subs(&t, u), u.clone()));
        }
    }
    None
}

fn subs_smart(e: &Expr, u: &Expr, t: &Expr, x: &Expr) -> Expr {
    let r = e.subs(u, t);
    if !r.has_sym(x) {
        return r;
    }
    let r = rewrite(&r, &|n| pattern(n, u, t, x));
    if !r.has_sym(x) {
        return r;
    }
    if let Some(inv) = invert(u, t, x) {
        return simplify::cancel(&r.subs(x, &inv));
    }
    r
}

fn even_power(n: &Expr, fname: &str, v: &Expr) -> Option<i64> {
    if let Pow(b, k) = n {
        if let Func(name, args) = &**b {
            if &**name == fname && args.len() == 1 && args[0] == *v {
                let k = k.as_i64()?;
                if k % 2 == 0 {
                    return Some(k / 2);
                }
            }
        }
    }
    None
}

fn pattern(n: &Expr, u: &Expr, t: &Expr, x: &Expr) -> Option<Expr> {
    if !n.has_sym(x) {
        return Some(n.clone());
    }
    let t2 = square(t.clone());
    match u {
        Pow(b, w) if !b.has_sym(x) => {
            // exponentials with proportional exponents
            if let Pow(b2, w2) = n {
                if b2 == b && w2.has_sym(x) {
                    let (a1, c1) = linear(w, x)?;
                    let (a2, c2) = linear(w2, x)?;
                    let k = div(a2, a1);
                    let cst = sub(c2, mul(vec![k.clone(), c1]));
                    return Some(mul(vec![pow((**b).clone(), cst), pow(t.clone(), k)]));
                }
            }
            None
        }
        Pow(v, p) if !p.has_sym(x) => {
            if let Pow(v2, m) = n {
                if v2 == v && !m.has_sym(x) {
                    let k = div((**m).clone(), (**p).clone());
                    if k.is_integer() {
                        return Some(pow(t.clone(), k));
                    }
                }
            }
            if n == &**v {
                let k = pow((**p).clone(), int(-1));
                if k.is_integer() {
                    return Some(pow(t.clone(), k));
                }
            }
            None
        }
        Func(name, args) if args.len() == 1 => {
            let v = &args[0];
            match &**name {
                "sin" => even_power(n, "cos", v).map(|k| pow(sub(one(), t2), int(k))),
                "cos" => even_power(n, "sin", v).map(|k| pow(sub(one(), t2), int(k))),
                "tan" => even_power(n, "cos", v).map(|k| pow(add(vec![one(), t2.clone()]), int(-k))).or_else(|| even_power(n, "sin", v).map(|k| pow(div(t2.clone(), add(vec![one(), t2.clone()])), int(k)))),
                "sinh" => even_power(n, "cosh", v).map(|k| pow(add(vec![one(), t2]), int(k))),
                "cosh" => even_power(n, "sinh", v).map(|k| pow(sub(t2, one()), int(k))),
                "tanh" => even_power(n, "cosh", v).map(|k| pow(sub(one(), t2), int(-k))),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Solve `u(x) = t` for x in the simple invertible cases.
fn invert(u: &Expr, t: &Expr, x: &Expr) -> Option<Expr> {
    if let Some((a, b)) = linear(u, x) {
        return Some(div(sub(t.clone(), b), a));
    }
    // u = c x^n
    {
        let (c, rest) = split_const(u, x);
        if let Pow(b, n) = &rest {
            if **b == *x {
                if let Some(k) = n.as_i64() {
                    if k >= 2 && k <= 12 {
                        return Some(pow(div(t.clone(), c), rat(1, k)));
                    }
                }
            }
        }
    }
    match u {
        Pow(l, p) if !p.has_sym(x) => {
            let (a, b) = linear(l, x)?;
            let r = p.as_rat()?;
            if !r.numer().is_one() {
                return None;
            }
            let n = r.denom().to_i64()?;
            Some(div(sub(pow(t.clone(), int(n)), b), a))
        }
        Pow(b, w) if !b.has_sym(x) => {
            let (a, c) = linear(w, x)?;
            Some(div(sub(div(ln(t.clone()), ln((**b).clone())), c), a))
        }
        Func(name, args) if &**name == "ln" && args.len() == 1 => {
            let (a, b) = linear(&args[0], x)?;
            Some(div(sub(exp(t.clone()), b), a))
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// integration by parts
// ---------------------------------------------------------------------------

fn parts_poly(e: &Expr, x: &Expr, depth: usize) -> Option<(Expr, Expr, Expr)> {
    let Mul(v) = e else { return None };
    let (ps, gs): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|t| poly::coeffs(t, x).is_some());
    if ps.is_empty() || gs.is_empty() {
        return None;
    }
    let easy = |g: &Expr| match g {
        Pow(c, w) => !c.has_sym(x) && linear(w, x).is_some(),
        Func(name, args) => args.len() == 1 && matches!(&**name, "sin" | "cos" | "sinh" | "cosh") && linear(&args[0], x).is_some(),
        _ => false,
    };
    if !gs.iter().all(easy) || gs.len() > 2 {
        return None;
    }
    let p = mul(ps);
    let g = mul(gs);
    let big_g = integ(&g, x, depth + 1)?;
    let dp = diff(&p, x);
    let rest = simplify::expand(&mul(vec![dp, big_g.clone()]));
    let r = integ(&rest, x, depth + 1)?;
    Some((sub(mul(vec![p.clone(), big_g]), r), p, g))
}

const LOGLIKE: &[&str] = &["ln", "atan", "asin", "acos", "asinh", "acosh", "atanh", "erf", "erfc", "erfi", "Si", "Ci", "Ei", "li", "fresnels", "fresnelc"];

fn parts_log(e: &Expr, x: &Expr, depth: usize) -> Option<(Expr, Expr, Expr)> {
    let fs = e.factors();
    let is_loglike = |t: &Expr| match t {
        Func(name, _) => LOGLIKE.contains(&&**name) && t.has_sym(x),
        Pow(b, k) => matches!(&**b, Func(name, _) if LOGLIKE.contains(&&**name)) && k.as_i64().map_or(false, |k| k > 0) && b.has_sym(x),
        _ => false,
    };
    let idx = fs.iter().position(is_loglike)?;
    let u = fs[idx].clone();
    let dv = mul(fs.iter().enumerate().filter(|(k, _)| *k != idx).map(|(_, t)| t.clone()).collect());
    if dv.has_any_fn(LOGLIKE) && depth > 4 {
        return None;
    }
    let v = integ(&dv, x, depth + 1)?;
    let w = simplify::cancel(&mul(vec![v.clone(), diff(&u, x)]));
    let r = integ(&w, x, depth + 2)?;
    Some((sub(mul(vec![u.clone(), v]), r), u, dv))
}

// ---------------------------------------------------------------------------
// trigonometric integrands
// ---------------------------------------------------------------------------

fn trig_methods(e: &Expr, x: &Expr, depth: usize) -> Option<(Expr, String)> {
    if !e.has_any_fn(&["sin", "cos", "tan"]) {
        return None;
    }
    let neg_trig_power = e.has(&|n| matches!(n, Pow(b, k) if matches!(&**b, Func(name, _) if matches!(&**name, "sin" | "cos" | "tan")) && k.is_negative_term()));
    if !neg_trig_power && !e.has_fn("tan") {
        let r = simplify::expand(&simplify::trig_reduce(e));
        if r != *e {
            if let Some(v) = integ(&r, x, depth + 1) {
                return Some((v, format!("Use trigonometric identities (power reduction, product-to-sum) to rewrite the integrand as {}:", text(&pretty(&r)))));
            }
        }
    }
    if e.has_fn("tan") {
        let r = rewrite(e, &|n| match n {
            Func(name, args) if &**name == "tan" && args.len() == 1 => Some(div(f("sin", &args[0]), f("cos", &args[0]))),
            _ => None,
        });
        if r != *e {
            if let Some(v) = integ(&r, x, depth + 2) {
                return Some((v, "Write tan as sin/cos:".into()));
            }
        }
    }
    weierstrass(e, x, depth).map(|r| (r, "Use the Weierstrass substitution u = tan(x/2), with sin(x) = 2u/(1 + u^2), cos(x) = (1 - u^2)/(1 + u^2), dx = 2 du/(1 + u^2):".to_string()))
}

fn weierstrass(e: &Expr, x: &Expr, depth: usize) -> Option<Expr> {
    if depth > 6 {
        return None;
    }
    // every trig function must have the same linear argument
    let mut arg: Option<Expr> = None;
    let mut consistent = true;
    find_trig_arg(e, x, &mut arg, &mut consistent);
    if !consistent {
        return None;
    }
    let v = arg?;
    let (a, _) = linear(&v, x)?;
    let t = sym(&format!("\u{3c4}{depth}"));
    let t2 = square(t.clone());
    let den = add(vec![one(), t2.clone()]);
    let s = div(mul(vec![int(2), t.clone()]), den.clone());
    let c = div(sub(one(), t2.clone()), den.clone());
    let r = rewrite(e, &|n| match n {
        Func(name, args) if args.len() == 1 && args[0] == v => match &**name {
            "sin" => Some(s.clone()),
            "cos" => Some(c.clone()),
            "tan" => Some(div(mul(vec![int(2), t.clone()]), sub(one(), t2.clone()))),
            _ => None,
        },
        _ => None,
    });
    if r.has_sym(x) {
        return None;
    }
    let integrand = simplify::cancel(&mul(vec![r, int(2), pow(mul(vec![a, den]), int(-1))]));
    let big = integ(&integrand, &t, depth + 3)?;
    Some(big.subs(&t, &f("tan", &mul(vec![half(), v]))))
}

fn find_trig_arg(e: &Expr, x: &Expr, arg: &mut Option<Expr>, consistent: &mut bool) {
    match e {
        Func(name, args) => {
            if matches!(&**name, "sin" | "cos" | "tan") && args.len() == 1 && args[0].has_sym(x) {
                match arg {
                    Some(a) if *a != args[0] => *consistent = false,
                    Some(_) => {}
                    None => *arg = Some(args[0].clone()),
                }
            }
            for a in args {
                find_trig_arg(a, x, arg, consistent);
            }
        }
        Add(v) | Mul(v) => {
            for t in v {
                find_trig_arg(t, x, arg, consistent);
            }
        }
        Pow(b, ex) => {
            find_trig_arg(b, x, arg, consistent);
            find_trig_arg(ex, x, arg, consistent);
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// rationalising substitutions
// ---------------------------------------------------------------------------

fn find_radicals(e: &Expr, x: &Expr, out: &mut Vec<(Expr, Rat)>) {
    match e {
        Pow(b, ex) => {
            if let Some(r) = ex.as_rat() {
                if !r.is_integer() && b.has_sym(x) {
                    out.push(((**b).clone(), r.clone()));
                }
            }
            find_radicals(b, x, out);
            find_radicals(ex, x, out);
        }
        Add(v) | Mul(v) => {
            for t in v {
                find_radicals(t, x, out);
            }
        }
        Func(_, args) => {
            for t in args {
                find_radicals(t, x, out);
            }
        }
        _ => {}
    }
}

/// Integrands rational in x and roots of one linear expression.
fn radical_subst(e: &Expr, x: &Expr, depth: usize) -> Option<Expr> {
    if depth > 6 {
        return None;
    }
    let mut rads = Vec::new();
    find_radicals(e, x, &mut rads);
    let (base, _) = rads.first()?.clone();
    if rads.iter().any(|(b, _)| *b != base) {
        return None;
    }
    let (a, b) = linear(&base, x)?;
    let mut n = 1i64;
    for (_, r) in &rads {
        n = n.lcm(&r.denom().to_i64()?);
    }
    if n > 12 {
        return None;
    }
    let t = sym(&format!("\u{3c1}{depth}"));
    let r = rewrite(e, &|node| match node {
        Pow(bb, ex) if **bb == base => {
            let r = ex.as_rat()?;
            if r.is_integer() {
                return None;
            }
            Some(pow(t.clone(), Num(r * Rat::from_integer(Int::from(n)))))
        }
        _ => None,
    });
    let xt = div(sub(pow(t.clone(), int(n)), b), a.clone());
    let r = r.subs(x, &xt);
    let integrand = simplify::cancel(&mul(vec![r, int(n), pow(t.clone(), int(n - 1)), pow(a, int(-1))]));
    let big = integ(&integrand, &t, depth + 2)?;
    Some(big.subs(&t, &pow(base, Num(Rat::new(Int::one(), Int::from(n))))))
}

/// Euler substitutions for integrands rational in x and sqrt(a x² + b x + c).
fn euler_subst(e: &Expr, x: &Expr, depth: usize) -> Option<Expr> {
    if depth > 5 {
        return None;
    }
    let mut rads = Vec::new();
    find_radicals(e, x, &mut rads);
    let (q, _) = rads.first()?.clone();
    if rads.iter().any(|(b, r)| *b != q || r.denom().to_i64() != Some(2)) {
        return None;
    }
    let (a, b, c) = quadratic(&q, x)?;
    let (ar, br, cr) = (a.as_rat()?.clone(), b.as_rat()?.clone(), c.as_rat()?.clone());
    let t = sym(&format!("\u{3b5}{depth}"));
    let t2 = square(t.clone());
    let (xt, root_t, dx, back) = if let Some(s) = simplify::rat_sqrt(&ar) {
        // sqrt(q) = t - s x
        let s = Num(s);
        let den = add(vec![Num(br.clone()), mul(vec![int(2), s.clone(), t.clone()])]);
        let xt = div(sub(t2.clone(), Num(cr.clone())), den.clone());
        let num = add(vec![mul(vec![s.clone(), t2.clone()]), mul(vec![Num(br.clone()), t.clone()]), mul(vec![s.clone(), Num(cr.clone())])]);
        let root = div(num.clone(), den.clone());
        let dx = div(mul(vec![int(2), num]), square(den));
        let back = add(vec![sqrt(q.clone()), mul(vec![s, x.clone()])]);
        (xt, root, dx, back)
    } else if let Some(s) = simplify::rat_sqrt(&cr) {
        // sqrt(q) = x t + s
        let s = Num(s);
        let den = sub(Num(ar.clone()), t2.clone());
        let xt = div(sub(mul(vec![int(2), s.clone(), t.clone()]), Num(br.clone())), den.clone());
        let root = add(vec![mul(vec![xt.clone(), t.clone()]), s.clone()]);
        let num = add(vec![mul(vec![s.clone(), t2.clone()]), neg(mul(vec![Num(br.clone()), t.clone()])), mul(vec![s.clone(), Num(ar.clone())])]);
        let dx = div(mul(vec![int(2), num]), square(den));
        let back = div(sub(sqrt(q.clone()), s), x.clone());
        (xt, root, dx, back)
    } else {
        return None;
    };
    let root_t = simplify::cancel(&root_t);
    let r = rewrite(e, &|node| match node {
        Pow(bb, ex) if **bb == q => {
            let r = ex.as_rat()?;
            if r.is_integer() {
                return None;
            }
            let k = (r * Rat::from_integer(Int::from(2))).to_integer().to_i64()?;
            Some(pow(root_t.clone(), int(k)))
        }
        _ => None,
    });
    let r = r.subs(x, &xt);
    if r.has_sym(x) {
        return None;
    }
    let integrand = simplify::cancel(&mul(vec![r, dx]));
    let big = integ(&integrand, &t, depth + 3)?;
    Some(big.subs(&t, &back))
}

// ---------------------------------------------------------------------------
// definite and numeric integration
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum Definite {
    /// Closed form (already checked against numerics when possible).
    Exact(Expr),
    /// Numeric value and error estimate.
    Numeric(f64, f64),
    Divergent,
    Unknown,
}

fn bound_f64(a: &Expr) -> Option<f64> {
    if matches!(a, Const(K::Inf)) {
        return Some(f64::INFINITY);
    }
    if a.is_neg_inf() {
        return Some(f64::NEG_INFINITY);
    }
    a.to_f64_const()
}

/// Numeric integral of `e` over `[a, b]`.
pub fn nintegrate(e: &Expr, x: &Expr, a: f64, b: f64) -> Option<crate::numeric::Quad> {
    let Sym(name) = x else { return None };
    let prog = crate::eval::Program::compile(e, &[name])?.with_real_roots();
    let fun = |t: f64| {
        let v = prog.eval1(t);
        if v.is_nan() {
            // removable singularity such as sin(x)/x at 0
            let h = 1e-7 * (1.0 + t.abs());
            0.5 * (prog.eval1(t + h) + prog.eval1(t - h))
        } else {
            v
        }
    };
    if a.is_infinite() || b.is_infinite() {
        return Some(crate::numeric::quad(&fun, a, b, 1e-10));
    }
    // break at interior singularities / kinks
    let mut breaks = Vec::new();
    let (_, den) = e.numer_denom();
    if !den.is_one() {
        if let Some(dp) = crate::eval::Program::compile(&den, &[name]) {
            for r in crate::numeric::find_roots(&|t| dp.eval1(t), a, b, 400) {
                if r > a && r < b {
                    breaks.push(r);
                }
            }
        }
    }
    if e.has_any_fn(&["abs", "sign", "floor", "ceil", "round", "heaviside", "max", "min", "frac"]) {
        let n = 64;
        for k in 1..n {
            breaks.push(a + (b - a) * k as f64 / n as f64);
        }
        let (lo, hi) = (a.ceil() as i64, b.floor() as i64);
        if hi - lo < 200 {
            for k in lo..=hi {
                breaks.push(k as f64);
            }
        }
    }
    breaks.retain(|r| *r > a && *r < b);
    breaks.sort_by(|p, q| p.partial_cmp(q).unwrap());
    breaks.dedup_by(|p, q| (*p - *q).abs() < 1e-12);
    Some(crate::numeric::quad_breaks(&fun, a, b, &breaks, 1e-10))
}

/// Does the integrand have a (non-removable) pole strictly inside (a, b)?
fn interior_pole(e: &Expr, x: &Expr, a: f64, b: f64) -> bool {
    let Sym(name) = x else { return false };
    let e = &rewrite(e, &|n| match n {
        Func(f0, args) if args.len() == 1 && matches!(&**f0, "tan" | "tanh") => {
            let (s, c) = if &**f0 == "tan" { ("sin", "cos") } else { ("sinh", "cosh") };
            Some(div(f(s, &args[0]), f(c, &args[0])))
        }
        _ => None,
    });
    let (num, den) = simplify::numer_denom(e);
    if den.is_one() {
        return false;
    }
    let (Some(dp), Some(np)) = (crate::eval::Program::compile(&den, &[name]), crate::eval::Program::compile(&num, &[name])) else {
        return false;
    };
    let (lo, hi) = (if a.is_finite() { a } else { -1e3 }, if b.is_finite() { b } else { 1e3 });
    for r in crate::numeric::find_roots(&|t| dp.eval1(t), lo, hi, 600) {
        if r > lo + 1e-9 && r < hi - 1e-9 && np.eval1(r).abs() > 1e-7 {
            return true;
        }
    }
    false
}

/// Definite integral of `e` for x from `a` to `b`.
pub fn definite(e: &Expr, x: &Expr, a: &Expr, b: &Expr) -> Definite {
    if a == b {
        return Definite::Exact(zero());
    }
    let (fa, fb) = (bound_f64(a), bound_f64(b));
    let only_x = e.free_symbols().iter().all(|s| matches!(x, Sym(n) if n == s));
    let numeric = match (fa, fb, only_x) {
        (Some(p), Some(q), true) => {
            if p <= q {
                nintegrate(e, x, p, q)
            } else {
                nintegrate(e, x, q, p).map(|mut r| {
                    r.value = -r.value;
                    r
                })
            }
        }
        _ => None,
    };
    let good_numeric = numeric.as_ref().filter(|q| q.converged && q.value.is_finite());
    let anti = integrate(e, x);
    if let Some(big) = &anti {
        let at = |p: &Expr, dir: Dir| -> Option<Expr> {
            if !p.is_inf() {
                let v = big.subs(x, p);
                if !v.has(&|n| matches!(n, Const(K::Undef | K::CInf | K::Inf))) {
                    return Some(v);
                }
            }
            limit(big, x, p, dir)
        };
        let reversed = matches!((fa, fb), (Some(p), Some(q)) if p > q);
        let (da, db) = if reversed { (Dir::Left, Dir::Right) } else { (Dir::Right, Dir::Left) };
        if let (Some(va), Some(vb)) = (at(a, da), at(b, db)) {
            let inf_a = va.is_inf();
            let inf_b = vb.is_inf();
            if inf_a || inf_b {
                if !(inf_a && inf_b) || va != vb {
                    if good_numeric.is_none() {
                        return Definite::Divergent;
                    }
                }
            } else {
                let v = simplify::simplify(&sub(vb, va));
                let real_integrand = !e.has_const(K::I);
                if real_integrand && v.has_const(K::I) && v.free_symbols().is_empty() {
                    if let (Some(p), Some(q), true) = (fa, fb, only_x) {
                        if interior_pole(e, x, p.min(q), p.max(q)) {
                            return Definite::Divergent;
                        }
                    }
                    if good_numeric.is_none() {
                        return Definite::Unknown;
                    }
                }
                match (good_numeric, v.to_f64_const()) {
                    (Some(q), Some(val)) => {
                        if (q.value - val).abs() <= 1e-6 * (1.0 + val.abs()) + 10.0 * q.error {
                            return Definite::Exact(v);
                        }
                    }
                    (None, _) => {
                        // no numeric confirmation: accept when nothing suspicious
                        match (fa, fb, only_x) {
                            (Some(p), Some(q), true) => {
                                let (lo, hi) = if p <= q { (p, q) } else { (q, p) };
                                if !interior_pole(e, x, lo, hi) && smooth_antiderivative(big, x, lo, hi) {
                                    return Definite::Exact(v);
                                }
                                if interior_pole(e, x, lo, hi) {
                                    return Definite::Divergent;
                                }
                            }
                            _ => return Definite::Exact(v),
                        }
                    }
                    (Some(_), None) => return Definite::Exact(v),
                }
            }
        }
    }
    match numeric {
        Some(q) if q.converged && q.value.is_finite() => Definite::Numeric(q.value, q.error),
        Some(q) if !q.value.is_finite() || q.value.abs() > 1e12 => Definite::Divergent,
        Some(_) => {
            if let (Some(p), Some(q), true) = (fa, fb, only_x) {
                if interior_pole(e, x, p.min(q), p.max(q)) {
                    return Definite::Divergent;
                }
            }
            Definite::Unknown
        }
        None => Definite::Unknown,
    }
}

/// Sample the antiderivative for jumps or non-finite values inside (a, b).
fn smooth_antiderivative(big: &Expr, x: &Expr, a: f64, b: f64) -> bool {
    let Sym(name) = x else { return false };
    let Some(prog) = crate::eval::Program::compile(big, &[name]) else { return false };
    let (lo, hi) = (if a.is_finite() { a } else { -200.0 }, if b.is_finite() { b } else { 200.0 });
    let n = 400;
    let mut prev: Option<f64> = None;
    let mut max_step = 0.0f64;
    let mut steps = Vec::with_capacity(n);
    for k in 1..n {
        let t = lo + (hi - lo) * k as f64 / n as f64;
        let v = prog.eval1(t);
        if !v.is_finite() {
            return false;
        }
        if let Some(p) = prev {
            let d = (v - p).abs();
            steps.push(d);
            max_step = max_step.max(d);
        }
        prev = Some(v);
    }
    if steps.is_empty() {
        return true;
    }
    if max_step < 1e-6 {
        return true;
    }
    // a jump is a step much larger than both of its neighbours
    for i in 1..steps.len().saturating_sub(1) {
        let around = steps[i - 1].max(steps[i + 1]);
        if steps[i] > 20.0 * around + 1e-6 * max_step.max(1.0) {
            return false;
        }
    }
    true
}
