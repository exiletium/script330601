//! Symbolic differentiation.

use crate::budget;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::print::text;
use crate::steps;

/// d/dx of `e`.
pub fn diff(e: &Expr, x: &Expr) -> Expr {
    d(e, x, &[])
}

/// n-th derivative.
pub fn diff_n(e: &Expr, x: &Expr, n: u32) -> Expr {
    let mut r = e.clone();
    for _ in 0..n {
        r = d(&r, x, &[]);
        if r.is_zero() {
            break;
        }
    }
    r
}

/// Derivative where the symbols in `deps` are unknown functions of `x`
/// (used for implicit differentiation and differential equations).
pub fn diff_dep(e: &Expr, x: &Expr, deps: &[Name]) -> Expr {
    d(e, x, deps)
}

fn holder(e: &Expr, x: &Expr) -> Expr {
    raw_fn("Derivative", vec![e.clone(), x.clone(), one()])
}

fn d(e: &Expr, x: &Expr, deps: &[Name]) -> Expr {
    budget::tick();
    match e {
        Num(_) | Float(_) | Const(_) => zero(),
        Sym(n) => {
            if e == x {
                one()
            } else if deps.contains(n) {
                holder(e, x)
            } else {
                zero()
            }
        }
        Add(v) => add(v.iter().map(|t| d(t, x, deps)).collect()),
        Mul(v) => {
            let mut terms = Vec::new();
            for k in 0..v.len() {
                let dk = d(&v[k], x, deps);
                if dk.is_zero() {
                    continue;
                }
                let mut fs: Vec<Expr> = Vec::with_capacity(v.len());
                for (j, f) in v.iter().enumerate() {
                    fs.push(if j == k { dk.clone() } else { f.clone() });
                }
                terms.push(mul(fs));
            }
            add(terms)
        }
        Pow(b, ex) => {
            let db = d(b, x, deps);
            let de = d(ex, x, deps);
            match (db.is_zero(), de.is_zero()) {
                (true, true) => zero(),
                (false, true) => mul(vec![(**ex).clone(), pow((**b).clone(), sub((**ex).clone(), one())), db]),
                (true, false) => mul(vec![e.clone(), ln((**b).clone()), de]),
                (false, false) => mul(vec![e.clone(), add(vec![mul(vec![de, ln((**b).clone())]), mul(vec![(**ex).clone(), db, pow((**b).clone(), int(-1))])])]),
            }
        }
        Func(name, args) => dfunc(e, name, args, x, deps),
        List(v) => List(v.iter().map(|t| d(t, x, deps)).collect()),
        Rel(op, a, b) => Rel(*op, Box::new(d(a, x, deps)), Box::new(d(b, x, deps))),
    }
}

/// Derivative of a one-argument function with respect to its argument.
pub fn dfunc1(name: &str, u: &Expr) -> Option<Expr> {
    let u = u.clone();
    let two_over_sqrt_pi = || mul(vec![int(2), pow(pi(), rat(-1, 2))]);
    Some(match name {
        "sin" => func("cos", vec![u]),
        "cos" => neg(func("sin", vec![u])),
        "tan" => pow(func("cos", vec![u]), int(-2)),
        "asin" => pow(sub(one(), square(u)), rat(-1, 2)),
        "acos" => neg(pow(sub(one(), square(u)), rat(-1, 2))),
        "atan" => pow(add(vec![one(), square(u)]), int(-1)),
        "sinh" => func("cosh", vec![u]),
        "cosh" => func("sinh", vec![u]),
        "tanh" => pow(func("cosh", vec![u]), int(-2)),
        "asinh" => pow(add(vec![square(u), one()]), rat(-1, 2)),
        "acosh" => mul(vec![pow(sub(u.clone(), one()), rat(-1, 2)), pow(add(vec![u, one()]), rat(-1, 2))]),
        "atanh" => pow(sub(one(), square(u)), int(-1)),
        "ln" => pow(u, int(-1)),
        "abs" => div(u.clone(), func("abs", vec![u])),
        "sign" | "floor" | "ceil" | "round" => zero(),
        "erf" => mul(vec![two_over_sqrt_pi(), exp(neg(square(u)))]),
        "erfc" => neg(mul(vec![two_over_sqrt_pi(), exp(neg(square(u)))])),
        "erfi" => mul(vec![two_over_sqrt_pi(), exp(square(u))]),
        "gamma" => mul(vec![func("gamma", vec![u.clone()]), func("digamma", vec![u])]),
        "lgamma" => func("digamma", vec![u]),
        "digamma" => func("polygamma", vec![one(), u]),
        "factorial" => {
            let u1 = add(vec![u, one()]);
            mul(vec![func("gamma", vec![u1.clone()]), func("digamma", vec![u1])])
        }
        "Si" => div(func("sin", vec![u.clone()]), u),
        "Ci" => div(func("cos", vec![u.clone()]), u),
        "Ei" => div(exp(u.clone()), u),
        "li" => pow(ln(u), int(-1)),
        "W" => {
            let w = func("W", vec![u.clone()]);
            div(w.clone(), mul(vec![u, add(vec![one(), w])]))
        }
        "fresnels" => func("sin", vec![mul(vec![rat(1, 2), pi(), square(u)])]),
        "fresnelc" => func("cos", vec![mul(vec![rat(1, 2), pi(), square(u)])]),
        "heaviside" => func("dirac", vec![u]),
        "sinc" => div(sub(mul(vec![u.clone(), func("cos", vec![u.clone()])]), func("sin", vec![u.clone()])), square(u)),
        "airyai" => func("airyaiprime", vec![u]),
        "airybi" => func("airybiprime", vec![u]),
        "airyaiprime" => mul(vec![u.clone(), func("airyai", vec![u])]),
        "airybiprime" => mul(vec![u.clone(), func("airybi", vec![u])]),
        "ellipticK" => {
            // dK/dm = (E - (1-m) K) / (2 m (1-m))
            let k = func("ellipticK", vec![u.clone()]);
            let ee = func("ellipticE", vec![u.clone()]);
            let om = sub(one(), u.clone());
            div(sub(ee, mul(vec![om.clone(), k])), mul(vec![int(2), u, om]))
        }
        "ellipticE" => {
            let k = func("ellipticK", vec![u.clone()]);
            let ee = func("ellipticE", vec![u.clone()]);
            div(sub(ee, k), mul(vec![int(2), u]))
        }
        _ => return None,
    })
}

fn dfunc(e: &Expr, name: &str, args: &[Expr], x: &Expr, deps: &[Name]) -> Expr {
    // holders
    match (name, args.len()) {
        ("Derivative", 3) => {
            if args[1] == *x {
                return raw_fn("Derivative", vec![args[0].clone(), x.clone(), add(vec![args[2].clone(), one()])]);
            }
            let inner = d(&args[0], x, deps);
            if inner.is_zero() {
                return zero();
            }
            return holder(e, x);
        }
        ("Integral", 2) => {
            if args[1] == *x {
                return args[0].clone();
            }
            let inner = d(&args[0], x, deps);
            if inner.is_zero() {
                return zero();
            }
            return raw_fn("Integral", vec![inner, args[1].clone()]);
        }
        ("Integral", 4) => {
            // Leibniz rule
            let (f, t, a, b) = (&args[0], &args[1], &args[2], &args[3]);
            let mut terms = Vec::new();
            let db = d(b, x, deps);
            if !db.is_zero() {
                terms.push(mul(vec![f.subs(t, b), db]));
            }
            let da = d(a, x, deps);
            if !da.is_zero() {
                terms.push(neg(mul(vec![f.subs(t, a), da])));
            }
            if t != x {
                let inner = d(f, x, deps);
                if !inner.is_zero() {
                    terms.push(raw_fn("Integral", vec![inner, t.clone(), a.clone(), b.clone()]));
                }
            }
            return add(terms);
        }
        ("Sum", 4) => {
            let inner = d(&args[0], x, deps);
            if inner.is_zero() {
                return zero();
            }
            return raw_fn("Sum", vec![inner, args[1].clone(), args[2].clone(), args[3].clone()]);
        }
        _ => {}
    }
    if args.len() == 1 {
        let du = d(&args[0], x, deps);
        if du.is_zero() {
            return zero();
        }
        if let Some(fp) = dfunc1(name, &args[0]) {
            return mul(vec![fp, du]);
        }
        return holder(e, x);
    }
    if args.len() == 2 {
        let (a, b) = (&args[0], &args[1]);
        let da = d(a, x, deps);
        let db = d(b, x, deps);
        if da.is_zero() && db.is_zero() {
            return zero();
        }
        let shift = |k: i64| add(vec![a.clone(), int(k)]);
        match name {
            "besselj" | "bessely" if da.is_zero() => {
                let f = |k: i64| func(name, vec![shift(k), b.clone()]);
                return mul(vec![rat(1, 2), sub(f(-1), f(1)), db]);
            }
            "besseli" if da.is_zero() => {
                let f = |k: i64| func(name, vec![shift(k), b.clone()]);
                return mul(vec![rat(1, 2), add(vec![f(-1), f(1)]), db]);
            }
            "besselk" if da.is_zero() => {
                let f = |k: i64| func(name, vec![shift(k), b.clone()]);
                return mul(vec![rat(-1, 2), add(vec![f(-1), f(1)]), db]);
            }
            "atan2" => {
                // atan2(y, x)
                let den = add(vec![square(a.clone()), square(b.clone())]);
                return div(sub(mul(vec![b.clone(), da]), mul(vec![a.clone(), db])), den);
            }
            "polylog" if da.is_zero() => {
                return mul(vec![func("polylog", vec![shift(-1), b.clone()]), pow(b.clone(), int(-1)), db]);
            }
            "polygamma" if da.is_zero() => {
                return mul(vec![func("polygamma", vec![shift(1), b.clone()]), db]);
            }
            "log" => {
                // log(base, value)
                return d(&div(ln(b.clone()), ln(a.clone())), x, deps);
            }
            "surd" if db.is_zero() => {
                // surd(u, n) = real n-th root
                return mul(vec![div(e.clone(), mul(vec![b.clone(), a.clone()])), da]);
            }
            "max" | "min" => {
                let cond = if name == "max" { sub(a.clone(), b.clone()) } else { sub(b.clone(), a.clone()) };
                let h = func("heaviside", vec![cond]);
                return add(vec![mul(vec![h.clone(), da]), mul(vec![sub(one(), h), db])]);
            }
            _ => {}
        }
    }
    if args.iter().all(|a| d(a, x, deps).is_zero()) {
        return zero();
    }
    holder(e, x)
}

// ---------------------------------------------------------------------------
// vector calculus helpers
// ---------------------------------------------------------------------------

pub fn gradient(f: &Expr, vars: &[Expr]) -> Vec<Expr> {
    vars.iter().map(|v| diff(f, v)).collect()
}

pub fn hessian(f: &Expr, vars: &[Expr]) -> Vec<Vec<Expr>> {
    vars.iter().map(|a| vars.iter().map(|b| diff(&diff(f, a), b)).collect()).collect()
}

pub fn jacobian(fs: &[Expr], vars: &[Expr]) -> Vec<Vec<Expr>> {
    fs.iter().map(|f| gradient(f, vars)).collect()
}

pub fn divergence(fs: &[Expr], vars: &[Expr]) -> Expr {
    add(fs.iter().zip(vars).map(|(f, v)| diff(f, v)).collect())
}

pub fn curl(f: &[Expr], v: &[Expr]) -> Option<Vec<Expr>> {
    if f.len() != 3 || v.len() != 3 {
        return None;
    }
    Some(vec![
        sub(diff(&f[2], &v[1]), diff(&f[1], &v[2])),
        sub(diff(&f[0], &v[2]), diff(&f[2], &v[0])),
        sub(diff(&f[1], &v[0]), diff(&f[0], &v[1])),
    ])
}

pub fn laplacian(f: &Expr, vars: &[Expr]) -> Expr {
    add(vars.iter().map(|v| diff_n(f, v, 2)).collect())
}

// ---------------------------------------------------------------------------
// worked solution
// ---------------------------------------------------------------------------

/// Differentiate while recording the rules used (see [`crate::steps`]).
pub fn diff_explained(e: &Expr, x: &Expr) -> Expr {
    let r = explain(e, x, 0);
    let s = crate::simplify::simplify(&r);
    if s != r {
        steps::step("Simplify:", Some(&s));
    }
    s
}

fn explain(e: &Expr, x: &Expr, depth: usize) -> Expr {
    let result = diff(e, x);
    let xs = text(x);
    if depth > 4 || !steps::enabled() {
        return result;
    }
    let dd = |u: &Expr| format!("d/d{xs}({})", text(u));
    match e {
        _ if !e.has_sym(x) => {
            steps::step(format!("The derivative of the constant {} is zero.", text(e)), None);
        }
        Sym(_) => {
            steps::step(format!("The derivative of {xs} is 1."), None);
        }
        Add(v) => {
            steps::step("Differentiate the sum term by term:", Some(&raw_fn("Derivative", vec![e.clone(), x.clone(), one()])));
            for t in v {
                if matches!(t, Add(_)) || !t.has_sym(x) {
                    continue;
                }
                steps::nested(|| explain(t, x, depth + 1));
            }
        }
        Mul(v) => {
            let (cs, fs): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|f| !f.has_sym(x));
            if !cs.is_empty() {
                let rest = mul(fs);
                steps::step(format!("Factor out the constant {}:", text(&mul(cs))), None);
                steps::nested(|| explain(&rest, x, depth + 1));
            } else if fs.len() >= 2 {
                let u = fs[0].clone();
                let w = mul(fs[1..].to_vec());
                steps::step(format!("Use the product rule, d/d{xs}(u v) = v du/d{xs} + u dv/d{xs}, with u = {} and v = {}:", text(&u), text(&w)), None);
                steps::nested(|| {
                    explain(&u, x, depth + 1);
                    explain(&w, x, depth + 1);
                });
            }
        }
        Pow(b, ex) => {
            if **b == *x && !ex.has_sym(x) {
                steps::step(format!("Use the power rule, d/d{xs}({xs}^n) = n {xs}^(n - 1), with n = {}:", text(ex)), None);
            } else if !ex.has_sym(x) {
                steps::step(format!("Use the chain rule, {} = d(u^n)/du · du/d{xs}, where u = {} and n = {}:", dd(e), text(b), text(ex)), None);
                steps::nested(|| explain(b, x, depth + 1));
            } else if !b.has_sym(x) {
                steps::step(format!("Use the chain rule with d/du({}^u) = {}^u log({}), where u = {}:", text(b), text(b), text(b), text(ex)), None);
                if **ex != *x {
                    steps::nested(|| explain(ex, x, depth + 1));
                }
            } else {
                steps::step(format!("Write {} as e^({} log({})) and use the chain rule:", text(e), text(ex), text(b)), None);
            }
        }
        Func(name, args) if args.len() == 1 => {
            let u = sym("u");
            if let Some(fp) = dfunc1(name, &u) {
                let fu = func(name, vec![u.clone()]);
                if args[0] == *x {
                    steps::step(format!("The derivative of {} is {}:", text(&fu.subs(&u, x)), text(&fp.subs(&u, x))), None);
                } else {
                    steps::step(format!("Use the chain rule, {} = d({})/du · du/d{xs}, where u = {} and d({})/du = {}:", dd(e), text(&fu), text(&args[0]), text(&fu), text(&fp)), None);
                    steps::nested(|| explain(&args[0], x, depth + 1));
                }
            }
        }
        _ => {}
    }
    steps::step(format!("{} =", dd(e)), Some(&result));
    result
}
