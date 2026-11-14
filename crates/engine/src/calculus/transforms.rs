//! Laplace transforms and their inverses (table based, with linearity,
//! frequency shifting and multiplication by powers of t).

use crate::expr::Expr::*;
use crate::expr::*;
use crate::poly;
use crate::simplify as s;

fn lin(e: &Expr, t: &Expr) -> Option<(Expr, Expr)> {
    if e == t {
        return Some((one(), zero()));
    }
    let c = poly::coeffs(e, t)?;
    if c.len() == 2 {
        Some((c[1].clone(), c[0].clone()))
    } else if c.len() == 1 {
        Some((zero(), c[0].clone()))
    } else {
        None
    }
}

/// L{f}(s) = ∫₀^∞ f(t) e^(-s t) dt
pub fn laplace(f: &Expr, t: &Expr, sv: &Expr) -> Option<Expr> {
    Some(s::simplify(&lt(&s::expand(f), t, sv)?))
}

fn lt(f: &Expr, t: &Expr, sv: &Expr) -> Option<Expr> {
    if !f.has_sym(t) {
        return Some(div(f.clone(), sv.clone()));
    }
    match f {
        Add(v) => return Some(add(v.iter().map(|x| lt(x, t, sv)).collect::<Option<_>>()?)),
        Mul(v) => {
            let (c, rest): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|x| !x.has_sym(t));
            if !c.is_empty() {
                return Some(mul(vec![mul(c), lt(&mul(rest), t, sv)?]));
            }
            // e^(a t) g(t) -> G(s - a)
            if let Some(k) = rest.iter().position(|x| matches!(x, Pow(b, e) if matches!(**b, Const(K::E)) && lin(e, t).is_some())) {
                let Pow(_, ex) = &rest[k] else { unreachable!() };
                let (a, b) = lin(ex, t)?;
                let others: Vec<Expr> = rest.iter().enumerate().filter(|(j, _)| *j != k).map(|(_, x)| x.clone()).collect();
                let g = lt(&mul(others), t, sv)?;
                return Some(mul(vec![exp(b), g.subs(sv, &sub(sv.clone(), a))]));
            }
            // t^n g(t) -> (-1)^n d^n/ds^n G(s)
            if let Some(k) = rest.iter().position(|x| x == t || matches!(x, Pow(b, e) if **b == *t && e.as_i64().map_or(false, |n| n > 0))) {
                let n = match &rest[k] {
                    Pow(_, e) => e.as_i64()?,
                    _ => 1,
                };
                let others: Vec<Expr> = rest.iter().enumerate().filter(|(j, _)| *j != k).map(|(_, x)| x.clone()).collect();
                if others.is_empty() {
                    return lt(&rest[k], t, sv);
                }
                let g = lt(&mul(others), t, sv)?;
                let d = super::diff_n(&g, sv, n as u32);
                return Some(mul(vec![int(if n % 2 == 0 { 1 } else { -1 }), d]));
            }
            None
        }
        _ if f == t => Some(pow(sv.clone(), int(-2))),
        Pow(b, e) if **b == *t => {
            // t^a -> Γ(a + 1)/s^(a + 1), a > -1
            let a = (**e).clone();
            if a.to_f64_const().map_or(true, |v| v <= -1.0) {
                return None;
            }
            Some(div(func("gamma", vec![add(vec![a.clone(), one()])]), pow(sv.clone(), add(vec![a, one()]))))
        }
        Pow(b, e) if matches!(**b, Const(K::E)) => {
            let (a, c) = lin(e, t)?;
            Some(div(exp(c), sub(sv.clone(), a)))
        }
        Pow(b, e) if !b.has_sym(t) => {
            // b^(a t) = e^(a ln b t)
            lt(&exp(mul(vec![(**e).clone(), ln((**b).clone())])), t, sv)
        }
        Func(name, args) if args.len() == 1 => {
            let (k, c) = lin(&args[0], t)?;
            if !c.is_zero() {
                // shift by expansion for sin/cos
                let ex = s::trig_expand(f);
                if &ex != f {
                    return lt(&s::expand(&ex), t, sv);
                }
                if &**name == "heaviside" && k.is_one() {
                    // u(t - a) -> e^(-a s)/s
                    return Some(div(exp(mul(vec![c, sv.clone()])), sv.clone()));
                }
                if &**name == "dirac" && k.is_one() {
                    return Some(exp(mul(vec![c, sv.clone()])));
                }
                return None;
            }
            let s2 = square(sv.clone());
            let k2 = square(k.clone());
            Some(match &**name {
                "sin" => div(k, add(vec![s2, k2])),
                "cos" => div(sv.clone(), add(vec![s2, k2])),
                "sinh" => div(k, sub(s2, k2)),
                "cosh" => div(sv.clone(), sub(s2, k2)),
                "heaviside" => pow(sv.clone(), int(-1)),
                "dirac" => one(),
                _ => return None,
            })
        }
        _ => None,
    }
}

/// Inverse Laplace transform of a rational function of s.
pub fn inverse_laplace(g: &Expr, sv: &Expr, t: &Expr) -> Option<Expr> {
    let g = s::simplify(g);
    let pf = s::apart(&g, sv).unwrap_or_else(|| g.clone());
    let mut out = Vec::new();
    for term in pf.terms() {
        out.push(ilt_term(&term, sv, t)?);
    }
    Some(s::simplify(&add(out)))
}

fn ilt_term(term: &Expr, sv: &Expr, t: &Expr) -> Option<Expr> {
    if !term.has_sym(sv) {
        // constant -> c δ(t)
        return Some(mul(vec![term.clone(), func("dirac", vec![t.clone()])]));
    }
    // split numeric/constant factor
    let (c, rest): (Vec<Expr>, Vec<Expr>) = term.factors().into_iter().partition(|x| !x.has_sym(sv));
    let c = mul(c);
    let rest = mul(rest);
    let (num, den) = rest.numer_denom();
    let (base, k) = match &den {
        Pow(b, e) => ((**b).clone(), e.as_i64()?),
        _ => (den.clone(), 1),
    };
    let dc = poly::coeffs(&base, sv)?;
    let nc = poly::coeffs(&num, sv)?;
    let tn = |n: i64| pow(t.clone(), int(n));
    let fact = |n: i64| func("factorial", vec![int(n)]);
    match dc.len() {
        2 => {
            // A / (p s + q)^k  ->  A/p^k t^(k-1) e^(-q/p t)/(k-1)!
            if nc.len() != 1 {
                return None;
            }
            let (p, q) = (dc[1].clone(), dc[0].clone());
            let a = div(nc[0].clone(), pow(p.clone(), int(k)));
            let r = neg(div(q, p));
            Some(mul(vec![c, a, tn(k - 1), exp(mul(vec![r, t.clone()])), pow(fact(k - 1), int(-1))]))
        }
        3 if k == 1 => {
            // (A s + B)/(a s^2 + b s + c): complete the square
            if nc.len() > 2 {
                return None;
            }
            let (a2, b2, c2) = (dc[2].clone(), dc[1].clone(), dc[0].clone());
            let aa = nc.get(1).cloned().unwrap_or_else(zero);
            let bb = nc.first().cloned().unwrap_or_else(zero);
            let alpha = s::simplify(&div(neg(b2.clone()), mul(vec![int(2), a2.clone()])));
            let w2 = s::simplify(&sub(div(c2, a2.clone()), square(alpha.clone())));
            let a_ = div(aa, a2.clone());
            let b_ = div(bb, a2);
            // (A (s - α) + (B + A α)) / ((s - α)^2 + w²)
            let bsh = s::simplify(&add(vec![b_, mul(vec![a_.clone(), alpha.clone()])]));
            let ea = exp(mul(vec![alpha, t.clone()]));
            let sign = w2.to_f64_const().unwrap_or(1.0);
            let (cosf, sinf) = if sign >= 0.0 { ("cos", "sin") } else { ("cosh", "sinh") };
            let w = s::simplify(&sqrt(if sign >= 0.0 { w2 } else { neg(w2) }));
            if w.is_zero() {
                return None;
            }
            Some(mul(vec![c, ea, add(vec![mul(vec![a_, f1(cosf, mul(vec![w.clone(), t.clone()]))]), mul(vec![div(bsh, w.clone()), f1(sinf, mul(vec![w, t.clone()]))])])]))
        }
        _ => None,
    }
}
