//! Worked solutions for limits, sums and equations.  The answers come from
//! the solvers; this module narrates a human-style derivation of them.

use crate::calculus::{self, Dir};
use crate::expr::Expr::*;
use crate::expr::*;
use crate::poly;
use crate::print::text;
use crate::simplify as s;
use crate::solve;
use crate::steps::Step;

fn st(out: &mut Vec<Step>, depth: usize, t: impl Into<String>, m: Option<Expr>) {
    out.push(Step { text: t.into(), math: m, depth });
}

fn lim_holder(f: &Expr, x: &Expr, a: &Expr) -> Expr {
    raw_fn("Limit", vec![f.clone(), x.clone(), a.clone()])
}

fn is_inf(e: &Expr) -> bool {
    e.is_inf() || e.is_neg_inf()
}

// ---------------------------------------------------------------------------
// limits
// ---------------------------------------------------------------------------

pub fn limit_steps(f: &Expr, x: &Expr, a: &Expr, dir: Dir) -> Vec<Step> {
    let mut out = Vec::new();
    let result = calculus::limit(f, x, a, dir);
    lim_rec(f, x, a, dir, 0, &mut out);
    if let Some(r) = result {
        st(&mut out, 0, "Therefore:", Some(eq(lim_holder(f, x, a), r)));
    } else {
        st(&mut out, 0, "The one-sided limits differ or are unbounded, so the limit does not exist.", None);
    }
    out
}

fn lim_rec(f: &Expr, x: &Expr, a: &Expr, dir: Dir, depth: usize, out: &mut Vec<Step>) {
    if depth > 3 {
        return;
    }
    let lim = |e: &Expr| calculus::limit(e, x, a, dir);
    let at = if is_inf(a) { None } else { Some(s::simplify(&f.subs(x, a))) };
    if let Some(v) = &at {
        if !v.has(&|n| matches!(n, Const(K::Undef | K::CInf | K::Inf))) && !f.has_any_fn(&["floor", "ceil", "sign", "abs", "heaviside"]) {
            st(out, depth, format!("The function is continuous at {} = {}, so substitute directly:", text(x), text(a)), Some(eq(lim_holder(f, x, a), v.clone())));
            return;
        }
    }
    let (num, den) = {
        let (n0, d0) = f.numer_denom();
        if d0.has_sym(x) { (n0, d0) } else { s::numer_denom(&s::together(f)) }
    };
    if !den.is_one() && den.has_sym(x) {
        let (ln, ld) = (lim(&num), lim(&den));
        if let (Some(ln), Some(ld)) = (&ln, &ld) {
            let zz = ln.is_zero() && ld.is_zero();
            let ii = is_inf(ln) && is_inf(ld);
            if zz || ii {
                let form = if zz { "0/0" } else { "∞/∞" };
                // rational function at infinity: divide by the dominant power
                if ii && is_inf(a) {
                    if let (Some(dn), Some(dd)) = (poly::degree(&num, x), poly::degree(&den, x)) {
                        let k = dn.max(dd) as i64;
                        let xn = pow(x.clone(), int(k));
                        st(out, depth, format!("Both numerator and denominator grow without bound ({form}). Divide both by {} :", text(&xn)), Some(div(s::expand(&div(num.clone(), xn.clone())), s::expand(&div(den.clone(), xn)))));
                        st(out, depth, "Every term with a negative power of x tends to 0.".to_string(), None);
                        return;
                    }
                }
                // polynomial 0/0: factor and cancel the common factor
                if zz && poly::coeffs(&num, x).is_some() && poly::coeffs(&den, x).is_some() {
                    let (fnum, fden) = (s::factor(&num), s::factor(&den));
                    let c = s::cancel(&div(num.clone(), den.clone()));
                    st(out, depth, format!("Substituting gives the indeterminate form 0/0, so numerator and denominator share the factor ({} - {}). Factor:", text(x), text(a)), Some(div(fnum, fden)));
                    st(out, depth, "Cancel the common factor:", Some(lim_holder(&c, x, a)));
                    lim_rec(&c, x, a, dir, depth + 1, out);
                    return;
                }
                let (dn, dd) = (s::simplify(&calculus::diff(&num, x)), s::simplify(&calculus::diff(&den, x)));
                st(out, depth, format!("Substituting gives the indeterminate form {form}. Apply L'Hôpital's rule (differentiate numerator and denominator):"), Some(lim_holder(&div(dn.clone(), dd.clone()), x, a)));
                let q = s::cancel(&div(dn, dd));
                lim_rec(&q, x, a, dir, depth + 1, out);
                return;
            }
            if ld.is_zero() && !ln.is_zero() {
                st(out, depth, format!("The denominator tends to 0 while the numerator tends to {}, so the function grows without bound; the sign depends on the side of approach.", text(ln)), None);
                return;
            }
        }
    }
    // 0·∞: rewrite the product as a quotient
    if let Mul(v) = f {
        let lims: Vec<Option<Expr>> = v.iter().map(|t| lim(t)).collect();
        let zi = lims.iter().position(|l| l.as_ref().map_or(false, |l| l.is_zero()));
        let ii = lims.iter().position(|l| l.as_ref().map_or(false, is_inf));
        if let (Some(zi), Some(ii)) = (zi, ii) {
            // keep logarithms in the numerator
            let flip = if v[zi].has_fn("ln") { ii } else { zi };
            let numer = mul(v.iter().enumerate().filter(|(k, _)| *k != flip).map(|(_, t)| t.clone()).collect());
            let denom = pow(v[flip].clone(), int(-1));
            st(out, depth, format!("This is the indeterminate form 0·∞. Rewrite the product as the quotient ({}) / ({}).", text(&numer), text(&denom)), None);
            let (dn, dd) = (s::simplify(&calculus::diff(&numer, x)), s::simplify(&calculus::diff(&denom, x)));
            let q = s::simplify(&s::cancel(&div(dn, dd)));
            st(out, depth, "Now the form is ∞/∞. Apply L'Hôpital's rule (differentiate numerator and denominator) and simplify:", Some(lim_holder(&q, x, a)));
            lim_rec(&q, x, a, dir, depth + 1, out);
            return;
        }
    }
    // ∞ − ∞ with a square root: multiply by the conjugate
    if let Add(v) = f {
        let is_root = |t: &Expr| t.factors().iter().any(|u| matches!(u, Pow(b, e) if b.has_sym(x) && **e == rat(1, 2)));
        if v.len() >= 2 && v.iter().filter(|t| is_root(t)).count() == 1 && is_inf(a) {
            let root = v.iter().find(|t| is_root(t)).cloned().unwrap_or_else(zero);
            let rest = add(v.iter().filter(|t| !is_root(t)).cloned().collect());
            let conj = sub(root.clone(), rest.clone());
            let top = s::expand(&sub(square(root.clone()), square(rest.clone())));
            st(out, depth, "This is the indeterminate form ∞ − ∞. Multiply and divide by the conjugate expression:", Some(conj.clone()));
            st(out, depth, format!("The numerator becomes a difference of squares, giving ({}) / ({}).", text(&top), text(&conj)), None);
            if let (Some(dt), Some(l)) = (poly::degree(&top, x), calculus::limit(&div(top.clone(), conj.clone()), x, a, dir)) {
                st(out, depth, format!("Divide numerator and denominator by {}; the remaining terms with negative powers tend to 0, leaving {}.", text(&pow(x.clone(), int(dt as i64))), text(&l)), None);
            }
            return;
        }
    }
    // products 0·∞ and powers 1^∞ via logarithms
    if f.has(&|n| matches!(n, Pow(_, e) if e.has_sym(x))) {
        let lg = s::simplify(&f1("ln", f.clone()));
        st(out, depth, "The variable appears in an exponent. Take logarithms and find the limit of:", Some(lim_holder(&s::expand_log(&lg), x, a)));
        if let Some(l) = lim(&s::expand_log(&lg)) {
            st(out, depth + 1, "", Some(eq(lim_holder(&s::expand_log(&lg), x, a), l.clone())));
            st(out, depth, "Exponentiate to undo the logarithm:", Some(eq(lim_holder(f, x, a), exp(l))));
        }
        return;
    }
    // series
    let point = if is_inf(a) { zero() } else { a.clone() };
    if !is_inf(a) {
        if let Some(ex) = calculus::series(f, x, &point, 3) {
            if let Some(t0) = ex.terms.first() {
                st(out, depth, format!("Expand in a series around {} = {}; the leading term determines the limit:", text(x), text(a)), Some(t0.clone()));
                return;
            }
        }
    }
    st(out, depth, "Analyse the dominant behaviour of each part of the expression:", None);
}

// ---------------------------------------------------------------------------
// sums
// ---------------------------------------------------------------------------

pub fn sum_steps(f: &Expr, k: &Expr, lo: &Expr, hi: &Expr) -> Vec<Step> {
    let mut out = Vec::new();
    let infinite = hi.is_inf();
    let holder = raw_fn("Sum", vec![f.clone(), k.clone(), lo.clone(), hi.clone()]);
    // polynomial terms: Faulhaber
    if let Some(cs) = poly::coeffs(f, k) {
        if !infinite {
            st(&mut out, 0, "The summand is a polynomial in the index. Split the sum by linearity and use the power-sum formulas:", None);
            let n = sym("n");
            let formulas = [
                (0usize, add(vec![n.clone()])),
                (1, div(mul(vec![n.clone(), add(vec![n.clone(), one()])]), int(2))),
                (2, div(mul(vec![n.clone(), add(vec![n.clone(), one()]), add(vec![mul(vec![int(2), n.clone()]), one()])]), int(6))),
                (3, square(div(mul(vec![n.clone(), add(vec![n.clone(), one()])]), int(2)))),
            ];
            for (p, fm) in formulas.iter() {
                if cs.get(*p).map_or(false, |c| !c.is_zero()) {
                    let lhs = raw_fn("Sum", vec![if *p == 0 { one() } else { pow(k.clone(), int(*p as i64)) }, k.clone(), one(), n.clone()]);
                    st(&mut out, 1, "", Some(eq(lhs, fm.clone())));
                }
            }
            if cs.len() > 4 {
                st(&mut out, 1, "Higher powers use Faulhaber's formula with Bernoulli numbers.", None);
            }
            finish_sum(&mut out, &holder);
            return out;
        }
        st(&mut out, 0, "The terms do not tend to 0, so the series diverges (divergence test).", None);
        return out;
    }
    // known Taylor series
    if f.has_fn("factorial") {
        st(&mut out, 0, "Recognise the Maclaurin series Σ x^k/k! = e^x (its even and odd parts give cosh and sinh; with alternating signs, cos and sin).", None);
        finish_sum(&mut out, &holder);
        return out;
    }
    // geometric
    let has_geo = f.has(&|n| matches!(n, Pow(b, e) if !b.has_sym(k) && e.has_sym(k)));
    if has_geo {
        let ratio = s::simplify(&div(f.subs(k, &add(vec![k.clone(), one()])), f.clone()));
        if !ratio.has_sym(k) {
            st(&mut out, 0, format!("Consecutive terms have the constant ratio r = {}, so this is a geometric series.", text(&ratio)), None);
            if infinite {
                let first = s::simplify(&f.subs(k, lo));
                st(&mut out, 0, format!("Since |r| < 1, the sum is (first term)/(1 − r) with first term {}:", text(&first)), Some(div(first, sub(one(), ratio))));
            } else {
                st(&mut out, 0, "Use the finite geometric sum a (1 − r^N)/(1 − r), where N is the number of terms.", None);
            }
            finish_sum(&mut out, &holder);
            return out;
        }
        st(&mut out, 0, "The summand is a polynomial times a geometric factor; use the derivative of the geometric series Σ z^k = 1/(1 − z).", None);
        finish_sum(&mut out, &holder);
        return out;
    }
    // rational: telescoping or ζ
    if !f.has(&|n| matches!(n, Func(..))) && poly::coeffs(&s::numer_denom(f).1, k).is_some() {
        if let Some(pf) = s::apart(f, k) {
            if pf.terms().len() > 1 {
                st(&mut out, 0, "Decompose the summand into partial fractions:", Some(pf.clone()));
                st(&mut out, 0, "The sum telescopes: consecutive terms cancel, leaving only the first and last few terms.", None);
                finish_sum(&mut out, &holder);
                return out;
            }
        }
        let (num, den) = s::numer_denom(f);
        if let (Some(dn), Some(dd)) = (poly::degree(&num, k), poly::degree(&den, k)) {
            let p = dd as i64 - dn as i64;
            if infinite {
                if p <= 1 {
                    st(&mut out, 0, format!("The terms behave like 1/{}^{p} for large {}; compare with the harmonic series Σ 1/{}: the series diverges.", text(k), text(k), text(k)), None);
                    return out;
                }
                st(&mut out, 0, format!("This is a p-series with p = {p} > 1, which converges; its value is the Riemann zeta function ζ({p}) (adjusted for the starting index)."), None);
                finish_sum(&mut out, &holder);
                return out;
            }
        }
    }
    if f.has(&|n| matches!(n, Pow(b, _) if **b == int(-1))) {
        st(&mut out, 0, "The series alternates in sign; recognise the Taylor series of log(1 + x) = Σ (−1)^(k+1) x^k/k, or apply the alternating series test.", None);
        finish_sum(&mut out, &holder);
        return out;
    }
    finish_sum(&mut out, &holder);
    out
}

fn finish_sum(out: &mut Vec<Step>, holder: &Expr) {
    let r = calculus::doit(holder);
    if !r.has(&|n| matches!(n, Func(f, _) if &**f == "Sum")) {
        st(out, 0, "Result:", Some(eq(holder.clone(), r)));
    }
}

// ---------------------------------------------------------------------------
// equations
// ---------------------------------------------------------------------------

pub fn equation_steps(lhs: &Expr, rhs: &Expr, x: &Expr) -> Vec<Step> {
    let mut out = Vec::new();
    let f = s::simplify(&sub(lhs.clone(), rhs.clone()));
    // polynomial
    if let Some(cs) = poly::coeffs(&s::expand(&f), x) {
        let deg = cs.len() - 1;
        if deg == 1 {
            st(&mut out, 0, format!("The equation is linear in {}. Collect the terms:", text(x)), Some(eq(mul(vec![cs[1].clone(), x.clone()]), neg(cs[0].clone()))));
            st(&mut out, 0, format!("Divide both sides by {}:", text(&cs[1])), Some(eq(x.clone(), s::simplify(&div(neg(cs[0].clone()), cs[1].clone())))));
            return out;
        }
        if deg >= 3 {
            let fac = s::factor(&f);
            if matches!(fac, Mul(_)) || matches!(fac, Pow(..)) {
                st(&mut out, 0, "Factor the polynomial:", Some(eq(fac.clone(), zero())));
                st(&mut out, 0, "A product is zero exactly when one of its factors is zero. Solve each factor:", None);
                for g in fac.factors() {
                    let (b, _) = g.base_exp();
                    if b.has_sym(x) {
                        let rs = solve::solve(&b, x);
                        let shown: Vec<String> = rs.roots.iter().map(|r| text(&eq(x.clone(), r.clone()))).collect();
                        st(&mut out, 1, format!("{} = 0  ⇒  {}", text(&b), shown.join(" or ")), None);
                    }
                }
                return out;
            }
            st(&mut out, 0, format!("The polynomial of degree {deg} has no factors over the rationals; use the general formula (degree 3) or numerical root finding."), None);
            return out;
        }
    }
    // a single occurrence of x: undo the operations one at a time
    let occurs = count(&f, x);
    if occurs == 1 {
        st(&mut out, 0, format!("{} occurs once, so undo the operations applied to it one at a time:", text(x)), None);
        let (mut l, mut r) = (lhs.clone(), rhs.clone());
        if !l.has_sym(x) {
            std::mem::swap(&mut l, &mut r);
        }
        let mut guard = 0;
        while l != *x && guard < 12 {
            guard += 1;
            let (nl, nr, what) = match &l {
                Add(v) => {
                    let (with, without): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|t| t.has_sym(x));
                    let c = add(without);
                    (with.into_iter().next().unwrap_or_else(zero), s::simplify(&sub(r.clone(), c.clone())), format!("Subtract {} from both sides:", text(&c)))
                }
                Mul(v) => {
                    let (with, without): (Vec<Expr>, Vec<Expr>) = v.iter().cloned().partition(|t| t.has_sym(x));
                    let c = mul(without);
                    (with.into_iter().next().unwrap_or_else(one), s::simplify(&div(r.clone(), c.clone())), format!("Divide both sides by {}:", text(&c)))
                }
                Pow(b, e) if b.has_sym(x) => {
                    let inv = s::simplify(&pow(r.clone(), pow((**e).clone(), int(-1))));
                    ((**b).clone(), inv, format!("Take the {} root of both sides (keep both signs for even roots):", if e.as_i64() == Some(2) { "square".to_string() } else { format!("({})-th", text(e)) }))
                }
                Pow(b, e) => {
                    let lb = if matches!(**b, Const(K::E)) { one() } else { ln((**b).clone()) };
                    ((**e).clone(), s::simplify(&div(ln(r.clone()), lb)), "Take logarithms of both sides:".into())
                }
                Func(n, a) if a.len() == 1 => {
                    let inv = match &**n {
                        "sin" => "asin",
                        "cos" => "acos",
                        "tan" => "atan",
                        "ln" => "exp",
                        "exp" => "ln",
                        "sinh" => "asinh",
                        "cosh" => "acosh",
                        "tanh" => "atanh",
                        "asin" => "sin",
                        "acos" => "cos",
                        "atan" => "tan",
                        _ => break,
                    };
                    let periodic = matches!(&**n, "sin" | "cos" | "tan");
                    let note = if periodic { format!(" ({} is periodic, so add multiples of its period; for sin and cos there is also a second solution in each period)", n) } else { String::new() };
                    (a[0].clone(), s::simplify(&func(inv, vec![r.clone()])), format!("Apply the inverse function {inv} to both sides{note}:"))
                }
                _ => break,
            };
            st(&mut out, 1, what, Some(eq(nl.clone(), nr.clone())));
            l = nl;
            r = nr;
        }
        return out;
    }
    // substitution for repeated kernels
    if f.has(&|n| matches!(n, Pow(b, e) if !b.has_sym(x) && e.has_sym(x))) {
        st(&mut out, 0, format!("The equation is a polynomial in an exponential of {}. Substitute u for the exponential, solve the resulting polynomial for u, then solve u = exponential for {} (discarding non-positive u).", text(x), text(x)), None);
        return out;
    }
    if f.has_any_fn(&["sin", "cos", "tan"]) {
        st(&mut out, 0, "Rewrite the equation in terms of a single trigonometric function (using sin² + cos² = 1 or the half-angle substitution t = tan(x/2)), solve for it, then invert, adding the period.".to_string(), None);
        return out;
    }
    if f.has(&|n| matches!(n, Pow(b, e) if b.has_sym(x) && e.as_rat().map_or(false, |r| !r.is_integer()))) {
        if let Some(()) = radical_steps(lhs, rhs, x, &mut out) {
            return out;
        }
        st(&mut out, 0, "Isolate a radical and raise both sides to the power of its index; repeat until no radicals remain. Squaring can introduce extraneous solutions, so check every candidate in the original equation.", None);
        return out;
    }
    if !f.free_symbols().is_empty() {
        st(&mut out, 0, "No algebraic method isolates the variable; the solutions are found numerically by bracketing sign changes and refining with Brent's method.", None);
    }
    out
}

/// √(…) = g(x): isolate the root, square, solve, and check the candidates.
fn radical_steps(lhs: &Expr, rhs: &Expr, x: &Expr, out: &mut Vec<Step>) -> Option<()> {
    let f = sub(lhs.clone(), rhs.clone());
    let half = |t: &Expr| matches!(t, Pow(b, e) if b.has_sym(x) && **e == rat(1, 2));
    let terms = f.terms();
    let rad: Vec<&Expr> = terms.iter().filter(|t| t.factors().iter().any(|u| half(u))).collect();
    if rad.len() != 1 {
        return None;
    }
    let r = rad[0].clone();
    let other = neg(add(terms.iter().filter(|t| **t != r).cloned().collect()));
    if other.has(&|n| matches!(n, Pow(_, e) if e.as_rat().map_or(false, |q| !q.is_integer()))) {
        return None;
    }
    st(out, 0, "Isolate the square root on one side:", Some(eq(r.clone(), other.clone())));
    let (l2, r2) = (s::expand(&square(r.clone())), s::expand(&square(other.clone())));
    st(out, 0, "Square both sides (this can introduce extraneous solutions):", Some(eq(l2.clone(), r2.clone())));
    let g = s::expand(&sub(r2, l2));
    st(out, 0, "Move everything to one side:", Some(eq(g.clone(), zero())));
    let cands = solve::solve(&g, x);
    if cands.roots.is_empty() {
        return Some(());
    }
    st(out, 0, format!("Solve this equation: {}", cands.roots.iter().map(|c| text(&eq(x.clone(), c.clone()))).collect::<Vec<_>>().join(" or ")), None);
    st(out, 0, "Check each candidate in the original equation:", None);
    for c in &cands.roots {
        let (lv, rv) = (lhs.subs(x, c).to_f64_const(), rhs.subs(x, c).to_f64_const());
        let ok = matches!((lv, rv), (Some(a), Some(b)) if (a - b).abs() < 1e-9 * (1.0 + a.abs()));
        let (ls, rs) = (s::simplify(&lhs.subs(x, c)), s::simplify(&rhs.subs(x, c)));
        st(out, 1, format!("{} = {}: left side {}, right side {} — {}", text(x), text(c), text(&ls), text(&rs), if ok { "valid ✓" } else { "extraneous ✗" }), None);
    }
    Some(())
}

fn count(e: &Expr, x: &Expr) -> usize {
    if e == x {
        return 1;
    }
    match e {
        Add(v) | Mul(v) | List(v) => v.iter().map(|t| count(t, x)).sum(),
        Pow(b, ex) => count(b, x) + count(ex, x),
        Func(_, a) => a.iter().map(|t| count(t, x)).sum(),
        _ => 0,
    }
}

