//! Ordinary differential equations: first-order linear, separable and
//! Bernoulli equations, n-th order linear equations with constant
//! coefficients (homogeneous and with a right-hand side), initial value
//! problems, and a numeric fallback.

use super::diff::diff;
use super::integrate::integrate;
use crate::budget;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::poly;
use crate::simplify as s;
use crate::solve;

#[derive(Clone, Debug)]
pub struct OdeSolution {
    /// `y = …` (explicit) or an implicit relation `F(x, y) = C`.
    pub general: Vec<Expr>,
    pub explicit: bool,
    pub kind: String,
    pub order: usize,
    /// Solution satisfying the initial conditions, when given.
    pub particular: Option<Expr>,
}

/// A derivative order `k` of `y` at a point: `y^(k)(x0) = v`.
#[derive(Clone, Debug)]
pub struct Condition {
    pub order: usize,
    pub at: Expr,
    pub value: Expr,
}

fn pvar(k: usize) -> Expr {
    sym(&format!("\u{1d4c5}{k}"))
}

pub fn constant(k: usize) -> Expr {
    sym(&format!("c_{k}"))
}

/// Replace `Derivative(y, x, k)` by placeholders p_k (p_0 = y).
fn flatten(e: &Expr, y: &Expr, x: &Expr) -> Option<(Expr, usize)> {
    let mut order = 0;
    fn walk(e: &Expr, y: &Expr, x: &Expr, order: &mut usize) -> Option<Expr> {
        Some(match e {
            Func(n, a) if &**n == "Derivative" && a.len() == 3 && a[0] == *y && a[1] == *x => {
                let k = a[2].as_i64()? as usize;
                *order = (*order).max(k);
                pvar(k)
            }
            _ if e == y => pvar(0),
            Add(v) => add(v.iter().map(|t| walk(t, y, x, order)).collect::<Option<_>>()?),
            Mul(v) => mul(v.iter().map(|t| walk(t, y, x, order)).collect::<Option<_>>()?),
            Pow(b, ex) => pow(walk(b, y, x, order)?, walk(ex, y, x, order)?),
            Func(n, a) => func(n, a.iter().map(|t| walk(t, y, x, order)).collect::<Option<_>>()?),
            _ => e.clone(),
        })
    }
    let r = walk(e, y, x, &mut order)?;
    Some((r, order))
}

/// Substitute a candidate solution into the flattened equation.
fn residual(f: &Expr, sol: &Expr, x: &Expr, order: usize) -> Expr {
    let mut pairs = Vec::new();
    let mut d = sol.clone();
    for k in 0..=order {
        pairs.push((pvar(k), d.clone()));
        if k < order {
            d = diff(&d, x);
        }
    }
    f.subs_many(&pairs)
}

pub fn verify(f: &Expr, sol: &Expr, x: &Expr, order: usize) -> bool {
    let r = residual(f, sol, x, order);
    match s::is_zero(&r) {
        Some(v) => v,
        None => s::numerically_zero(&r),
    }
}

/// Solve the ODE `lhs = rhs` for y(x).
pub fn dsolve(eq: &Expr, y: &Expr, x: &Expr, conds: &[Condition]) -> Option<OdeSolution> {
    let f0 = solve::as_zero(eq);
    let (f, order) = flatten(&f0, y, x)?;
    if order == 0 {
        return None;
    }
    let f = s::expand(&f);
    let mut sol = budget::attempt_opt(budget::remaining().min(12.0) * 0.8, || {
        linear_constant(&f, x, order)
            .or_else(|| if order == 1 { first_order(&f, x) } else { None })
    })?;
    // check explicit solutions
    if sol.explicit {
        sol.general.retain(|g| verify(&f, g, x, order));
        if sol.general.is_empty() {
            return None;
        }
    }
    if !conds.is_empty() && sol.explicit && sol.general.len() == 1 {
        sol.particular = apply_conditions(&sol.general[0], x, conds, order);
    }
    Some(sol)
}

fn apply_conditions(g: &Expr, x: &Expr, conds: &[Condition], order: usize) -> Option<Expr> {
    let cs: Vec<Expr> = (1..=order).map(constant).filter(|c| g.has_sym(c)).collect();
    let mut eqs = Vec::new();
    for c in conds {
        let mut d = g.clone();
        for _ in 0..c.order {
            d = diff(&d, x);
        }
        eqs.push(sub(d.subs(x, &c.at), c.value.clone()));
    }
    if cs.is_empty() {
        return Some(g.clone());
    }
    let sol = solve::solve_system(&eqs, &cs);
    let first = sol.sols.first()?;
    Some(s::simplify(&g.subs_many(first)))
}

// ---------------------------------------------------------------------------
// linear equations with constant coefficients
// ---------------------------------------------------------------------------

fn linear_constant(f: &Expr, x: &Expr, order: usize) -> Option<OdeSolution> {
    // f = Σ a_k p_k - g(x), a_k constant
    let mut coeffs = vec![zero(); order + 1];
    let mut rest = Vec::new();
    for t in f.terms() {
        let ks: Vec<usize> = (0..=order).filter(|k| t.has_sym(&pvar(*k))).collect();
        match ks.as_slice() {
            [] => rest.push(t.clone()),
            [k] => {
                let c = s::cancel(&div(t.clone(), pvar(*k)));
                if c.has_sym(&pvar(*k)) || c.has_sym(x) {
                    return None;
                }
                coeffs[*k] = add(vec![coeffs[*k].clone(), c]);
            }
            _ => return None,
        }
    }
    let g = s::simplify(&neg(add(rest)));
    if coeffs[order].is_zero() {
        return None;
    }
    // characteristic polynomial
    let r = sym("\u{3bb}c");
    let roots = solve::solve(&add(coeffs.iter().enumerate().map(|(k, c)| mul(vec![c.clone(), pow(r.clone(), int(k as i64))])).collect()), &r);
    if roots.numeric && roots.roots.iter().any(|z| z.has_float()) && order > 4 {
        return None;
    }
    // multiplicities
    let charp = add(coeffs.iter().enumerate().map(|(k, c)| mul(vec![c.clone(), pow(r.clone(), int(k as i64))])).collect());
    let mut basis: Vec<Expr> = Vec::new();
    let mut done: Vec<Expr> = Vec::new();
    for root in &roots.roots {
        if done.iter().any(|d| s::equal(d, root)) {
            continue;
        }
        // multiplicity
        let mut m = 0;
        let mut q = charp.clone();
        while m < order {
            let v = s::simplify(&q.subs(&r, root));
            let zero_here = v.is_zero() || crate::eval::const_c64(&v).map_or(false, |z| z.norm() < 1e-9);
            if !zero_here {
                break;
            }
            m += 1;
            q = diff(&q, &r);
        }
        let m = m.max(1);
        match crate::funcs::re_im(root) {
            Some((a, b)) if !s::simplify(&b).is_zero() => {
                let (a, b) = (s::simplify(&a), s::simplify(&b));
                if b.to_f64_const().map_or(false, |v| v < 0.0) {
                    // conjugate handled with its partner
                    done.push(root.clone());
                    continue;
                }
                for j in 0..m {
                    let xj = pow(x.clone(), int(j as i64));
                    let ea = exp(mul(vec![a.clone(), x.clone()]));
                    basis.push(mul(vec![xj.clone(), ea.clone(), f1("cos", mul(vec![b.clone(), x.clone()]))]));
                    basis.push(mul(vec![xj, ea, f1("sin", mul(vec![b.clone(), x.clone()]))]));
                }
            }
            _ => {
                for j in 0..m {
                    basis.push(mul(vec![pow(x.clone(), int(j as i64)), exp(mul(vec![root.clone(), x.clone()]))]));
                }
            }
        }
        done.push(root.clone());
    }
    if basis.len() != order {
        return None;
    }
    let homog = add(basis.iter().enumerate().map(|(k, b)| mul(vec![constant(k + 1), b.clone()])).collect());
    let particular = if g.is_zero() { zero() } else { variation_of_parameters(&basis, &g, &coeffs[order], x)? };
    let general = s::simplify(&add(vec![homog, particular]));
    let kind = format!("{} linear ordinary differential equation with constant coefficients{}", ordinal(order), if g.is_zero() { " (homogeneous)" } else { "" });
    Some(OdeSolution { general: vec![general], explicit: true, kind, order, particular: None })
}

fn ordinal(n: usize) -> &'static str {
    match n {
        1 => "first-order",
        2 => "second-order",
        3 => "third-order",
        4 => "fourth-order",
        _ => "higher-order",
    }
}

/// Particular solution of a_n y^(n) + … = g via variation of parameters.
fn variation_of_parameters(basis: &[Expr], g: &Expr, an: &Expr, x: &Expr) -> Option<Expr> {
    let n = basis.len();
    // Wronskian matrix
    let mut w: Vec<Vec<Expr>> = vec![basis.to_vec()];
    for k in 1..n {
        let prev = w[k - 1].clone();
        w.push(prev.iter().map(|b| diff(b, x)).collect());
    }
    let wdet = s::simplify(&solve::det(&w));
    if wdet.is_zero() {
        return None;
    }
    let rhs = div(g.clone(), an.clone());
    let mut terms = Vec::new();
    for i in 0..n {
        // replace column i by (0, …, 0, 1)
        let wi: Vec<Vec<Expr>> = w.iter().enumerate().map(|(r, row)| row.iter().enumerate().map(|(c, e)| if c == i { if r == n - 1 { one() } else { zero() } } else { e.clone() }).collect()).collect();
        let num = s::simplify(&solve::det(&wi));
        let integrand = s::simplify(&div(mul(vec![num, rhs.clone()]), wdet.clone()));
        let ui = integrate(&integrand, x)?;
        terms.push(mul(vec![ui, basis[i].clone()]));
    }
    // drop pieces that are multiples of the homogeneous basis
    let p = s::simplify(&add(terms));
    Some(p)
}

// ---------------------------------------------------------------------------
// first-order equations
// ---------------------------------------------------------------------------

fn first_order(f: &Expr, x: &Expr) -> Option<OdeSolution> {
    let y = pvar(0);
    let dy = pvar(1);
    // solve for y' = F(x, y)
    let c1 = poly::coeffs(f, &dy)?;
    if c1.len() != 2 {
        return None;
    }
    let rhs = s::simplify(&div(neg(c1[0].clone()), c1[1].clone()));
    let yy = sym("y");
    let to_y = |e: &Expr| e.subs(&y, &yy);
    // linear: y' = -P y + Q
    if let Some(cs) = poly::coeffs(&s::expand(&rhs), &y) {
        if cs.len() <= 2 {
            let p = s::simplify(&neg(cs.get(1).cloned().unwrap_or_else(zero)));
            let q = cs[0].clone();
            let ip = if p.is_zero() { zero() } else { integrate(&p, x)? };
            let mu = s::simplify(&exp(ip.clone()));
            let inner = if q.is_zero() { zero() } else { integrate(&s::simplify(&mul(vec![q.clone(), mu.clone()])), x)? };
            let sol = s::simplify(&mul(vec![exp(neg(ip)), add(vec![inner, constant(1)])]));
            return Some(OdeSolution { general: vec![sol], explicit: true, kind: "first-order linear ordinary differential equation".into(), order: 1, particular: None });
        }
    }
    // Bernoulli: y' = A y + B y^n
    if let Some(sol) = bernoulli(&rhs, x) {
        return Some(sol);
    }
    // separable: y' = g(x) h(y)
    let factors = s::factor(&rhs).factors();
    let (gx, hy): (Vec<Expr>, Vec<Expr>) = factors.into_iter().partition(|t| !t.has_sym(&y));
    let (gx, hy) = (mul(gx), mul(hy));
    if !hy.has_sym(x) {
        let lhs_int = integrate(&s::simplify(&pow(hy.clone(), int(-1))), &y)?;
        let rhs_int = if gx.has_sym(x) { integrate(&gx, x)? } else { mul(vec![gx.clone(), x.clone()]) };
        let relation = sub(lhs_int.clone(), rhs_int.clone());
        // try to solve for y explicitly
        let rs = solve::solve(&sub(relation.clone(), constant(1)), &y);
        let explicit: Vec<Expr> = rs.roots.iter().filter(|r| !r.has_sym(&solve::param()) && !r.has_float()).map(to_y).map(|e| s::simplify(&e)).collect();
        if !explicit.is_empty() && !rs.numeric {
            let ys: Vec<Expr> = explicit.iter().map(|e| e.subs(&yy, &y)).collect();
            return Some(OdeSolution { general: ys, explicit: true, kind: "first-order separable ordinary differential equation".into(), order: 1, particular: None });
        }
        return Some(OdeSolution { general: vec![eq(to_y(&lhs_int), add(vec![to_y(&rhs_int), constant(1)]))], explicit: false, kind: "first-order separable ordinary differential equation (implicit solution)".into(), order: 1, particular: None });
    }
    None
}

fn bernoulli(rhs: &Expr, x: &Expr) -> Option<OdeSolution> {
    let y = pvar(0);
    let terms = s::expand(rhs).terms();
    let mut a = Vec::new();
    let mut b = Vec::new();
    let mut n: Option<Expr> = None;
    for t in terms {
        let fs = t.factors();
        let ypart: Vec<&Expr> = fs.iter().filter(|f| f.has_sym(&y)).collect();
        match ypart.as_slice() {
            [f] if **f == y => a.push(s::cancel(&div(t.clone(), y.clone()))),
            [Pow(base, e)] if **base == y && !e.has_sym(x) => {
                if n.as_ref().map_or(false, |m| m != &**e) {
                    return None;
                }
                n = Some((**e).clone());
                b.push(s::cancel(&div(t.clone(), pow(y.clone(), (**e).clone()))));
            }
            _ => return None,
        }
    }
    let n = n?;
    if n.is_one() || n.is_zero() {
        return None;
    }
    let (a, b) = (add(a), add(b));
    // v = y^(1-n): v' = (1-n)(A v + B)
    let k = sub(one(), n.clone());
    let p = s::simplify(&neg(mul(vec![k.clone(), a])));
    let q = s::simplify(&mul(vec![k.clone(), b]));
    let ip = if p.is_zero() { zero() } else { integrate(&p, x)? };
    let mu = s::simplify(&exp(ip.clone()));
    let inner = integrate(&s::simplify(&mul(vec![q, mu])), x)?;
    let v = s::simplify(&mul(vec![exp(neg(ip)), add(vec![inner, constant(1)])]));
    let ysol = s::simplify(&pow(v, pow(k, int(-1))));
    Some(OdeSolution { general: vec![ysol], explicit: true, kind: "first-order Bernoulli ordinary differential equation".into(), order: 1, particular: None })
}

/// Numeric solution of an initial value problem y^(n) = F(x, y, …) on
/// [x0, x0 + span]; returns sample points (x, y).
pub fn numeric_ivp(eq: &Expr, y: &Expr, x: &Expr, conds: &[Condition], span: f64) -> Option<Vec<[f64; 2]>> {
    let f0 = solve::as_zero(eq);
    let (f, order) = flatten(&f0, y, x)?;
    let top = pvar(order);
    let cs = poly::coeffs(&s::expand(&f), &top)?;
    if cs.len() != 2 {
        return None;
    }
    let rhs = s::simplify(&div(neg(cs[0].clone()), cs[1].clone()));
    let x0 = conds.first()?.at.to_f64_const()?;
    let mut y0 = vec![0.0; order];
    for c in conds {
        if c.order < order && c.at.to_f64_const()? == x0 {
            y0[c.order] = c.value.to_f64_const()?;
        }
    }
    let Sym(xn) = x else { return None };
    let names: Vec<String> = std::iter::once(xn.to_string()).chain((0..order).map(|k| match pvar(k) {
        Sym(n) => n.to_string(),
        _ => unreachable!(),
    })).collect();
    let refs: Vec<&str> = names.iter().map(|s| s.as_str()).collect();
    let prog = crate::eval::Program::compile(&rhs, &refs)?;
    let field = |t: f64, v: &[f64]| -> Vec<f64> {
        let mut args = vec![t];
        args.extend_from_slice(v);
        let mut out: Vec<f64> = v[1..].to_vec();
        out.push(prog.eval(&args));
        out
    };
    let mut pts = Vec::new();
    for dir in [-1.0, 1.0] {
        let path = crate::numeric::ode45(&field, x0, &y0, x0 + dir * span, 20000);
        let mut seg: Vec<[f64; 2]> = path.iter().map(|(t, v)| [*t, v[0]]).filter(|p| p[1].is_finite() && p[1].abs() < 1e12).collect();
        if dir < 0.0 {
            seg.reverse();
        }
        pts.extend(seg);
    }
    pts.sort_by(|a, b| a[0].partial_cmp(&b[0]).unwrap());
    pts.dedup_by(|a, b| (a[0] - b[0]).abs() < 1e-12);
    Some(pts)
}
