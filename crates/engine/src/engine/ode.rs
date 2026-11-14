//! Differential-equation queries: `y'' + y = 0`, `dy/dx = x y`,
//! `y' = y, y(0) = 1`, `solve y'' - 3y' + 2y = e^x`.

use super::*;
use crate::calculus::ode::{self, Condition};
use crate::expr::Expr::*;
use crate::expr::*;
use crate::parse::{self, Ctx};
use crate::plot as pl;
use crate::print::text as ptext;
use regex::Regex;
use std::sync::OnceLock;

macro_rules! re {
    ($s:expr) => {{
        static R: OnceLock<Regex> = OnceLock::new();
        R.get_or_init(|| Regex::new($s).unwrap())
    }};
}

fn split_top(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut depth = 0;
    let mut cur = String::new();
    for ch in s.chars() {
        match ch {
            '(' | '[' => {
                depth += 1;
                cur.push(ch)
            }
            ')' | ']' => {
                depth -= 1;
                cur.push(ch)
            }
            ',' | ';' if depth == 0 => out.push(std::mem::take(&mut cur)),
            _ => cur.push(ch),
        }
    }
    out.push(cur);
    out.into_iter().flat_map(|p| p.split(" and ").map(|s| s.trim().to_string()).collect::<Vec<_>>()).filter(|s| !s.is_empty()).collect()
}

pub fn handle(q: &Query) -> Option<Answer> {
    let l = q.lower.trim();
    let mut body = q.clean.trim().to_string();
    for p in ["solve the differential equation ", "solve differential equation ", "differential equation ", "dsolve ", "solve ode ", "ode ", "solve "] {
        if body.to_lowercase().starts_with(p) {
            body = body[p.len()..].to_string();
            break;
        }
    }
    let body = body.trim_matches(|c| c == '{' || c == '}').to_string();
    // Leibniz notation
    let body = re!(r"d\^?2\s*([a-z])\s*/\s*d([a-z])\^?2").replace_all(&body, "$1''").to_string();
    let body = re!(r"\bd([a-z])\s*/\s*d([a-z])\b").replace_all(&body, "$1'").to_string();
    // dependent variable: a letter followed by a prime
    let dep = re!(r"([a-zA-Z])'+").captures(&body)?.get(1)?.as_str().to_string();
    if !(l.contains('\'') || l.contains("dy/d") || l.contains("differential")) {
        return None;
    }
    let indep = if body.contains(&format!("{dep}(t)")) || dep == "x" { "t".to_string() } else { "x".to_string() };
    // separate initial conditions
    let mut conds = Vec::new();
    let mut eqs = Vec::new();
    let cond_re = Regex::new(&format!(r"^\s*{}('*)\(\s*([^()]+?)\s*\)\s*=\s*(.+)$", regex::escape(&dep))).ok()?;
    for part in split_top(&body) {
        if let Some(m) = cond_re.captures(&part) {
            let at = parse::parse_expr(&m[2]).ok()?;
            if at.free_symbols().is_empty() {
                let value = parse::parse_expr(&m[3]).ok()?;
                conds.push(Condition { order: m[1].len(), at, value });
                continue;
            }
        }
        eqs.push(part);
    }
    if eqs.len() != 1 {
        return None;
    }
    let ctx = Ctx { functions: vec![dep.clone()], prime_var: Some(indep.clone()), ..Default::default() };
    let parsed = parse::parse_with(&eqs[0], &ctx).ok()?;
    let eqn = match parsed.expr.clone() {
        e @ Rel(RelOp::Eq, ..) => e,
        e => Rel(RelOp::Eq, Box::new(e), Box::new(zero())),
    };
    let (y, x) = (sym(&dep), sym(&indep));
    if !eqn.has(&|n| matches!(n, Func(f, a) if &**f == "Derivative" && a.first() == Some(&y))) {
        return None;
    }
    let mut pods = Vec::new();
    let mut input = print::d_text(&parsed.d);
    for c in &conds {
        input += &format!(", {}{}({}) = {}", dep, "'".repeat(c.order), ptext(&c.at), ptext(&c.value));
    }
    pods.push(pod("Input", "Input", text(input.clone())));
    let yx = raw_fn(&dep, vec![x.clone()]);
    let display = |e: &Expr| e.subs(&y, &yx);
    let sol = ode::dsolve(&eqn, &y, &x, &conds);
    match &sol {
        Some(sol) => {
            pods.push(pod("ODEClassification", "ODE classification", text(sol.kind.clone())));
            if let Some(p) = &sol.particular {
                pods.push(primary(pod("IVPSolution", "Differential equation solution", math(&eq(yx.clone(), p.clone())))));
            }
            let rows: Vec<Vec<Content>> = sol.general.iter().map(|g| vec![math(&if sol.explicit { eq(yx.clone(), g.clone()) } else { display(g) })]).collect();
            let mut gpod = pod("GeneralSolution", "General solution", table(None, rows));
            if sol.particular.is_none() {
                gpod.primary = true;
            }
            pods.push(gpod);
            // plots
            if let Some(p) = &sol.particular {
                if let Some(pl) = pl::cartesian(std::slice::from_ref(p), &x, None) {
                    pods.push(pod("Plot", "Plot of the solution", plot(pl)));
                }
            } else if sol.explicit {
                let g = &sol.general[0];
                let cs: Vec<Expr> = (1..=sol.order).map(ode::constant).filter(|c| g.has_sym(c)).collect();
                let mut family = Vec::new();
                for v in [-1i64, 0, 1, 2] {
                    let pairs: Vec<(Expr, Expr)> = cs.iter().enumerate().map(|(k, c)| (c.clone(), int(if k == 0 { v } else { 1 }))).collect();
                    family.push(crate::simplify::simplify(&g.subs_many(&pairs)));
                }
                if family.iter().all(|f| f.free_symbols().len() <= 1) {
                    if let Some(mut pl) = pl::cartesian(&family, &x, Some((-3.0, 3.0))) {
                        for (k, s0) in pl.series.iter_mut().enumerate() {
                            s0.label = format!("{} = {}", ptext(&cs.first().cloned().unwrap_or_else(|| sym("c_1"))), [-1, 0, 1, 2][k]);
                        }
                        pods.push(pod("Plots", "Sample solution family", plot(pl)));
                    }
                }
            }
        }
        None => {
            pods.push(pod("Result", "Result", text("No closed-form solution was found.")));
        }
    }
    // numeric solution for initial value problems without a closed form
    if sol.as_ref().map_or(true, |s| s.particular.is_none()) && !conds.is_empty() {
        if let Some(pts) = ode::numeric_ivp(&eqn, &y, &x, &conds, 5.0) {
            let mut p = pl::scatter(vec![], "", None);
            p.kind = pl::PlotKind::Cartesian;
            p.series = vec![pl::Series { label: format!("{dep}({indep}) (numeric)"), segments: vec![pts.clone()], points: false }];
            p.x_range = (pts.first()?[0], pts.last()?[0]);
            let (mut lo, mut hi) = (f64::INFINITY, f64::NEG_INFINITY);
            for q in &pts {
                lo = lo.min(q[1]);
                hi = hi.max(q[1]);
            }
            let pad = 0.08 * (hi - lo).max(1e-9);
            p.y_range = (lo - pad, hi + pad);
            p.x_label = indep.clone();
            p.y_label = dep.clone();
            let mut np = pod("NumericSolution", "Numerical solution (Runge–Kutta 4/5)", plot(p));
            if pods.iter().all(|p| !p.primary) {
                np.primary = true;
            }
            pods.push(np);
            let x0 = conds[0].at.to_f64_const().unwrap_or(0.0);
            let rows: Vec<Vec<Content>> = [1.0, 2.0, 3.0]
                .iter()
                .filter_map(|dx| {
                    let t = x0 + dx;
                    let nearest = pts.iter().min_by(|a, b| (a[0] - t).abs().partial_cmp(&(b[0] - t).abs()).unwrap())?;
                    Some(vec![text(format!("{dep}({})", crate::print::fmt_float(t, 6))), text(format!("≈ {}", crate::print::fmt_float(nearest[1], 7)))])
                })
                .collect();
            pods.push(pod("Values", "Values", table(None, rows)));
        }
    }
    Some(Answer::new(input, pods))
}
