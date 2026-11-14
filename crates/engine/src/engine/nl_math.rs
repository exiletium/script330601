//! Direct answers for mathematical questions asked in words: analytic
//! geometry, function properties, applications of integrals, vector
//! calculus, matrices and vectors, probability.

use super::nl::{again, answer, bound, caps, decimal_pod, ex, main_var, promote, split_top};
use super::*;
use crate::calculus;
use crate::calculus::integrate::Definite;
use crate::calculus::sum::SumResult;
use crate::calculus::Dir;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::linalg::{self, Mat};
use crate::plot as pl;
use crate::poly;
use crate::print::text as ptext;
use crate::simplify as s;
use crate::solve;
use regex::Regex;
use std::sync::OnceLock;

macro_rules! re {
    ($s:expr) => {{
        static R: OnceLock<Regex> = OnceLock::new();
        R.get_or_init(|| Regex::new($s).unwrap())
    }};
}

fn g(m: &[Option<String>], k: usize) -> String {
    m.get(k).cloned().flatten().unwrap_or_default()
}

fn strip_fx(t: &str) -> String {
    re!(r"^\s*(?:the function\s+|the curve\s+|the graph of\s+|the parabola\s+|the line\s+)?(?:[a-zA-Z]\s*\(\s*[a-z]\s*\)|y)\s*=\s*").replace(t, "").trim().to_string()
}

fn fx(t: &str) -> Option<Expr> {
    ex(&strip_fx(t))
}

fn plot_pod(fs: &[Expr], x: &Expr, range: Option<(f64, f64)>) -> Option<Pod> {
    Some(pod("Plot", "Plot", plot(pl::cartesian(fs, x, range)?)))
}

fn point(x: &Expr, y: &Expr) -> Expr {
    raw_fn("Tuple", vec![List(vec![x.clone(), y.clone()])])
}

/// Only simple mathematical text: no English words other than function names.
fn mathy(t: &str) -> bool {
    const OK: &[&str] = &["sin", "cos", "tan", "cot", "sec", "csc", "asin", "acos", "atan", "sinh", "cosh", "tanh", "log", "ln", "exp", "sqrt", "abs", "pi", "arcsin", "arccos", "arctan", "cbrt", "floor", "ceil", "gamma", "erf"];
    re!(r"[a-zA-Z]+").find_iter(t).all(|w| w.as_str().len() == 1 || OK.contains(&w.as_str().to_lowercase().as_str()))
}

fn definite_expr(f: &Expr, x: &Expr, a: &Expr, b: &Expr) -> Option<Expr> {
    match calculus::integrate::definite(f, x, a, b) {
        Definite::Exact(v) => Some(v),
        Definite::Numeric(v, _) => Some(float(v)),
        _ => None,
    }
}

fn value_pods(title: &str, lhs: Option<Expr>, v: &Expr) -> Vec<Pod> {
    let shown = match lhs {
        Some(l) => eq(l, v.clone()),
        None => v.clone(),
    };
    let mut pods = vec![primary(pod("Result", title, math(&shown)))];
    if let Some(d) = decimal_pod(v) {
        pods.push(d);
    }
    pods
}

// ---------------------------------------------------------------------------
// algebra and analytic geometry
// ---------------------------------------------------------------------------

pub fn algebra(q: &Query, l: &str, c: &str) -> Option<Answer> {
    let cap = |r: &Regex| caps(r, l, c);

    // completing the square
    if let Some(m) = cap(re!(r"^complet(?:e|ing) the square\s*(?:of|for|in|on|:)?\s+(.+)$")) {
        let e = fx(&g(&m, 1))?;
        let e = if let Rel(RelOp::Eq, a, b) = &e { sub((**a).clone(), (**b).clone()) } else { e };
        let x = main_var(&e);
        let cs = poly::coeffs(&s::expand(&e), &x)?;
        if cs.len() != 3 {
            return None;
        }
        let (a, b, c0) = (cs[2].clone(), cs[1].clone(), cs[0].clone());
        let h = s::simplify(&div(b.clone(), mul(vec![int(2), a.clone()])));
        let k = s::simplify(&sub(c0.clone(), div(square(b.clone()), mul(vec![int(4), a.clone()]))));
        let res = add(vec![mul(vec![a.clone(), pow(add(vec![x.clone(), h.clone()]), int(2))]), k.clone()]);
        let mut steps = Vec::new();
        let mut st = |t: String, m: Option<Expr>| steps.push(crate::steps::Step { text: t, math: m, depth: 0 });
        if !a.is_one() {
            st(format!("Factor {} out of the first two terms:", ptext(&a)), Some(add(vec![mul(vec![a.clone(), add(vec![square(x.clone()), mul(vec![s::simplify(&div(b.clone(), a.clone())), x.clone()])])]), c0.clone()])));
        }
        st(format!("Half of the coefficient of {} is {}; its square is {}. Add and subtract that square:", ptext(&x), ptext(&h), ptext(&s::simplify(&square(h.clone())))), None);
        st("Write the perfect square trinomial as a square and collect the constants:".into(), Some(res.clone()));
        let mut pods = vec![primary(pod("Result", "Completed square", math(&res)))];
        pods.push(pod("Vertex", "Vertex", math(&point(&neg(h.clone()), &k))));
        pods.push(pod("Steps", "Step-by-step solution", steps_content(&steps)));
        if let Some(p) = plot_pod(&[e.clone()], &x, None) {
            pods.push(p);
        }
        return answer(format!("complete the square {}", ptext(&e)), format!("complete the square: {}", ptext(&e)), pods);
    }
    // domain and range
    if let Some(m) = cap(re!(r"^(?:the\s+)?(domain and range|range and domain|domain|range)\s+(?:of|for)\s+(.+)$")) {
        let body = strip_fx(&g(&m, 2));
        let e = ex(&body)?;
        if e.free_symbols().len() != 1 {
            return None;
        }
        let a = again(q, &body)?;
        let props = a.pods.iter().find(|p| p.id == "Properties")?;
        let Content::Table { rows, .. } = &props.subpods.first()?.content else { return None };
        let what = g(&m, 1).to_lowercase();
        let pick = |name: &str| rows.iter().find(|r| r.first().map_or(false, |c0| content_text(c0).eq_ignore_ascii_case(name))).and_then(|r| r.get(1)).cloned();
        let mut pods = Vec::new();
        if what.contains("domain") {
            pods.push(primary(pod("Domain", "Domain", pick("Domain")?)));
        }
        if what.contains("range") {
            let mut p = pod("Range", "Range", pick("Range")?);
            p.primary = !what.contains("domain");
            pods.push(p);
        }
        if let Some(p) = a.pods.iter().find(|p| p.id == "Plot") {
            pods.push(p.clone());
        }
        return answer(format!("{what} of {}", ptext(&e)), format!("{what} of {}", ptext(&e)), pods);
    }
    // parity
    if let Some(m) = cap(re!(r"^is\s+(.+?)\s+(?:an?\s+)?(?:odd or even|even or odd|even|odd|symmetric)(?:\s+function)?$")) {
        let e = fx(&g(&m, 1))?;
        if e.free_symbols().len() == 1 {
            let x = main_var(&e);
            let fm = e.subs(&x, &neg(x.clone()));
            let even = s::simplify(&sub(fm.clone(), e.clone())).is_zero();
            let odd = s::simplify(&add(vec![fm.clone(), e.clone()])).is_zero();
            let verdict = match (even, odd) {
                (true, true) => "both even and odd (it is identically zero)",
                (true, false) => "an even function: f(-x) = f(x), so its graph is symmetric about the y-axis",
                (false, true) => "an odd function: f(-x) = -f(x), so its graph is symmetric about the origin",
                _ => "neither even nor odd",
            };
            let fx_name = raw_fn("f", vec![neg(x.clone())]);
            let mut pods = vec![primary(pod("Result", "Result", text(format!("{} is {verdict}.", ptext(&e)))))];
            pods.push(pod("Check", "Check", math(&eq(fx_name, s::simplify(&fm)))));
            if let Some(p) = plot_pod(&[e.clone()], &x, None) {
                pods.push(p);
            }
            return answer(format!("parity of {}", ptext(&e)), format!("parity of {}", ptext(&e)), pods);
        }
    }
    // parabolas
    if let Some(m) = cap(re!(r"^(?:the\s+)?(vertex|axis of symmetry|focus|directrix)\s+(?:of|for)\s+(.+)$")) {
        let e = fx(&g(&m, 2))?;
        let x = main_var(&e);
        let cs = poly::coeffs(&s::expand(&e), &x)?;
        if cs.len() != 3 || e.free_symbols().len() != 1 {
            return None;
        }
        let (a, b, c0) = (cs[2].clone(), cs[1].clone(), cs[0].clone());
        let h = s::simplify(&neg(div(b.clone(), mul(vec![int(2), a.clone()]))));
        let k = s::simplify(&sub(c0, div(square(b), mul(vec![int(4), a.clone()]))));
        let p4 = s::simplify(&div(one(), mul(vec![int(4), a.clone()])));
        let what = g(&m, 1).to_lowercase();
        let up = a.to_f64_const().map_or(true, |v| v > 0.0);
        let mut pods = vec![
            pod("Vertex", "Vertex", math(&point(&h, &k))),
            pod("Axis", "Axis of symmetry", math(&eq(x.clone(), h.clone()))),
            pod("Focus", "Focus", math(&point(&h, &s::simplify(&add(vec![k.clone(), p4.clone()]))))),
            pod("Directrix", "Directrix", math(&eq(sym("y"), s::simplify(&sub(k.clone(), p4))))),
            pod("Form", "Vertex form", math(&eq(sym("y"), add(vec![mul(vec![a.clone(), pow(sub(x.clone(), h.clone()), int(2))]), k.clone()])))),
            pod("Opens", "Direction", text(if up { "opens upward (the vertex is a minimum)" } else { "opens downward (the vertex is a maximum)" })),
        ];
        let idx = match what.as_str() {
            "vertex" => 0,
            "axis of symmetry" => 1,
            "focus" => 2,
            _ => 3,
        };
        let chosen = pods.remove(idx);
        pods.insert(0, primary(chosen));
        if let Some(p) = plot_pod(&[e.clone()], &x, None) {
            pods.push(p);
        }
        return answer(format!("{what} of {}", ptext(&e)), format!("{what} of the parabola y = {}", ptext(&e)), pods);
    }
    // two points
    let pts: Vec<Vec<Expr>> = re!(r"\(\s*([^()]+?)\s*\)").captures_iter(c).filter_map(|m| {
        let parts = split_top(&m[1].replace(" and ", " AND "));
        let v: Vec<Expr> = parts.iter().filter_map(|p| ex(p)).collect();
        (v.len() == parts.len() && (2..=3).contains(&v.len()) && v.iter().all(|e| e.free_symbols().is_empty())).then_some(v)
    }).collect();
    let line_kw = re!(r"^(?:the\s+)?(slope|gradient|equation|line|distance|midpoint|mid-point|length)\b").captures(l).map(|m| m[1].to_string());
    if let (Some(kw), true) = (line_kw, pts.len() == 2 && pts[0].len() == pts[1].len() && !l.contains("vector") && !l.contains("product")) {
        let (p, r) = (&pts[0], &pts[1]);
        let d2 = add(p.iter().zip(r.iter()).map(|(a, b)| square(sub(b.clone(), a.clone()))).collect());
        let dist = s::simplify(&sqrt(d2));
        let mid: Vec<Expr> = p.iter().zip(r.iter()).map(|(a, b)| s::simplify(&div(add(vec![a.clone(), b.clone()]), int(2)))).collect();
        let tup = |v: &[Expr]| raw_fn("Tuple", vec![List(v.to_vec())]);
        let mut pods: Vec<Pod> = Vec::new();
        let label = format!("{} and {}", ptext(&tup(p)), ptext(&tup(r)));
        if p.len() == 2 {
            let (dx, dy) = (s::simplify(&sub(r[0].clone(), p[0].clone())), s::simplify(&sub(r[1].clone(), p[1].clone())));
            let (x, y) = (sym("x"), sym("y"));
            if dx.is_zero() {
                pods.push(pod("Slope", "Slope", text("undefined (the line is vertical)")));
                pods.push(pod("Equation", "Equation of the line", math(&eq(x.clone(), p[0].clone()))));
            } else {
                let mslope = s::simplify(&div(dy.clone(), dx.clone()));
                let b0 = s::simplify(&sub(p[1].clone(), mul(vec![mslope.clone(), p[0].clone()])));
                let rhs = add(vec![mul(vec![mslope.clone(), x.clone()]), b0.clone()]);
                pods.push(pod_multi("Slope", "Slope", vec![(String::new(), math(&eq(sym("m"), mslope.clone()))), ("rise over run".into(), text(format!("({}) / ({})", ptext(&dy), ptext(&dx))))]));
                pods.push(pod_multi("Equation", "Equation of the line", vec![("slope-intercept form".into(), math(&eq(y.clone(), rhs.clone()))), ("point-slope form".into(), math(&eq(sub(y.clone(), p[1].clone()), mul(vec![mslope.clone(), sub(x.clone(), p[0].clone())]))))]));
                pods.push(pod("Intercepts", "Intercepts", table(None, vec![vec![text("y-intercept"), math(&b0)], vec![text("x-intercept"), if mslope.is_zero() { text("none") } else { math(&s::simplify(&neg(div(b0.clone(), mslope.clone())))) }]])));
                let lo = p[0].to_f64_const()?.min(r[0].to_f64_const()?);
                let hi = p[0].to_f64_const()?.max(r[0].to_f64_const()?);
                let pad = ((hi - lo) * 0.5).max(1.0);
                if let Some(pp) = plot_pod(&[rhs], &x, Some((lo - pad, hi + pad))) {
                    pods.push(pp);
                }
            }
        }
        pods.insert(0, pod("Distance", "Distance between the points", math(&dist)));
        pods.insert(1, pod("Midpoint", "Midpoint", math(&tup(&mid))));
        let want = match kw.as_str() {
            "slope" | "gradient" => "Slope",
            "equation" | "line" => "Equation",
            "midpoint" | "mid-point" => "Midpoint",
            _ => "Distance",
        };
        let k = pods.iter().position(|p| p.id == want)?;
        let chosen = pods.remove(k);
        if want == "Distance" {
            if let Some(d) = decimal_pod(&dist) {
                pods.insert(0, d);
            }
        }
        pods.insert(0, primary(chosen));
        return answer(format!("{} for {label}", want.to_lowercase()), format!("{} — points {label}", want.to_lowercase()), pods);
    }
    // polynomial division
    let pdiv = cap(re!(r"^(?:polynomial\s+)?(?:long\s+)?division\s*(?:of\s+)?\((.+)\)\s*(?:/|÷|by)\s*\((.+)\)$"))
        .or_else(|| cap(re!(r"^(?:polynomial\s+)?(?:long\s+)?divi(?:de|sion(?:\s+of)?)\s+(.+?)\s+(?:by|÷)\s+(.+)$")))
        .or_else(|| cap(re!(r"^(?:the\s+)?(?:quotient and remainder|remainder|quotient)\s+(?:of|when)\s+(.+?)\s+(?:is\s+)?(?:divided by|÷|/)\s+(.+)$")));
    if let Some(m) = pdiv {
        let (n, d) = (ex(&g(&m, 1))?, ex(&g(&m, 2))?);
        if let (Some(a), Some(b)) = (n.as_int(), d.as_int()) {
            if !d.is_zero() && l.contains("remainder") {
                use num_integer::Integer;
                let (qv, r) = a.div_mod_floor(b);
                return answer(format!("{a} divided by {b}"), format!("{a} ÷ {b}"), vec![primary(pod("Result", "Quotient and remainder", text(format!("quotient {qv}, remainder {r}")))), pod("Check", "Check", text(format!("{a} = {b} × {qv} + {r}")))]);
            }
        }
        if !n.free_symbols().is_empty() || !d.free_symbols().is_empty() {
            let x = main_var(&add(vec![n.clone(), d.clone()]));
            let (qt, rm) = poly_divrem(&n, &d, &x)?;
            let mut pods = vec![primary(pod_multi("Result", "Quotient and remainder", vec![("quotient".into(), math(&qt)), ("remainder".into(), math(&rm))]))];
            pods.push(pod("Identity", "Division identity", math(&eq(n.clone(), add(vec![mul(vec![d.clone(), qt.clone()]), rm.clone()])))));
            if rm.is_zero() {
                pods.push(pod("Note", "Note", text(format!("The remainder is 0, so {} divides {} exactly.", ptext(&d), ptext(&n)))));
            }
            return answer(format!("({}) ÷ ({})", ptext(&n), ptext(&d)), format!("polynomial division: ({}) ÷ ({})", ptext(&n), ptext(&d)), pods);
        }
    }
    // rationalising denominators
    if let Some(m) = cap(re!(r"^rationali[sz]e\s+(?:the\s+)?(?:denominator\s+)?(?:of\s+|in\s+)?(.+)$")) {
        let p = crate::parse::parse(g(&m, 1).trim()).ok()?;
        let r = s::simplify(&p.expr);
        let mut pods = vec![primary(pod("Result", "Rationalized form", math(&r)))];
        if let Some(d) = decimal_pod(&r) {
            pods.push(d);
        }
        return Some(Answer::new(format!("rationalize {}", ptext(&p.expr)), [vec![super::math::input_pod(&p.d)], pods].concat()));
    }
    // complex numbers
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:complex\s+)?conjugate\s+of\s+(.+)$")) {
        let e = ex(&g(&m, 1))?;
        let r = s::simplify(&e.subs(&i(), &neg(i())));
        let mut pods = vec![primary(pod("Result", "Complex conjugate", math(&r)))];
        pods.push(pod("Product", "Product with the conjugate", math(&s::simplify(&s::expand(&mul(vec![e.clone(), r.clone()]))))));
        return answer(format!("conjugate of {}", ptext(&e)), format!("complex conjugate of {}", ptext(&e)), pods);
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(polar form|polar coordinates|exponential form|trigonometric form|modulus|magnitude|argument|phase|angle|real part|imaginary part|real and imaginary parts)\s+of\s+(.+)$")) {
        let body = g(&m, 2);
        if ex(&body)?.has_const(K::I) {
            let ids: &[&str] = match g(&m, 1).to_lowercase().as_str() {
                "polar form" | "exponential form" | "trigonometric form" => &["PolarForm"],
                "real part" | "imaginary part" | "real and imaginary parts" => &["Parts"],
                _ => &["PolarCoordinates"],
            };
            return promote(q, &body, ids);
        }
    }
    // evaluating at a point
    let at = cap(re!(r"^(?:the\s+)?(?:value of\s+)?(.+?)\s+(?:at|when|where|for|if|with|given)\s+((?:[a-z]\s*=\s*[^,=]+?)(?:\s*(?:,|and|,\s*and)\s*[a-z]\s*=\s*[^,=]+?)*)$"))
        .or_else(|| cap(re!(r"^(?:substitute|plug(?: in)?|put)\s+((?:[a-z]\s*=\s*[^,=]+?)(?:\s*(?:,|and)\s*[a-z]\s*=\s*[^,=]+?)*)\s+(?:in|into|in to)\s+(.+)$")).map(|m| vec![m[0].clone(), m[2].clone(), m[1].clone()]));
    if let Some(m) = at {
        let body = strip_fx(&g(&m, 1));
        if mathy(&body) && !body.contains('=') {
            let e = ex(&body)?;
            let mut pairs = Vec::new();
            for part in split_top(&g(&m, 2)) {
                let (v, val) = part.split_once('=')?;
                let val = ex(val)?;
                if !val.free_symbols().is_empty() {
                    return None;
                }
                pairs.push((sym(v.trim()), val));
            }
            if pairs.iter().all(|(v, _)| e.has_sym(v)) {
                let r = s::simplify(&e.subs_many(&pairs));
                let cond = pairs.iter().map(|(v, a)| format!("{} = {}", ptext(v), ptext(a))).collect::<Vec<_>>().join(", ");
                let pods = value_pods("Result", None, &r);
                return answer(format!("{} where {cond}", ptext(&e)), format!("{} where {cond}", ptext(&e)), pods);
            }
        }
    }
    None
}

/// Polynomial long division in `x`, with symbolic coefficients.
fn poly_divrem(n: &Expr, d: &Expr, x: &Expr) -> Option<(Expr, Expr)> {
    let mut rem = poly::coeffs(&s::expand(n), x)?;
    let dc = poly::coeffs(&s::expand(d), x)?;
    let dd = dc.len() - 1;
    let lead = dc[dd].clone();
    if lead.is_zero() {
        return None;
    }
    let mut quo = vec![zero(); rem.len().saturating_sub(dd).max(1)];
    while rem.len() > dd && rem.len() > 0 {
        let k = rem.len() - 1 - dd;
        let f = s::simplify(&div(rem[rem.len() - 1].clone(), lead.clone()));
        quo[k] = f.clone();
        for (j, dcj) in dc.iter().enumerate() {
            rem[k + j] = s::simplify(&sub(rem[k + j].clone(), mul(vec![f.clone(), dcj.clone()])));
        }
        rem.pop();
        if dd == 0 && rem.is_empty() {
            break;
        }
    }
    let build = |cs: &[Expr]| add(cs.iter().enumerate().map(|(k, cf)| mul(vec![cf.clone(), pow(x.clone(), int(k as i64))])).collect());
    Some((build(&quo), build(&rem)))
}

// ---------------------------------------------------------------------------
// calculus applications
// ---------------------------------------------------------------------------

pub fn calculus_q(q: &Query, l: &str, c: &str) -> Option<Answer> {
    let cap = |r: &Regex| caps(r, l, c);
    let _ = q;

    // inflection points
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:inflection points?|inflexion points?|points? of inflection|concavity)\s+(?:of|for)\s+(.+)$")) {
        let e = fx(&g(&m, 1))?;
        let x = main_var(&e);
        if e.free_symbols().len() != 1 {
            return None;
        }
        let d2 = s::simplify(&calculus::diff_n(&e, &x, 2));
        let Sym(name) = &x else { return None };
        let prog = crate::eval::Program::compile(&d2, &[name])?;
        let rs = solve::solve(&d2, &x);
        let mut cands: Vec<(Expr, f64)> = rs.real().into_iter().filter_map(|r| r.to_f64_const().map(|v| (r, v))).collect();
        cands.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
        cands.dedup_by(|a, b| (a.1 - b.1).abs() < 1e-9);
        let mut rows = Vec::new();
        let mut xs = Vec::new();
        for (r, v) in &cands {
            let h = 1e-3 * (1.0 + v.abs());
            let (lft, rgt) = (prog.eval1(v - h), prog.eval1(v + h));
            if lft.is_finite() && rgt.is_finite() && lft * rgt < 0.0 {
                let y = s::simplify(&e.subs(&x, r));
                rows.push(vec![math(&point(r, &y)), text(if lft < 0.0 { "concave down → concave up" } else { "concave up → concave down" })]);
                xs.push(*v);
            }
        }
        let mut pods = vec![pod("SecondDerivative", "Second derivative", math(&eq(raw_fn("Derivative", vec![e.clone(), x.clone(), int(2)]), d2.clone())))];
        if rows.is_empty() {
            pods.insert(0, primary(pod("Result", "Inflection points", text(if rs.periodic { "The second derivative changes sign periodically; see the plot." } else { "There are no inflection points: the second derivative never changes sign." }))));
        } else {
            pods.insert(0, primary(pod("Result", "Inflection points", table(Some(vec!["point", "concavity change"]), rows))));
        }
        let range = if xs.is_empty() { None } else { Some((xs[0] - 3.0, xs[xs.len() - 1] + 3.0)) };
        if let Some(p) = plot_pod(&[e.clone()], &x, range) {
            pods.push(p);
        }
        return answer(format!("inflection points of {}", ptext(&e)), format!("inflection points of {}", ptext(&e)), pods);
    }
    // arc length
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:arc ?length|length of the curve|curve length|length)\s+(?:of\s+)?(.+?)\s+(?:from|between|for)\s+(?:[a-z]\s*=\s*)?(.+?)\s+(?:to|and)\s+(?:[a-z]\s*=\s*)?(.+)$")) {
        let e = fx(&g(&m, 1))?;
        let x = main_var(&e);
        let (a, b) = (bound(&g(&m, 2))?, bound(&g(&m, 3))?);
        if e.free_symbols().len() != 1 || !mathy(&strip_fx(&g(&m, 1))) {
            return None;
        }
        let integrand = s::simplify(&sqrt(add(vec![one(), square(calculus::diff(&e, &x))])));
        let v = definite_expr(&integrand, &x, &a, &b)?;
        let holder = raw_fn("Integral", vec![integrand.clone(), x.clone(), a.clone(), b.clone()]);
        let mut pods = value_pods("Arc length", Some(holder), &v);
        pods.push(pod("Formula", "Formula", text("L = ∫ sqrt(1 + (dy/dx)²) dx")));
        if let (Some(fa), Some(fb)) = (a.to_f64_const(), b.to_f64_const()) {
            if let Some(p) = plot_pod(&[e.clone()], &x, Some((fa, fb))) {
                pods.push(p);
            }
        }
        return answer(format!("arc length of {}", ptext(&e)), format!("arc length of y = {} from {} = {} to {}", ptext(&e), ptext(&x), ptext(&a), ptext(&b)), pods);
    }
    // area between curves
    if let Some(m) = cap(re!(r"^(?:the\s+)?area\s+(?:of the region\s+)?(?:between|enclosed by|bounded by|enclosed between|bounded between)\s+(?:the\s+)?(?:curves\s+|graphs of\s+|lines\s+)?(.+?)\s+and\s+(.+?)(?:\s+(?:from|between|for)\s+(?:x\s*=\s*)?(\S+)\s+(?:to|and)\s+(?:x\s*=\s*)?(\S+))?$")) {
        let (f, h) = (fx(&g(&m, 1))?, fx(&g(&m, 2))?);
        let dfn = s::simplify(&sub(f.clone(), h.clone()));
        let x = main_var(&dfn);
        if dfn.free_symbols().len() != 1 {
            return None;
        }
        let mut cuts: Vec<Expr> = solve::solve(&dfn, &x).real();
        let given = match (m.get(3).cloned().flatten(), m.get(4).cloned().flatten()) {
            (Some(a), Some(b)) => Some((bound(&a)?, bound(&b)?)),
            _ => None,
        };
        if let Some((a, b)) = &given {
            let (fa, fb) = (a.to_f64_const()?, b.to_f64_const()?);
            cuts.retain(|r| r.to_f64_const().map_or(false, |v| v > fa + 1e-12 && v < fb - 1e-12));
            cuts.push(a.clone());
            cuts.push(b.clone());
        }
        let mut cuts: Vec<(Expr, f64)> = cuts.into_iter().filter_map(|r| r.to_f64_const().map(|v| (r, v))).collect();
        cuts.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
        cuts.dedup_by(|a, b| (a.1 - b.1).abs() < 1e-12);
        if cuts.len() < 2 || cuts.len() > 12 {
            return None;
        }
        let mut total = Vec::new();
        for w in cuts.windows(2) {
            let piece = definite_expr(&dfn, &x, &w[0].0, &w[1].0)?;
            let neg_piece = piece.to_f64_const().map_or(false, |v| v < 0.0);
            total.push(if neg_piece { neg(piece) } else { piece });
        }
        let area = s::simplify(&add(total));
        let mut pods = value_pods("Area", None, &area);
        pods.push(pod("Intersections", if given.is_some() { "Interval endpoints and crossings" } else { "Intersection points" }, text(cuts.iter().map(|(r, _)| format!("{} = {}", ptext(&x), ptext(r))).collect::<Vec<_>>().join(",  "))));
        pods.push(pod("Setup", "Integral", math(&raw_fn("Integral", vec![raw_fn("abs", vec![dfn.clone()]), x.clone(), cuts[0].0.clone(), cuts[cuts.len() - 1].0.clone()]))));
        let pad = (cuts[cuts.len() - 1].1 - cuts[0].1) * 0.25;
        if let Some(p) = plot_pod(&[f.clone(), h.clone()], &x, Some((cuts[0].1 - pad, cuts[cuts.len() - 1].1 + pad))) {
            pods.push(p);
        }
        return answer(format!("area between {} and {}", ptext(&f), ptext(&h)), format!("area between y = {} and y = {}", ptext(&f), ptext(&h)), pods);
    }
    // volumes of revolution
    if let Some(m) = cap(re!(r"^(?:the\s+)?volume\s+of\s+(?:the\s+)?(?:solid\s+)?(?:of\s+)?(?:revolution|rotation)\s+(?:of|for|generated by|obtained by rotating|formed by rotating)\s+(.+?)\s+(?:from|between)\s+(?:x\s*=\s*)?(\S+)\s+(?:to|and)\s+(?:x\s*=\s*)?(\S+?)(?:\s+(?:about|around)\s+(?:the\s+)?([xy])[- ]?axis)?$")) {
        let e = fx(&g(&m, 1))?;
        let x = main_var(&e);
        let (a, b) = (bound(&g(&m, 2))?, bound(&g(&m, 3))?);
        let about_y = g(&m, 4) == "y";
        let integrand = if about_y { mul(vec![int(2), pi(), x.clone(), e.clone()]) } else { mul(vec![pi(), square(e.clone())]) };
        let v = definite_expr(&s::simplify(&integrand), &x, &a, &b)?;
        let holder = raw_fn("Integral", vec![integrand, x.clone(), a.clone(), b.clone()]);
        let mut pods = value_pods("Volume", Some(holder), &v);
        pods.push(pod("Method", "Method", text(if about_y { "cylindrical shells about the y-axis: V = 2π ∫ x f(x) dx" } else { "disks about the x-axis: V = π ∫ f(x)² dx" })));
        return answer(format!("volume of revolution of {}", ptext(&e)), format!("volume of the solid obtained by rotating y = {} about the {}-axis, {} from {} to {}", ptext(&e), if about_y { "y" } else { "x" }, ptext(&x), ptext(&a), ptext(&b)), pods);
    }
    // vector calculus
    if let Some(m) = cap(re!(r"^(?:the\s+)?(gradient|grad|laplacian|hessian)\s+(?:matrix\s+)?(?:of\s+)?(.+)$")) {
        let e = fx(&g(&m, 2))?;
        let vars: Vec<Expr> = e.free_symbols().iter().map(|n| sym(n)).collect();
        if vars.len() >= 2 && vars.len() <= 4 && mathy(&strip_fx(&g(&m, 2))) {
            let kw = g(&m, 1).to_lowercase();
            let (title, r) = match kw.as_str() {
                "laplacian" => ("Laplacian", s::simplify(&calculus::diff::laplacian(&e, &vars))),
                "hessian" => ("Hessian matrix", List(calculus::diff::hessian(&e, &vars).into_iter().map(|row| List(row.iter().map(s::simplify).collect())).collect())),
                _ => ("Gradient", vec_expr(&calculus::diff::gradient(&e, &vars).iter().map(s::simplify).collect::<Vec<_>>())),
            };
            let mut pods = vec![primary(pod("Result", title, math(&r)))];
            pods.push(pod("Variables", "Variables", text(vars.iter().map(ptext).collect::<Vec<_>>().join(", "))));
            if kw.starts_with("grad") {
                let crit = solve::solve_system(&calculus::diff::gradient(&e, &vars), &vars);
                if !crit.sols.is_empty() && crit.sols.len() <= 6 {
                    pods.push(pod("Critical", "Critical points (gradient = 0)", text(crit.sols.iter().map(|sv| format!("({})", sv.iter().map(|(a, b)| format!("{} = {}", ptext(a), ptext(b))).collect::<Vec<_>>().join(", "))).collect::<Vec<_>>().join(";  "))));
                }
            }
            return answer(format!("{kw} of {}", ptext(&e)), format!("{} of {}", title.to_lowercase(), ptext(&e)), pods);
        }
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(divergence|div|curl|rot|jacobian)\s+(?:matrix\s+)?(?:of\s+)?(?:the\s+)?(?:vector field\s+)?(?:f\s*=\s*)?[(\[{<]\s*(.+?)\s*[)\]}>]$")) {
        let comps: Vec<Expr> = split_top(&g(&m, 2)).iter().filter_map(|p| ex(p)).collect();
        if comps.len() >= 2 && comps.len() <= 3 {
            let vars: Vec<Expr> = ["x", "y", "z"][..comps.len()].iter().map(|n| sym(n)).collect();
            let kw = g(&m, 1).to_lowercase();
            let field = vec_expr(&comps);
            let (title, r) = match kw.as_str() {
                "divergence" | "div" => ("Divergence", s::simplify(&calculus::diff::divergence(&comps, &vars))),
                "jacobian" => ("Jacobian matrix", List(calculus::diff::jacobian(&comps, &vars).into_iter().map(|row| List(row.iter().map(s::simplify).collect())).collect())),
                _ => {
                    let (c3, v3) = if comps.len() == 2 { ([comps.clone(), vec![zero()]].concat(), vec![sym("x"), sym("y"), sym("z")]) } else { (comps.clone(), vars.clone()) };
                    ("Curl", vec_expr(&calculus::diff::curl(&c3, &v3)?.iter().map(s::simplify).collect::<Vec<_>>()))
                }
            };
            let mut pods = vec![primary(pod("Result", title, math(&r)))];
            if title == "Curl" {
                let zero_curl = matches!(&r, Func(_, a) if a.iter().all(|t| t.as_list().map_or(false, |v| v.iter().all(|u| u.is_zero()))));
                pods.push(pod("Note", "Interpretation", text(if zero_curl { "The curl vanishes everywhere, so the field is irrotational (conservative on simply connected domains)." } else { "The curl is not identically zero, so the field is not conservative." })));
            }
            if title == "Divergence" && r.is_zero() {
                pods.push(pod("Note", "Interpretation", text("The divergence is zero, so the field is incompressible (solenoidal).")));
            }
            return answer(format!("{kw} of {}", ptext(&field)), format!("{} of the vector field {}", title.to_lowercase(), ptext(&field)), pods);
        }
    }
    // double integrals
    let dbl = cap(re!(r"^(?:double\s+)?integra(?:l|te)(?:\s+of)?\s+(.+?)\s+d([a-z])\s*d([a-z])\s*,?\s*(?:for\s+|with\s+|where\s+|over\s+)?([a-z])\s*(?:from|=|in)\s*(.+?)\s*(?:to|\.\.)\s*(.+?)\s*(?:,\s*and|,|and)\s*([a-z])\s*(?:from|=|in)\s*(.+?)\s*(?:to|\.\.)\s*(.+)$"))
        .map(|m| (g(&m, 1), g(&m, 4), g(&m, 5), g(&m, 6), g(&m, 7), g(&m, 8), g(&m, 9)))
        .or_else(|| cap(re!(r"^(?:double\s+)?integra(?:l|te)(?:\s+of)?\s+(.+?)\s*,?\s+(?:over|for|with|where)\s+([a-z])\s*(?:from|=|in)\s*(.+?)\s*(?:to|\.\.)\s*(.+?)\s*(?:,\s*and|,|and)\s*([a-z])\s*(?:from|=|in)\s*(.+?)\s*(?:to|\.\.)\s*(.+)$")).map(|m| (g(&m, 1), g(&m, 2), g(&m, 3), g(&m, 4), g(&m, 5), g(&m, 6), g(&m, 7))));
    if let Some((body, v1, a1, b1, v2, a2, b2)) = dbl {
        let f = ex(&body)?;
        let (x1, x2) = (sym(&v1), sym(&v2));
        let (a1, b1, a2, b2) = (bound(&a1)?, bound(&b1)?, bound(&a2)?, bound(&b2)?);
        // integrate first over the variable whose limits are constants of the other
        let (inner, outer) = if a2.has_sym(&x1) || b2.has_sym(&x1) { ((x2.clone(), a2, b2), (x1.clone(), a1, b1)) } else { ((x1.clone(), a1, b1), (x2.clone(), a2, b2)) };
        let first = match calculus::integrate::definite(&f, &inner.0, &inner.1, &inner.2) {
            Definite::Exact(v) => v,
            _ => return None,
        };
        let v = definite_expr(&first, &outer.0, &outer.1, &outer.2)?;
        let holder = raw_fn("Integral", vec![raw_fn("Integral", vec![f.clone(), inner.0.clone(), inner.1.clone(), inner.2.clone()]), outer.0.clone(), outer.1.clone(), outer.2.clone()]);
        let mut pods = value_pods("Result", Some(holder), &v);
        pods.push(pod("Inner", &format!("Inner integral over {}", ptext(&inner.0)), math(&eq(raw_fn("Integral", vec![f.clone(), inner.0.clone(), inner.1.clone(), inner.2.clone()]), first))));
        return answer(format!("double integral of {}", ptext(&f)), format!("double integral of {} over {} and {}", ptext(&f), ptext(&inner.0), ptext(&outer.0)), pods);
    }
    // asymptotes
    if let Some(m) = cap(re!(r"^(?:the\s+)?(vertical |horizontal |oblique |slant )?asymptotes?\s+(?:of|for)\s+(.+)$")) {
        let e = fx(&g(&m, 2))?;
        let x = main_var(&e);
        if e.free_symbols().len() != 1 {
            return None;
        }
        let (num, den) = s::numer_denom(&s::cancel(&s::together(&e)));
        let mut vertical = Vec::new();
        for r in solve::solve(&den, &x).real() {
            let lft = calculus::limit(&e, &x, &r, Dir::Left);
            let rgt = calculus::limit(&e, &x, &r, Dir::Right);
            let infinite = |v: &Option<Expr>| v.as_ref().map_or(false, |v| v.is_inf() || v.is_neg_inf());
            if infinite(&lft) || infinite(&rgt) {
                vertical.push(r);
            }
        }
        let mut horizontal: Vec<Expr> = Vec::new();
        for target in [inf(), neg(inf())] {
            if let Some(v) = calculus::limit(&e, &x, &target, Dir::Both) {
                if !v.is_inf() && !v.is_neg_inf() && v.free_symbols().is_empty() && !horizontal.contains(&v) {
                    horizontal.push(v);
                }
            }
        }
        let mut oblique = None;
        if let (Some(dn), Some(dd)) = (poly::degree(&num, &x), poly::degree(&den, &x)) {
            if dn == dd + 1 && dd >= 1 {
                oblique = poly_divrem(&num, &den, &x).map(|(qt, _)| qt);
            }
        }
        let y = sym("y");
        let mut rows = Vec::new();
        for v in &vertical {
            rows.push(vec![text("vertical"), math(&eq(x.clone(), v.clone()))]);
        }
        for h in &horizontal {
            rows.push(vec![text("horizontal"), math(&eq(y.clone(), h.clone()))]);
        }
        if let Some(o) = &oblique {
            rows.push(vec![text("oblique"), math(&eq(y.clone(), o.clone()))]);
        }
        let mut pods = vec![primary(pod("Result", "Asymptotes", if rows.is_empty() { text("The function has no vertical, horizontal or oblique asymptotes.") } else { table(None, rows) }))];
        let range = vertical.iter().filter_map(|v| v.to_f64_const()).fold(None, |acc: Option<(f64, f64)>, v| Some(acc.map_or((v - 5.0, v + 5.0), |(a, b)| (a.min(v - 5.0), b.max(v + 5.0)))));
        let mut curves = vec![e.clone()];
        curves.extend(horizontal.iter().cloned());
        curves.extend(oblique.iter().cloned());
        if let Some(p) = plot_pod(&curves, &x, range) {
            pods.push(p);
        }
        return answer(format!("asymptotes of {}", ptext(&e)), format!("asymptotes of y = {}", ptext(&e)), pods);
    }
    // convergence of series
    let conv = cap(re!(r"^(?:is|does)\s+(?:the\s+)?(?:infinite\s+)?(?:series\s+|sum\s+)?(?:of\s+)?(.+?)\s+(?:converge|convergent|diverge|divergent|converge or diverge|convergent or divergent|converges)$"))
        .or_else(|| cap(re!(r"^(?:convergence|divergence)\s+(?:test\s+)?(?:of|for)\s+(?:the\s+)?(?:series\s+|sum\s+)?(?:of\s+)?(.+)$")));
    if let Some(m) = conv {
        let mut body = g(&m, 1);
        let mut start = one();
        if let Some(mm) = re!(r"^(.+?)\s*,?\s+(?:for\s+|from\s+)?([a-z])\s*=\s*(\S+)\s+to\s+(?:infinity|inf|oo|∞)$").captures(&body.clone()) {
            start = ex(&mm[3])?;
            body = mm[1].to_string();
        }
        let f = ex(&body)?;
        if f.free_symbols().len() != 1 || !mathy(&body) {
            return None;
        }
        let k = main_var(&f);
        let res = calculus::sum::sum(&f, &k, &start, &inf());
        let term_lim = calculus::limit(&f, &k, &inf(), Dir::Both);
        let ratio = calculus::limit(&s::simplify(&div(f.subs(&k, &add(vec![k.clone(), one()])), f.clone())), &k, &inf(), Dir::Both).map(|r| s::simplify(&raw_abs(&r)));
        let holder = raw_fn("Sum", vec![f.clone(), k.clone(), start.clone(), inf()]);
        let verdict = match &res {
            SumResult::Exact(v) if !v.is_inf() && !v.is_neg_inf() => Some((true, format!("The series converges; its sum is {}.", ptext(v)))),
            SumResult::Exact(_) => Some((false, "The series diverges.".to_string())),
            SumResult::Numeric(v) => Some((true, format!("The series converges; its sum is approximately {}.", crate::stats::fmt(*v)))),
            SumResult::Divergent(why) => Some((false, format!("The series diverges: {why}."))),
            SumResult::Unknown => match (&term_lim, ratio.as_ref().and_then(|r| r.to_f64_const())) {
                (Some(t), _) if !t.is_zero() => Some((false, "The series diverges: its terms do not tend to 0.".to_string())),
                (_, Some(r)) if r < 1.0 - 1e-12 => Some((true, "The series converges by the ratio test.".to_string())),
                (_, Some(r)) if r > 1.0 + 1e-12 => Some((false, "The series diverges by the ratio test.".to_string())),
                _ => None,
            },
        };
        let (_, msg) = verdict?;
        let mut pods = vec![primary(pod("Result", "Result", text(msg)))];
        let mut tests = Vec::new();
        if let Some(t) = &term_lim {
            tests.push(vec![text("limit of the terms"), math(t), text(if t.is_zero() { "inconclusive (necessary, not sufficient)" } else { "diverges" })]);
        }
        if let Some(r) = &ratio {
            let rv = r.to_f64_const();
            tests.push(vec![text("ratio test, lim |a(n+1)/a(n)|"), math(r), text(match rv {
                Some(v) if v < 1.0 - 1e-12 => "converges absolutely",
                Some(v) if v > 1.0 + 1e-12 => "diverges",
                _ => "inconclusive",
            })]);
        }
        if let (Some(dn), Some(dd)) = (poly::degree(&s::numer_denom(&f).0, &k), poly::degree(&s::numer_denom(&f).1, &k)) {
            let p = dd as i64 - dn as i64;
            tests.push(vec![text("comparison with a p-series"), text(format!("terms behave like 1/{}^{p}", ptext(&k))), text(if p > 1 { "converges (p > 1)" } else { "diverges (p ≤ 1)" })]);
        }
        if !tests.is_empty() {
            pods.push(pod("Tests", "Convergence tests", table(Some(vec!["test", "value", "conclusion"]), tests)));
        }
        return answer(format!("convergence of {}", ptext(&holder)), format!("does {} converge?", ptext(&holder)), pods);
    }
    // slope / derivative at a point
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:slope|derivative|gradient|instantaneous rate of change|rate of change)\s+of\s+(?:the\s+)?(?:tangent(?: line)?\s+to\s+)?(.+?)\s+(?:at|when)\s+(?:the point\s+)?(?:where\s+)?([a-z])\s*=\s*(.+)$")) {
        let e = fx(&g(&m, 1))?;
        let x = sym(&g(&m, 2));
        let a = ex(&g(&m, 3))?;
        if !e.has_sym(&x) || !mathy(&strip_fx(&g(&m, 1))) {
            return None;
        }
        let d = s::simplify(&calculus::diff(&e, &x));
        let v = s::simplify(&d.subs(&x, &a));
        let mut pods = value_pods("Slope", None, &v);
        pods.push(pod("Derivative", "Derivative", math(&eq(raw_fn("Derivative", vec![e.clone(), x.clone(), one()]), d))));
        let y0 = s::simplify(&e.subs(&x, &a));
        let tangent = s::simplify(&add(vec![y0.clone(), mul(vec![v.clone(), sub(x.clone(), a.clone())])]));
        pods.push(pod("Tangent", "Tangent line", math(&eq(sym("y"), s::expand(&tangent)))));
        if let Some(fa) = a.to_f64_const() {
            if let Some(p) = plot_pod(&[e.clone(), tangent], &x, Some((fa - 3.0, fa + 3.0))) {
                pods.push(p);
            }
        }
        return answer(format!("slope of {} at {} = {}", ptext(&e), ptext(&x), ptext(&a)), format!("slope of y = {} at {} = {}", ptext(&e), ptext(&x), ptext(&a)), pods);
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:average|mean)\s+value\s+of\s+(.+?)\s+(?:from|between|over|on)\s+(?:[a-z]\s*=\s*)?(\S+)\s+(?:to|and)\s+(?:[a-z]\s*=\s*)?(\S+)$")) {
        let e = fx(&g(&m, 1))?;
        let x = main_var(&e);
        let (a, b) = (bound(&g(&m, 2))?, bound(&g(&m, 3))?);
        if e.free_symbols().len() != 1 {
            return None;
        }
        let integral = definite_expr(&e, &x, &a, &b)?;
        let v = s::simplify(&div(integral.clone(), sub(b.clone(), a.clone())));
        let mut pods = value_pods("Average value", None, &v);
        pods.push(pod("Formula", "Formula", text("f_avg = 1/(b − a) · ∫ f(x) dx over [a, b]")));
        pods.push(pod("Integral", "Integral", math(&eq(raw_fn("Integral", vec![e.clone(), x.clone(), a.clone(), b.clone()]), integral))));
        if let (Some(fa), Some(fb)) = (a.to_f64_const(), b.to_f64_const()) {
            if let Some(p) = plot_pod(&[e.clone(), v.clone()], &x, Some((fa, fb))) {
                pods.push(p);
            }
        }
        return answer(format!("average value of {}", ptext(&e)), format!("average value of {} on [{}, {}]", ptext(&e), ptext(&a), ptext(&b)), pods);
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?average rate of change\s+of\s+(.+?)\s+(?:from|between|over|on)\s+(?:[a-z]\s*=\s*)?(\S+)\s+(?:to|and)\s+(?:[a-z]\s*=\s*)?(\S+)$")) {
        let e = fx(&g(&m, 1))?;
        let x = main_var(&e);
        let (a, b) = (ex(&g(&m, 2))?, ex(&g(&m, 3))?);
        let v = s::simplify(&div(sub(e.subs(&x, &b), e.subs(&x, &a)), sub(b.clone(), a.clone())));
        let mut pods = value_pods("Average rate of change", None, &v);
        pods.push(pod("Formula", "Formula", text(format!("(f({}) − f({})) / ({} − {})", ptext(&b), ptext(&a), ptext(&b), ptext(&a)))));
        return answer(format!("average rate of change of {}", ptext(&e)), format!("average rate of change of {} from {} to {}", ptext(&e), ptext(&a), ptext(&b)), pods);
    }
    None
}

fn raw_abs(e: &Expr) -> Expr {
    match e.to_f64_const() {
        Some(v) if v < 0.0 => neg(e.clone()),
        Some(_) => e.clone(),
        None => func("abs", vec![e.clone()]),
    }
}

// ---------------------------------------------------------------------------
// matrices and vectors
// ---------------------------------------------------------------------------

fn mat_of(t: &str) -> Option<Mat> {
    let t = t.trim().replace("[[", "{{").replace("]]", "}}").replace("],", "},").replace(", [", ", {").replace(",[", ",{");
    linalg::from_expr(&ex(&t)?)
}

fn vec_of(t: &str) -> Option<Vec<Expr>> {
    let t = t.trim();
    let inner = t.strip_prefix(['(', '[', '{', '<', '⟨'])?.strip_suffix([')', ']', '}', '>', '⟩'])?;
    let parts = split_top(inner);
    let v: Vec<Expr> = parts.iter().filter_map(|p| ex(p)).collect();
    (v.len() == parts.len() && v.len() >= 2 && v.iter().all(|e| !e.is_list())).then_some(v)
}

fn vec_expr(v: &[Expr]) -> Expr {
    raw_fn("Tuple", vec![List(v.to_vec())])
}

const MAT: &str = r"(\{\{.+\}\}|\[\[.+\]\])";
const VEC: &str = r"([(\[{<⟨][^()\[\]{}<>⟨⟩]+[)\]}>⟩])";

pub fn matrices(q: &Query, l: &str, c: &str) -> Option<Answer> {
    let cap = |r: &Regex| caps(r, l, c);
    let has_matrix = l.contains("{{") || l.contains("[[");

    if has_matrix {
        static UNARY: OnceLock<Regex> = OnceLock::new();
        let unary = UNARY.get_or_init(|| Regex::new(&format!(r"^(?:the\s+)?(determinant|det|inverse|transpose|trace|rank|eigenvalues?|eigenvectors?|eigensystem|null ?space|kernel|nullity|row reduce|row reduction(?: of)?|rref|row echelon form|reduced row echelon form|characteristic polynomial|adjugate|adjoint|diagonali[sz]e|square|cube)\s*(?:of\s+)?(?:the\s+)?(?:matrix\s+)?{MAT}$")).unwrap());
        if let Some(m) = cap(unary) {
            let a = mat_of(&g(&m, 2))?;
            let (rws, cls) = (a.len(), a[0].len());
            let square_m = rws == cls;
            let op = g(&m, 1).to_lowercase();
            let me = linalg::to_expr(&a);
            let lam = sym("λ");
            let (title, content): (&str, Content) = match op.as_str() {
                "determinant" | "det" if square_m => ("Determinant", math(&linalg::det(&a))),
                "inverse" if square_m => match linalg::inverse(&a) {
                    Some(inv) => ("Inverse", math(&linalg::to_expr(&inv))),
                    None => ("Inverse", text("The matrix is singular (its determinant is 0), so it has no inverse.")),
                },
                "transpose" => ("Transpose", math(&linalg::to_expr(&linalg::transpose(&a)))),
                "trace" if square_m => ("Trace", math(&linalg::trace(&a))),
                "rank" => ("Rank", text(linalg::rank(&a).to_string())),
                "nullity" => ("Nullity", text((cls - linalg::rank(&a)).to_string())),
                "eigenvalue" | "eigenvalues" if square_m => {
                    let ev = linalg::eigenvalues(&a);
                    if ev.is_empty() {
                        return None;
                    }
                    ("Eigenvalues", table(None, ev.iter().enumerate().map(|(k, (v, mult))| {
                        let mut row = vec![math(&eq(sym(&format!("λ_{}", k + 1)), v.clone()))];
                        if *mult > 1 {
                            row.push(text(format!("multiplicity {mult}")));
                        }
                        row
                    }).collect()))
                }
                "eigenvector" | "eigenvectors" | "eigensystem" | "diagonalize" | "diagonalise" if square_m => {
                    let ev = linalg::eigenvectors(&a);
                    if ev.is_empty() {
                        return None;
                    }
                    ("Eigenvectors", table(Some(vec!["eigenvalue", "eigenvector"]), ev.iter().map(|(v, w)| vec![math(v), math(&vec_expr(w))]).collect()))
                }
                "null space" | "nullspace" | "kernel" => {
                    let ns = linalg::nullspace(&a);
                    ("Null space", if ns.is_empty() { text("{0} — only the zero vector (the columns are linearly independent)") } else { table(Some(vec!["basis vector"]), ns.iter().map(|w| vec![math(&vec_expr(w))]).collect()) })
                }
                "row reduce" | "row reduction" | "row reduction of" | "rref" | "row echelon form" | "reduced row echelon form" => ("Reduced row echelon form", math(&linalg::to_expr(&linalg::rref(&a).0))),
                "characteristic polynomial" if square_m => ("Characteristic polynomial", math(&s::expand(&linalg::charpoly(&a, &lam)))),
                "adjugate" | "adjoint" if square_m => {
                    let inv = linalg::inverse(&a)?;
                    let d = linalg::det(&a);
                    ("Adjugate", math(&linalg::to_expr(&inv.iter().map(|r| r.iter().map(|v| s::simplify(&mul(vec![d.clone(), v.clone()]))).collect()).collect())))
                }
                "square" if square_m => ("Square", math(&linalg::to_expr(&linalg::matpow(&a, 2)?))),
                "cube" if square_m => ("Cube", math(&linalg::to_expr(&linalg::matpow(&a, 3)?))),
                _ => return answer(format!("{op} of a matrix"), format!("{op} of {}", ptext(&me)), vec![primary(pod("Result", "Result", text(format!("The {op} is only defined for square matrices; this matrix is {rws} × {cls}."))))]),
            };
            let mut pods = vec![primary(pod("Result", title, content))];
            if let Some(extra) = again(q, &ptext(&me)) {
                pods.extend(extra.pods.into_iter().filter(|p| p.id != "Input" && p.title != title).map(|mut p| {
                    p.primary = false;
                    p
                }));
            }
            return answer(format!("{op} of {}", ptext(&me)), format!("{} of {}", title.to_lowercase(), ptext(&me)), pods);
        }
        // powers
        static POWER: OnceLock<Regex> = OnceLock::new();
        if let Some(m) = cap(POWER.get_or_init(|| Regex::new(&format!(r"^{MAT}\s*(?:\^|\*\*|to the power(?: of)?)\s*\(?(-?\d+)\)?$")).unwrap())) {
            let a = mat_of(&g(&m, 1))?;
            let k: i64 = g(&m, 2).parse().ok()?;
            let r = linalg::matpow(&a, k)?;
            let me = linalg::to_expr(&a);
            return answer(format!("{}^{k}", ptext(&me)), format!("{}^{k}", ptext(&me)), vec![primary(pod("Result", "Result", math(&linalg::to_expr(&r))))]);
        }
        // products, sums and differences of two matrices; matrix times vector
        let text_norm = c.replace("[[", "{{").replace("]]", "}}");
        let lower_norm = text_norm.to_lowercase();
        static BINARY: OnceLock<Regex> = OnceLock::new();
        let binary = BINARY.get_or_init(|| Regex::new(r"^(?:multiply\s+|the product of\s+|product of\s+|add\s+|subtract\s+)?(\{\{.+?\}\})\s*(\*|\.|·|×|x|times|\+|-|plus|minus|by|and|with|from)?\s*(\{\{.+\}\}|\{[^{}]+\})$").unwrap());
        if let Some(m) = caps(binary, &lower_norm, &text_norm) {
            let a = mat_of(&g(&m, 1))?;
            let rhs = g(&m, 3);
            let b = if rhs.starts_with("{{") { mat_of(&rhs)? } else { ex(&rhs)?.as_list()?.iter().map(|v| vec![v.clone()]).collect() };
            let op = g(&m, 2).to_lowercase();
            let adding = op == "+" || op == "plus" || lower_norm.starts_with("add ");
            let subtracting = op == "-" || op == "minus";
            let same = a.len() == b.len() && a[0].len() == b[0].len();
            let (sym_op, r): (&str, Mat) = if adding || subtracting {
                if !same {
                    return answer("matrix sum", "matrix sum", vec![primary(pod("Result", "Result", text("Matrices can only be added or subtracted when they have the same dimensions.")))]);
                }
                (if adding { "+" } else { "−" }, a.iter().zip(b.iter()).map(|(ra, rb)| ra.iter().zip(rb.iter()).map(|(u, v)| s::simplify(&if adding { add(vec![u.clone(), v.clone()]) } else { sub(u.clone(), v.clone()) })).collect()).collect())
            } else {
                match linalg::matmul(&a, &b) {
                    Some(r) => ("·", r),
                    None => return answer("matrix product", "matrix product", vec![primary(pod("Result", "Result", text(format!("These matrices cannot be multiplied: the first has {} columns but the second has {} rows.", a[0].len(), b.len()))))]),
                }
            };
            let label = format!("{} {sym_op} {}", ptext(&linalg::to_expr(&a)), ptext(&linalg::to_expr(&b)));
            let mut pods = vec![primary(pod("Result", "Result", math(&linalg::to_expr(&r))))];
            pods.push(pod("Dimensions", "Dimensions", text(format!("{} × {}", r.len(), r[0].len()))));
            if sym_op == "·" && a.len() == a[0].len() && same {
                if let Some(ba) = linalg::matmul(&b, &a) {
                    pods.push(pod("Reverse", "Product in the other order", math(&linalg::to_expr(&ba))));
                    pods.push(pod("Commute", "Commutativity", text(if ba == r { "These two matrices commute." } else { "These two matrices do not commute." })));
                }
            }
            return answer(label.clone(), label, pods);
        }
    }
    // identity matrices
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:(\d+)\s*(?:x|by|×)\s*\d+\s+identity matrix|identity matrix\s+(?:of\s+)?(?:size\s+|order\s+|dimension\s+)?(\d+)(?:\s*(?:x|by|×)\s*\d+)?)$")) {
        let n: usize = if g(&m, 1).is_empty() { g(&m, 2) } else { g(&m, 1) }.parse().ok()?;
        if n == 0 || n > 12 {
            return None;
        }
        return answer(format!("{n} × {n} identity matrix"), format!("{n} × {n} identity matrix"), vec![primary(pod("Result", "Result", math(&linalg::to_expr(&linalg::identity(n)))))]);
    }
    // vectors
    static TWO: OnceLock<Regex> = OnceLock::new();
    let two = TWO.get_or_init(|| Regex::new(&format!(r"^(?:the\s+)?(dot product|scalar product|inner product|cross product|vector product|angle between|angle|projection|distance between|sum|difference)\s+(?:of\s+)?(?:the\s+)?(?:vectors?\s+)?{VEC}\s*(?:and|,|with|onto|on|to|x|\.)\s*(?:the\s+)?(?:vector\s+)?{VEC}$")).unwrap());
    static INFIX: OnceLock<Regex> = OnceLock::new();
    let infix = INFIX.get_or_init(|| Regex::new(&format!(r"^{VEC}\s*(\.|·|dot|cross|×|x)\s*{VEC}$")).unwrap());
    let pair = cap(two).map(|m| (g(&m, 1).to_lowercase(), g(&m, 2), g(&m, 3))).or_else(|| {
        cap(infix).map(|m| {
            let op = match g(&m, 2).to_lowercase().as_str() {
                "." | "·" | "dot" => "dot product",
                _ => "cross product",
            };
            (op.to_string(), g(&m, 1), g(&m, 3))
        })
    });
    if let Some((op, ta, tb)) = pair {
        let (a, b) = (vec_of(&ta)?, vec_of(&tb)?);
        if a.len() != b.len() || (op.starts_with("distance") && !l.contains("vector")) {
            return None;
        }
        let dot = |u: &[Expr], v: &[Expr]| s::simplify(&add(u.iter().zip(v.iter()).map(|(p, r)| mul(vec![p.clone(), r.clone()])).collect()));
        let norm = |u: &[Expr]| s::simplify(&sqrt(dot(u, u)));
        let label = format!("{} and {}", ptext(&vec_expr(&a)), ptext(&vec_expr(&b)));
        let mut pods: Vec<Pod> = Vec::new();
        match op.as_str() {
            "dot product" | "scalar product" | "inner product" => {
                let d = dot(&a, &b);
                pods.extend(value_pods("Dot product", None, &d));
                pods.push(pod("Work", "Calculation", text(a.iter().zip(b.iter()).map(|(p, r)| format!("({})({})", ptext(p), ptext(r))).collect::<Vec<_>>().join(" + "))));
                if d.is_zero() {
                    pods.push(pod("Note", "Note", text("The dot product is 0, so the vectors are perpendicular.")));
                }
            }
            "cross product" | "vector product" => {
                let (a3, b3) = if a.len() == 2 { ([a.clone(), vec![zero()]].concat(), [b.clone(), vec![zero()]].concat()) } else { (a.clone(), b.clone()) };
                if a3.len() != 3 {
                    return None;
                }
                let comp = |i: usize, j: usize| s::simplify(&sub(mul(vec![a3[i].clone(), b3[j].clone()]), mul(vec![a3[j].clone(), b3[i].clone()])));
                let r = vec![comp(1, 2), comp(2, 0), comp(0, 1)];
                pods.push(primary(pod("Result", "Cross product", math(&vec_expr(&r)))));
                pods.push(pod("Magnitude", "Magnitude (area of the parallelogram)", math(&norm(&r))));
                if r.iter().all(|v| v.is_zero()) {
                    pods.push(pod("Note", "Note", text("The cross product is the zero vector, so the vectors are parallel.")));
                }
            }
            "angle between" | "angle" => {
                let cosv = s::simplify(&div(dot(&a, &b), mul(vec![norm(&a), norm(&b)])));
                let ang = s::simplify(&func("acos", vec![cosv.clone()]));
                let deg = ang.to_f64_const().map(|v| v.to_degrees());
                pods.push(primary(pod("Result", "Angle", match deg {
                    Some(d) => text(format!("{}°  ({} radians)", crate::stats::fmt(d), ptext(&ang))),
                    None => math(&ang),
                })));
                pods.push(pod("Cosine", "Cosine of the angle", math(&cosv)));
                pods.push(pod("Formula", "Formula", text("cos θ = (a · b) / (|a| |b|)")));
            }
            "projection" => {
                let k = s::simplify(&div(dot(&a, &b), dot(&b, &b)));
                let r: Vec<Expr> = b.iter().map(|v| s::simplify(&mul(vec![k.clone(), v.clone()]))).collect();
                pods.push(primary(pod("Result", "Vector projection", math(&vec_expr(&r)))));
                pods.push(pod("Scalar", "Scalar projection", math(&s::simplify(&div(dot(&a, &b), norm(&b))))));
            }
            "sum" | "difference" => {
                let r: Vec<Expr> = a.iter().zip(b.iter()).map(|(p, r)| s::simplify(&if op == "sum" { add(vec![p.clone(), r.clone()]) } else { sub(p.clone(), r.clone()) })).collect();
                pods.push(primary(pod("Result", "Result", math(&vec_expr(&r)))));
            }
            _ => {
                let d: Vec<Expr> = a.iter().zip(b.iter()).map(|(p, r)| sub(p.clone(), r.clone())).collect();
                pods.extend(value_pods("Distance", None, &norm(&d)));
            }
        }
        return answer(format!("{op} of {label}"), format!("{op}: {label}"), pods);
    }
    static ONE: OnceLock<Regex> = OnceLock::new();
    let one_v = ONE.get_or_init(|| Regex::new(&format!(r"^(?:the\s+)?(norm|magnitude|length|modulus|unit vector|normali[sz]e|direction)\s*(?:of\s+|in the direction of\s+|for\s+|along\s+)?(?:the\s+)?(?:vector\s+)?{VEC}$")).unwrap());
    if let Some(m) = cap(one_v) {
        let a = vec_of(&g(&m, 2))?;
        let op = g(&m, 1).to_lowercase();
        let n = s::simplify(&sqrt(add(a.iter().map(|v| square(v.clone())).collect())));
        let unit: Vec<Expr> = a.iter().map(|v| s::simplify(&div(v.clone(), n.clone()))).collect();
        let label = ptext(&vec_expr(&a));
        let mut pods = if matches!(op.as_str(), "norm" | "magnitude" | "length" | "modulus") {
            let mut p = value_pods("Magnitude", None, &n);
            p.push(pod("Unit", "Unit vector", math(&vec_expr(&unit))));
            p
        } else {
            vec![primary(pod("Result", "Unit vector", math(&vec_expr(&unit)))), pod("Magnitude", "Magnitude", math(&n))]
        };
        if a.len() == 2 {
            if let (Some(x0), Some(y0)) = (a[0].to_f64_const(), a[1].to_f64_const()) {
                pods.push(pod("Direction", "Direction angle", text(format!("{}° from the positive x-axis", crate::stats::fmt(y0.atan2(x0).to_degrees())))));
            }
        }
        return answer(format!("{op} of {label}"), format!("{op} of the vector {label}"), pods);
    }
    None
}

// ---------------------------------------------------------------------------
// statistics and probability
// ---------------------------------------------------------------------------

fn word_num(t: &str) -> Option<u64> {
    Some(match t.trim() {
        "a" | "an" | "one" | "single" => 1,
        "two" | "a pair of" => 2,
        "three" => 3,
        "four" => 4,
        "five" => 5,
        "six" => 6,
        "seven" => 7,
        "eight" => 8,
        "nine" => 9,
        "ten" => 10,
        n => n.parse().ok()?,
    })
}

fn frac(n: u64, d: u64) -> Expr {
    num(Rat::new(n.into(), d.into()))
}

fn list_f64(t: &str) -> Option<Vec<f64>> {
    let inner = t.trim().trim_start_matches(['{', '[', '(']).trim_end_matches(['}', ']', ')']);
    let v: Vec<f64> = split_top(inner).iter().filter_map(|p| ex(p)).filter_map(|e| e.to_f64_const()).collect();
    (v.len() >= 2).then_some(v)
}

pub fn statistics(q: &Query, l: &str, c: &str) -> Option<Answer> {
    let cap = |r: &Regex| caps(r, l, c);
    let _ = q;
    let fmt = crate::stats::fmt;

    // correlation and covariance
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:pearson\s+)?(correlation(?: coefficient)?|covariance)\s+(?:of|between|for)\s+([{\[(].+?[}\])])\s*(?:and|,|with)\s*([{\[(].+[}\])])$")) {
        let (a, b) = (list_f64(&g(&m, 2))?, list_f64(&g(&m, 3))?);
        if a.len() != b.len() {
            return answer("correlation", "correlation", vec![primary(pod("Result", "Result", text("The two lists must have the same number of values.")))]);
        }
        let n = a.len() as f64;
        let (ma, mb) = (a.iter().sum::<f64>() / n, b.iter().sum::<f64>() / n);
        let sxy: f64 = a.iter().zip(b.iter()).map(|(x, y)| (x - ma) * (y - mb)).sum();
        let (sxx, syy): (f64, f64) = (a.iter().map(|x| (x - ma).powi(2)).sum(), b.iter().map(|y| (y - mb).powi(2)).sum());
        let r = sxy / (sxx * syy).sqrt();
        let cov = sxy / (n - 1.0);
        let is_cov = g(&m, 1).to_lowercase().starts_with("cov");
        let strength = match r.abs() {
            v if v > 0.9 => "very strong",
            v if v > 0.7 => "strong",
            v if v > 0.4 => "moderate",
            v if v > 0.2 => "weak",
            _ => "negligible",
        };
        let mut pods = vec![pod("Correlation", "Pearson correlation coefficient", text(format!("r = {}", fmt(r)))), pod("Covariance", "Sample covariance", text(fmt(cov)))];
        pods.swap(0, if is_cov { 1 } else { 0 });
        pods[0].primary = true;
        pods.push(pod("R2", "Coefficient of determination", text(format!("r² = {}", fmt(r * r)))));
        pods.push(pod("Interpretation", "Interpretation", text(format!("{strength} {} linear relationship", if r >= 0.0 { "positive" } else { "negative" }))));
        let slope = sxy / sxx;
        pods.push(pod("Fit", "Least-squares line", text(format!("y = {} x + {}", fmt(slope), fmt(mb - slope * ma)))));
        pods.push(pod("Plot", "Scatter plot", plot(pl::scatter(a.iter().zip(b.iter()).map(|(x, y)| [*x, *y]).collect(), "data", None))));
        return answer("correlation of two data sets", format!("correlation of {} and {}", g(&m, 2), g(&m, 3)), pods);
    }
    // critical z values
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:critical\s+)?z[- ]?(?:score|value|statistic|critical value|\*)?\s*(?:for|of|at)\s+(?:an?\s+)?([\d.]+)\s*(?:%|percent)\s*(?:confidence)?(?:\s+(?:level|interval))?$")) {
        use statrs::distribution::{ContinuousCDF, Normal};
        let conf: f64 = g(&m, 1).parse().ok()?;
        if !(0.0..100.0).contains(&conf) {
            return None;
        }
        let nd = Normal::new(0.0, 1.0).ok()?;
        let alpha = 1.0 - conf / 100.0;
        let two = nd.inverse_cdf(1.0 - alpha / 2.0);
        let one_s = nd.inverse_cdf(1.0 - alpha);
        let label = format!("z critical value for {conf}% confidence");
        return answer(label.clone(), label, vec![
            primary(pod("Result", "Two-sided critical value", text(format!("z* = ±{}", fmt((two * 1e4).round() / 1e4))))),
            pod("OneSided", "One-sided critical value", text(format!("z = {}", fmt((one_s * 1e4).round() / 1e4)))),
            pod("Alpha", "Significance level", text(format!("α = {}", fmt(alpha)))),
            pod("Use", "Confidence interval", text(format!("x̄ ± {} · σ/√n", fmt((two * 1e3).round() / 1e3)))),
        ]);
    }
    if let Some(m) = cap(re!(r"^z[- ]?score\s+(?:of|for)\s+(?:x\s*=\s*)?(-?[\d.]+)\s*(?:,|with|given|when|if)?\s*(?:a\s+)?(?:mean|μ|mu)\s*(?:of|=|is)?\s*(-?[\d.]+)\s*(?:,|and)?\s*(?:a\s+)?(?:standard deviation|sd|std|σ|sigma|std dev)\s*(?:of|=|is)?\s*([\d.]+)$")) {
        use statrs::distribution::{ContinuousCDF, Normal};
        let (x, mu, sd): (f64, f64, f64) = (g(&m, 1).parse().ok()?, g(&m, 2).parse().ok()?, g(&m, 3).parse().ok()?);
        if sd <= 0.0 {
            return None;
        }
        let z = (x - mu) / sd;
        let p = Normal::new(0.0, 1.0).ok()?.cdf(z);
        let label = format!("z-score of {x} (mean {mu}, standard deviation {sd})");
        return answer(label.clone(), label, vec![primary(pod("Result", "z-score", text(format!("z = ({x} − {mu}) / {sd} = {}", fmt(z))))), pod("Percentile", "Percentile", text(format!("{}% of a normal population lies below this value", fmt(p * 100.0)))), pod("Tail", "Upper tail probability", text(fmt(1.0 - p)))]);
    }
    // sums of dice
    let dice = cap(re!(r"^(?:the\s+)?(?:probability|chance|odds|likelihood)\s+(?:of|that)\s+(?:rolling|getting|throwing|i roll|you roll)\s+(?:a\s+|an\s+)?(?:sum\s+of\s+|total\s+of\s+)?(?:(at least|at most|more than|less than|exactly)\s+)?(?:a\s+|an\s+)?(\d+)\s+(?:with|on|using|when rolling|when throwing|from|in|rolling)\s+(\w+|a pair of)\s+(?:fair\s+)?(?:standard\s+)?(?:six[- ]sided\s+)?(?:dice|die)$"));
    if let Some(m) = dice {
        let n = word_num(&g(&m, 3))?;
        let target: u64 = g(&m, 2).parse().ok()?;
        if n == 0 || n > 20 {
            return None;
        }
        let dist = dice_dist(n as usize);
        let total = 6u64.pow(n as u32);
        let mode = g(&m, 1).to_lowercase();
        let ways: u64 = dist.iter().enumerate().filter(|(sum, _)| match mode.as_str() {
            "at least" => *sum as u64 >= target,
            "at most" => *sum as u64 <= target,
            "more than" => *sum as u64 > target,
            "less than" => (*sum as u64) < target,
            _ => *sum as u64 == target,
        }).map(|(_, w)| *w).sum();
        let p = frac(ways, total);
        let label = format!("probability of {}{target} with {n} {}", if mode.is_empty() { "a sum of ".to_string() } else { format!("a sum of {mode} ") }, if n == 1 { "die" } else { "dice" });
        let mut pods = vec![primary(pod("Result", "Probability", math(&p)))];
        pods.push(pod("Decimal", "Decimal form", text(format!("{}  ({}%)", fmt(ways as f64 / total as f64), fmt(100.0 * ways as f64 / total as f64)))));
        pods.push(pod("Count", "Favourable outcomes", text(format!("{ways} of {total} equally likely outcomes"))));
        pods.push(dice_table(&dist, total));
        return answer(label.clone(), label, pods);
    }
    if let Some(m) = cap(re!(r"^(?:sum of\s+|rolling\s+|throwing\s+)?(\w+|a pair of)\s+(?:fair\s+)?(?:six[- ]sided\s+)?dice(?:\s+(?:sum|sums|total|distribution|probabilities|outcomes))?$")) {
        let n = word_num(&g(&m, 1))?;
        if (1..=10).contains(&n) {
            let dist = dice_dist(n as usize);
            let total = 6u64.pow(n as u32);
            let label = format!("sum of {n} fair dice");
            let mean = 3.5 * n as f64;
            let var = 35.0 / 12.0 * n as f64;
            return answer(label.clone(), label, vec![primary(dice_table(&dist, total)), pod("Stats", "Expected value and spread", table(None, vec![vec![text("mean"), text(fmt(mean))], vec![text("standard deviation"), text(fmt(var.sqrt()))], vec![text("outcomes"), text(total.to_string())]]))]);
        }
    }
    // coin tosses
    let coins = cap(re!(r"^(\w+)\s+(?:fair\s+)?coin\s+(?:tosses|flips|toss|flip)$")).or_else(|| cap(re!(r"^(?:flip|toss|flipping|tossing)\s+(\w+)\s+(?:fair\s+)?coins$"))).or_else(|| cap(re!(r"^(?:flip|toss)\s+a\s+(?:fair\s+)?coin\s+(\w+)\s+times$")));
    if let Some(m) = coins {
        let n = word_num(&g(&m, 1))?;
        if (2..=60).contains(&n) {
            let total = 2f64.powi(n as i32);
            let rows: Vec<Vec<Content>> = (0..=n).map(|k| {
                let ways = crate::numtheory::binomial(&n.into(), k);
                let w = num_traits::ToPrimitive::to_f64(&ways).unwrap_or(0.0);
                vec![text(k.to_string()), text(ways.to_string()), text(fmt(w / total))]
            }).collect();
            let label = format!("{n} tosses of a fair coin");
            let bars: Vec<(String, f64)> = (0..=n).map(|k| (k.to_string(), num_traits::ToPrimitive::to_f64(&crate::numtheory::binomial(&n.into(), k)).unwrap_or(0.0) / total)).collect();
            let mut pods = vec![primary(pod("Result", "Distribution of the number of heads", table(Some(vec!["heads", "ways", "probability"]), rows)))];
            pods.push(pod("Stats", "Expected value and spread", table(None, vec![vec![text("expected heads"), text(fmt(n as f64 / 2.0))], vec![text("standard deviation"), text(fmt((n as f64).sqrt() / 2.0))], vec![text("possible sequences"), text(format!("2^{n} = {}", fmt(total)))]])));
            pods.push(pod("Plot", "Probability plot", plot(pl::bars(bars))));
            return answer(label.clone(), label, pods);
        }
    }
    None
}

/// Number of ways to roll each total with `n` dice (index = total).
fn dice_dist(n: usize) -> Vec<u64> {
    let mut dist = vec![1u64];
    for _ in 0..n {
        let mut next = vec![0u64; dist.len() + 6];
        for (s0, w) in dist.iter().enumerate() {
            for face in 1..=6 {
                next[s0 + face] += w;
            }
        }
        dist = next;
    }
    dist
}

fn dice_table(dist: &[u64], total: u64) -> Pod {
    let rows: Vec<Vec<Content>> = dist.iter().enumerate().filter(|(_, w)| **w > 0).map(|(sum, w)| vec![text(sum.to_string()), text(w.to_string()), math(&frac(*w, total)), text(crate::stats::fmt(*w as f64 / total as f64))]).collect();
    pod("Distribution", "Distribution of the total", table(Some(vec!["total", "ways", "probability", "decimal"]), rows))
}
