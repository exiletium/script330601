//! Natural-language commands: "derivative of …", "integrate … from a to b",
//! "solve … for y", "plot …", "factor …", "is 97 prime", …

use super::math::{self, input_pod};
use super::*;
use crate::calculus;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::numtheory as nt;
use crate::parse::{self, Parsed};
use crate::plot as pl;
use crate::print::{text as ptext, D};
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

fn parse_e(s: &str) -> Option<Parsed> {
    parse::parse(s.trim()).ok()
}

fn ex(s: &str) -> Option<Expr> {
    parse_e(s).map(|p| p.expr)
}

fn var_or_default(e: &Expr, v: Option<&str>) -> Expr {
    if let Some(v) = v {
        return sym(v.trim());
    }
    let fs = e.free_symbols();
    for c in ["x", "t", "y", "z", "n", "k"] {
        if fs.iter().any(|n| &**n == c) {
            return sym(c);
        }
    }
    fs.into_iter().next().map(|n| sym(&n)).unwrap_or_else(|| sym("x"))
}

/// Turn a holder expression into the full calculus answer.
fn holder_answer(e: Expr, interp_d: D, q: &Query) -> Option<Answer> {
    let p = Parsed { ast: parse::Ast::Num(Rat::from_integer(0.into()), String::new()), expr: e, d: interp_d, has_decimal: false, notes: vec![] };
    let interp = print::d_text(&p.d);
    Some(Answer::new(interp, math::calculus_pods(&p, q.opts)))
}

fn d_of(e: &Expr) -> D {
    print::to_d(e, &Default::default())
}

fn bound(s: &str) -> Option<Expr> {
    let t = s.trim().to_lowercase();
    match t.as_str() {
        "inf" | "infinity" | "∞" | "+inf" | "+infinity" | "oo" => Some(inf()),
        "-inf" | "-infinity" | "-∞" | "-oo" => Some(neg(inf())),
        _ => ex(s),
    }
}

pub fn handle(q: &Query) -> Option<Answer> {
    let l = q.lower.as_str();
    let c = q.clean.as_str();
    // keep the original casing of the math part: work on byte offsets of `c`
    let cap = |r: &Regex| r.captures(l).map(|m| (0..m.len()).map(|k| m.get(k).map(|g| c[g.start()..g.end()].to_string())).collect::<Vec<Option<String>>>());

    // ---------------------------------------------------------- derivatives
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:(second|third|2nd|3rd|(\d+)(?:st|nd|rd|th))\s+)?(?:derivative|differentiate|diff)\s*(?:of\s+)?(.+?)(?:\s+(?:with respect to|wrt|w\.r\.t\.?|by)\s+([a-z]\w*))?$")) {
        let f = ex(m[3].as_deref()?)?;
        let x = var_or_default(&f, m[4].as_deref());
        let n = match m[1].as_deref().map(|s| s.to_lowercase()) {
            Some(ref s) if s.starts_with("second") || s.starts_with("2") => 2,
            Some(ref s) if s.starts_with("third") || s.starts_with("3") => 3,
            Some(_) => m[2].as_deref().and_then(|s| s.parse().ok()).unwrap_or(1),
            None => 1,
        };
        let h = raw_fn("Derivative", vec![f, x, int(n)]);
        return holder_answer(h.clone(), d_of(&h), q);
    }
    if let Some(m) = cap(re!(r"^d(\^\d)?\s*/\s*d([a-z])(?:\^\d)?\s*(.+)$")) {
        let f = ex(m[3].as_deref()?)?;
        let n: i64 = m[1].as_deref().map(|s| s[1..].parse().unwrap_or(1)).unwrap_or(1);
        let h = raw_fn("Derivative", vec![f, sym(m[2].as_deref()?), int(n)]);
        return holder_answer(h.clone(), d_of(&h), q);
    }
    // ------------------------------------------------------------ integrals
    if let Some(m) = cap(re!(r"^(?:integrate|integral(?:\s+of)?|antiderivative(?:\s+of)?|∫)\s*(.+?)(?:\s*d([a-z]))?(?:\s+(?:from|between)\s+(.+?)\s+(?:to|and)\s+(.+?))?(?:\s+d([a-z]))?$")) {
        let body = m[1].as_deref()?;
        let f = ex(body)?;
        let x = var_or_default(&f, m[2].as_deref().or(m[5].as_deref()));
        let h = match (m[3].as_deref(), m[4].as_deref()) {
            (Some(a), Some(b)) => raw_fn("Integral", vec![f, x, bound(a)?, bound(b)?]),
            _ => raw_fn("Integral", vec![f, x]),
        };
        return holder_answer(h.clone(), d_of(&h), q);
    }
    if let Some(m) = cap(re!(r"^area (?:under|of)\s+(.+?)\s+(?:from|between)\s+(.+?)\s+(?:to|and)\s+(.+)$")) {
        let f = ex(m[1].as_deref()?)?;
        let x = var_or_default(&f, None);
        let h = raw_fn("Integral", vec![f, x, bound(m[2].as_deref()?)?, bound(m[3].as_deref()?)?]);
        return holder_answer(h.clone(), d_of(&h), q);
    }
    // --------------------------------------------------------------- limits
    if let Some(m) = cap(re!(r"^lim(?:it)?\s*(?:of\s+)?(.+?)\s+(?:as\s+)?([a-z])\s*(?:->|→|approaches|goes to|tends to|to)\s*(.+?)(?:\s*(?:from (?:the )?(left|right|above|below))|([+-]))?$")) {
        let f = ex(m[1].as_deref()?)?;
        let x = sym(m[2].as_deref()?);
        let a = bound(m[3].as_deref()?)?;
        let dir = match (m[4].as_deref().map(|s| s.to_lowercase()), m[5].as_deref()) {
            (Some(ref s), _) if s == "left" || s == "below" => Some(-1),
            (Some(_), _) => Some(1),
            (None, Some("-")) => Some(-1),
            (None, Some("+")) => Some(1),
            _ => None,
        };
        let mut args = vec![f, x, a];
        if let Some(d) = dir {
            args.push(int(d));
        }
        let h = raw_fn("Limit", args);
        return holder_answer(h.clone(), d_of(&h), q);
    }
    if let Some(m) = cap(re!(r"^lim(?:it)?\s*\(?\s*([a-z])\s*(?:->|→)\s*([^\s)]+)\s*\)?\s+(.+)$")) {
        let f = ex(m[3].as_deref()?)?;
        let h = raw_fn("Limit", vec![f, sym(m[1].as_deref()?), bound(m[2].as_deref()?)?]);
        return holder_answer(h.clone(), d_of(&h), q);
    }
    // ------------------------------------------------------------ sums
    if let Some(m) = cap(re!(r"^(sum|product)\s+(?:of\s+)?(.+?)\s*,?\s+(?:for\s+)?([a-z])\s*=\s*(.+?)\s+(?:to|\.\.)\s+(.+)$")) {
        let f = ex(m[2].as_deref()?)?;
        let holder = if m[1].as_deref()?.to_lowercase() == "sum" { "Sum" } else { "Product" };
        let h = raw_fn(holder, vec![f, sym(m[3].as_deref()?), bound(m[4].as_deref()?)?, bound(m[5].as_deref()?)?]);
        return holder_answer(h.clone(), d_of(&h), q);
    }
    // ---------------------------------------------------- Laplace transforms
    if let Some(m) = cap(re!(r"^(inverse )?laplace(?: transform)?\s+(?:of\s+)?(.+?)$")) {
        let inverse = m[1].is_some();
        let p = parse_e(m[2].as_deref()?)?;
        let f = p.expr.clone();
        let (from, to) = if inverse { (sym("s"), sym("t")) } else { (var_or_default(&f, if f.has_sym(&sym("t")) { Some("t") } else { None }), sym("s")) };
        let r = if inverse { calculus::transforms::inverse_laplace(&f, &from, &to)? } else { calculus::transforms::laplace(&f, &from, &to)? };
        let lhs = if inverse { raw_fn("user:ℒ^(-1)", vec![f.clone()]) } else { raw_fn("user:ℒ", vec![f.clone()]) };
        let lhs = match lhs {
            Func(n, a) => raw_fn(n.trim_start_matches("user:"), a),
            other => other,
        };
        let title = if inverse { "Inverse Laplace transform" } else { "Laplace transform" };
        let mut pods = vec![pod("Input", "Input", text(format!("{} of {} ({} → {})", title.to_lowercase(), ptext(&f), ptext(&from), ptext(&to)))), primary(pod("Result", title, math(&eq(lhs, r.clone()))))];
        if inverse && r.free_symbols().len() <= 1 {
            if let Some(pl) = pl::cartesian(std::slice::from_ref(&r), &to, Some((0.0, 10.0))) {
                pods.push(pod("Plot", "Plot", plot(pl)));
            }
        }
        if let Some(alt) = math::alternate_forms(&r, &[]).first() {
            pods.push(pod("AlternateForm", "Alternate form", math(alt)));
        }
        return Some(Answer::new(title, pods));
    }
    // ------------------------------------------------------------ series
    if let Some(m) = cap(re!(r"^(?:taylor|maclaurin|power)?\s*(?:series|expansion|series expansion)\s+(?:of\s+)?(.+?)(?:\s+(?:at|about|around)\s+([a-z])\s*=\s*(.+?))?(?:\s+(?:to|up to)\s+(?:order\s+)?(\d+))?$")) {
        let f = ex(m[1].as_deref()?)?;
        let x = m[2].as_deref().map(sym).unwrap_or_else(|| var_or_default(&f, None));
        let a = m[3].as_deref().and_then(bound).unwrap_or_else(zero);
        let n: usize = m[4].as_deref().and_then(|s| s.parse().ok()).unwrap_or(6);
        let ser = calculus::series(&f, &x, &a, n.min(30))?;
        let mut ds: Vec<D> = ser.terms.iter().map(d_of).collect();
        if let Some(o) = ser.big_o {
            ds.push(d_of(&raw_fn("O", vec![o])));
        }
        let shown = D::Sum(ds);
        let mut pods = vec![pod("Input", "Input", text(format!("series of {} at {} = {}", ptext(&f), ptext(&x), ptext(&a))))];
        pods.push(primary(pod("Series", "Series expansion", math_d(&shown))));
        if let Some(pl) = pl::cartesian(&[f.clone(), ser.poly.clone()], &x, None) {
            let mut pl = pl;
            pl.series[1].label = "series approximation".into();
            pods.push(pod("Plot", "Approximations", plot(pl)));
        }
        if a.is_zero() {
            if let Some(gen) = general_term(&f, &x) {
                pods.push(pod("GeneralTerm", "General term", math(&gen)));
            }
        }
        return Some(Answer::new(format!("series {} at {}={}", ptext(&f), ptext(&x), ptext(&a)), pods));
    }
    // ---------------------------------------------------- algebra commands
    if let Some(m) = cap(re!(r"^(factor|factorise|factorize|expand|simplify|partial fractions?(?: of)?|apart|cancel|together|collect)\s+(.+)$")) {
        let cmd = m[1].as_deref()?.to_lowercase();
        let p = parse_e(m[2].as_deref()?)?;
        let e = p.expr.clone();
        if cmd.starts_with("factor") {
            if let Some(n) = e.as_int() {
                let pods = vec![input_pod(&p.d), primary(pod("Result", "Prime factorization", text(int_factor_text(n))))];
                return Some(Answer::new(format!("factor {}", n), pods));
            }
        }
        let x = var_or_default(&e, None);
        let r = match cmd.as_str() {
            c if c.starts_with("factor") => s::factor(&e),
            "expand" => s::expand(&e),
            "simplify" => s::simplify(&e),
            "cancel" => s::cancel(&e),
            "together" => s::together(&e),
            "collect" => s::collect(&e, &x)?,
            _ => s::apart(&e, &x)?,
        };
        let title = match cmd.as_str() {
            c if c.starts_with("factor") => "Factored form",
            "expand" => "Expanded form",
            c if c.starts_with("partial") || c == "apart" => "Partial fraction expansion",
            _ => "Result",
        };
        let mut pods = vec![input_pod(&p.d), primary(pod("Result", title, math(&r)))];
        if let Some(alt) = math::alternate_forms(&e, &[r.clone()]).first() {
            pods.push(pod("AlternateForm", "Alternate form", math(alt)));
        }
        if e.free_symbols().len() == 1 {
            if let Some(pl) = pl::cartesian(std::slice::from_ref(&e), &x, None) {
                pods.push(pod("Plot", "Plot", plot(pl)));
            }
        }
        return Some(Answer::new(format!("{cmd} {}", ptext(&e)), pods));
    }
    // ---------------------------------------------------------------- solve
    if let Some(m) = cap(re!(r"^(?:solve|roots? of|zeros? of|find the roots? of)\s+(.+?)(?:\s+for\s+([a-z](?:\s*,\s*[a-z])*))?$")) {
        let body = m[1].as_deref()?;
        let p = parse_e(body)?;
        let vars: Option<Vec<Expr>> = m[2].as_deref().map(|v| v.split(',').map(|s| sym(s.trim())).collect());
        match (&p.expr, vars) {
            (List(eqs), Some(vs)) => {
                let sol = solve::solve_system(eqs, &vs);
                let rows: Vec<Vec<Content>> = sol.sols.iter().map(|sv| vec![math(&raw_fn("Conj", sv.iter().map(|(a, b)| eq(a.clone(), b.clone())).collect()))]).collect();
                let pods = vec![input_pod(&p.d), primary(pod("Solution", "Solutions", if rows.is_empty() { text("no solutions exist") } else { table(None, rows) }))];
                return Some(Answer::new(format!("solve {}", print::d_text(&p.d)), pods));
            }
            (_, Some(vs)) if vs.len() == 1 => {
                let f = solve::as_zero(&p.expr);
                let x = vs[0].clone();
                let rs = solve::solve(&f, &x);
                let mut pods = vec![input_pod(&p.d)];
                if rs.roots.is_empty() {
                    pods.push(primary(pod("Result", "Result", text("no solutions found"))));
                } else {
                    pods.push(primary(pod("Solutions", &format!("Solutions for {}", ptext(&x)), math::roots_list(&x, &rs.roots, rs.periodic, rs.numeric))));
                }
                return Some(Answer::new(format!("solve {} for {}", print::d_text(&p.d), ptext(&x)), pods));
            }
            (Rel(..), None) | (List(_), None) => return math::analyze(q, p),
            (_, None) => {
                // "roots of x^2 - 4": treat as equation = 0
                let e = p.expr.clone();
                let full = Rel(RelOp::Eq, Box::new(e.clone()), Box::new(zero()));
                let p2 = Parsed { expr: full.clone(), d: D::Rel(vec![p.d.clone(), D::Num("0".into())], vec!["=".into()]), ..p };
                return math::analyze(q, p2);
            }
            _ => return None,
        }
    }
    // ---------------------------------------------------------------- plots
    if let Some(m) = cap(re!(r"^(?:plot|graph|draw|sketch)\s+(.+?)(?:\s*,?\s+(?:from\s+)?([a-z])\s*=\s*(.+?)\s+(?:to|\.\.)\s+(.+?))?(?:\s*,?\s+(?:from\s+)?([a-z])\s*=\s*(.+?)\s+(?:to|\.\.)\s+(.+?))?$")) {
        return plot_command(q, m[1].as_deref()?, m[2].as_deref(), m[3].as_deref(), m[4].as_deref());
    }
    if let Some(m) = cap(re!(r"^(?:polar plot|plot polar)\s+r\s*=\s*(.+?)(?:\s+(?:from\s+)?(?:theta|t|θ)\s*=\s*(.+?)\s+to\s+(.+))?$")) {
        let r = ex(&m[1].as_deref()?.replace("theta", "t").replace('θ', "t"))?;
        let t = sym("t");
        let a = m[2].as_deref().and_then(bound).and_then(|e| e.to_f64_const()).unwrap_or(0.0);
        let b = m[3].as_deref().and_then(bound).and_then(|e| e.to_f64_const()).unwrap_or(2.0 * std::f64::consts::PI);
        let pl = pl::polar(&r, &t, (a, b))?;
        return Some(Answer::new(format!("polar plot r = {}", ptext(&r)), vec![pod("Input", "Input", text(format!("polar plot r(θ) = {}", ptext(&r).replace('t', "θ")))), primary(pod("Plot", "Polar plot", plot(pl)))]));
    }
    if let Some(m) = cap(re!(r"^(?:parametric plot|plot parametric)\s*\(?(.+?),\s*(.+?)\)?(?:\s+(?:from\s+)?t\s*=\s*(.+?)\s+to\s+(.+))?$")) {
        let fx = ex(m[1].as_deref()?)?;
        let fy = ex(m[2].as_deref()?)?;
        let a = m[3].as_deref().and_then(bound).and_then(|e| e.to_f64_const()).unwrap_or(0.0);
        let b = m[4].as_deref().and_then(bound).and_then(|e| e.to_f64_const()).unwrap_or(2.0 * std::f64::consts::PI);
        let pl = pl::parametric(&fx, &fy, &sym("t"), (a, b))?;
        return Some(Answer::new("parametric plot", vec![pod("Input", "Input", text(format!("parametric plot ({}, {})", ptext(&fx), ptext(&fy)))), primary(pod("Plot", "Parametric plot", plot(pl)))]));
    }
    if let Some(m) = cap(re!(r"^(mandelbrot(?: set)?|julia set(?:\s+(?:for\s+)?c\s*=\s*(.+))?)$")) {
        let julia = m[2].as_deref().and_then(ex).and_then(|e| crate::eval::const_c64(&e)).map(|z| (z.re, z.im));
        let julia = if m[1].as_deref()?.to_lowercase().starts_with("julia") { Some(julia.unwrap_or((-0.8, 0.156))) } else { None };
        let pl = pl::fractal(julia, 640, 480);
        let title = if julia.is_some() { "Julia set" } else { "Mandelbrot set" };
        let mut pods = vec![pod("Input", "Input", text(title)), primary(pod("Image", title, plot(pl)))];
        pods.push(pod(
            "Definition",
            "Definition",
            text(if julia.is_some() { "Points z₀ for which z ↦ z² + c stays bounded." } else { "Points c for which z ↦ z² + c, starting at z = 0, stays bounded." }),
        ));
        if julia.is_none() {
            pods.push(pod("Area", "Area (estimate)", text("≈ 1.50659 (Monte Carlo / pixel counting estimates)")));
        }
        return Some(Answer::new(title, pods));
    }
    // -------------------------------------------------------- extrema
    if let Some(m) = cap(re!(r"^(minimize|maximize|minimum of|maximum of|min|max|extrema of|local extrema of|critical points of)\s+(.+?)$")) {
        let p = parse_e(m[2].as_deref()?)?;
        let ans = math::analyze(q, p)?;
        let wanted: Vec<Pod> = ans.pods.iter().filter(|p| matches!(p.id.as_str(), "Input" | "GlobalMaximum" | "GlobalMinimum" | "LocalExtrema" | "CriticalPoints" | "Plot" | "3DPlot")).cloned().collect();
        if wanted.len() > 1 {
            let mut wanted = wanted;
            if let Some(p) = wanted.iter_mut().find(|p| p.id != "Input" && !p.id.contains("Plot")) {
                p.primary = true;
            }
            return Some(Answer::new(ans.interpretation, wanted));
        }
        return Some(ans);
    }
    // ----------------------------------------------------- tangent line
    if let Some(m) = cap(re!(r"^tangent(?: line)?\s+(?:to|of)\s+(.+?)\s+at\s+([a-z])\s*=\s*(.+)$")) {
        let f = ex(m[1].as_deref()?)?;
        let x = sym(m[2].as_deref()?);
        let a = ex(m[3].as_deref()?)?;
        let fa = s::simplify(&f.subs(&x, &a));
        let slope = s::simplify(&calculus::diff(&f, &x).subs(&x, &a));
        let line = s::simplify(&add(vec![fa, mul(vec![slope.clone(), sub(x.clone(), a.clone())])]));
        let mut pods = vec![pod("Input", "Input", text(format!("tangent line to {} at {} = {}", ptext(&f), ptext(&x), ptext(&a))))];
        pods.push(primary(pod("Result", "Tangent line", math(&eq(sym("y"), line.clone())))));
        pods.push(pod("Slope", "Slope", math(&slope)));
        if let Some(pl) = pl::cartesian(&[f.clone(), line], &x, None) {
            pods.push(pod("Plot", "Plot", plot(pl)));
        }
        return Some(Answer::new("tangent line", pods));
    }
    // ---------------------------------------------------------- inverse fn
    if let Some(m) = cap(re!(r"^inverse (?:function )?(?:of\s+)?(.+)$")) {
        let p = parse_e(m[1].as_deref()?)?;
        if p.expr.is_matrix() {
            let mm = crate::linalg::from_expr(&p.expr)?;
            let inv = crate::linalg::inverse(&mm)?;
            return Some(Answer::new("matrix inverse", vec![input_pod(&p.d), primary(pod("Result", "Inverse", math(&crate::linalg::to_expr(&inv))))]));
        }
        let f = p.expr.clone();
        let x = var_or_default(&f, None);
        let y = sym("y");
        let rs = solve::solve(&sub(f.subs(&x, &y), x.clone()), &y);
        let real = rs.real();
        if real.is_empty() {
            return None;
        }
        let rows: Vec<Vec<Content>> = real.iter().map(|r| vec![math(&eq(raw_fn("user:f^(-1)", vec![x.clone()]), r.clone()))]).collect();
        let mut pods = vec![input_pod(&p.d), primary(pod("Result", "Inverse function", table(None, rows)))];
        if let Some(pl) = pl::cartesian(&[f.clone(), real[0].clone(), x.clone()], &x, Some((-5.0, 5.0))) {
            pods.push(pod("Plot", "Plot", plot(pl)));
        }
        return Some(Answer::new("inverse function", pods));
    }
    // ------------------------------------------------------- number theory
    if let Some(m) = cap(re!(r"^is\s+(.+?)\s+(?:a\s+)?(prime|even|odd|a perfect square|perfect square|square)(?: number)?$")) {
        let n = ex(m[1].as_deref()?)?;
        let n = s::simplify(&n);
        let n = n.as_int()?.clone();
        let what = m[2].as_deref()?.to_lowercase();
        let ans = match what.as_str() {
            "prime" => nt::is_prime(&n),
            "even" => (&n % 2u32) == 0.into(),
            "odd" => (&n % 2u32) != 0.into(),
            _ => nt::is_square(&n),
        };
        let mut pods = vec![pod("Input", "Input", text(format!("is {n} {what}?"))), primary(pod("Result", "Result", text(if ans { format!("{n} is {what}") } else { format!("{n} is not {what}") })))];
        if what == "prime" && !ans && n > 1.into() {
            pods.push(pod("Factorization", "Prime factorization", text(int_factor_text(&n))));
            pods.push(pod("NextPrime", "Next prime", text(nt::next_prime(&n).to_string())));
        }
        return Some(Answer::new(format!("is {n} {what}"), pods));
    }
    if let Some(m) = cap(re!(r"^(gcd|lcm|greatest common divisor|least common multiple)\s*(?:of\s+)?\(?(.+?)\)?$")) {
        let args: Vec<Expr> = split_args(m[2].as_deref()?).iter().filter_map(|a| ex(a)).collect();
        if args.len() < 2 {
            return None;
        }
        let cmd = m[1].as_deref()?.to_lowercase();
        let name = if cmd.starts_with('g') { "gcd" } else { "lcm" };
        let r = if args.iter().all(|a| a.is_integer()) { func(name, args.clone()) } else if name == "gcd" { args[1..].iter().fold(args[0].clone(), |acc, b| s::poly_gcd(&acc, b)) } else { return None };
        let interp = format!("{name}({})", args.iter().map(ptext).collect::<Vec<_>>().join(", "));
        return Some(Answer::new(interp.clone(), vec![pod("Input", "Input", text(interp)), primary(pod("Result", "Result", math(&r)))]));
    }
    if let Some(m) = cap(re!(r"^(\d+)(?:st|nd|rd|th)?\s+prime(?: number)?$|^prime\s*\(\s*(\d+)\s*\)$")) {
        let k: u64 = m[1].as_deref().or(m[2].as_deref())?.parse().ok()?;
        let pnum = nt::nth_prime(k)?;
        return Some(Answer::new(format!("prime({k})"), vec![pod("Input", "Input", text(format!("the {k}th prime number"))), primary(pod("Result", "Result", text(pnum.to_string())))]));
    }
    if let Some(m) = cap(re!(r"^(?:number of primes|primes) (?:less than|below|up to|under)\s+(\d+)$")) {
        let k: u64 = m[1].as_deref()?.parse().ok()?;
        let cnt = nt::prime_pi(k)?;
        let mut pods = vec![pod("Input", "Input", text(format!("π({k})"))), primary(pod("Result", "Number of primes", text(cnt.to_string())))];
        if k <= 1000 {
            pods.push(pod("Primes", "Primes", text(nt::sieve(k as usize).iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", "))));
        }
        return Some(Answer::new("prime counting", pods));
    }
    if let Some(m) = cap(re!(r"^(.+?)\s+(?:in|to)\s+(binary|octal|hex|hexadecimal|base\s*(\d+)|roman(?: numerals)?)$")) {
        let target = m[2].as_deref()?.to_lowercase();
        let n = s::simplify(&ex(m[1].as_deref()?)?);
        let n = n.as_int()?.clone();
        let (label, out) = if target.starts_with("roman") {
            ("Roman numerals", nt::to_roman(num_traits::ToPrimitive::to_u64(&n)?)?)
        } else {
            let b: u32 = match target.as_str() {
                "binary" => 2,
                "octal" => 8,
                "hex" | "hexadecimal" => 16,
                _ => m[3].as_deref()?.parse().ok()?,
            };
            if !(2..=36).contains(&b) {
                return None;
            }
            ("Result", format!("{}_{}", nt::to_base(&n, b).to_uppercase(), b))
        };
        return Some(Answer::new(format!("{n} in {target}"), vec![pod("Input", "Input", text(format!("convert {n} to {target}"))), primary(pod("Result", label, text(out)))]));
    }
    if let Some(m) = cap(re!(r"^(?:roman numeral\s+)?([mdclxvi]+)(?:\s+in (?:arabic|decimal|numbers?))?$")) {
        let r = m[1].as_deref()?;
        if r.len() >= 2 && r.chars().all(|c| "MDCLXVI".contains(c)) {
            let v = nt::from_roman(r)?;
            return Some(Answer::new(format!("roman numeral {r}"), vec![pod("Input", "Input", text(format!("{r} (Roman numerals)"))), primary(pod("Result", "Result", text(v.to_string())))]));
        }
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(\d+)(?:st|nd|rd|th)?\s+fibonacci(?: number)?$|^fibonacci\s*\(?\s*(\d+)\s*\)?$")) {
        let k: u64 = m[1].as_deref().or(m[2].as_deref())?.parse().ok()?;
        if k > 100_000 {
            return None;
        }
        let f = nt::fib(k);
        return Some(Answer::new(format!("Fibonacci({k})"), vec![pod("Input", "Input", text(format!("F({k})"))), primary(pod("Result", "Result", text(f.to_string())))]));
    }
    if let Some(m) = cap(re!(r"^continued fraction (?:of|for)\s+(.+)$")) {
        let e = s::simplify(&ex(m[1].as_deref()?)?);
        let cf = if let Some(r) = e.as_rat() { nt::continued_fraction(r.numer(), r.denom()).iter().map(|c| c.to_string()).collect::<Vec<_>>() } else { nt::continued_fraction_f64(e.to_f64_const()?, 20).iter().map(|c| c.to_string()).collect() };
        let body = format!("[{}; {}]", cf[0], cf[1..].join(", "));
        return Some(Answer::new("continued fraction", vec![pod("Input", "Input", text(format!("continued fraction of {}", ptext(&e)))), primary(pod("Result", "Continued fraction", text(body)))]));
    }
    // ------------------------------------------------------------- stats
    if let Some(m) = cap(re!(r"^(mean|average|median|mode|standard deviation|stdev|std|variance|range|sum|max|min)\s+(?:of\s+)?\{?(.+?)\}?$")) {
        let vals: Vec<f64> = split_args(m[2].as_deref()?).iter().filter_map(|a| ex(a)).filter_map(|e| e.to_f64_const()).collect();
        if vals.len() < 2 {
            return None;
        }
        let st = crate::stats::summarize(&vals)?;
        let what = m[1].as_deref()?.to_lowercase();
        let v = match what.as_str() {
            "mean" | "average" => st.mean,
            "median" => st.median,
            "mode" => *st.modes.first()?,
            "variance" => st.var_sample,
            "range" => st.max - st.min,
            "sum" => st.sum,
            "max" => st.max,
            "min" => st.min,
            _ => st.var_sample.sqrt(),
        };
        let list = List(vals.iter().map(|v| crate::calculus::limit::tidy_float(*v)).collect());
        let p = Parsed { ast: parse::Ast::Num(Rat::from_integer(0.into()), String::new()), d: d_of(&list), expr: list.clone(), has_decimal: false, notes: vec![] };
        let mut pods = vec![pod("Input", "Input", text(format!("{what} of {}", ptext(&list)))), primary(pod("Result", "Result", text(crate::stats::fmt(v))))];
        if let Some(more) = crate::stats::list_pods(&p, list.as_list()?) {
            pods.extend(more.into_iter().skip(1).map(|mut p| {
                p.primary = false;
                p
            }));
        }
        return Some(Answer::new(format!("{what} of data"), pods));
    }
    if let Some(m) = cap(re!(r"^(linear|quadratic|exponential)?\s*(?:fit|regression|least squares)\s+(?:of\s+|for\s+)?(.+)$")) {
        let e = ex(m[2].as_deref()?)?;
        let pts: Vec<[f64; 2]> = e.as_list()?.iter().filter_map(|p| {
            let v = p.as_list()?;
            Some([v.first()?.to_f64_const()?, v.get(1)?.to_f64_const()?])
        }).collect();
        if pts.len() < 2 {
            return None;
        }
        let kind = m[1].as_deref().unwrap_or("linear").to_lowercase();
        let mut pods = vec![pod("Input", "Input", text(format!("{kind} fit of {}", ptext(&e))))];
        pods.extend(crate::stats::fit_pods(&pts, &kind)?);
        return Some(Answer::new(format!("{kind} fit"), pods));
    }
    None
}

fn split_args(s: &str) -> Vec<String> {
    // split on commas at depth 0 (or on whitespace when there are no commas)
    let mut out = Vec::new();
    let mut depth = 0;
    let mut cur = String::new();
    let has_comma = s.contains(',');
    for ch in s.chars() {
        match ch {
            '(' | '[' | '{' => {
                depth += 1;
                cur.push(ch)
            }
            ')' | ']' | '}' => {
                depth -= 1;
                cur.push(ch)
            }
            ',' if depth == 0 => out.push(std::mem::take(&mut cur)),
            ' ' if depth == 0 && !has_comma => {
                if !cur.trim().is_empty() {
                    out.push(std::mem::take(&mut cur))
                }
            }
            _ => cur.push(ch),
        }
    }
    if !cur.trim().is_empty() {
        out.push(cur);
    }
    out.into_iter().map(|s| s.trim().to_string()).filter(|s| !s.is_empty() && s != "and").collect()
}

fn int_factor_text(n: &Int) -> String {
    let fs = nt::factor(&num_traits::Signed::abs(n));
    if fs.is_empty() {
        return n.to_string();
    }
    let mut parts: Vec<String> = fs.iter().map(|(p, k)| if *k == 1 { p.to_string() } else { format!("{p}^{k}") }).collect();
    if num_traits::Signed::is_negative(n) {
        parts.insert(0, "-1".into());
    }
    parts.join(" × ")
}

/// The general term of a few well-known Maclaurin series.
fn general_term(f: &Expr, x: &Expr) -> Option<Expr> {
    let n = sym("n");
    let fact = |e: Expr| func("factorial", vec![e]);
    let sign = pow(int(-1), n.clone());
    let t = match f {
        Pow(b, e) if matches!(**b, Const(K::E)) && **e == *x => div(pow(x.clone(), n.clone()), fact(n.clone())),
        Func(name, a) if a.len() == 1 && a[0] == *x => match &**name {
            "sin" => {
                let k = add(vec![mul(vec![int(2), n.clone()]), one()]);
                div(mul(vec![sign, pow(x.clone(), k.clone())]), fact(k))
            }
            "cos" => {
                let k = mul(vec![int(2), n.clone()]);
                div(mul(vec![sign, pow(x.clone(), k.clone())]), fact(k))
            }
            "sinh" => {
                let k = add(vec![mul(vec![int(2), n.clone()]), one()]);
                div(pow(x.clone(), k.clone()), fact(k))
            }
            "cosh" => {
                let k = mul(vec![int(2), n.clone()]);
                div(pow(x.clone(), k.clone()), fact(k))
            }
            "atan" => {
                let k = add(vec![mul(vec![int(2), n.clone()]), one()]);
                div(mul(vec![sign, pow(x.clone(), k.clone())]), k)
            }
            _ => return None,
        },
        _ => return None,
    };
    Some(raw_fn("Sum", vec![t, n, zero(), inf()]))
}

fn plot_command(q: &Query, body: &str, v1: Option<&str>, a1: Option<&str>, b1: Option<&str>) -> Option<Answer> {
    // several functions separated by "and" or commas
    let parts: Vec<String> = if body.contains(" and ") { body.split(" and ").map(|s| s.to_string()).collect() } else { split_args(body).into_iter().filter(|p| !p.is_empty()).collect() };
    let parts: Vec<String> = if parts.len() > 1 && parts.iter().all(|p| parse_e(p).is_some()) { parts } else { vec![body.to_string()] };
    let exprs: Vec<Expr> = parts.iter().filter_map(|p| ex(p)).collect();
    if exprs.is_empty() {
        return None;
    }
    let range = match (a1.and_then(bound), b1.and_then(bound)) {
        (Some(a), Some(b)) => Some((a.to_f64_const()?, b.to_f64_const()?)),
        _ => None,
    };
    let all = List(exprs.clone());
    let mut syms: Vec<Name> = all.free_symbols().into_iter().collect();
    syms.sort_by_key(|n| match &**n {
        "x" => 0,
        "y" => 1,
        _ => 5,
    });
    let first = &exprs[0];
    let interp = format!("plot {}", exprs.iter().map(ptext).collect::<Vec<_>>().join(", "));
    let input = pod("Input", "Input", text(interp.clone()));
    // equations → implicit curves
    if let Rel(RelOp::Eq, l, r) = first {
        if syms.len() == 2 {
            let f = sub((**l).clone(), (**r).clone());
            let rng = range.unwrap_or((-6.0, 6.0));
            let pl = pl::implicit(&f, &sym(&syms[0]), &sym(&syms[1]), rng, rng)?;
            return Some(Answer::new(interp, vec![input, primary(pod("Plot", "Implicit plot", plot(pl)))]));
        }
        if syms.len() == 1 && (**l == sym("y") || **r == sym("y")) {
            // y = f(x)
        }
    }
    let funcs: Vec<Expr> = exprs
        .iter()
        .map(|e| match e {
            Rel(RelOp::Eq, l, r) if **l == sym("y") => (**r).clone(),
            Rel(RelOp::Eq, l, r) if **r == sym("y") => (**l).clone(),
            _ => e.clone(),
        })
        .collect();
    let fsyms: Vec<Name> = List(funcs.clone()).free_symbols().into_iter().collect();
    match fsyms.len() {
        0 | 1 => {
            let x = v1.map(sym).unwrap_or_else(|| fsyms.first().map(|n| sym(n)).unwrap_or_else(|| sym("x")));
            let pl = pl::cartesian(&funcs, &x, range)?;
            let _ = q;
            Some(Answer::new(interp, vec![input, primary(pod("Plot", "Plot", plot(pl)))]))
        }
        2 => {
            let (x, y) = (sym(&fsyms.iter().find(|n| &***n == "x").cloned().unwrap_or(fsyms[0].clone())), sym(&fsyms.iter().find(|n| &***n == "y").cloned().unwrap_or(fsyms[1].clone())));
            let rng = range.unwrap_or((-3.0, 3.0));
            let s3 = pl::surface(&funcs[0], &x, &y, rng, rng, 72, pl::PlotKind::Surface)?;
            let c = pl::surface(&funcs[0], &x, &y, rng, rng, 160, pl::PlotKind::Contour)?;
            Some(Answer::new(interp, vec![input, primary(pod("3DPlot", "3D plot", plot(s3))), pod("ContourPlot", "Contour plot", plot(c))]))
        }
        _ => None,
    }
}
