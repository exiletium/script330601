//! General mathematics: understands a parsed expression, equation,
//! inequality, system, matrix or list and produces result pods.

use super::*;
use crate::calculus::{self, Dir};
use crate::expr::Expr::*;
use crate::expr::*;
use crate::numtheory as nt;
use crate::parse::{self, Parsed};
use crate::plot as pl;
use crate::print::{text as ptext, D};
use crate::simplify as s;
use crate::solve;
use num_traits::{One, Signed, ToPrimitive, Zero};

pub fn handle(q: &Query) -> Option<Answer> {
    // a bare function name means the function itself: "sin" -> sin(x)
    const BARE: &[&str] = &["sin", "cos", "tan", "sec", "csc", "cot", "sinh", "cosh", "tanh", "arcsin", "arccos", "arctan", "asin", "acos", "atan", "ln", "log", "exp", "sqrt", "gamma", "erf", "abs", "floor", "ceil", "zeta", "sinc", "W"];
    let input = if BARE.contains(&q.clean.trim()) { format!("{}(x)", q.clean.trim()) } else { q.clean.clone() };
    let parsed = parse::parse(&input).ok()?;
    analyze(q, parsed)
}

/// Input pod echoing the parsed input.
pub fn input_pod(d: &D) -> Pod {
    pod("Input", "Input", math_d(d))
}

fn sorted_symbols(e: &Expr) -> Vec<Expr> {
    let mut v: Vec<Name> = e.free_symbols().into_iter().filter(|n| &**n != solve::PARAM || true).collect();
    // conventional variable order: x, y, z, t first
    v.sort_by_key(|n| match &**n {
        "x" => 0,
        "y" => 1,
        "z" => 2,
        "t" => 3,
        _ => 10,
    });
    v.into_iter().map(|n| sym(&n)).collect()
}

/// Main dispatch on the kind of input.
pub fn analyze(q: &Query, p: Parsed) -> Option<Answer> {
    let e = p.expr.clone();
    let interp = print::d_text(&p.d);
    let has_holder = e.has(&|n| matches!(n, Func(name, _) if matches!(&**name, "Integral" | "Derivative" | "Limit" | "Sum" | "Product")));
    if has_holder {
        return Some(Answer::new(interp, calculus_pods(&p, q.opts)));
    }
    let pods = match &e {
        Rel(RelOp::Eq, a, b) => equation_pods(&p, a, b, q.opts),
        Rel(op, a, b) => inequality_pods(&p, *op, a, b),
        List(v) if !v.is_empty() && v.iter().all(|x| matches!(x, Rel(RelOp::Eq, ..))) => system_pods(&p, v),
        List(_) if e.is_matrix() => crate::linalg::matrix_pods(&p)?,
        List(v) if !v.is_empty() && v.iter().all(|x| x.free_symbols().is_empty() && !x.is_list()) => crate::stats::list_pods(&p, v)?,
        _ if is_logic(&e) => logic_pods(&p),
        _ => {
            let vars = sorted_symbols(&e);
            match vars.len() {
                0 => number_pods(&p, q.opts),
                1 => function_pods(&p, &vars[0], q.opts),
                2 => two_var_pods(&p, &vars[0], &vars[1]),
                _ => multi_var_pods(&p),
            }
        }
    };
    if pods.len() <= 1 {
        return None;
    }
    Some(Answer::new(interp, pods))
}

// ---------------------------------------------------------------------------
// numbers
// ---------------------------------------------------------------------------

fn is_complex(e: &Expr) -> bool {
    e.has_const(K::I)
}

pub fn number_pods(p: &Parsed, opts: &Options) -> Vec<Pod> {
    let mut pods = vec![input_pod(&p.d)];
    let e = p.expr.clone();
    let v = s::simplify(&e);
    if v.is_undef() || matches!(v, Const(K::CInf)) {
        pods.push(primary(pod("Result", "Result", text(if matches!(v, Const(K::CInf)) { "complex infinity" } else { "(undefined)" }))));
        return pods;
    }
    if let Const(K::True | K::False) = v {
        pods.push(primary(pod("Result", "Result", math(&v))));
        return pods;
    }
    let digits = opts.digits.max(6);
    let v = if is_complex(&v) || v.has(&|n| matches!(n, Pow(b, k) if matches!(**b, Add(_)) && k.is_integer())) {
        let ev = s::simplify(&s::expand(&v));
        if ev.size() <= v.size() + 2 { ev } else { v }
    } else {
        v
    };
    let approx = s::evalf(&v);
    // the main result
    if p.has_decimal || v.has_float() {
        pods.push(primary(pod("Result", "Result", math_digits(&approx, digits))));
    } else {
        pods.push(primary(pod("Result", "Result", math(&v))));
        let gaussian_int = is_complex(&v) && crate::eval::const_c64(&v).map_or(false, |z| z.re.fract() == 0.0 && z.im.fract() == 0.0);
        if !v.is_integer() && !gaussian_int && ptext(&approx) != ptext(&v) {
            let more = crate::bigfloat::decimal(&v, 50).map(|s| text(format!("{s}…"))).unwrap_or_else(|| math_digits(&approx, 15));
            pods.push(pod("DecimalApproximation", "Decimal approximation", more));
        }
    }
    let ve = v.clone();
    let mut jobs: Vec<PodJob> = Vec::new();
    if let Some(n) = v.as_int().cloned() {
        jobs.push(job(move || Some(integer_pods(&n))));
    } else if let Some(r) = v.as_rat().cloned() {
        if !p.has_decimal {
            jobs.push(job(move || Some(rational_pods(&r))));
        }
    } else if is_complex(&v) {
        jobs.push(job(move || complex_pods(&ve)));
    } else {
        let e2 = v.clone();
        jobs.push(job1(move || alternate_forms_pod(&e2, &[])));
        let e3 = v.clone();
        jobs.push(job1(move || {
            let x = e3.to_f64_const()?;
            let cf = nt::continued_fraction_f64(x, 12);
            let body = format!("[{}; {}{}]", cf[0], cf[1..].iter().map(|c| c.to_string()).collect::<Vec<_>>().join(", "), if cf.len() >= 12 { ", …" } else { "" });
            Some(pod("ContinuedFraction", "Continued fraction", text(body)))
        }));
        let e4 = v.clone();
        jobs.push(job1(move || {
            let x = e4.to_f64_const()?;
            if !x.is_finite() {
                return None;
            }
            let span = x.abs().max(1.0) * 1.5;
            let pl = pl::number_line(vec![pl::Span { lo: x, hi: x, lo_closed: true, hi_closed: true }]);
            let mut pl = pl;
            pl.x_range = (x - span, x + span);
            Some(pod("NumberLine", "Number line", plot(pl)))
        }));
    }
    pods.extend(run_jobs(jobs));
    pods
}


pub fn integer_pods(n: &Int) -> Vec<Pod> {
    let mut pods = Vec::new();
    let abs = n.abs();
    let digits = abs.to_string().len();
    if let Some(name) = nt::number_name(n) {
        pods.push(pod("NumberName", "Number name", text(name)));
    }
    if digits > 20 {
        let st = abs.to_string();
        let sci = format!("{}{}.{}… × 10^{}", if n.is_negative() { "-" } else { "" }, &st[..1], &st[1..10], digits - 1);
        pods.push(pod("ScientificNotation", "Scientific notation", text(sci)));
        pods.push(pod("NumberLength", "Number length", text(format!("{digits} decimal digits"))));
    }
    if abs > Int::one() && digits <= 60 {
        let (fs, complete) = nt::factor_full(&abs);
        let prime = fs.len() == 1 && fs[0].1 == 1;
        if prime {
            pods.push(pod("PrimeFactorization", "Prime factorization", text(format!("{abs} is a prime number"))));
        } else {
            let mut parts: Vec<String> = fs.iter().map(|(p, k)| if *k == 1 { p.to_string() } else { format!("{p}^{k}") }).collect();
            if n.is_negative() {
                parts.insert(0, "-1".into());
            }
            let mut t = parts.join(" × ");
            if !complete {
                t += " (last factor not proven prime)";
            }
            let count: u32 = fs.iter().map(|f| f.1).sum();
            pods.push(pod_multi("PrimeFactorization", "Prime factorization", vec![(String::new(), text(t)), (String::new(), text(format!("({count} prime factors, {} distinct)", fs.len())))]));
        }
    }
    // properties
    let mut props: Vec<String> = Vec::new();
    let two = Int::from(2);
    let n_disp = if digits > 20 { format!("{}…", &n.to_string()[..12]) } else { n.to_string() };
    let n = &n.clone();
    let _ = &n_disp;
    props.push(if (n % &two).is_zero() { format!("{n_disp} is an even number.") } else { format!("{n_disp} is an odd number.") });
    if abs > Int::one() && digits <= 60 && nt::is_prime(&abs) {
        props.push(format!("{n_disp} is a prime number."));
    }
    if !n.is_negative() && nt::is_square(n) && n > &Int::one() {
        props.push(format!("{n_disp} is a perfect square ({}²).", if digits > 20 { "…".to_string() } else { n.sqrt().to_string() }));
    }
    if n > &Int::one() && digits <= 30 {
        let sig = nt::sigma(1, n);
        if sig == n * Int::from(2) {
            props.push(format!("{n_disp} is a perfect number."));
        }
        let tri = (Int::from(8) * n + Int::one()).sqrt();
        if &tri * &tri == Int::from(8) * n + Int::one() {
            props.push(format!("{n_disp} is a triangular number."));
        }
        // Fibonacci test: 5n²±4 square
        let f = Int::from(5) * n * n;
        if nt::is_square(&(&f + Int::from(4))) || nt::is_square(&(&f - Int::from(4))) {
            props.push(format!("{n_disp} is a Fibonacci number."));
        }
        let s = abs.to_string();
        if s.len() > 1 && s.chars().rev().collect::<String>() == s {
            props.push(format!("{n_disp} is a palindrome."));
        }
    }
    pods.push(pod("Properties", "Properties", table(None, props.into_iter().map(|p| vec![text(p)]).collect())));
    if !n.is_negative() && digits <= 40 {
        pods.push(pod(
            "Bases",
            "Number base conversions",
            table(
                None,
                vec![
                    vec![text("binary"), text(nt::to_base(n, 2))],
                    vec![text("octal"), text(nt::to_base(n, 8))],
                    vec![text("hexadecimal"), text(nt::to_base(n, 16).to_uppercase())],
                ],
            ),
        ));
    }
    if let Some(k) = n.to_u64() {
        if k > 0 && k < 4000 {
            if let Some(r) = nt::to_roman(k) {
                pods.push(pod("RomanNumerals", "Roman numerals", text(r)));
            }
        }
        if k > 1 && k <= 1_000_000_000_000 {
            let ds = nt::divisors(n);
            if ds.len() <= 64 {
                pods.push(pod("Divisors", "Divisors", text(ds.iter().map(|d| d.to_string()).collect::<Vec<_>>().join(", "))));
            } else {
                pods.push(pod("Divisors", "Divisors", text(format!("{} divisors", ds.len()))));
            }
            pods.push(pod("Totient", "Euler's totient", text(nt::totient(n).to_string())));
        }
    }
    pods
}

pub fn rational_pods(r: &Rat) -> Vec<Pod> {
    let mut pods = Vec::new();
    let (p, q) = (r.numer().clone(), r.denom().clone());
    if p.abs() > q {
        let w = &p / &q;
        let rem = (&p % &q).abs();
        pods.push(pod("MixedFraction", "Mixed fraction", text(format!("{w} {rem}/{q}"))));
    }
    if let Some((int, pre, rep)) = nt::repeating_decimal(&p, &q, 200) {
        let t = if rep.is_empty() { format!("{int}.{pre}") } else { format!("{int}.{pre}({rep}) (period {})", rep.len()) };
        pods.push(pod("RepeatingDecimal", "Repeating decimal", text(t)));
    }
    let cf = nt::continued_fraction(&p, &q);
    if cf.len() > 1 {
        pods.push(pod("ContinuedFraction", "Continued fraction", text(format!("[{}; {}]", cf[0], cf[1..].iter().map(|c| c.to_string()).collect::<Vec<_>>().join(", ")))));
    }
    if p.is_positive() && p < q {
        let eg = nt::egyptian(&p, &q);
        if !eg.is_empty() && eg.len() <= 8 {
            pods.push(pod("EgyptianFraction", "Egyptian fraction expansion", text(eg.iter().map(|d| format!("1/{d}")).collect::<Vec<_>>().join(" + "))));
        }
    }
    pods.push(pod("Percent", "Percentage", text(format!("{}%", crate::print::fmt_float(rat_to_f64(r) * 100.0, 8)))));
    pods
}

fn complex_pods(v: &Expr) -> Option<Vec<Pod>> {
    let (re, im) = crate::funcs::re_im(v)?;
    let (re, im) = (s::simplify(&re), s::simplify(&im));
    let r = s::simplify(&sqrt(add(vec![square(re.clone()), square(im.clone())])));
    let theta = s::simplify(&f1("arg", v.clone()));
    let mut pods = vec![pod("PolarCoordinates", "Polar coordinates", table(None, vec![vec![text("r (modulus)"), math(&r)], vec![text("θ (argument)"), math(&theta)]]))];
    let e_part = Pow(Box::new(e()), Box::new(Mul(vec![i(), theta.clone()])));
    let polar = if r.is_one() { e_part } else { Mul(vec![r.clone(), e_part]) };
    pods.push(pod("PolarForm", "Polar form", math(&polar)));
    pods.push(pod("Parts", "Real and imaginary parts", table(None, vec![vec![text("real part"), math(&re)], vec![text("imaginary part"), math(&im)]])));
    if let (Some(x), Some(y)) = (re.to_f64_const(), im.to_f64_const()) {
        let mut p = pl::scatter(vec![[x, y]], &ptext(v), None);
        let m = x.abs().max(y.abs()).max(1.0) * 1.4;
        p.x_range = (-m, m);
        p.y_range = (-m, m);
        p.x_label = "Re".into();
        p.y_label = "Im".into();
        p.equal_aspect = true;
        pods.push(pod("ComplexPlane", "Position in the complex plane", plot(p)));
    }
    Some(pods)
}

// ---------------------------------------------------------------------------
// alternate forms
// ---------------------------------------------------------------------------

/// Distinct, reasonably small rewrites of `e`, excluding those in `seen`.
pub fn alternate_forms(e: &Expr, seen: &[Expr]) -> Vec<Expr> {
    let vars = sorted_symbols(e);
    let mut cands: Vec<Expr> = Vec::new();
    let mut add_c = |c: Expr| cands.push(c);
    add_c(s::simplify(e));
    add_c(s::factor(e));
    add_c(s::expand(e));
    add_c(s::together(e));
    if let Some(x) = vars.first() {
        if let Some(a) = s::apart(e, x) {
            add_c(a);
        }
    }
    if e.has_any_fn(&["sin", "cos", "tan"]) {
        add_c(s::trig_expand(e));
        add_c(s::trig_reduce(e));
        add_c(s::rewrite_exp_trig(e));
    }
    if e.has_any_fn(&["sinh", "cosh", "tanh"]) {
        add_c(s::rewrite_exp(e));
    }
    if e.has_fn("ln") {
        add_c(s::expand_log(e));
        add_c(s::combine_log(e));
    }
    if e.has(&|n| matches!(n, Pow(_, x) if !x.is_integer())) {
        add_c(s::power_simplify(e));
        add_c(s::denest(e));
    }
    let mut out: Vec<Expr> = Vec::new();
    let mut texts: Vec<String> = seen.iter().map(ptext).collect();
    texts.push(ptext(e));
    for c in cands {
        if c.is_undef() || c.size() > 3 * e.size() + 30 {
            continue;
        }
        if !e.has_const(K::I) && c.has_const(K::I) {
            continue;
        }
        let t = ptext(&c);
        if texts.contains(&t) {
            continue;
        }
        texts.push(t);
        out.push(c);
    }
    out.sort_by_key(|c| c.size());
    out.truncate(4);
    out
}

fn alternate_forms_pod(e: &Expr, seen: &[Expr]) -> Option<Pod> {
    let forms = alternate_forms(e, seen);
    if forms.is_empty() {
        return None;
    }
    let title = if forms.len() == 1 { "Alternate form" } else { "Alternate forms" };
    Some(pod_multi("AlternateForm", title, forms.iter().map(|f| (String::new(), math(f))).collect()))
}

// ---------------------------------------------------------------------------
// functions of one variable
// ---------------------------------------------------------------------------

fn is_polynomial(e: &Expr, x: &Expr) -> bool {
    crate::poly::coeffs(e, x).is_some()
}

pub fn function_pods(p: &Parsed, x: &Expr, opts: &Options) -> Vec<Pod> {
    let e = p.expr.clone();
    let mut pods = vec![input_pod(&p.d)];
    let simp = s::simplify(&e);
    let show_result = ptext(&simp) != print::d_text(&p.d) && simp.size() < e.size();
    if show_result {
        pods.push(primary(pod("Result", "Result", math(&simp))));
    }
    let f = if simp.size() <= e.size() { simp.clone() } else { e.clone() };
    let mut jobs: Vec<PodJob> = Vec::new();
    {
        let (f, x) = (f.clone(), x.clone());
        jobs.push(job1(move || {
            let pl = pl::cartesian(std::slice::from_ref(&f), &x, None)?;
            Some(pod("Plot", "Plot", plot(pl)))
        }));
    }
    {
        let (e, simp) = (e.clone(), simp.clone());
        jobs.push(job1(move || alternate_forms_pod(&e, &[simp])));
    }
    {
        let (f, x) = (f.clone(), x.clone());
        jobs.push(job(move || roots_pods(&f, x.clone(), "Root", "Roots")));
    }
    if is_polynomial(&f, x) {
        let (f, x) = (f.clone(), x.clone());
        jobs.push(job1(move || {
            let cs = crate::poly::coeffs(&f, &x)?;
            let deg = cs.len() - 1;
            if !(2..=6).contains(&deg) {
                return None;
            }
            let disc = discriminant(&cs)?;
            Some(pod("Discriminant", "Polynomial discriminant", math(&eq(sym("Δ"), disc))))
        }));
    }
    {
        let (f, x) = (f.clone(), x.clone());
        jobs.push(job1(move || properties_pod(&f, &x)));
    }
    {
        let (f, x, want_steps) = (f.clone(), x.clone(), opts.steps);
        jobs.push(job(move || {
            let (d, st) = if want_steps { crate::steps::record(|| calculus::diff::diff_explained(&f, &x)) } else { (s::simplify(&calculus::diff(&f, &x)), vec![]) };
            let lhs = raw_fn("Derivative", vec![f.clone(), x.clone(), one()]);
            let mut out = vec![pod("Derivative", "Derivative", math(&eq(lhs, d)))];
            if !st.is_empty() {
                out.push(pod("DerivativeSteps", "Derivative · step by step", steps_content(&st)));
            }
            Some(out)
        }));
    }
    {
        let (f, x) = (f.clone(), x.clone());
        jobs.push(job1(move || {
            let big = calculus::integrate::integrate(&f, &x)?;
            let lhs = raw_fn("Integral", vec![f.clone(), x.clone()]);
            Some(pod("IndefiniteIntegral", "Indefinite integral", math(&eq(lhs, add(vec![big, sym("constant")])))))
        }));
    }
    {
        let (f, x) = (f.clone(), x.clone());
        jobs.push(job(move || extrema_pods(&f, &x)));
    }
    {
        let (f, x) = (f.clone(), x.clone());
        jobs.push(job1(move || limits_pod(&f, &x)));
    }
    {
        let (f, x) = (f.clone(), x.clone());
        jobs.push(job1(move || series_pod(&f, &x)));
    }
    pods.extend(run_jobs(jobs));
    pods
}

pub fn discriminant(cs: &[Expr]) -> Option<Expr> {
    // Δ = (-1)^(n(n-1)/2) / a_n · Res(p, p')
    let n = cs.len() - 1;
    let x = sym("\u{3be}d");
    let p = add(cs.iter().enumerate().map(|(k, c)| mul(vec![c.clone(), pow(x.clone(), int(k as i64))])).collect());
    let dp = calculus::diff(&p, &x);
    let r = solve::resultant(&p, &dp, &x)?;
    let sign = if (n * (n - 1) / 2) % 2 == 0 { 1 } else { -1 };
    Some(s::simplify(&mul(vec![int(sign), r, pow(cs[n].clone(), int(-1))])))
}

/// "Real roots" and "Complex roots" pods for f = 0.
pub fn roots_pods(f: &Expr, x: Expr, single: &str, plural: &str) -> Option<Vec<Pod>> {
    let rs = solve::solve(f, &x);
    if rs.identity {
        return Some(vec![pod("Roots", plural, text("true for all values"))]);
    }
    let mut out = Vec::new();
    let real = rs.real();
    let cplx = rs.complex();
    let label = |n: usize, base: &str| if n == 1 { format!("Real {}", base.to_lowercase()) } else { format!("Real {}", plural_of(base)) };
    let _ = single;
    if !real.is_empty() {
        let title = if rs.periodic { "Real solutions".to_string() } else { label(real.len(), single) };
        out.push(pod("RealRoots", &title, roots_list(&x, &real, rs.periodic, rs.numeric)));
    }
    if !cplx.is_empty() {
        let title = if cplx.len() == 1 { "Complex root".to_string() } else { "Complex roots".to_string() };
        out.push(pod("ComplexRoots", &title, roots_list(&x, &cplx, rs.periodic, rs.numeric)));
    }
    if rs.partial && rs.numeric && !out.is_empty() {
        out.push(pod("RootNote", "Note", text("Roots found numerically in the scanned range; others may exist.")));
    }
    let _ = plural;
    if out.is_empty() {
        return None;
    }
    Some(out)
}

fn plural_of(base: &str) -> String {
    let b = base.to_lowercase();
    if b.ends_with("ion") {
        format!("{b}s")
    } else {
        format!("{b}s")
    }
}

/// `x = r` lines; adds numeric approximations for complicated exact roots.
pub fn roots_list(x: &Expr, roots: &[Expr], periodic: bool, numeric: bool) -> Content {
    let mut rows = Vec::new();
    for r in roots.iter().take(24) {
        let mut line = eq(x.clone(), r.clone());
        if periodic && r.has_sym(&solve::param()) {
            line = raw_fn("Where", vec![line, raw_fn("Element", vec![solve::param(), sym("ℤ")])]);
        }
        let mut row = vec![math(&line)];
        if !numeric && r.free_symbols().is_empty() && !r.is_numeric() && (r.size() > 8 || r.has_const(K::I)) {
            row.push(math(&raw_fn("Approx", vec![x.clone(), s::evalf(r)])));
        }
        rows.push(row);
    }
    if roots.len() > 24 {
        rows.push(vec![text(format!("… ({} more)", roots.len() - 24))]);
    }
    table(None, rows)
}

fn properties_pod(f: &Expr, x: &Expr) -> Option<Pod> {
    let Sym(name) = x else { return None };
    let prog = crate::eval::Program::compile(f, &[name])?.with_real_roots();
    let mut rows = Vec::new();
    // domain by sampling
    let dom = domain_text(&prog, x);
    rows.push(vec![text("Domain"), text(dom)]);
    // range estimate
    if let Some(r) = range_text(&prog) {
        rows.push(vec![text("Range"), text(r)]);
    }
    // parity
    let mut even = true;
    let mut odd = true;
    let mut tested = 0;
    for k in 1..40 {
        let t = 0.173 * k as f64 + 0.05;
        let (a, b) = (prog.eval1(t), prog.eval1(-t));
        if !a.is_finite() || !b.is_finite() {
            continue;
        }
        tested += 1;
        let tol = 1e-9 * (1.0 + a.abs());
        even &= (a - b).abs() <= tol;
        odd &= (a + b).abs() <= tol;
    }
    if tested > 5 {
        let parity = if even && odd {
            "even and odd"
        } else if even {
            "even"
        } else if odd {
            "odd"
        } else {
            "neither even nor odd"
        };
        rows.push(vec![text("Parity"), text(parity)]);
    }
    if let Some(per) = period(f, x) {
        rows.push(vec![text("Period"), math(&per)]);
    }
    Some(pod("Properties", "Properties as a real function", table(None, rows)))
}

fn domain_text(prog: &crate::eval::Program, x: &Expr) -> String {
    let n = 4000;
    let (lo, hi) = (-1000.0, 1000.0);
    let mut breaks: Vec<(f64, bool)> = Vec::new(); // boundary, becomes defined
    let samples: Vec<bool> = (0..=n).map(|k| prog.eval1(lo + (hi - lo) * k as f64 / n as f64).is_finite()).collect();
    let first = samples[0];
    let mut last = first;
    let mut isolated: Vec<f64> = Vec::new();
    for k in 1..=n {
        let t = lo + (hi - lo) * k as f64 / n as f64;
        let ok = samples[k];
        if !ok && k < n && samples[k - 1] && samples[k + 1] {
            isolated.push(t);
            continue;
        }
        if ok != last {
            breaks.push((t, ok));
        }
        last = ok;
    }
    // poles between samples: sign changes with huge magnitude
    let mut prev = prog.eval1(lo);
    for k in 1..=n {
        let t = lo + (hi - lo) * k as f64 / n as f64;
        let v = prog.eval1(t);
        if v.is_finite() && prev.is_finite() && v.signum() != prev.signum() && (v.abs() > 1e3 || prev.abs() > 1e3) {
            let (mut a, mut b) = (t - (hi - lo) / n as f64, t);
            for _ in 0..80 {
                let m = 0.5 * (a + b);
                let fm = prog.eval1(m);
                if !fm.is_finite() {
                    a = m;
                    b = m;
                    break;
                }
                if fm.signum() == prev.signum() {
                    a = m;
                } else {
                    b = m;
                }
            }
            let m = 0.5 * (a + b);
            if prog.eval1(m).abs() > 1e6 || !prog.eval1(m).is_finite() {
                isolated.push(m);
            }
        }
        prev = v;
    }
    let xs = ptext(x);
    if breaks.is_empty() && first && !isolated.is_empty() && isolated.len() <= 6 {
        let pts: Vec<String> = isolated.iter().map(|p| ptext(&crate::calculus::limit::tidy_float(*p))).collect();
        let conds: Vec<String> = pts.iter().map(|p| format!("{xs} ≠ {p}")).collect();
        return format!("{{{xs} ∈ ℝ : {}}}", conds.join(", "));
    }
    if breaks.is_empty() && first && isolated.len() > 6 {
        return "all real numbers except isolated points (see plot)".into();
    }
    if breaks.is_empty() {
        return if first { "all real numbers".into() } else { "(no real values)".into() };
    }
    // refine boundaries
    let mut parts = Vec::new();
    for (t, becomes_ok) in &breaks {
        let (mut a, mut b) = (t - (hi - lo) / n as f64, *t);
        for _ in 0..60 {
            let m = 0.5 * (a + b);
            if prog.eval1(m).is_finite() == *becomes_ok {
                b = m;
            } else {
                a = m;
            }
        }
        parts.push((crate::calculus::limit::tidy_float(0.5 * (a + b)), *becomes_ok));
    }
    if parts.len() == 1 {
        let (v, ok) = &parts[0];
        let vs = ptext(v);
        let closed = prog.eval1(v.to_f64_const().unwrap_or(0.0)).is_finite();
        return if *ok {
            format!("{{{xs} ∈ ℝ : {xs} {} {vs}}}", if closed { "≥" } else { ">" })
        } else {
            format!("{{{xs} ∈ ℝ : {xs} {} {vs}}}", if closed { "≤" } else { "<" })
        };
    }
    "subset of the real numbers (see plot)".into()
}

fn range_text(prog: &crate::eval::Program) -> Option<String> {
    let mut lo = f64::INFINITY;
    let mut hi = f64::NEG_INFINITY;
    let n = 20000;
    for k in 0..=n {
        let u = -1.0 + 2.0 * k as f64 / n as f64;
        let t = u / (1.0 - u * u).max(1e-9) * 3.0; // dense near 0, reaches far out
        let v = prog.eval1(t);
        if v.is_finite() {
            lo = lo.min(v);
            hi = hi.max(v);
        }
    }
    if !lo.is_finite() || !hi.is_finite() {
        return None;
    }
    // typical magnitude, to recognise values blowing up near poles
    let mut mags: Vec<f64> = (0..=200).map(|k| prog.eval1(-10.0 + 0.1 * k as f64).abs()).filter(|v| v.is_finite()).collect();
    mags.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let typical = mags.get(mags.len() / 2).cloned().unwrap_or(1.0).max(1e-6);
    let big = (1e3 * typical).max(1e3).min(1e8);
    let fmt = |v: f64| crate::print::fmt_float(crate::eval::chop(v), 6);
    Some(match (lo < -big, hi > big) {
        (true, true) => "all real numbers".into(),
        (true, false) => format!("{{y ∈ ℝ : y ≤ {}}} (approximately)", fmt(hi)),
        (false, true) => format!("{{y ∈ ℝ : y ≥ {}}} (approximately)", fmt(lo)),
        (false, false) => format!("{{y ∈ ℝ : {} ≤ y ≤ {}}} (approximately)", fmt(lo), fmt(hi)),
    })
}

fn period(f: &Expr, x: &Expr) -> Option<Expr> {
    // trigonometric functions with linear arguments
    let mut periods: Vec<Rat> = Vec::new();
    let mut ok = true;
    f.has(&|_| false);
    fn walk(e: &Expr, x: &Expr, out: &mut Vec<Rat>, ok: &mut bool) {
        match e {
            Func(n, a) if a.len() == 1 && a[0].has_sym(x) => {
                let base = match &**n {
                    "sin" | "cos" => Some(Rat::from_integer(Int::from(2))),
                    "tan" => Some(Rat::one()),
                    _ => None,
                };
                match (base, crate::poly::coeffs(&a[0], x)) {
                    (Some(b), Some(cs)) if cs.len() == 2 => {
                        // period = b π / |k| with k = q π? keep only rational k
                        match cs[1].as_rat() {
                            Some(k) if !k.is_zero() => out.push(b / k.abs()),
                            _ => *ok = false,
                        }
                    }
                    _ => *ok = false,
                }
            }
            Add(v) | Mul(v) => v.iter().for_each(|t| walk(t, x, out, ok)),
            Pow(b, ex) => {
                walk(b, x, out, ok);
                if ex.has_sym(x) {
                    *ok = false;
                }
            }
            Sym(_) if e == x => *ok = false,
            Func(_, a) => {
                if a.iter().any(|t| t.has_sym(x)) {
                    *ok = false;
                }
            }
            _ => {}
        }
    }
    walk(f, x, &mut periods, &mut ok);
    if !ok || periods.is_empty() {
        return None;
    }
    // lcm of rationals p_i: lcm(numerators)/gcd(denominators)
    let mut num = Int::one();
    let mut den = Int::zero();
    for p in &periods {
        num = num_integer::Integer::lcm(&num, p.numer());
        den = num_integer::Integer::gcd(&den, p.denom());
    }
    let per = mul(vec![Num(Rat::new(num, den)), pi()]);
    // verify numerically
    let Sym(name) = x else { return None };
    let prog = crate::eval::Program::compile(f, &[name])?;
    let pv = per.to_f64_const()?;
    for k in 0..10 {
        let t = 0.37 + 0.71 * k as f64;
        let (a, b) = (prog.eval1(t), prog.eval1(t + pv));
        if a.is_finite() && b.is_finite() && (a - b).abs() > 1e-7 * (1.0 + a.abs()) {
            return None;
        }
    }
    Some(per)
}

fn extrema_pods(f: &Expr, x: &Expr) -> Option<Vec<Pod>> {
    let d = s::simplify(&calculus::diff(f, x));
    if d.is_zero() || !d.has_sym(x) {
        return None;
    }
    let rs = solve::solve(&d, x);
    if rs.periodic {
        return None;
    }
    let crit: Vec<Expr> = rs.real();
    let Sym(name) = x else { return None };
    let prog = crate::eval::Program::compile(f, &[name])?;
    let at_inf = |dir: f64| prog.eval1(dir * 1e8);
    let (lim_hi, lim_lo) = (at_inf(1.0), at_inf(-1.0));
    let mut vals: Vec<(Expr, Expr, f64)> = Vec::new();
    for c in crit.iter().take(12) {
        let Some(cv) = c.to_f64_const() else { continue };
        let fv = prog.eval1(cv);
        if !fv.is_finite() {
            continue;
        }
        let exact = s::simplify(&f.subs(x, c));
        vals.push((c.clone(), exact, fv));
    }
    if vals.is_empty() {
        return None;
    }
    let mut out = Vec::new();
    let maxv = vals.iter().map(|v| v.2).fold(f64::NEG_INFINITY, f64::max);
    let minv = vals.iter().map(|v| v.2).fold(f64::INFINITY, f64::min);
    let unbounded_hi = !lim_hi.is_finite() || !lim_lo.is_finite() || lim_hi > maxv + 1e-9 || lim_lo > maxv + 1e-9;
    let unbounded_lo = !lim_hi.is_finite() || !lim_lo.is_finite() || lim_hi < minv - 1e-9 || lim_lo < minv - 1e-9;
    let fname = |kind: &str, e: &Expr, at: &Expr| -> Content {
        if e.size() + at.size() > 30 {
            let (ev, av) = (s::evalf(e), s::evalf(at));
            return math(&raw_fn("Where", vec![raw_fn("Approx", vec![raw_fn(kind, vec![f.clone()]), ev]), raw_fn("Approx", vec![x.clone(), av])]));
        }
        math(&raw_fn("Where", vec![eq(raw_fn(kind, vec![f.clone()]), e.clone()), eq(x.clone(), at.clone())]))
    };
    let mut rows_max = Vec::new();
    let mut rows_min = Vec::new();
    for (c, ex, v) in &vals {
        if (*v - maxv).abs() <= 1e-9 * (1.0 + maxv.abs()) && !unbounded_hi {
            rows_max.push((String::new(), fname("max", ex, c)));
        }
        if (*v - minv).abs() <= 1e-9 * (1.0 + minv.abs()) && !unbounded_lo {
            rows_min.push((String::new(), fname("min", ex, c)));
        }
    }
    if !rows_max.is_empty() {
        out.push(pod_multi("GlobalMaximum", "Global maximum", rows_max));
    }
    if !rows_min.is_empty() {
        out.push(pod_multi("GlobalMinimum", "Global minimum", rows_min));
    }
    if out.is_empty() {
        // local extrema via the second derivative
        let d2 = calculus::diff(&d, x);
        let mut rows = Vec::new();
        for (c, ex, _) in &vals {
            let s2 = d2.subs(x, c).to_f64_const().unwrap_or(0.0);
            let kind = if s2 < 0.0 {
                "max"
            } else if s2 > 0.0 {
                "min"
            } else {
                continue;
            };
            rows.push((String::new(), fname(kind, ex, c)));
        }
        if !rows.is_empty() {
            out.push(pod_multi("LocalExtrema", "Local extrema", rows));
        }
    }
    if out.is_empty() {
        None
    } else {
        Some(out)
    }
}

fn limits_pod(f: &Expr, x: &Expr) -> Option<Pod> {
    let hi = calculus::limit(f, x, &inf(), Dir::Both);
    let lo = calculus::limit(f, x, &neg(inf()), Dir::Both);
    let mut rows = Vec::new();
    for (to, v) in [(inf(), hi), (neg(inf()), lo)] {
        if let Some(v) = v {
            rows.push(vec![math(&eq(raw_fn("Limit", vec![f.clone(), x.clone(), to]), v))]);
        }
    }
    if rows.is_empty() {
        return None;
    }
    Some(pod("Limits", "Limits", table(None, rows)))
}

fn series_pod(f: &Expr, x: &Expr) -> Option<Pod> {
    if crate::poly::coeffs(f, x).is_some() {
        return None;
    }
    let ex = calculus::series(f, x, &zero(), 5)?;
    if ex.big_o.is_none() {
        return None;
    }
    let o = ex.big_o.map(|o| raw_fn("O", vec![o]));
    let mut terms = ex.terms.clone();
    if terms.is_empty() {
        return None;
    }
    // ascending order display: build a presentation sum
    terms.sort_by_key(|t| crate::poly::degree(t, x).unwrap_or(0));
    let mut ds: Vec<D> = terms.iter().map(|t| print::to_d(t, &Default::default())).collect();
    if let Some(o) = o {
        ds.push(print::to_d(&o, &Default::default()));
    }
    let d = D::Sum(ds);
    let title = format!("Series expansion at {} = 0", ptext(x));
    Some(pod("SeriesExpansion", &title, math_d(&d)))
}

// ---------------------------------------------------------------------------
// two and more variables
// ---------------------------------------------------------------------------

pub fn two_var_pods(p: &Parsed, x: &Expr, y: &Expr) -> Vec<Pod> {
    let e = p.expr.clone();
    let mut pods = vec![input_pod(&p.d)];
    let simp = s::simplify(&e);
    if ptext(&simp) != print::d_text(&p.d) && simp.size() < e.size() {
        pods.push(primary(pod("Result", "Result", math(&simp))));
    }
    let mut jobs: Vec<PodJob> = Vec::new();
    {
        let (f, x, y) = (simp.clone(), x.clone(), y.clone());
        jobs.push(job(move || {
            let s3 = pl::surface(&f, &x, &y, (-3.0, 3.0), (-3.0, 3.0), 64, pl::PlotKind::Surface)?;
            let c = pl::surface(&f, &x, &y, (-3.0, 3.0), (-3.0, 3.0), 160, pl::PlotKind::Contour)?;
            Some(vec![pod("3DPlot", "3D plot", plot(s3)), pod("ContourPlot", "Contour plot", plot(c))])
        }));
    }
    {
        let (e, simp) = (e.clone(), simp.clone());
        jobs.push(job1(move || alternate_forms_pod(&e, &[simp])));
    }
    {
        let (f, y) = (simp.clone(), y.clone());
        jobs.push(job1(move || {
            let rs = solve::solve(&f, &y);
            if rs.roots.is_empty() || rs.numeric {
                return None;
            }
            Some(pod("RootsY", &format!("Roots (solving for {})", ptext(&y)), roots_list(&y, &rs.roots, rs.periodic, false)))
        }));
    }
    {
        let (f, x, y) = (simp.clone(), x.clone(), y.clone());
        jobs.push(job1(move || {
            let fx = s::simplify(&calculus::diff(&f, &x));
            let fy = s::simplify(&calculus::diff(&f, &y));
            Some(pod_multi(
                "PartialDerivatives",
                "Partial derivatives",
                vec![(String::new(), math(&eq(raw_fn("Derivative", vec![f.clone(), x.clone(), one()]), fx))), (String::new(), math(&eq(raw_fn("Derivative", vec![f.clone(), y.clone(), one()]), fy)))],
            ))
        }));
    }
    {
        let (f, x, y) = (simp.clone(), x.clone(), y.clone());
        jobs.push(job1(move || {
            let g = vec![calculus::diff(&f, &x), calculus::diff(&f, &y)];
            let sol = solve::solve_system(&g, &[x.clone(), y.clone()]);
            if sol.sols.is_empty() || sol.sols.len() > 6 || sol.underdetermined {
                return None;
            }
            let h = calculus::diff::hessian(&f, &[x.clone(), y.clone()]);
            let mut rows = Vec::new();
            for sv in &sol.sols {
                let at = |e: &Expr| s::simplify(&e.subs_many(sv));
                let (fxx, fyy, fxy) = (at(&h[0][0]), at(&h[1][1]), at(&h[0][1]));
                let det = s::simplify(&sub(mul(vec![fxx.clone(), fyy]), square(fxy)));
                let kind = match (det.to_f64_const(), fxx.to_f64_const()) {
                    (Some(d), Some(a)) if d > 0.0 && a > 0.0 => "local minimum",
                    (Some(d), Some(a)) if d > 0.0 && a < 0.0 => "local maximum",
                    (Some(d), _) if d < 0.0 => "saddle point",
                    _ => "critical point",
                };
                let pt = List(sv.iter().map(|(_, v)| v.clone()).collect());
                rows.push(vec![text(kind), math(&raw_fn("Tuple", vec![pt])), math(&at(&f))]);
            }
            Some(pod("CriticalPoints", "Critical points", table(Some(vec!["type", &format!("({}, {})", ptext(&x), ptext(&y)), "value"]), rows)))
        }));
    }
    pods.extend(run_jobs(jobs));
    pods
}

fn multi_var_pods(p: &Parsed) -> Vec<Pod> {
    let e = p.expr.clone();
    let mut pods = vec![input_pod(&p.d)];
    let simp = s::simplify(&e);
    if ptext(&simp) != print::d_text(&p.d) {
        pods.push(primary(pod("Result", "Result", math(&simp))));
    }
    if let Some(a) = alternate_forms_pod(&e, &[simp.clone()]) {
        pods.push(a);
    }
    let vars = sorted_symbols(&e);
    let grads: Vec<(String, Content)> = vars.iter().take(4).map(|v| (String::new(), math(&eq(raw_fn("Derivative", vec![simp.clone(), v.clone(), one()]), s::simplify(&calculus::diff(&simp, v)))))).collect();
    pods.push(pod_multi("PartialDerivatives", "Partial derivatives", grads));
    pods
}

// ---------------------------------------------------------------------------
// equations
// ---------------------------------------------------------------------------

pub fn equation_pods(p: &Parsed, a: &Expr, b: &Expr, opts: &Options) -> Vec<Pod> {
    let mut pods = vec![input_pod(&p.d)];
    let full = Rel(RelOp::Eq, Box::new(a.clone()), Box::new(b.clone()));
    let vars = sorted_symbols(&full);
    if vars.is_empty() {
        let v = crate::funcs::rel(RelOp::Eq, a.clone(), b.clone());
        let t = match v {
            Const(K::True) => "True",
            Const(K::False) => "False",
            _ => {
                if s::equal(a, b) {
                    "True"
                } else {
                    "False"
                }
            }
        };
        pods.push(primary(pod("Result", "Result", text(t))));
        return pods;
    }
    let f = s::simplify(&sub(a.clone(), b.clone()));
    let mut jobs: Vec<PodJob> = Vec::new();
    if vars.len() == 1 {
        let x = vars[0].clone();
        {
            let (a, b, x) = (a.clone(), b.clone(), x.clone());
            jobs.push(job1(move || {
                let fs: Vec<Expr> = [a, b].into_iter().filter(|e| e.has_sym(&x)).collect();
                let (lhs, rhs) = (fs.first()?.clone(), fs.get(1).cloned());
                let mut list = vec![lhs];
                if let Some(r) = rhs {
                    list.push(r);
                }
                let pl = pl::cartesian(&list, &x, None)?;
                Some(pod("Plot", "Plot", plot(pl)))
            }));
        }
        {
            let (f, x) = (f.clone(), x.clone());
            let input_text = print::d_text(&p.d);
            jobs.push(job1(move || {
                let fa = s::factor(&f);
                let shown = if ptext(&fa) != ptext(&f) && fa.size() <= f.size() + 4 { fa } else { f.clone() };
                if shown.is_zero() || ptext(&eq(shown.clone(), zero())) == input_text {
                    return None;
                }
                let _ = &x;
                Some(pod("AlternateForm", "Alternate form", math(&eq(shown, zero()))))
            }));
        }
        {
            let (f, x, want_steps) = (f.clone(), x.clone(), opts.steps);
            jobs.push(job(move || {
                let mut out = solution_pods(&f, &x)?;
                if want_steps {
                    if let Some(st) = quadratic_steps(&f, &x) {
                        out.push(pod("SolutionSteps", "Solution · step by step", steps_content(&st)));
                    }
                }
                Some(out)
            }));
        }
    } else if vars.len() == 2 {
        let (x, y) = (vars[0].clone(), vars[1].clone());
        {
            let (f, x, y) = (f.clone(), x.clone(), y.clone());
            jobs.push(job(move || {
                let mut out = Vec::new();
                let pl = pl::implicit(&f, &x, &y, (-6.0, 6.0), (-6.0, 6.0))?;
                let pl = refit_implicit(&f, &x, &y, pl);
                out.push(pod("ImplicitPlot", "Implicit plot", plot(pl)));
                if let Some(c) = conic(&f, &x, &y) {
                    out.push(c);
                }
                Some(out)
            }));
        }
        {
            let (f, y) = (f.clone(), y.clone());
            jobs.push(job1(move || {
                let rs = solve::solve(&f, &y);
                if rs.roots.is_empty() || rs.numeric {
                    return None;
                }
                Some(pod("Solutions", &format!("Solutions for {}", ptext(&y)), roots_list(&y, &rs.roots, rs.periodic, false)))
            }));
        }
        {
            let (full, x, y) = (full.clone(), x.clone(), y.clone());
            jobs.push(job1(move || {
                let d = solve::implicit_derivative(&full, &x, &y)?;
                let lhs = raw_fn("Derivative", vec![y.clone(), x.clone(), one()]);
                Some(pod("ImplicitDerivative", "Implicit derivative", math(&eq(lhs, d))))
            }));
        }
    } else {
        let x = vars[0].clone();
        let f2 = f.clone();
        jobs.push(job1(move || {
            let rs = solve::solve(&f2, &x);
            if rs.roots.is_empty() {
                return None;
            }
            Some(pod("Solutions", &format!("Solutions for {}", ptext(&x)), roots_list(&x, &rs.roots, rs.periodic, rs.numeric)))
        }));
    }
    let mut more = run_jobs(jobs);
    if let Some(pos) = more.iter().position(|p| matches!(p.id.as_str(), "RealSolutions" | "Solutions" | "ComplexSolutions")) {
        more[pos].primary = true;
    }
    pods.extend(more);
    pods
}

fn refit_implicit(f: &Expr, x: &Expr, y: &Expr, pl: pl::Plot) -> pl::Plot {
    // zoom to the curve when it is small, widen when it fills the window
    let pts: Vec<[f64; 2]> = pl.series.iter().flat_map(|s| s.segments.iter().flatten().cloned()).collect();
    if pts.is_empty() {
        return pl;
    }
    let (mut x0, mut x1, mut y0, mut y1) = (f64::INFINITY, f64::NEG_INFINITY, f64::INFINITY, f64::NEG_INFINITY);
    for p in &pts {
        x0 = x0.min(p[0]);
        x1 = x1.max(p[0]);
        y0 = y0.min(p[1]);
        y1 = y1.max(p[1]);
    }
    let w = (x1 - x0).max(y1 - y0);
    if w < 3.0 {
        let c = (0.5 * (x0 + x1), 0.5 * (y0 + y1));
        let h = (w * 0.8).max(0.5);
        return pl::implicit(f, x, y, (c.0 - h, c.0 + h), (c.1 - h, c.1 + h)).unwrap_or(pl);
    }
    pl
}

/// Classify a quadratic curve A x² + B x y + C y² + D x + E y + F = 0.
fn conic(f: &Expr, x: &Expr, y: &Expr) -> Option<Pod> {
    let ex = s::expand(f);
    let cx = crate::poly::coeffs(&ex, x)?;
    if cx.len() > 3 {
        return None;
    }
    let get = |k: usize| cx.get(k).cloned().unwrap_or_else(zero);
    let c0 = crate::poly::coeffs(&get(0), y)?;
    let c1 = crate::poly::coeffs(&get(1), y)?;
    let c2 = crate::poly::coeffs(&get(2), y)?;
    if c0.len() > 3 || c1.len() > 2 || c2.len() > 1 {
        return None;
    }
    let g = |v: &Vec<Expr>, k: usize| v.get(k).and_then(|e| e.to_f64_const()).unwrap_or(0.0);
    let (a, b, c) = (g(&c2, 0), g(&c1, 1), g(&c0, 2));
    let (d, e, ff) = (g(&c1, 0), g(&c0, 1), g(&c0, 0));
    if a == 0.0 && b == 0.0 && c == 0.0 {
        return None;
    }
    let disc = b * b - 4.0 * a * c;
    let kind = if disc.abs() < 1e-12 {
        "parabola"
    } else if disc < 0.0 {
        if b == 0.0 && (a - c).abs() < 1e-12 {
            "circle"
        } else {
            "ellipse"
        }
    } else {
        "hyperbola"
    };
    let mut rows = vec![vec![text("Curve"), text(kind)]];
    if kind == "circle" {
        let h = -d / (2.0 * a);
        let k = -e / (2.0 * a);
        let r2 = h * h + k * k - ff / a;
        if r2 > 0.0 {
            let center = List(vec![crate::calculus::limit::tidy_float(h), crate::calculus::limit::tidy_float(k)]);
            rows.push(vec![text("Center"), math(&raw_fn("Tuple", vec![center]))]);
            let rr = crate::calculus::limit::tidy_float(r2);
            rows.push(vec![text("Radius"), math(&s::simplify(&sqrt(rr.clone())))]);
            rows.push(vec![text("Area"), math(&s::simplify(&mul(vec![pi(), rr])))]);
        }
    }
    Some(pod("Geometry", "Geometric figure", table(None, rows)))
}

fn solution_pods(f: &Expr, x: &Expr) -> Option<Vec<Pod>> {
    let rs = solve::solve(f, x);
    if rs.identity {
        return Some(vec![pod("Solutions", "Solution", text("The equation holds for all values."))]);
    }
    let real = rs.real();
    let cplx = rs.complex();
    let mut out = Vec::new();
    if !real.is_empty() {
        let title = if rs.periodic {
            "Real solutions".to_string()
        } else if real.len() == 1 {
            "Real solution".to_string()
        } else {
            "Real solutions".to_string()
        };
        out.push(pod("RealSolutions", &title, roots_list(x, &real, rs.periodic, rs.numeric)));
    }
    if !cplx.is_empty() {
        out.push(pod("ComplexSolutions", if cplx.len() == 1 { "Complex solution" } else { "Complex solutions" }, roots_list(x, &cplx, rs.periodic, rs.numeric)));
    }
    if out.is_empty() {
        out.push(pod("Solutions", "Result", text("no solutions exist")));
    }
    if rs.numeric && rs.partial {
        out.push(pod("Note", "Note", text("Solutions were found numerically in the range −1000 ≤ x ≤ 1000.")));
    }
    Some(out)
}

/// Worked solution for a quadratic equation.
fn quadratic_steps(f: &Expr, x: &Expr) -> Option<Vec<crate::steps::Step>> {
    let cs = crate::poly::coeffs(&s::expand(f), x)?;
    if cs.len() != 3 || cs.iter().any(|c| c.as_rat().is_none()) {
        return None;
    }
    let (a, b, c) = (cs[2].clone(), cs[1].clone(), cs[0].clone());
    let ((), st) = crate::steps::record(|| {
        let std_form = eq(add(vec![mul(vec![a.clone(), square(x.clone())]), mul(vec![b.clone(), x.clone()]), c.clone()]), zero());
        crate::steps::step("Write the equation in standard form a x^2 + b x + c = 0:", Some(&std_form));
        crate::steps::step(format!("Here a = {}, b = {}, c = {}.", ptext(&a), ptext(&b), ptext(&c)), None);
        let disc = s::simplify(&sub(square(b.clone()), mul(vec![int(4), a.clone(), c.clone()])));
        crate::steps::step("Compute the discriminant b^2 - 4 a c:", Some(&eq(sym("Δ"), disc.clone())));
        let sign = disc.to_f64_const().unwrap_or(0.0);
        if sign > 0.0 {
            crate::steps::step("The discriminant is positive, so there are two distinct real solutions.", None);
        } else if sign == 0.0 {
            crate::steps::step("The discriminant is zero, so there is one repeated real solution.", None);
        } else {
            crate::steps::step("The discriminant is negative, so the solutions are complex conjugates.", None);
        }
        let formula = eq(x.clone(), div(add(vec![neg(sym("b")), raw_fn("PlusMinus", vec![sqrt(sym("Δ"))])]), mul(vec![int(2), sym("a")])));
        crate::steps::step("Apply the quadratic formula:", Some(&formula));
        for r in solve::quadratic_formula(&a, &b, &c) {
            crate::steps::step("", Some(&eq(x.clone(), s::simplify(&r))));
        }
    });
    Some(st)
}

// ---------------------------------------------------------------------------
// inequalities
// ---------------------------------------------------------------------------

pub fn inequality_pods(p: &Parsed, op: RelOp, a: &Expr, b: &Expr) -> Vec<Pod> {
    let mut pods = vec![input_pod(&p.d)];
    let full = Rel(op, Box::new(a.clone()), Box::new(b.clone()));
    let vars = sorted_symbols(&full);
    if vars.is_empty() {
        let v = crate::funcs::rel(op, a.clone(), b.clone());
        pods.push(primary(pod("Result", "Result", math(&v))));
        return pods;
    }
    if vars.len() == 1 {
        let x = vars[0].clone();
        if let Some(sol) = solve::solve_inequality(op, a, b, &x) {
            pods.push(primary(pod("Solution", "Solution", math(&sol))));
            if let Some(spans) = spans_of(&sol, &x) {
                pods.push(pod("NumberLine", "Number line", plot(pl::number_line(spans))));
            }
            if let Some(pl) = pl::cartesian(&[sub(a.clone(), b.clone())], &x, None) {
                pods.push(pod("Plot", &format!("Plot of {}", ptext(&sub(a.clone(), b.clone()))), plot(pl)));
            }
        }
    } else if vars.len() == 2 {
        // region plot via the boundary curve
        let f = sub(a.clone(), b.clone());
        if let Some(pl) = pl::implicit(&f, &vars[0], &vars[1], (-6.0, 6.0), (-6.0, 6.0)) {
            pods.push(pod("Boundary", "Boundary of the region", plot(pl)));
        }
    }
    pods
}

fn spans_of(sol: &Expr, x: &Expr) -> Option<Vec<pl::Span>> {
    let parts = match sol {
        Func(n, v) if &**n == "Or" => v.clone(),
        Const(K::True) => return Some(vec![pl::Span { lo: f64::NEG_INFINITY, hi: f64::INFINITY, lo_closed: false, hi_closed: false }]),
        Const(K::False) => return Some(vec![]),
        _ => vec![sol.clone()],
    };
    let mut out = Vec::new();
    for p in parts {
        match &p {
            Rel(op, a, b) if **a == *x => {
                let v = b.to_f64_const()?;
                let closed = matches!(op, RelOp::Le | RelOp::Ge | RelOp::Eq);
                out.push(match op {
                    RelOp::Lt | RelOp::Le => pl::Span { lo: f64::NEG_INFINITY, hi: v, lo_closed: false, hi_closed: closed },
                    RelOp::Gt | RelOp::Ge => pl::Span { lo: v, hi: f64::INFINITY, lo_closed: closed, hi_closed: false },
                    _ => pl::Span { lo: v, hi: v, lo_closed: true, hi_closed: true },
                });
            }
            Func(n, v) if &**n == "And" && v.len() == 2 => {
                let (Rel(o1, l, _), Rel(o2, _, h)) = (&v[0], &v[1]) else { return None };
                out.push(pl::Span { lo: l.to_f64_const()?, hi: h.to_f64_const()?, lo_closed: *o1 == RelOp::Le, hi_closed: *o2 == RelOp::Le });
            }
            _ => return None,
        }
    }
    Some(out)
}

// ---------------------------------------------------------------------------
// systems
// ---------------------------------------------------------------------------

pub fn system_pods(p: &Parsed, eqs: &[Expr]) -> Vec<Pod> {
    let mut pods = vec![input_pod(&p.d)];
    let all = List(eqs.to_vec());
    let vars = sorted_symbols(&all);
    let sol = solve::solve_system(eqs, &vars);
    if sol.inconsistent || (sol.sols.is_empty() && !sol.numeric) {
        pods.push(primary(pod("Solution", "Result", text("no solutions exist"))));
    } else if !sol.sols.is_empty() {
        let rows: Vec<Vec<Content>> = sol
            .sols
            .iter()
            .take(24)
            .map(|sv| {
                let line = sv.iter().map(|(v, e)| eq(v.clone(), e.clone())).collect::<Vec<_>>();
                vec![math(&raw_fn("Conj", line))]
            })
            .collect();
        let title = if sol.sols.len() == 1 { "Solution" } else { "Solutions" };
        pods.push(primary(pod("Solution", title, table(None, rows))));
        if sol.underdetermined {
            pods.push(pod("Note", "Note", text("The system has infinitely many solutions; the remaining variables are free.")));
        }
    }
    if vars.len() == 2 {
        let curves: Vec<Expr> = eqs.iter().map(solve::as_zero).collect();
        let mut combined: Option<pl::Plot> = None;
        for f in &curves {
            if let Some(pl) = pl::implicit(f, &vars[0], &vars[1], (-8.0, 8.0), (-8.0, 8.0)) {
                match &mut combined {
                    None => combined = Some(pl),
                    Some(c) => c.series.extend(pl.series),
                }
            }
        }
        if let Some(mut c) = combined {
            for (k, s0) in c.series.iter_mut().enumerate() {
                s0.label = ptext(&eqs[k.min(eqs.len() - 1)]);
            }
            pods.push(pod("Plot", "Implicit plot", plot(c)));
        }
    }
    pods
}

// ---------------------------------------------------------------------------
// logic
// ---------------------------------------------------------------------------

fn is_logic(e: &Expr) -> bool {
    matches!(e, Func(n, _) if matches!(&**n, "and" | "or" | "not" | "xor" | "implies" | "iff" | "nand" | "nor")) && !e.free_symbols().is_empty()
}

fn logic_pods(p: &Parsed) -> Vec<Pod> {
    let e = p.expr.clone();
    let mut pods = vec![input_pod(&p.d)];
    let vars: Vec<Name> = e.free_symbols().into_iter().collect();
    if vars.len() > 6 {
        return pods;
    }
    let n = vars.len();
    let mut rows = Vec::new();
    let mut minterms = Vec::new();
    for k in 0..(1usize << n) {
        let assign: Vec<(Expr, Expr)> = vars.iter().enumerate().map(|(j, v)| (sym(v), boolean((k >> (n - 1 - j)) & 1 == 1))).collect();
        let v = e.subs_many(&assign);
        let v = s::simplify(&v);
        let truth = matches!(v, Const(K::True));
        if truth {
            minterms.push(k);
        }
        let mut row: Vec<Content> = assign.iter().map(|(_, b)| text(if matches!(b, Const(K::True)) { "T" } else { "F" })).collect();
        row.push(text(if truth { "T" } else { "F" }));
        rows.push(row);
    }
    let mut header: Vec<String> = vars.iter().map(|v| v.to_string()).collect();
    header.push(print::d_text(&p.d));
    pods.push(pod("TruthTable", "Truth table", Content::Table { header: Some(header), rows }));
    // minimal DNF by listing minterms (readable for small inputs)
    let kind = if minterms.is_empty() {
        "contradiction"
    } else if minterms.len() == 1 << n {
        "tautology"
    } else {
        "satisfiable"
    };
    pods.push(pod("Properties", "Logic properties", text(format!("The formula is {kind}."))));
    if !minterms.is_empty() && minterms.len() < (1 << n) {
        let terms: Vec<String> = minterms
            .iter()
            .map(|k| {
                let lits: Vec<String> = vars.iter().enumerate().map(|(j, v)| if (k >> (n - 1 - j)) & 1 == 1 { v.to_string() } else { format!("¬{v}") }).collect();
                format!("({})", lits.join(" ∧ "))
            })
            .collect();
        pods.push(pod("DNF", "Disjunctive normal form", text(terms.join(" ∨ "))));
    }
    pods
}

// ---------------------------------------------------------------------------
// calculus holders
// ---------------------------------------------------------------------------

pub fn calculus_pods(p: &Parsed, opts: &Options) -> Vec<Pod> {
    let e = p.expr.clone();
    let mut pods = vec![input_pod(&p.d)];
    let want_steps = opts.steps;
    // derivative steps
    let mut st = Vec::new();
    let result = if let Func(n, a) = &e {
        if &**n == "Derivative" && a.len() == 3 && a[2].is_one() && want_steps {
            let (r, s1) = crate::steps::record(|| calculus::diff::diff_explained(&a[0], &a[1]));
            st = s1;
            r
        } else {
            calculus::doit(&e)
        }
    } else {
        calculus::doit(&e)
    };
    let unevaluated = result.has(&|n| matches!(n, Func(name, _) if matches!(&**name, "Integral" | "Limit" | "Sum" | "Product")));
    if unevaluated {
        // explain why
        let msg = explain_failure(&e);
        pods.push(primary(pod("Result", "Result", text(msg))));
        return pods;
    }
    let result = if matches!(&e, Func(n, _) if &**n == "Integral") && e.as_list().is_none() {
        match &e {
            Func(_, a) if a.len() == 2 => add(vec![result, sym("constant")]),
            _ => result,
        }
    } else {
        result
    };
    pods.push(primary(pod("Result", "Result", math(&eq(e.clone(), result.clone())))));
    if !result.free_symbols().is_empty() || result.is_numeric() && !result.is_integer() {
        let r2 = result.clone();
        if r2.free_symbols().is_empty() {
            if let Some(d) = crate::bigfloat::decimal(&r2, 30) {
                pods.push(pod("DecimalApproximation", "Decimal approximation", text(format!("{d}…"))));
            }
        }
    } else if result.free_symbols().is_empty() && !result.is_numeric() {
        if let Some(d) = crate::bigfloat::decimal(&result, 30) {
            pods.push(pod("DecimalApproximation", "Decimal approximation", text(format!("{d}…"))));
        }
    }
    if !st.is_empty() {
        pods.push(pod("Steps", "Step-by-step solution", steps_content(&st)));
    }
    // plots for integrals and derivatives of one variable
    match &e {
        Func(n, a) if &**n == "Integral" && a.len() == 4 => {
            if let (Some(lo), Some(hi)) = (a[2].to_f64_const(), a[3].to_f64_const()) {
                let w = (hi - lo).abs().max(1e-9);
                if let Some(mut pl) = pl::cartesian(std::slice::from_ref(&a[0]), &a[1], Some((lo - 0.25 * w, hi + 0.25 * w))) {
                    pl.spans.push(pl::Span { lo, hi, lo_closed: true, hi_closed: true });
                    pods.push(pod("VisualRepresentation", "Visual representation of the integral", plot(pl)));
                }
            }
        }
        Func(n, a) if &**n == "Integral" && a.len() == 2 => {
            let fns = vec![a[0].clone(), drop_c(&result)];
            if fns.iter().all(|f| f.free_symbols().len() <= 1) {
                if let Some(mut pl) = pl::cartesian(&fns, &a[1], None) {
                    pl.series[1].label = "antiderivative".into();
                    pods.push(pod("Plots", "Plots of the integral", plot(pl)));
                }
            }
        }
        Func(n, a) if &**n == "Derivative" && a.len() == 3 && a[0].free_symbols().len() <= 1 => {
            if let Some(mut pl) = pl::cartesian(&[a[0].clone(), result.clone()], &a[1], None) {
                pl.series[1].label = "derivative".into();
                pods.push(pod("Plot", "Plot", plot(pl)));
            }
        }
        _ => {}
    }
    if let Some(alt) = alternate_forms_pod(&drop_c(&result), &[]) {
        if result.size() > 4 {
            pods.push(alt);
        }
    }
    pods
}

fn drop_c(e: &Expr) -> Expr {
    e.subs(&sym("constant"), &zero())
}

fn explain_failure(e: &Expr) -> String {
    match e {
        Func(n, a) if &**n == "Integral" && a.len() == 4 => match calculus::integrate::definite(&a[0], &a[1], &a[2], &a[3]) {
            calculus::integrate::Definite::Divergent => "The integral does not converge.".into(),
            _ => "No closed form was found, and the numeric integration did not converge.".into(),
        },
        Func(n, a) if &**n == "Integral" && a.len() == 2 => "No elementary antiderivative was found (it may not exist in closed form).".into(),
        Func(n, a) if &**n == "Sum" && a.len() == 4 => match calculus::sum::sum(&a[0], &a[1], &a[2], &a[3]) {
            calculus::sum::SumResult::Divergent(why) => format!("The series diverges: {why}."),
            _ => "No closed form was found for this sum.".into(),
        },
        Func(n, _) if &**n == "Limit" => "The limit does not exist (or could not be determined).".into(),
        _ => "This could not be evaluated.".into(),
    }
}
