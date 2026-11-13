//! Output formatting.
//!
//! Expressions are first converted to a presentation tree ([`D`]) that fixes
//! the order of terms, how fractions and radicals are grouped, and so on.
//! Three renderers consume it: plain text, a two-dimensional layout tree
//! ([`MathNode`]) for the GUI, and LaTeX (derived from the layout tree).

use crate::expr::Expr::*;
use crate::expr::*;
use num_bigint::BigInt;
use num_traits::{One, Signed, ToPrimitive};
use serde::Serialize;

#[derive(Clone, Debug, PartialEq)]
pub enum D {
    Num(String),
    Ident(String),
    Text(String),
    Sum(Vec<D>),
    Neg(Box<D>),
    Prod(Vec<D>),
    Frac(Box<D>, Box<D>),
    Pow(Box<D>, Box<D>),
    Sqrt(Box<D>),
    Root(Box<D>, Box<D>),
    Call(String, Vec<D>),
    CallPow(String, Box<D>, Vec<D>),
    Abs(Box<D>),
    Fact(Box<D>),
    List(Vec<D>),
    Tuple(Vec<D>),
    Matrix(Vec<Vec<D>>),
    Rel(Vec<D>, Vec<String>),
    Infix(Box<D>, String, Box<D>),
    Prefix(String, Box<D>),
    Postfix(Box<D>, String),
    Integral { body: Box<D>, var: Box<D>, lo: Option<Box<D>>, hi: Option<Box<D>> },
    BigOp { sym: &'static str, body: Box<D>, var: Box<D>, lo: Box<D>, hi: Box<D> },
    Limit { body: Box<D>, var: Box<D>, to: Box<D>, dir: i8 },
    Deriv { body: Box<D>, var: Box<D>, order: Box<D> },
    Sub(Box<D>, Box<D>),
}

#[derive(Clone, Copy, Debug)]
pub struct Fmt {
    /// Significant digits for floating-point numbers.
    pub digits: usize,
    /// Order sums by ascending degree (series style).
    pub ascending: bool,
}

impl Default for Fmt {
    fn default() -> Self {
        Fmt { digits: 6, ascending: false }
    }
}

// ---------------------------------------------------------------------------
// numbers
// ---------------------------------------------------------------------------

/// Format a float with the given number of significant digits.
pub fn fmt_float(v: f64, digits: usize) -> String {
    if v == 0.0 {
        return "0".into();
    }
    if v.is_nan() {
        return "undefined".into();
    }
    if v.is_infinite() {
        return if v > 0.0 { "∞".into() } else { "-∞".into() };
    }
    let digits = digits.clamp(1, 17);
    let exp = v.abs().log10().floor() as i32;
    if exp < -5 || exp >= 10.max(digits as i32 + 3) {
        let s = format!("{:.*e}", digits - 1, v);
        let (mant, ex) = s.split_once('e').unwrap();
        let mant = trim_zeros(mant);
        return format!("{mant}×10^{ex}");
    }
    let decimals = (digits as i32 - 1 - exp).max(0) as usize;
    let s = format!("{:.*}", decimals, v);
    let s = trim_zeros(&s);
    if s == "-0" {
        "0".into()
    } else {
        s
    }
}

fn trim_zeros(s: &str) -> String {
    if s.contains('.') {
        s.trim_end_matches('0').trim_end_matches('.').to_string()
    } else {
        s.to_string()
    }
}

/// Insert thin grouping for long integers? (kept plain for re-parsing)
fn int_d(n: &BigInt) -> D {
    if n.is_negative() {
        D::Neg(Box::new(D::Num((-n).to_string())))
    } else {
        D::Num(n.to_string())
    }
}

fn float_d(v: f64, fmt: &Fmt) -> D {
    let s = fmt_float(v.abs(), fmt.digits);
    let body = match s.split_once("×10^") {
        Some((m, e)) => {
            let ex: i64 = e.parse().unwrap_or(0);
            let p = D::Pow(Box::new(D::Num("10".into())), Box::new(if ex < 0 { D::Neg(Box::new(D::Num((-ex).to_string()))) } else { D::Num(ex.to_string()) }));
            if m == "1" {
                p
            } else {
                D::Prod(vec![D::Num(m.to_string()), p])
            }
        }
        None => {
            if s == "∞" {
                D::Ident("∞".into())
            } else if s == "undefined" {
                D::Text(s)
            } else {
                D::Num(s)
            }
        }
    };
    if v < 0.0 && body != D::Num("0".into()) {
        D::Neg(Box::new(body))
    } else {
        body
    }
}

// ---------------------------------------------------------------------------
// Expr -> D
// ---------------------------------------------------------------------------

fn sym_degree(e: &Expr, s: &Name) -> i64 {
    match e {
        Sym(n) => (n == s) as i64,
        Pow(b, ex) => match (&**b, ex.as_rat()) {
            (Sym(n), Some(q)) if n == s => q.to_integer().to_i64().unwrap_or(0),
            _ => 0,
        },
        Mul(v) => v.iter().map(|f| sym_degree(f, s)).sum(),
        _ => 0,
    }
}

fn term_key(t: &Expr, syms: &[Name]) -> (u8, i64, Vec<i64>) {
    let has_syms = !t.free_symbols().is_empty();
    if has_syms {
        let degs: Vec<i64> = syms.iter().map(|s| -sym_degree(t, s)).collect();
        (0, degs.iter().sum(), degs)
    } else if t.has_const(K::I) {
        (3, 0, vec![])
    } else if t.is_numeric() {
        (1, 0, vec![])
    } else {
        (2, 0, vec![])
    }
}

fn factor_class(f: &Expr) -> (u8, String) {
    match f {
        Pow(b, ex) if matches!(**b, Num(_)) && matches!(**ex, Num(_)) => (0, String::new()),
        Const(K::I) => (2, String::new()),
        Const(_) => (1, String::new()),
        Pow(b, _) if matches!(**b, Const(K::E)) => (3, String::new()),
        Pow(b, _) if matches!(**b, Const(_)) => (1, String::new()),
        Sym(s) => (4, s.to_string()),
        Pow(b, _) => match &**b {
            Sym(s) => (4, s.to_string()),
            Func(..) => (6, String::new()),
            _ => (5, String::new()),
        },
        Add(_) => (5, String::new()),
        Func(n, _) => (6, n.to_string()),
        _ => (5, String::new()),
    }
}

const INVERSE_NAMES: &[(&str, &str)] = &[("asin", "sin"), ("acos", "cos"), ("atan", "tan"), ("asinh", "sinh"), ("acosh", "cosh"), ("atanh", "tanh")];
const RECIPROCAL: &[(&str, &str)] = &[("cos", "sec"), ("sin", "csc"), ("tan", "cot"), ("cosh", "sech"), ("sinh", "csch"), ("tanh", "coth")];

fn pretty_fn(name: &str) -> String {
    match name {
        "ln" => "log",
        "gamma" => "Γ",
        "zeta" => "ζ",
        "digamma" => "ψ",
        "heaviside" => "θ",
        "dirac" => "δ",
        "totient" => "φ",
        "fibonacci" => "F",
        "lucas" => "L",
        "besselj" => "J",
        "bessely" => "Y",
        "besseli" => "I",
        "besselk" => "K",
        "airyai" => "Ai",
        "airybi" => "Bi",
        "ellipticK" => "K",
        "ellipticE" => "E",
        "fresnels" => "S",
        "fresnelc" => "C",
        "polylog" => "Li",
        "harmonic" => "H",
        "prime" => "p",
        "primepi" => "π",
        "bernoulli" => "B",
        other => other,
    }
    .to_string()
}

pub fn to_d(e: &Expr, fmt: &Fmt) -> D {
    crate::budget::tick();
    match e {
        Num(r) => {
            if r.is_integer() {
                int_d(r.numer())
            } else {
                let f = D::Frac(Box::new(D::Num(r.numer().abs().to_string())), Box::new(D::Num(r.denom().to_string())));
                if r.is_negative() {
                    D::Neg(Box::new(f))
                } else {
                    f
                }
            }
        }
        Float(v) => float_d(v.0, fmt),
        Sym(s) => D::Ident(s.to_string()),
        Const(c) => match c {
            K::Pi => D::Ident("π".into()),
            K::E => D::Ident("e".into()),
            K::I => D::Ident("i".into()),
            K::Inf => D::Ident("∞".into()),
            K::CInf => D::Text("complex infinity".into()),
            K::Undef => D::Text("undefined".into()),
            K::EulerGamma => D::Ident("γ".into()),
            K::Golden => D::Ident("ϕ".into()),
            K::Catalan => D::Ident("C".into()),
            K::True => D::Text("True".into()),
            K::False => D::Text("False".into()),
        },
        Add(v) => sum_d(v, fmt),
        Mul(v) => prod_d(v, fmt),
        Pow(b, ex) => pow_d(b, ex, fmt),
        Func(name, args) => func_d(name, args, fmt),
        List(v) => {
            if e.is_matrix() {
                D::Matrix(v.iter().map(|r| r.as_list().unwrap().iter().map(|x| to_d(x, fmt)).collect()).collect())
            } else {
                D::List(v.iter().map(|x| to_d(x, fmt)).collect())
            }
        }
        Rel(op, a, b) => D::Rel(vec![to_d(a, fmt), to_d(b, fmt)], vec![op.symbol().to_string()]),
    }
}

fn sum_d(v: &[Expr], fmt: &Fmt) -> D {
    let mut syms: Vec<Name> = Vec::new();
    for t in v {
        for s in t.free_symbols() {
            if !syms.contains(&s) {
                syms.push(s);
            }
        }
    }
    syms.sort();
    let mut terms: Vec<&Expr> = v.iter().collect();
    terms.sort_by_cached_key(|t| {
        let k = term_key(t, &syms);
        if fmt.ascending && k.0 == 0 {
            (4, -k.1, k.2.iter().map(|d| -d).collect())
        } else if fmt.ascending {
            (k.0, k.1, k.2)
        } else {
            k
        }
    });
    let mut ds: Vec<D> = terms.iter().map(|t| to_d(t, fmt)).collect();
    // common denominator: x/2 + y/2 -> (x + y)/2
    if ds.len() >= 2 {
        let den = |d: &D| -> Option<String> {
            let inner = match d {
                D::Neg(x) => &**x,
                x => x,
            };
            match inner {
                D::Frac(_, q) => match &**q {
                    D::Num(s) => Some(s.clone()),
                    _ => None,
                },
                _ => None,
            }
        };
        if let Some(first) = den(&ds[0]) {
            if ds.iter().all(|d| den(d).as_deref() == Some(first.as_str())) {
                let nums: Vec<D> = ds
                    .iter()
                    .map(|d| match d {
                        D::Neg(x) => match &**x {
                            D::Frac(n, _) => D::Neg(n.clone()),
                            _ => unreachable!(),
                        },
                        D::Frac(n, _) => (**n).clone(),
                        _ => unreachable!(),
                    })
                    .collect();
                let nums = reorder_sign(nums);
                return D::Frac(Box::new(D::Sum(nums)), Box::new(D::Num(first)));
            }
        }
    }
    // keep "a + b i" order for complex numbers
    let complex_tail = terms.last().map_or(false, |t| t.has_const(K::I) && t.free_symbols().is_empty());
    if !complex_tail {
        ds = reorder_sign(ds);
    }
    D::Sum(ds)
}

fn reorder_sign(mut ds: Vec<D>) -> Vec<D> {
    if ds.len() == 2 && matches!(ds[0], D::Neg(_)) && !matches!(ds[1], D::Neg(_)) {
        ds.swap(0, 1);
    }
    ds
}

fn prod_d(v: &[Expr], fmt: &Fmt) -> D {
    let mut negative = false;
    let mut num_coef: Option<D> = None; // positive numerator coefficient
    let mut den_coef: Option<D> = None;
    let mut numer: Vec<Expr> = Vec::new();
    let mut denom: Vec<Expr> = Vec::new();
    for f in v {
        match f {
            Num(r) => {
                negative ^= r.is_negative();
                if !r.numer().abs().is_one() {
                    num_coef = Some(D::Num(r.numer().abs().to_string()));
                }
                if !r.denom().is_one() {
                    den_coef = Some(D::Num(r.denom().to_string()));
                }
            }
            Float(x) => {
                negative ^= x.0 < 0.0;
                num_coef = Some(float_d(x.0.abs(), fmt));
            }
            Pow(b, ex) if ex.is_negative_term() && !matches!(**b, Const(K::E)) => denom.push(pow((**b).clone(), neg((**ex).clone()))),
            other => numer.push(other.clone()),
        }
    }
    let build = |mut fs: Vec<Expr>, coef: Option<D>| -> Vec<D> {
        fs.sort_by_cached_key(factor_class);
        let mut out: Vec<D> = Vec::new();
        if let Some(c) = coef {
            out.push(c);
        }
        // merge numeric square roots: sqrt(2) sqrt(3) -> sqrt(6)
        let mut radicand = BigInt::one();
        let mut rest: Vec<Expr> = Vec::new();
        for f in fs {
            match &f {
                Pow(b, ex) if **ex == rat(1, 2) && b.as_int().map_or(false, |n| n.is_positive()) => radicand *= b.as_int().unwrap(),
                _ => rest.push(f),
            }
        }
        if !radicand.is_one() {
            out.push(D::Sqrt(Box::new(D::Num(radicand.to_string()))));
        }
        out.extend(rest.iter().map(|f| to_d(f, fmt)));
        out
    };
    let n = build(numer, num_coef);
    let d = build(denom, den_coef);
    let wrap = |mut fs: Vec<D>| -> D {
        match fs.len() {
            0 => D::Num("1".into()),
            1 => fs.pop().unwrap(),
            _ => D::Prod(fs),
        }
    };
    let body = if d.is_empty() { wrap(n) } else { D::Frac(Box::new(wrap(n)), Box::new(wrap(d))) };
    if negative {
        D::Neg(Box::new(body))
    } else {
        body
    }
}

fn pow_d(b: &Expr, ex: &Expr, fmt: &Fmt) -> D {
    if let Num(q) = ex {
        if *q == Rat::new(BigInt::one(), BigInt::from(2)) {
            return D::Sqrt(Box::new(to_d(b, fmt)));
        }
        if *q == Rat::new(BigInt::one(), BigInt::from(3)) {
            return D::Root(Box::new(to_d(b, fmt)), Box::new(D::Num("3".into())));
        }
    }
    if let Func(name, args) = b {
        if let Some(k) = ex.as_i64() {
            if k < 0 {
                if let Some((_, rec)) = RECIPROCAL.iter().find(|(n, _)| *n == &**name) {
                    let a: Vec<D> = args.iter().map(|x| to_d(x, fmt)).collect();
                    return if k == -1 { D::Call(rec.to_string(), a) } else { D::CallPow(rec.to_string(), Box::new(D::Num((-k).to_string())), a) };
                }
            } else if k > 1 && matches!(&**name, "sin" | "cos" | "tan" | "sinh" | "cosh" | "tanh" | "ln") && args.len() == 1 {
                let a: Vec<D> = args.iter().map(|x| to_d(x, fmt)).collect();
                return D::CallPow(pretty_fn(name), Box::new(D::Num(k.to_string())), a);
            }
        }
    }
    if ex.is_negative_term() && !matches!(b, Const(K::E)) {
        let inner = pow(b.clone(), neg(ex.clone()));
        return D::Frac(Box::new(D::Num("1".into())), Box::new(to_d(&inner, fmt)));
    }
    D::Pow(Box::new(to_d(b, fmt)), Box::new(to_d(ex, fmt)))
}

fn func_d(name: &str, args: &[Expr], fmt: &Fmt) -> D {
    call_d(name, args.iter().map(|x| to_d(x, fmt)).collect())
}

/// Presentation of a named function applied to already-formatted arguments.
fn tk(args: &mut [D], k: usize) -> Box<D> {
    Box::new(std::mem::replace(&mut args[k], D::Num(String::new())))
}

pub fn call_d(name: &str, mut args: Vec<D>) -> D {
    let n = args.len();
    let one = || Box::new(D::Num("1".into()));
    match (name, n) {
        ("abs", 1) => D::Abs(tk(&mut args, 0)),
        ("factorial", 1) => D::Fact(tk(&mut args, 0)),
        ("factorial2", 1) => D::Postfix(tk(&mut args, 0), "!!".into()),
        ("sqrt", 1) => D::Sqrt(tk(&mut args, 0)),
        ("cbrt", 1) => D::Root(tk(&mut args, 0), Box::new(D::Num("3".into()))),
        ("exp", 1) => D::Pow(Box::new(D::Ident("e".into())), tk(&mut args, 0)),
        ("Integral", 2) => D::Integral { body: tk(&mut args, 0), var: tk(&mut args, 1), lo: None, hi: None },
        ("Integral", 4) => D::Integral { body: tk(&mut args, 0), var: tk(&mut args, 1), lo: Some(tk(&mut args, 2)), hi: Some(tk(&mut args, 3)) },
        ("Sum", 4) => D::BigOp { sym: "∑", body: tk(&mut args, 0), var: tk(&mut args, 1), lo: tk(&mut args, 2), hi: tk(&mut args, 3) },
        ("Product", 4) => D::BigOp { sym: "∏", body: tk(&mut args, 0), var: tk(&mut args, 1), lo: tk(&mut args, 2), hi: tk(&mut args, 3) },
        ("Limit", 3) => D::Limit { body: tk(&mut args, 0), var: tk(&mut args, 1), to: tk(&mut args, 2), dir: 0 },
        ("Limit", 4) => {
            let dir = match &args[3] {
                D::Neg(_) => -1,
                D::Num(s) if s == "1" => 1,
                _ => 0,
            };
            D::Limit { body: tk(&mut args, 0), var: tk(&mut args, 1), to: tk(&mut args, 2), dir }
        }
        ("Derivative", 2) => D::Deriv { body: tk(&mut args, 0), var: tk(&mut args, 1), order: one() },
        ("Derivative", 3) => D::Deriv { body: tk(&mut args, 0), var: tk(&mut args, 1), order: tk(&mut args, 2) },
        ("not", 1) => D::Prefix("¬".into(), tk(&mut args, 0)),
        ("and" | "or" | "xor" | "implies" | "iff", k) if k >= 2 => {
            let op = match name {
                "and" => "∧",
                "or" => "∨",
                "xor" => "⊻",
                "implies" => "⇒",
                _ => "⇔",
            };
            let mut it = args.into_iter();
            let mut acc = it.next().unwrap();
            for x in it {
                acc = D::Infix(Box::new(acc), op.into(), Box::new(x));
            }
            acc
        }
        ("mod", 2) => D::Infix(tk(&mut args, 0), "mod".into(), tk(&mut args, 1)),
        ("log", 2) => {
            let b = sub_text(&args[0]);
            D::Call(format!("log_{b}"), vec![*tk(&mut args, 1)])
        }
        ("log10", 1) => D::Call("log_10".into(), args),
        ("log2", 1) => D::Call("log_2".into(), args),
        ("besselj" | "bessely" | "besseli" | "besselk" | "polylog", 2) => {
            let b = sub_text(&args[0]);
            D::Call(format!("{}_{}", pretty_fn(name), b), vec![*tk(&mut args, 1)])
        }
        ("fibonacci" | "lucas" | "harmonic" | "prime" | "bernoulli", 1) => D::Sub(Box::new(D::Ident(pretty_fn(name))), tk(&mut args, 0)),
        _ => {
            if let Some((_, base)) = INVERSE_NAMES.iter().find(|(nm, _)| *nm == name) {
                if n == 1 {
                    return D::CallPow(base.to_string(), Box::new(D::Neg(one())), args);
                }
            }
            D::Call(pretty_fn(name), args)
        }
    }
}

// ---------------------------------------------------------------------------
// D -> text
// ---------------------------------------------------------------------------

const GREEK: &[(&str, &str)] = &[
    ("alpha", "α"),
    ("beta", "β"),
    ("gamma", "γ"),
    ("delta", "δ"),
    ("epsilon", "ε"),
    ("zeta", "ζ"),
    ("eta", "η"),
    ("theta", "θ"),
    ("iota", "ι"),
    ("kappa", "κ"),
    ("lambda", "λ"),
    ("mu", "μ"),
    ("nu", "ν"),
    ("xi", "ξ"),
    ("rho", "ρ"),
    ("sigma", "σ"),
    ("tau", "τ"),
    ("upsilon", "υ"),
    ("phi", "φ"),
    ("chi", "χ"),
    ("psi", "ψ"),
    ("omega", "ω"),
    ("Gamma", "Γ"),
    ("Delta", "Δ"),
    ("Theta", "Θ"),
    ("Lambda", "Λ"),
    ("Xi", "Ξ"),
    ("Sigma", "Σ"),
    ("Phi", "Φ"),
    ("Psi", "Ψ"),
    ("Omega", "Ω"),
];

/// Split an identifier into its base (with Greek names replaced) and subscript.
pub fn ident_parts(name: &str) -> (String, Option<String>) {
    let greek = |s: &str| GREEK.iter().find(|(n, _)| *n == s).map(|(_, g)| g.to_string()).unwrap_or_else(|| s.to_string());
    if let Some((b, s)) = name.split_once('_') {
        if !b.is_empty() && !s.is_empty() {
            return (greek(b), Some(greek(s)));
        }
    }
    let digits_at = name.find(|c: char| c.is_ascii_digit());
    if let Some(k) = digits_at {
        if k > 0 && name[k..].chars().all(|c| c.is_ascii_digit()) {
            return (greek(&name[..k]), Some(name[k..].to_string()));
        }
    }
    (greek(name), None)
}

fn ident_text(name: &str) -> String {
    match ident_parts(name) {
        (b, Some(s)) => format!("{b}_{s}"),
        (b, None) => b,
    }
}

fn is_atom(d: &D) -> bool {
    matches!(d, D::Num(_) | D::Ident(_) | D::Text(_) | D::Call(..) | D::CallPow(..) | D::Abs(_) | D::Sqrt(_) | D::List(_) | D::Tuple(_) | D::Matrix(_) | D::Sub(..))
}

/// Text of `d` in a context that binds with strength `prec`
/// (1 relation, 2 logic, 4 sum, 5 product, 6 power base, 7 exponent, 9 atom).
fn text_d(d: &D, prec: u8) -> String {
    let (s, own) = match d {
        D::Num(s) => (s.clone(), 9),
        D::Ident(s) => (ident_text(s), 9),
        D::Text(s) => (s.clone(), 9),
        D::Sum(ts) => {
            let mut out = String::new();
            for (k, t) in ts.iter().enumerate() {
                match t {
                    D::Neg(x) => {
                        out.push_str(if k == 0 { "-" } else { " - " });
                        out.push_str(&text_d(x, 5));
                    }
                    x => {
                        if k > 0 {
                            out.push_str(" + ");
                        }
                        out.push_str(&text_d(x, 4));
                    }
                }
            }
            (out, 3)
        }
        D::Neg(x) => (format!("-{}", text_d(x, 5)), 3),
        D::Prod(fs) => {
            let mut out = String::new();
            for (k, f) in fs.iter().enumerate() {
                if k > 0 {
                    let prev_num = matches!(fs[k - 1], D::Num(_));
                    let cur_numlike = matches!(f, D::Num(_)) || matches!(f, D::Pow(b, _) if matches!(**b, D::Num(_)));
                    out.push_str(if prev_num && cur_numlike { "×" } else { " " });
                }
                let needs = matches!(f, D::Frac(..));
                let t = text_d(f, 5);
                if needs {
                    out.push_str(&format!("({t})"));
                } else {
                    out.push_str(&t);
                }
            }
            (out, 5)
        }
        D::Frac(n, q) => {
            let ns = match &**n {
                D::Prod(_) | D::Sum(_) | D::Neg(_) | D::Frac(..) => format!("({})", text_d(n, 0)),
                x => text_d(x, 6),
            };
            let qs = match &**q {
                D::Prod(_) | D::Sum(_) | D::Neg(_) | D::Frac(..) => format!("({})", text_d(q, 0)),
                x => text_d(x, 6),
            };
            (format!("{ns}/{qs}"), 5)
        }
        D::Pow(b, ex) => {
            let bs = if is_atom(b) && !matches!(**b, D::CallPow(..)) { text_d(b, 9) } else { format!("({})", text_d(b, 0)) };
            let simple_exp = match &**ex {
                D::Num(s) => s.chars().all(|c| c.is_ascii_digit() || c == '.'),
                D::Ident(_) => true,
                _ => false,
            };
            let es = if simple_exp { text_d(ex, 9) } else { format!("({})", text_d(ex, 0)) };
            (format!("{bs}^{es}"), 6)
        }
        D::Sqrt(x) => (format!("sqrt({})", text_d(x, 0)), 9),
        D::Root(x, n) => {
            let bs = if is_atom(x) { text_d(x, 9) } else { format!("({})", text_d(x, 0)) };
            (format!("{bs}^(1/{})", text_d(n, 9)), 6)
        }
        D::Call(name, args) => (format!("{}({})", name, args.iter().map(|a| text_d(a, 0)).collect::<Vec<_>>().join(", ")), 9),
        D::CallPow(name, ex, args) => {
            let es = match &**ex {
                D::Num(s) => s.clone(),
                other => format!("({})", text_d(other, 0)),
            };
            (format!("{}^{}({})", name, es, args.iter().map(|a| text_d(a, 0)).collect::<Vec<_>>().join(", ")), 9)
        }
        D::Abs(x) => (format!("|{}|", text_d(x, 0)), 9),
        D::Fact(x) => {
            let bs = if matches!(**x, D::Num(_) | D::Ident(_)) { text_d(x, 9) } else { format!("({})", text_d(x, 0)) };
            (format!("{bs}!"), 8)
        }
        D::List(v) => (format!("{{{}}}", v.iter().map(|a| text_d(a, 0)).collect::<Vec<_>>().join(", ")), 9),
        D::Tuple(v) => (format!("({})", v.iter().map(|a| text_d(a, 0)).collect::<Vec<_>>().join(", ")), 9),
        D::Matrix(rows) => (format!("{{{}}}", rows.iter().map(|r| format!("{{{}}}", r.iter().map(|a| text_d(a, 0)).collect::<Vec<_>>().join(", "))).collect::<Vec<_>>().join(", ")), 9),
        D::Rel(parts, ops) => {
            let mut out = text_d(&parts[0], 2);
            for (k, op) in ops.iter().enumerate() {
                out.push_str(&format!(" {} {}", op, text_d(&parts[k + 1], 2)));
            }
            (out, 1)
        }
        D::Infix(a, op, b) => (format!("{} {} {}", text_d(a, 3), op, text_d(b, 3)), 2),
        D::Prefix(op, a) => (format!("{}{}", op, text_d(a, 6)), 5),
        D::Postfix(a, op) => (format!("{}{}", text_d(a, 6), op), 8),
        D::Integral { body, var, lo, hi } => {
            let b = match &**body {
                D::Sum(_) | D::Neg(_) => format!("({})", text_d(body, 0)),
                x => text_d(x, 5),
            };
            let lim = match (lo, hi) {
                (Some(l), Some(h)) => format!("_{}^{}", sub_text(l), sub_text(h)),
                _ => String::new(),
            };
            (format!("∫{} {} d{}", lim, b, text_d(var, 9)), 3)
        }
        D::BigOp { sym, body, var, lo, hi } => {
            let b = match &**body {
                D::Sum(_) | D::Neg(_) => format!("({})", text_d(body, 0)),
                x => text_d(x, 5),
            };
            (format!("{}_({}={})^{} {}", sym, text_d(var, 9), text_d(lo, 0), sub_text(hi), b), 3)
        }
        D::Limit { body, var, to, dir } => {
            let side = match dir {
                1 => "⁺",
                -1 => "⁻",
                _ => "",
            };
            let b = match &**body {
                D::Sum(_) | D::Neg(_) => format!("({})", text_d(body, 0)),
                x => text_d(x, 5),
            };
            (format!("lim_({}→{}{}) {}", text_d(var, 9), text_d(to, 0), side, b), 3)
        }
        D::Deriv { body, var, order } => {
            let o = text_d(order, 9);
            let head = if o == "1" { format!("d/d{}", text_d(var, 9)) } else { format!("d^{o}/d{}^{o}", text_d(var, 9)) };
            (format!("{}({})", head, text_d(body, 0)), 9)
        }
        D::Sub(a, b) => (format!("{}_{}", text_d(a, 9), sub_text(b)), 9),
    };
    if own < prec {
        format!("({s})")
    } else {
        s
    }
}

fn sub_text(d: &D) -> String {
    match d {
        D::Num(_) | D::Ident(_) => text_d(d, 9),
        other => format!("({})", text_d(other, 0)),
    }
}

pub fn d_text(d: &D) -> String {
    text_d(d, 0)
}

/// Plain-text form of an expression.
pub fn text(e: &Expr) -> String {
    text_with(e, &Fmt::default())
}

pub fn text_with(e: &Expr, fmt: &Fmt) -> String {
    text_d(&to_d(e, fmt), 0)
}

pub fn text_digits(e: &Expr, digits: usize) -> String {
    text_with(e, &Fmt { digits, ascending: false })
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&text(self))
    }
}

// ---------------------------------------------------------------------------
// D -> MathNode (two-dimensional layout)
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "t", content = "c")]
pub enum MathNode {
    /// Upright text: numbers, operators, function names.
    Text(String),
    /// Italic identifier.
    Ident(String),
    Row(Vec<MathNode>),
    Frac(Box<MathNode>, Box<MathNode>),
    Sup(Box<MathNode>, Box<MathNode>),
    Sub(Box<MathNode>, Box<MathNode>),
    SubSup(Box<MathNode>, Box<MathNode>, Box<MathNode>),
    Sqrt(Box<MathNode>, Option<Box<MathNode>>),
    Fenced(String, String, Box<MathNode>),
    Matrix(Vec<Vec<MathNode>>),
}

fn t(s: &str) -> MathNode {
    MathNode::Text(s.to_string())
}

fn op(s: &str) -> MathNode {
    MathNode::Text(format!(" {s} "))
}

fn paren(n: MathNode) -> MathNode {
    MathNode::Fenced("(".into(), ")".into(), Box::new(n))
}

fn ident_node(name: &str) -> MathNode {
    match ident_parts(name) {
        (b, Some(s)) => MathNode::Sub(Box::new(MathNode::Ident(b)), Box::new(MathNode::Text(s))),
        (b, None) => {
            if b.chars().count() > 1 && b.is_ascii() {
                MathNode::Text(b)
            } else {
                MathNode::Ident(b)
            }
        }
    }
}

fn node_p(d: &D, prec: u8) -> MathNode {
    let (n, own) = node_inner(d);
    if own < prec {
        paren(n)
    } else {
        n
    }
}

pub fn d_node(d: &D) -> MathNode {
    node_p(d, 0)
}

fn comma_row(items: &[D]) -> MathNode {
    let mut row = Vec::new();
    for (k, a) in items.iter().enumerate() {
        if k > 0 {
            row.push(t(", "));
        }
        row.push(node_p(a, 0));
    }
    MathNode::Row(row)
}

fn node_inner(d: &D) -> (MathNode, u8) {
    match d {
        D::Num(s) => (t(s), 9),
        D::Ident(s) => (ident_node(s), 9),
        D::Text(s) => (t(s), 9),
        D::Sum(ts) => {
            let mut row = Vec::new();
            for (k, term) in ts.iter().enumerate() {
                match term {
                    D::Neg(x) => {
                        row.push(if k == 0 { t("−") } else { op("−") });
                        row.push(node_p(x, 5));
                    }
                    x => {
                        if k > 0 {
                            row.push(op("+"));
                        }
                        row.push(node_p(x, 4));
                    }
                }
            }
            (MathNode::Row(row), 3)
        }
        D::Neg(x) => (MathNode::Row(vec![t("−"), node_p(x, 5)]), 3),
        D::Prod(fs) => {
            let mut row = Vec::new();
            for (k, f) in fs.iter().enumerate() {
                if k > 0 {
                    let prev_num = matches!(fs[k - 1], D::Num(_));
                    let cur_numlike = matches!(f, D::Num(_)) || matches!(f, D::Pow(b, _) if matches!(**b, D::Num(_)));
                    row.push(if prev_num && cur_numlike { t("×") } else { t("\u{2009}") });
                }
                row.push(node_p(f, 5));
            }
            (MathNode::Row(row), 5)
        }
        D::Frac(n, q) => (MathNode::Frac(Box::new(node_p(n, 0)), Box::new(node_p(q, 0))), 5),
        D::Pow(b, ex) => {
            let base = if is_atom(b) && !matches!(**b, D::CallPow(..)) { node_p(b, 0) } else { paren(node_p(b, 0)) };
            (MathNode::Sup(Box::new(base), Box::new(node_p(ex, 0))), 6)
        }
        D::Sqrt(x) => (MathNode::Sqrt(Box::new(node_p(x, 0)), None), 9),
        D::Root(x, n) => (MathNode::Sqrt(Box::new(node_p(x, 0)), Some(Box::new(node_p(n, 0)))), 9),
        D::Call(name, args) => {
            let head = match name.split_once('_') {
                Some((b, s)) if !b.is_empty() => MathNode::Sub(Box::new(t(b)), Box::new(t(s))),
                _ => t(name),
            };
            let node = match (name.as_str(), args.len()) {
                ("floor", 1) => MathNode::Fenced("⌊".into(), "⌋".into(), Box::new(node_p(&args[0], 0))),
                ("ceil", 1) => MathNode::Fenced("⌈".into(), "⌉".into(), Box::new(node_p(&args[0], 0))),
                ("binomial", 2) => MathNode::Fenced("(".into(), ")".into(), Box::new(MathNode::Matrix(vec![vec![node_p(&args[0], 0)], vec![node_p(&args[1], 0)]]))),
                _ => MathNode::Row(vec![head, paren(comma_row(args))]),
            };
            (node, 9)
        }
        D::CallPow(name, ex, args) => (MathNode::Row(vec![MathNode::Sup(Box::new(t(name)), Box::new(node_p(ex, 0))), paren(comma_row(args))]), 9),
        D::Abs(x) => (MathNode::Fenced("|".into(), "|".into(), Box::new(node_p(x, 0))), 9),
        D::Fact(x) => {
            let base = if matches!(**x, D::Num(_) | D::Ident(_)) { node_p(x, 0) } else { paren(node_p(x, 0)) };
            (MathNode::Row(vec![base, t("!")]), 8)
        }
        D::List(v) => (MathNode::Fenced("{".into(), "}".into(), Box::new(comma_row(v))), 9),
        D::Tuple(v) => (paren(comma_row(v)), 9),
        D::Matrix(rows) => (MathNode::Fenced("(".into(), ")".into(), Box::new(MathNode::Matrix(rows.iter().map(|r| r.iter().map(|x| node_p(x, 0)).collect()).collect()))), 9),
        D::Rel(parts, ops) => {
            let mut row = vec![node_p(&parts[0], 2)];
            for (k, o) in ops.iter().enumerate() {
                row.push(op(o));
                row.push(node_p(&parts[k + 1], 2));
            }
            (MathNode::Row(row), 1)
        }
        D::Infix(a, o, b) => (MathNode::Row(vec![node_p(a, 3), op(o), node_p(b, 3)]), 2),
        D::Prefix(o, a) => (MathNode::Row(vec![t(o), node_p(a, 6)]), 5),
        D::Postfix(a, o) => (MathNode::Row(vec![node_p(a, 6), t(o)]), 8),
        D::Integral { body, var, lo, hi } => {
            let sign = match (lo, hi) {
                (Some(l), Some(h)) => MathNode::SubSup(Box::new(t("∫")), Box::new(node_p(l, 0)), Box::new(node_p(h, 0))),
                _ => t("∫"),
            };
            let b = match &**body {
                D::Sum(_) | D::Neg(_) => paren(node_p(body, 0)),
                x => node_p(x, 5),
            };
            (MathNode::Row(vec![sign, t("\u{2009}"), b, t("\u{2009}"), MathNode::Ident("d".into()), node_p(var, 9)]), 3)
        }
        D::BigOp { sym, body, var, lo, hi } => {
            let lower = MathNode::Row(vec![node_p(var, 9), t("="), node_p(lo, 0)]);
            let b = match &**body {
                D::Sum(_) | D::Neg(_) => paren(node_p(body, 0)),
                x => node_p(x, 5),
            };
            (MathNode::Row(vec![MathNode::SubSup(Box::new(t(sym)), Box::new(lower), Box::new(node_p(hi, 0))), t("\u{2009}"), b]), 3)
        }
        D::Limit { body, var, to, dir } => {
            let mut target = vec![node_p(var, 9), t("→"), node_p(to, 0)];
            match dir {
                1 => target = vec![node_p(var, 9), t("→"), MathNode::Sup(Box::new(node_p(to, 6)), Box::new(t("+")))],
                -1 => target = vec![node_p(var, 9), t("→"), MathNode::Sup(Box::new(node_p(to, 6)), Box::new(t("−")))],
                _ => {}
            }
            let b = match &**body {
                D::Sum(_) | D::Neg(_) => paren(node_p(body, 0)),
                x => node_p(x, 5),
            };
            (MathNode::Row(vec![MathNode::Sub(Box::new(t("lim")), Box::new(MathNode::Row(target))), t("\u{2009}"), b]), 3)
        }
        D::Deriv { body, var, order } => {
            let dd = MathNode::Ident("d".into());
            let frac = if **order == D::Num("1".into()) {
                MathNode::Frac(Box::new(dd.clone()), Box::new(MathNode::Row(vec![dd, node_p(var, 9)])))
            } else {
                let o = node_p(order, 0);
                MathNode::Frac(
                    Box::new(MathNode::Sup(Box::new(dd.clone()), Box::new(o.clone()))),
                    Box::new(MathNode::Row(vec![dd, MathNode::Sup(Box::new(node_p(var, 9)), Box::new(o))])),
                )
            };
            (MathNode::Row(vec![frac, paren(node_p(body, 0))]), 9)
        }
        D::Sub(a, b) => (MathNode::Sub(Box::new(node_p(a, 9)), Box::new(node_p(b, 0))), 9),
    }
}

/// Two-dimensional layout of an expression.
pub fn node(e: &Expr) -> MathNode {
    d_node(&to_d(e, &Fmt::default()))
}

pub fn node_with(e: &Expr, fmt: &Fmt) -> MathNode {
    d_node(&to_d(e, fmt))
}

// ---------------------------------------------------------------------------
// MathNode -> LaTeX
// ---------------------------------------------------------------------------

fn latex_text(s: &str, italic: bool) -> String {
    const MAP: &[(&str, &str)] = &[
        ("π", "\\pi "),
        ("∞", "\\infty "),
        ("×", "\\times "),
        ("−", "-"),
        ("≤", "\\le "),
        ("≥", "\\ge "),
        ("≠", "\\ne "),
        ("→", "\\to "),
        ("∫", "\\int "),
        ("∑", "\\sum "),
        ("∏", "\\prod "),
        ("∧", "\\land "),
        ("∨", "\\lor "),
        ("¬", "\\lnot "),
        ("⊻", "\\oplus "),
        ("∈", "\\in "),
        ("ℤ", "\\mathbb{Z}"),
        ("ℝ", "\\mathbb{R}"),
        ("∪", "\\cup "),
        ("≈", "\\approx "),
        ("±", "\\pm "),
        ("°", "^{\\circ}"),
        ("\u{2009}", "\\,"),
        ("α", "\\alpha "),
        ("β", "\\beta "),
        ("γ", "\\gamma "),
        ("δ", "\\delta "),
        ("ε", "\\epsilon "),
        ("ζ", "\\zeta "),
        ("η", "\\eta "),
        ("θ", "\\theta "),
        ("ι", "\\iota "),
        ("κ", "\\kappa "),
        ("λ", "\\lambda "),
        ("μ", "\\mu "),
        ("ν", "\\nu "),
        ("ξ", "\\xi "),
        ("ρ", "\\rho "),
        ("σ", "\\sigma "),
        ("τ", "\\tau "),
        ("υ", "\\upsilon "),
        ("φ", "\\varphi "),
        ("ϕ", "\\phi "),
        ("χ", "\\chi "),
        ("ψ", "\\psi "),
        ("ω", "\\omega "),
        ("Γ", "\\Gamma "),
        ("Δ", "\\Delta "),
        ("Θ", "\\Theta "),
        ("Λ", "\\Lambda "),
        ("Ξ", "\\Xi "),
        ("Σ", "\\Sigma "),
        ("Φ", "\\Phi "),
        ("Ψ", "\\Psi "),
        ("Ω", "\\Omega "),
        ("{", "\\{"),
        ("}", "\\}"),
        ("%", "\\%"),
        ("_", "\\_"),
        ("&", "\\&"),
        ("#", "\\#"),
    ];
    let trimmed = s.trim();
    if !italic && trimmed.len() > 1 && trimmed.chars().all(|c| c.is_ascii_alphabetic()) {
        const KNOWN: &[&str] = &["sin", "cos", "tan", "cot", "sec", "csc", "sinh", "cosh", "tanh", "coth", "log", "ln", "exp", "lim", "min", "max", "gcd", "det", "arg", "deg"];
        return if KNOWN.contains(&trimmed) { format!("\\{trimmed} ") } else { format!("\\operatorname{{{trimmed}}}") };
    }
    if !italic && trimmed.len() > 1 && trimmed.chars().any(|c| c == ' ') && trimmed.chars().all(|c| c.is_ascii_alphabetic() || c == ' ') {
        return format!("\\text{{{s}}}");
    }
    let mut out = String::new();
    for ch in s.chars() {
        let cs = ch.to_string();
        match MAP.iter().find(|(k, _)| *k == cs) {
            Some((_, v)) => out.push_str(v),
            None => out.push(ch),
        }
    }
    out
}

pub fn node_latex(n: &MathNode) -> String {
    match n {
        MathNode::Text(s) => latex_text(s, false),
        MathNode::Ident(s) => latex_text(s, true),
        MathNode::Row(v) => v.iter().map(node_latex).collect::<Vec<_>>().join(""),
        MathNode::Frac(a, b) => format!("\\frac{{{}}}{{{}}}", node_latex(a), node_latex(b)),
        MathNode::Sup(a, b) => format!("{{{}}}^{{{}}}", node_latex(a), node_latex(b)),
        MathNode::Sub(a, b) => format!("{{{}}}_{{{}}}", node_latex(a), node_latex(b)),
        MathNode::SubSup(a, b, c) => format!("{}_{{{}}}^{{{}}}", node_latex(a), node_latex(b), node_latex(c)),
        MathNode::Sqrt(a, None) => format!("\\sqrt{{{}}}", node_latex(a)),
        MathNode::Sqrt(a, Some(k)) => format!("\\sqrt[{}]{{{}}}", node_latex(k), node_latex(a)),
        MathNode::Fenced(l, r, a) => {
            let fence = |s: &str| match s {
                "{" => "\\{".to_string(),
                "}" => "\\}".to_string(),
                "⌊" => "\\lfloor ".to_string(),
                "⌋" => "\\rfloor ".to_string(),
                "⌈" => "\\lceil ".to_string(),
                "⌉" => "\\rceil ".to_string(),
                other => other.to_string(),
            };
            format!("\\left{}{}\\right{}", fence(l), node_latex(a), fence(r))
        }
        MathNode::Matrix(rows) => {
            let body = rows.iter().map(|r| r.iter().map(node_latex).collect::<Vec<_>>().join(" & ")).collect::<Vec<_>>().join(" \\\\ ");
            format!("\\begin{{matrix}}{body}\\end{{matrix}}")
        }
    }
}

pub fn latex(e: &Expr) -> String {
    node_latex(&node(e))
}

/// Flatten a layout tree back to a single line (used for copy/paste of nodes
/// that have no expression behind them).
pub fn node_plain(n: &MathNode) -> String {
    match n {
        MathNode::Text(s) | MathNode::Ident(s) => s.clone(),
        MathNode::Row(v) => v.iter().map(node_plain).collect(),
        MathNode::Frac(a, b) => format!("({})/({})", node_plain(a), node_plain(b)),
        MathNode::Sup(a, b) => format!("{}^({})", node_plain(a), node_plain(b)),
        MathNode::Sub(a, b) => format!("{}_({})", node_plain(a), node_plain(b)),
        MathNode::SubSup(a, b, c) => format!("{}_({})^({})", node_plain(a), node_plain(b), node_plain(c)),
        MathNode::Sqrt(a, None) => format!("sqrt({})", node_plain(a)),
        MathNode::Sqrt(a, Some(k)) => format!("root({}, {})", node_plain(a), node_plain(k)),
        MathNode::Fenced(l, r, a) => format!("{}{}{}", l, node_plain(a), r),
        MathNode::Matrix(rows) => format!("{{{}}}", rows.iter().map(|r| format!("{{{}}}", r.iter().map(node_plain).collect::<Vec<_>>().join(", "))).collect::<Vec<_>>().join(", ")),
    }
}
