//! A broad natural-language front end.  It recognises many ways of asking
//! for something the engine can already do and either rewrites the question
//! into the engine's canonical form or answers it directly.

use super::*;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::numtheory as nt;
use crate::parse;
use crate::print::text as ptext;
use num_bigint::BigInt;
use num_traits::{One, Signed, ToPrimitive, Zero};
use regex::Regex;
use std::cell::Cell;
use std::sync::OnceLock;

macro_rules! re {
    ($s:expr) => {{
        static R: OnceLock<Regex> = OnceLock::new();
        R.get_or_init(|| Regex::new($s).unwrap())
    }};
}

thread_local!(static DEPTH: Cell<u32> = const { Cell::new(0) });

struct DepthGuard;
impl Drop for DepthGuard {
    fn drop(&mut self) {
        DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
    }
}

/// Run a rewritten query through every handler.
pub fn again(q: &Query, new_text: &str) -> Option<Answer> {
    if DEPTH.with(|d| d.get()) >= 3 {
        return None;
    }
    DEPTH.with(|d| d.set(d.get() + 1));
    let _g = DepthGuard;
    let clean = clean_query(new_text);
    let q2 = Query { raw: new_text, lower: clean.to_lowercase(), clean, opts: q.opts };
    for (name, h) in super::handlers() {
        if name == "encyclopedia" {
            continue;
        }
        if let Some(a) = h(&q2) {
            if !a.pods.is_empty() {
                return Some(a);
            }
        }
    }
    None
}

pub(crate) fn ex(t: &str) -> Option<Expr> {
    parse::parse(t.trim()).ok().map(|p| p.expr)
}

pub(crate) fn main_var(e: &Expr) -> Expr {
    let fs = e.free_symbols();
    for c in ["x", "t", "y", "z", "n", "k"] {
        if fs.iter().any(|n| &**n == c) {
            return sym(c);
        }
    }
    fs.into_iter().next().map(|n| sym(&n)).unwrap_or_else(|| sym("x"))
}

pub(crate) fn bound(t: &str) -> Option<Expr> {
    match t.trim().to_lowercase().as_str() {
        "inf" | "infinity" | "∞" | "+inf" | "+infinity" | "oo" => Some(inf()),
        "-inf" | "-infinity" | "-∞" | "-oo" | "minus infinity" | "negative infinity" => Some(neg(inf())),
        _ => ex(t),
    }
}

/// Split on commas, semicolons and the word "and" outside brackets.
pub(crate) fn split_top(t: &str) -> Vec<String> {
    let t = t.replace(" and ", " , ").replace(';', ",");
    let mut out = Vec::new();
    let (mut depth, mut cur) = (0i32, String::new());
    for ch in t.chars() {
        match ch {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            _ => {}
        }
        if ch == ',' && depth == 0 {
            out.push(cur.trim().to_string());
            cur.clear();
        } else {
            cur.push(ch);
        }
    }
    if !cur.trim().is_empty() {
        out.push(cur.trim().to_string());
    }
    out.into_iter().filter(|p| !p.is_empty()).collect()
}

pub(crate) fn answer(interp: impl Into<String>, input: impl Into<String>, mut pods: Vec<Pod>) -> Option<Answer> {
    let interp = interp.into();
    let mut all = vec![pod("Input", "Input interpretation", text(input.into()))];
    all.append(&mut pods);
    Some(Answer::new(interp, all))
}

pub(crate) fn decimal_pod(e: &Expr) -> Option<Pod> {
    if e.is_integer() || !e.free_symbols().is_empty() {
        return None;
    }
    let d = crate::bigfloat::decimal(e, 20)?;
    Some(pod("DecimalApproximation", "Decimal approximation", text(format!("{d}…"))))
}

/// Re-run `text` and move the pods named in `ids` to the top as the result.
pub(crate) fn promote(q: &Query, new_text: &str, ids: &[&str]) -> Option<Answer> {
    let mut a = again(q, new_text)?;
    let k = a.pods.iter().position(|p| ids.contains(&p.id.as_str()))?;
    for p in a.pods.iter_mut() {
        p.primary = false;
    }
    let mut chosen = a.pods.remove(k);
    chosen.primary = true;
    let at = a.pods.iter().position(|p| p.id == "Input").map_or(0, |i| i + 1);
    a.pods.insert(at, chosen);
    Some(a)
}

pub fn handle(q: &Query) -> Option<Answer> {
    if DEPTH.with(|d| d.get()) >= 3 {
        return None;
    }
    let l = q.lower.as_str();
    let c = q.clean.as_str();
    if let Some(new) = rewrite(l, c) {
        if new.to_lowercase() != l {
            if let Some(a) = again(q, &new) {
                return Some(a);
            }
        }
    }
    systems(q, l, c)
        .or_else(|| numbers(q, l, c))
        .or_else(|| super::nl_math::algebra(q, l, c))
        .or_else(|| super::nl_math::calculus_q(q, l, c))
        .or_else(|| super::nl_math::matrices(q, l, c))
        .or_else(|| super::nl_math::statistics(q, l, c))
}

pub(crate) fn caps(r: &Regex, l: &str, c: &str) -> Option<Vec<Option<String>>> {
    r.captures(l).map(|m| (0..m.len()).map(|k| m.get(k).and_then(|g| c.get(g.start()..g.end())).map(|t| t.to_string())).collect())
}

// ---------------------------------------------------------------------------
// rewrites into canonical queries
// ---------------------------------------------------------------------------

fn rewrite(l: &str, c: &str) -> Option<String> {
    let cap = |r: &Regex| caps(r, l, c);
    let g = |m: &Vec<Option<String>>, k: usize| m.get(k).cloned().flatten().unwrap_or_default();

    // 1 + 2 + 3 + ... + 100
    if let Some(m) = cap(re!(r"^(?:sum\s+(?:of\s+)?)?(-?\d+)\s*([+*])\s*(-?\d+)\s*[+*]\s*(?:(-?\d+)\s*[+*]\s*)?(?:\.\.\.|…|\.\.)\s*[+*]\s*(-?\d+)$")) {
        let (a, b, last): (i64, i64, i64) = (g(&m, 1).parse().ok()?, g(&m, 3).parse().ok()?, g(&m, 5).parse().ok()?);
        let third: Option<i64> = g(&m, 4).parse().ok();
        let op = if g(&m, 2) == "+" { "sum" } else { "product" };
        let d = b - a;
        let arithmetic = d != 0 && third.map_or(true, |t| t - b == d) && (last - a) % d == 0 && (last - a) / d >= 2;
        if arithmetic {
            return Some(format!("{op} {a} + {d}*k, k=0 to {}", (last - a) / d));
        }
        // geometric progression: 1 + 2 + 4 + ... + 1024
        if a != 0 && b % a == 0 && (b / a).abs() >= 2 && third.map_or(true, |t| b != 0 && t % b == 0 && t / b == b / a) {
            let r = b / a;
            let (mut term, mut n) = (a, 0i64);
            while term.abs() < last.abs() && n < 200 {
                term *= r;
                n += 1;
            }
            if term == last {
                return Some(format!("{op} {a}*({r})^k, k=0 to {n}"));
            }
        }
        return None;
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:absolute value|modulus|magnitude)\s+of\s+(.+)$")) {
        if ex(&g(&m, 1)).map_or(false, |e| e.free_symbols().is_empty() && !e.is_list()) {
            return Some(format!("abs({})", g(&m, 1)));
        }
    }
    if let Some(m) = cap(re!(r"^(?:plot|graph|draw|sketch)\s+(?:the\s+)?(?:line\s+|function\s+|curve\s+|parabola\s+)?(?:y|f\s*\(\s*x\s*\))\s*=\s*([^=]+)$")) {
        if ex(&g(&m, 1)).map_or(false, |e| !e.has_sym(&sym("y"))) {
            return Some(format!("plot {}", g(&m, 1)));
        }
    }
    if let Some(m) = cap(re!(r"^(?:how many\s+)?(?:prime numbers|primes)\s+(?:are there\s+|are\s+|exist\s+)(?:less than|below|up to|under|before|smaller than)\s+(\d+)$")) {
        return Some(format!("primes below {}", g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?prime (?:factori[sz]ation|factors|decomposition)\s+(?:of|for)\s+(.+)$")) {
        return Some(format!("factor {}", g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^(gcd|gcf|hcf|lcm|greatest common (?:divisor|factor)|highest common factor|least common multiple|lowest common multiple|mean|average|median|mode|standard deviation|variance|range)\s+(?:of\s+)?(.+?),?\s+and\s+(.+)$")) {
        let kw = match g(&m, 1).to_lowercase().as_str() {
            "gcf" | "hcf" | "greatest common factor" | "highest common factor" | "greatest common divisor" => "gcd".to_string(),
            "lowest common multiple" | "least common multiple" => "lcm".to_string(),
            k => k.to_string(),
        };
        return Some(format!("{kw} of {}, {}", g(&m, 2), g(&m, 3)));
    }
    if let Some(m) = cap(re!(r"^(gcf|hcf|greatest common factor|highest common factor|lowest common multiple)\s+(?:of\s+)?(.+)$")) {
        let kw = if g(&m, 1).to_lowercase().starts_with('l') { "lcm" } else { "gcd" };
        return Some(format!("{kw} of {}", g(&m, 2)));
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(sum|product|difference|quotient)\s+of\s+(-?[\d.]+)\s+and\s+(-?[\d.]+)$")) {
        let op = match g(&m, 1).to_lowercase().as_str() {
            "sum" => "+",
            "product" => "*",
            "difference" => "-",
            _ => "/",
        };
        return Some(format!("({}) {op} ({})", g(&m, 2), g(&m, 3)));
    }
    // sums without an explicit index
    if let Some(m) = cap(re!(r"^(sum|product)\s+(?:of\s+)?(.+?)\s+(?:for\s+)?from\s+(-?[\w.]+)\s+to\s+(-?[\w.]+)$")) {
        let f = ex(&g(&m, 2))?;
        let mut fs: Vec<String> = f.free_symbols().iter().map(|n| n.to_string()).collect();
        let hi = g(&m, 4);
        fs.retain(|v| *v != hi);
        let pick = ["k", "n", "i", "j", "m"].iter().find(|v| fs.iter().any(|f| f == *v)).map(|v| v.to_string()).or_else(|| fs.first().cloned())?;
        return Some(format!("{} {}, {pick}={} to {}", g(&m, 1), g(&m, 2), g(&m, 3), hi));
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?sum of the first\s+(\w+)\s+(natural numbers|positive integers|integers|whole numbers|counting numbers|odd numbers|even numbers|squares|square numbers|cubes|cube numbers)$")) {
        let term = match g(&m, 2).to_lowercase().as_str() {
            "odd numbers" => "2k-1",
            "even numbers" => "2k",
            "squares" | "square numbers" => "k^2",
            "cubes" | "cube numbers" => "k^3",
            _ => "k",
        };
        return Some(format!("sum {term}, k=1 to {}", g(&m, 1)));
    }
    // series
    if l.contains("series expansion") {
        return Some(c.to_lowercase().replace("series expansion", "series"));
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?taylor polynomial of (?:degree|order)\s+(\d+)\s+(?:for|of)\s+(.+?)(\s+(?:at|about|around)\s+.+)?$")) {
        return Some(format!("series of {}{} to order {}", g(&m, 2), g(&m, 3), g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(\w+)(?:st|nd|rd|th)[- ](?:degree|order) taylor polynomial (?:for|of)\s+(.+?)(\s+(?:at|about|around)\s+.+)?$")) {
        return Some(format!("series of {}{} to order {}", g(&m, 2), g(&m, 3), g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^((?:taylor |maclaurin |power |laurent )?series)\s+(?:of\s+|for\s+)?(.+?)\s+(?:at|about|around|centered at|centred at)\s+([^=]+)$")) {
        let f = ex(&g(&m, 2))?;
        return Some(format!("series of {} at {} = {}", g(&m, 2), ptext(&main_var(&f)), g(&m, 3)));
    }
    // plots
    if let Some(m) = cap(re!(r"^(?:plot|graph|draw|sketch)(?:\s+of)?\s+(.+?)\s+(?:from|for|between|on|over)\s+(?:([a-z])\s*=\s*)?(-?[^\s,]+)\s+(?:to|and)\s+(-?[^\s,]+)$")) {
        let f = ex(g(&m, 1).trim_start_matches("y =").trim_start_matches("y="))?;
        let v = if g(&m, 2).is_empty() { ptext(&main_var(&f)) } else { g(&m, 2) };
        return Some(format!("plot {}, {v}={} to {}", g(&m, 1), g(&m, 3), g(&m, 4)));
    }
    if let Some(m) = cap(re!(r"^(?:3d|3-d|three[- ]dimensional|surface|contour|density)\s+(?:plot|graph)(?:\s+of)?\s+(.+)$")) {
        return Some(format!("plot {}", g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^(?:plot|graph|draw|sketch)\s+(?:of\s+|the function\s+|the graph of\s+|the curve\s+)(.+)$")) {
        return Some(format!("plot {}", g(&m, 1)));
    }
    // extrema
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:local|relative)\s+(?:maxima|minima|maximum|minimum|maximums|minimums|max|min|extrema|extremes)(?:\s+(?:and|or)\s+(?:maxima|minima|maximum|minimum|max|min))?\s+of\s+(.+)$")) {
        return Some(format!("local extrema of {}", g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:maxima|minima|extreme values|maxima and minima|minima and maxima|max and min|turning points?|stationary points?)\s+of\s+(.+)$")) {
        return Some(format!("local extrema of {}", g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:global|absolute|greatest|largest|smallest|least)?\s*(maximum|minimum|max|min)(?:imum)?\s+value\s+of\s+(.+)$")) {
        let kw = if g(&m, 1).to_lowercase().starts_with("max") { "maximum" } else { "minimum" };
        return Some(format!("{kw} of {}", g(&m, 2)));
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:global|absolute)\s+(maximum|minimum)\s+of\s+(.+)$")) {
        return Some(format!("{} of {}", g(&m, 1), g(&m, 2)));
    }
    // derivatives
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:(first|second|third)\s+)?(?:order\s+)?partial derivative of\s+(.+)$")) {
        let ord = match g(&m, 1).to_lowercase().as_str() {
            "second" => "second ",
            "third" => "third ",
            _ => "",
        };
        return Some(format!("{ord}derivative of {}", g(&m, 2)));
    }
    if let Some(m) = cap(re!(r"^differentiate\s+(.+?)\s+(twice|two times|three times|thrice)$")) {
        let ord = if g(&m, 2).starts_with("tw") { "second" } else { "third" };
        return Some(format!("{ord} derivative of {}", g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:rate of change|derivative) of\s+(.+?)\s+with respect to\s+([a-z])$")) {
        return Some(format!("derivative of {} wrt {}", g(&m, 1), g(&m, 2)));
    }
    // integrals
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:definite |indefinite )?integral of\s+(.+?)\s+with respect to\s+([a-z])(.*)$")) {
        return Some(format!("integrate {} d{}{}", g(&m, 1), g(&m, 2), g(&m, 3)));
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?area under (?:the curve\s+)?(?:y\s*=\s*)?(.+?)\s+(?:from|between)\s+(?:x\s*=\s*)?(.+?)\s+(?:to|and)\s+(?:x\s*=\s*)?(.+)$")) {
        return Some(format!("integrate {} from {} to {}", g(&m, 1), g(&m, 2), g(&m, 3)));
    }
    // limits
    if let Some(m) = cap(re!(r"^(?:the\s+)?limit of\s+(.+?)\s+(?:as|when)\s+([a-z])\s+(?:approaches|goes to|tends to|gets close to|->)\s+(.+)$")) {
        let to = g(&m, 3).replace("positive infinity", "infinity").replace("negative infinity", "-infinity").replace("minus infinity", "-infinity");
        return Some(format!("limit of {} as {}->{}", g(&m, 1), g(&m, 2), to));
    }
    // primes
    if let Some(m) = cap(re!(r"^how many (?:prime numbers|primes)\s+(?:are there\s+|are\s+)?(?:less than|below|up to|under|before|smaller than)\s+(\d+)$")) {
        return Some(format!("primes below {}", g(&m, 1)));
    }
    // bases
    if let Some(m) = cap(re!(r"^convert\s+(.+?)\s+(?:in|to|into)\s+(binary|octal|hex|hexadecimal|base\s*\d+|roman(?: numerals)?)$")) {
        return Some(format!("{} to {}", g(&m, 1), g(&m, 2)));
    }
    // logarithms and small word operators
    if let Some(m) = cap(re!(r"^log(?:arithm)?(?:\s+(?:to|in|with))?(?:\s+the)?\s+base\s+(\S+)\s+of\s+(.+)$")) {
        return Some(format!("log({}, {})", g(&m, 1), g(&m, 2)));
    }
    if let Some(m) = cap(re!(r"^log(?:arithm)?(?:\s+of)?\s+(\S+)\s+(?:to|in|with)?\s*(?:the\s+)?base\s+(\S+)$")) {
        return Some(format!("log({}, {})", g(&m, 2), g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(reciprocal|multiplicative inverse|square|cube|half|double|twice|triple|a third|one third|a quarter|one quarter|negative|opposite|additive inverse)\s+(?:of\s+)?(-?[\d(][^,]*|pi|e)$")) {
        let x = g(&m, 2);
        return Some(match g(&m, 1).to_lowercase().as_str() {
            "reciprocal" | "multiplicative inverse" => format!("1/({x})"),
            "square" => format!("({x})^2"),
            "cube" => format!("({x})^3"),
            "half" => format!("({x})/2"),
            "double" | "twice" => format!("2*({x})"),
            "triple" => format!("3*({x})"),
            "a third" | "one third" => format!("({x})/3"),
            "a quarter" | "one quarter" => format!("({x})/4"),
            _ => format!("-({x})"),
        });
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(sine|cosine|tangent|cotangent|secant|cosecant|arcsine|arccosine|arctangent|inverse sine|inverse cosine|inverse tangent|hyperbolic sine|hyperbolic cosine|natural logarithm|logarithm|exponential)\s+of\s+(.+)$")) {
        let f = match g(&m, 1).to_lowercase().as_str() {
            "sine" => "sin",
            "cosine" => "cos",
            "tangent" => "tan",
            "cotangent" => "cot",
            "secant" => "sec",
            "cosecant" => "csc",
            "arcsine" | "inverse sine" => "asin",
            "arccosine" | "inverse cosine" => "acos",
            "arctangent" | "inverse tangent" => "atan",
            "hyperbolic sine" => "sinh",
            "hyperbolic cosine" => "cosh",
            "natural logarithm" => "ln",
            "logarithm" => "log",
            _ => "exp",
        };
        return Some(format!("{f}({})", g(&m, 2)));
    }
    // "f(x) = 2x + 3" style wrappers
    if let Some(m) = cap(re!(r"^inverse (?:function )?of\s+(?:[a-z]\s*\(\s*[a-z]\s*\)|y)\s*=\s*(.+)$")) {
        return Some(format!("inverse of {}", g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^(derivative|integral|antiderivative|domain|range|roots|zeros|plot|graph|extrema|critical points|inflection points|asymptotes|limit|series) (of\s+)?[fgh]\s*\(\s*[a-z]\s*\)\s*=\s*(.+)$")) {
        return Some(format!("{} {}{}", g(&m, 1), g(&m, 2), g(&m, 3)));
    }
    if let Some(m) = cap(re!(r"^(?:x[- ]intercepts?|zeroes) of\s+(?:y\s*=\s*)?(.+)$")) {
        return Some(format!("roots of {}", g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^(?:where|when) (?:does|is)\s+(.+?)\s+(?:equal|equals|equal to|=)\s+(.+)$")) {
        return Some(format!("solve {} = {}", g(&m, 1), g(&m, 2)));
    }
    if let Some(m) = cap(re!(r"^(?:find|solve for|determine)\s+([a-z])\s+(?:if|when|where|such that|given(?: that)?|in|:)\s+(.+=.+)$")) {
        return Some(format!("solve {} for {}", g(&m, 2), g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^solve\s+for\s+([a-z])\s*[:,]?\s+(.+=.+)$")) {
        return Some(format!("solve {} for {}", g(&m, 2), g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^truth table\s*(?:of|for)?\s+(.+)$")) {
        return Some(g(&m, 1));
    }
    // statistics
    if let Some(m) = cap(re!(r"^(.*p\s*\(.+\).*?)\s+(?:for|of|in|under|with)\s+(?:a\s+|the\s+)?standard normal(?: distribution)?$")) {
        return Some(format!("normal distribution mean 0 sd 1 {}", g(&m, 1)));
    }
    if let Some(m) = cap(re!(r"^(linear|quadratic|exponential)?\s*(?:fit|regression|least squares|best fit(?: line)?)\s*(?:of|for|through)?\s*((?:\(\s*-?[\d.]+\s*,\s*-?[\d.]+\s*\)\s*,?\s*(?:and\s+)?)+)$")) {
        let pts = g(&m, 2).replace(" and ", ", ").replace('(', "{").replace(')', "}");
        let kind = if g(&m, 1).is_empty() { "linear".to_string() } else { g(&m, 1) };
        return Some(format!("{kind} fit {{{}}}", pts.trim().trim_end_matches(',')));
    }
    // physical constants phrased as "mass of the electron"
    if let Some(m) = cap(re!(r"^(?:rest\s+)?(mass|charge|radius)\s+of\s+(?:the\s+|an?\s+)?(electron|proton|neutron)$")) {
        return Some(format!("{} {}", g(&m, 2), g(&m, 1)));
    }
    // dates
    if let Some(m) = cap(re!(r"^(?:what\s+|which\s+)?day(?: of the week)?\s+(?:was|is|will be|will|for|on|does)\s+(?:it\s+)?(?:on\s+)?(.+?)(?:\s+(?:fall on|be))?$")) {
        if crate::domains::misc::parse_date(&g(&m, 1)).is_some() {
            return Some(g(&m, 1));
        }
    }
    None
}

// ---------------------------------------------------------------------------
// systems of equations written with commas or "and"
// ---------------------------------------------------------------------------

fn systems(q: &Query, l: &str, c: &str) -> Option<Answer> {
    if l.matches('=').count() < 2 || l.contains("==") || l.contains("{") || l.contains('\'') {
        return None;
    }
    let m = caps(re!(r"^(?:solve\s+)?(?:the\s+)?(?:system(?: of equations)?\s*:?\s*)?(.+?)(?:\s+for\s+([a-z](?:\s*(?:,|and)\s*[a-z])*))?$"), l, c)?;
    let body = m[1].clone()?;
    let parts = split_top(&body);
    if parts.len() < 2 || !parts.iter().all(|p| p.matches('=').count() == 1 && !p.contains('<') && !p.contains('>')) {
        return None;
    }
    // every part must be an equation that parses
    for p in &parts {
        if !matches!(ex(p)?, Rel(RelOp::Eq, ..)) {
            return None;
        }
    }
    let braces = format!("{{{}}}", parts.join(", "));
    match m[2].as_deref() {
        Some(vs) => again(q, &format!("solve {braces} for {}", vs.replace(" and ", ", "))),
        None => again(q, &braces),
    }
}

// ---------------------------------------------------------------------------
// numbers
// ---------------------------------------------------------------------------

fn int_of(t: &str) -> Option<BigInt> {
    let e = ex(&t.replace(',', ""))?;
    e.as_int().cloned()
}

fn join_big(v: &[BigInt]) -> String {
    v.iter().map(|d| d.to_string()).collect::<Vec<_>>().join(", ")
}

fn rng() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    static STATE: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let mut x = STATE.load(std::sync::atomic::Ordering::Relaxed);
    if x == 0 {
        x = SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_nanos() as u64).unwrap_or(0x9E37_79B9_7F4A_7C15) | 1;
    }
    x ^= x << 13;
    x ^= x >> 7;
    x ^= x << 17;
    STATE.store(x, std::sync::atomic::Ordering::Relaxed);
    x.wrapping_mul(0x2545_F491_4F6C_DD1D)
}

fn numbers(q: &Query, l: &str, c: &str) -> Option<Answer> {
    let cap = |r: &Regex| caps(r, l, c);
    let g = |m: &Vec<Option<String>>, k: usize| m.get(k).cloned().flatten().unwrap_or_default();

    // divisors
    if let Some(m) = cap(re!(r"^(?:all\s+)?(?:the\s+)?(?:positive\s+)?(number of\s+|how many\s+|sum of\s+(?:the\s+)?)?(?:divisors|factors)\s+(?:of|does)\s+(.+?)(?:\s+have)?$")) {
        let n = int_of(&g(&m, 2))?;
        if n.is_zero() || n.bits() > 80 {
            return None;
        }
        let ds = nt::divisors(&n.abs());
        let what = g(&m, 1).to_lowercase();
        let main = if what.starts_with("number") || what.starts_with("how") {
            pod("Result", "Number of divisors", text(ds.len().to_string()))
        } else if what.starts_with("sum") {
            pod("Result", "Sum of divisors", text(ds.iter().sum::<BigInt>().to_string()))
        } else {
            pod("Result", "Divisors", text(join_big(&ds)))
        };
        let mut pods = vec![primary(main)];
        pods.push(pod("Count", "Number of divisors", text(ds.len().to_string())));
        pods.push(pod("Sum", "Sum of divisors", text(ds.iter().sum::<BigInt>().to_string())));
        pods.push(pod("Factorization", "Prime factorization", text(nt::factor(&n.abs()).iter().map(|(p, k)| if *k == 1 { p.to_string() } else { format!("{p}^{k}") }).collect::<Vec<_>>().join(" × "))));
        pods.dedup_by(|a, b| a.title == b.title);
        return answer(format!("divisors of {n}"), format!("divisors of {n}"), pods);
    }
    // next / previous prime
    if let Some(m) = cap(re!(r"^(?:the\s+)?(next|first|smallest|previous|last|largest|greatest|biggest)\s+prime(?: number)?\s+(after|greater than|larger than|bigger than|above|over|before|less than|smaller than|below|under)\s+(.+)$")) {
        let n = int_of(&g(&m, 3))?;
        let up = matches!(g(&m, 2).to_lowercase().as_str(), "after" | "greater than" | "larger than" | "bigger than" | "above" | "over");
        let p = if up { nt::next_prime(&n) } else { nt::prev_prime(&n)? };
        let label = if up { format!("next prime after {n}") } else { format!("previous prime before {n}") };
        let other = if up { nt::prev_prime(&(&n + 1)).map(|v| ("Previous prime", v)) } else { Some(("Next prime", nt::next_prime(&(&n - 1)))) };
        let mut pods = vec![primary(pod("Result", "Result", text(p.to_string())))];
        pods.push(pod("Gap", "Distance", text(format!("{}", (&p - &n).abs()))));
        if let Some((t, v)) = other {
            pods.push(pod("Other", t, text(v.to_string())));
        }
        return answer(label.clone(), label, pods);
    }
    // primes in a range, first n primes
    if let Some(m) = cap(re!(r"^(?:list\s+(?:of\s+)?|all\s+)?(?:the\s+)?(?:prime numbers|primes)\s+(?:between|from)\s+(\d+)\s+(?:and|to)\s+(\d+)$")) {
        let (a, b): (u64, u64) = (g(&m, 1).parse().ok()?, g(&m, 2).parse().ok()?);
        if b > 10_000_000 || b < a {
            return None;
        }
        let ps: Vec<u64> = nt::sieve(b as usize).into_iter().filter(|p| *p >= a).collect();
        let shown = ps.iter().take(400).map(|p| p.to_string()).collect::<Vec<_>>().join(", ");
        let label = format!("primes between {a} and {b}");
        return answer(label.clone(), label, vec![primary(pod("Result", "Primes", text(if ps.len() > 400 { format!("{shown}, …") } else { shown }))), pod("Count", "Count", text(ps.len().to_string()))]);
    }
    if let Some(m) = cap(re!(r"^(?:list\s+(?:of\s+)?)?(?:the\s+)?first\s+(\d+)\s+(?:prime numbers|primes)$")) {
        let n: usize = g(&m, 1).parse().ok()?;
        if n == 0 || n > 2000 {
            return None;
        }
        let ps: Vec<u64> = nt::sieve(20 * n + 100).into_iter().take(n).collect();
        let label = format!("first {n} primes");
        return answer(label.clone(), label, vec![primary(pod("Result", "Primes", text(ps.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", ")))), pod("Sum", "Sum", text(ps.iter().sum::<u64>().to_string()))]);
    }
    // divisibility
    let div = cap(re!(r"^is\s+(.+?)\s+(?:evenly\s+)?(?:divisible by|a multiple of)\s+(.+)$"))
        .map(|m| (g(&m, 1), g(&m, 2)))
        .or_else(|| cap(re!(r"^(?:is\s+(.+?)\s+a (?:factor|divisor) of|does\s+(.+?)\s+divide(?:\s+into)?)\s+(.+)$")).map(|m| (g(&m, 3), if g(&m, 1).is_empty() { g(&m, 2) } else { g(&m, 1) })));
    if let Some((a, b)) = div {
        let (a, b) = (int_of(&a)?, int_of(&b)?);
        if b.is_zero() {
            return None;
        }
        let (qv, r) = (&a / &b, &a % &b);
        let yes = r.is_zero();
        let label = format!("is {a} divisible by {b}");
        let detail = if yes { format!("{a} = {b} × {qv}") } else { format!("{a} = {b} × {qv} + {r}") };
        return answer(label.clone(), label, vec![primary(pod("Result", "Result", text(if yes { format!("Yes, {a} is divisible by {b}.") } else { format!("No, {a} is not divisible by {b} (remainder {r}).") }))), pod("Division", "Division", text(detail))]);
    }
    // more number properties
    if let Some(m) = cap(re!(r"^is\s+(.+?)\s+(?:an?\s+)?(perfect|palindrome|palindromic|fibonacci|triangular|power of (?:2|two)|perfect cube|cube|composite|abundant|deficient|square[- ]free|squarefree|happy)(?: number)?$")) {
        let n = int_of(&g(&m, 1))?;
        if n.is_negative() || n.bits() > 64 {
            return None;
        }
        let kind = g(&m, 2).to_lowercase();
        let sig = || nt::sigma(1, &n) - &n;
        let digits = n.to_string();
        let (yes, why) = match kind.as_str() {
            "perfect" => (!n.is_zero() && sig() == n, format!("the sum of its proper divisors is {}", sig())),
            "abundant" => (sig() > n, format!("the sum of its proper divisors is {}", sig())),
            "deficient" => (sig() < n, format!("the sum of its proper divisors is {}", sig())),
            "palindrome" | "palindromic" => (digits.chars().rev().collect::<String>() == digits, format!("reversed it reads {}", digits.chars().rev().collect::<String>())),
            "fibonacci" => {
                let five = &n * &n * 5;
                (nt::is_square(&(&five + 4)) || nt::is_square(&(&five - 4)), "n is a Fibonacci number exactly when 5n² ± 4 is a perfect square".to_string())
            }
            "triangular" => (nt::is_square(&(&n * 8 + 1)), "n is triangular exactly when 8n + 1 is a perfect square".to_string()),
            "power of 2" | "power of two" => (!n.is_zero() && nt::to_base(&n, 2).matches('1').count() == 1, format!("in binary it is {}", nt::to_base(&n, 2))),
            "perfect cube" | "cube" => {
                let r = n.to_f64().map(|v| v.cbrt().round()).and_then(BigInt::from_f64_checked).unwrap_or_default();
                (&r * &r * &r == n, format!("the nearest integer cube root is {r}"))
            }
            "composite" => (n > BigInt::one() && !nt::is_prime(&n), format!("its factorization is {}", nt::factor(&n).iter().map(|(p, k)| if *k == 1 { p.to_string() } else { format!("{p}^{k}") }).collect::<Vec<_>>().join(" × "))),
            "happy" => {
                let mut v = n.clone();
                let mut seen = std::collections::HashSet::new();
                while v != BigInt::one() && seen.insert(v.clone()) {
                    v = v.to_string().bytes().map(|b| BigInt::from((b - b'0') as u32).pow(2)).sum();
                }
                (v.is_one(), "repeatedly summing the squares of its digits is followed until it reaches 1 or loops".to_string())
            }
            _ => (nt::factor(&n).iter().all(|(_, k)| *k == 1), "a square-free number has no repeated prime factor".to_string()),
        };
        let label = format!("is {n} a {kind} number");
        return answer(label.clone(), label, vec![primary(pod("Result", "Result", text(format!("{}, {n} is {}a {kind} number.", if yes { "Yes" } else { "No" }, if yes { "" } else { "not " })))), pod("Reason", "Explanation", text(why))]);
    }
    // digit sums and digit counts
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:sum of (?:the\s+)?digits|digit sum|digital sum)\s+(?:of|in)\s+(.+)$")) {
        let n = ex(&g(&m, 1))?.as_int().cloned()?;
        let label = format!("sum of the digits of {}", ptext(&big(n.clone())));
        let ds = nt::digit_sum(&n.abs(), 10);
        let mut root = ds.clone();
        while root >= BigInt::from(10) {
            root = nt::digit_sum(&root, 10);
        }
        return answer(label.clone(), label, vec![primary(pod("Result", "Digit sum", text(ds.to_string()))), pod("Root", "Digital root", text(root.to_string()))]);
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:number of digits|how many digits)\s+(?:of|in|does|are in|are there in)\s+(.+?)(?:\s+have)?$")) {
        let e = ex(&g(&m, 1))?;
        let n = e.as_int().cloned()?;
        let label = format!("number of digits of {}", g(&m, 1));
        return answer(label.clone(), label, vec![primary(pod("Result", "Number of decimal digits", text(n.abs().to_string().len().to_string()))), pod("Binary", "Number of binary digits", text(n.bits().to_string()))]);
    }
    if let Some(m) = cap(re!(r"^(?:euler(?:'s)?\s+)?(?:totient|phi)(?: function)?\s*(?:of\s+)?\(?(\d+)\)?$")) {
        let n = int_of(&g(&m, 1))?;
        let label = format!("φ({n})");
        return answer(label.clone(), format!("Euler's totient of {n}"), vec![primary(pod("Result", "Result", text(nt::totient(&n).to_string())))]);
    }
    // arrangements
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:number of\s+)?(?:permutations|arrangements|orderings|ways to (?:arrange|order|line up|seat))\s+(?:of\s+)?(\d+)\s+(?:distinct\s+|different\s+)?(?:objects|items|things|people|elements|books|letters|students)?$")) {
        let n: u64 = g(&m, 1).parse().ok()?;
        if n > 5000 {
            return None;
        }
        let label = format!("permutations of {n} objects");
        return answer(label.clone(), label, vec![primary(pod("Result", "Result", math(&eq(raw_fn("factorial", vec![int(n as i64)]), big(nt::factorial(n))))))]);
    }
    if let Some(m) = cap(re!(r"^(?:the\s+)?(?:number of\s+)?(?:(?:distinct\s+)?(?:permutations|arrangements|anagrams)|ways to arrange)\s+(?:of\s+)?(?:the\s+)?(?:letters\s+(?:of|in)\s+)?(?:the word\s+)?([a-z]+)$")) {
        let w = g(&m, 1).to_lowercase();
        if w.len() < 2 || w.len() > 60 {
            return None;
        }
        let mut counts = std::collections::BTreeMap::new();
        for ch in w.chars() {
            *counts.entry(ch).or_insert(0u64) += 1;
        }
        let mut total = nt::factorial(w.len() as u64);
        for k in counts.values() {
            total /= nt::factorial(*k);
        }
        let reps: Vec<String> = counts.iter().filter(|(_, k)| **k > 1).map(|(ch, k)| format!("{ch} × {k}")).collect();
        let formula = format!("{}! / ({})", w.len(), counts.values().filter(|k| **k > 1).map(|k| format!("{k}!")).collect::<Vec<_>>().join(" · "));
        let label = format!("distinct arrangements of the letters of \"{w}\"");
        let mut pods = vec![primary(pod("Result", "Result", text(total.to_string())))];
        pods.push(pod("Formula", "Formula", text(if reps.is_empty() { format!("{}!", w.len()) } else { formula })));
        if !reps.is_empty() {
            pods.push(pod("Repeats", "Repeated letters", text(reps.join(", "))));
        }
        return answer(label.clone(), label, pods);
    }
    // digits of a constant or expression
    let digits = cap(re!(r"^(.+?)\s+to\s+(\d+)\s+(digits|decimal places|decimals|places|significant (?:figures|digits)|sig figs?|dp|sf)$"))
        .map(|m| (g(&m, 1), g(&m, 2), g(&m, 3)))
        .or_else(|| cap(re!(r"^(?:the\s+)?(?:first\s+)?(\d+)\s+(digits|decimal places|decimals)\s+of\s+(.+)$")).map(|m| (g(&m, 3), g(&m, 1), g(&m, 2))))
        .or_else(|| cap(re!(r"^n\s*[\[(]\s*(.+)\s*,\s*(\d+)\s*[\])]$")).map(|m| (g(&m, 1), g(&m, 2), "digits".to_string())));
    if let Some((body, n, unit)) = digits {
        let body = body.trim_start_matches("round ").trim_start_matches("value of ").to_string();
        let n: usize = n.parse().ok()?;
        let e = ex(&body)?;
        let small = unit.starts_with("decimal") || unit == "places" || unit == "dp" || unit.starts_with("sig") || unit == "sf";
        if e.free_symbols().is_empty() && !e.has_const(K::I) && n <= 12 && (e.has_float() || small) {
            let v = e.to_f64_const()?;
            let places = unit.starts_with("decimal") || unit == "places" || unit == "dp";
            let r = if places {
                format!("{:.*}", n, v)
            } else {
                let mag = if v == 0.0 { 0 } else { v.abs().log10().floor() as i32 + 1 };
                let scale = 10f64.powi(n as i32 - mag);
                crate::stats::fmt((v * scale).round() / scale)
            };
            let label = format!("{} rounded to {n} {unit}", ptext(&e));
            return answer(label.clone(), label, vec![primary(pod("Result", "Result", text(r)))]);
        }
        if e.free_symbols().is_empty() && n <= 100_000 && n >= 1 {
            let places = unit.starts_with("decimal") || unit == "places" || unit == "dp";
            let int_digits = e.to_f64_const().map_or(1, |v| if v.abs() < 1.0 { 0 } else { v.abs().log10().floor() as usize + 1 });
            let sig = if places { n + int_digits } else { n };
            // "to n digits" rounds; "the first n digits of" truncates
            let rounding = !c.to_lowercase().contains("first");
            let d = if rounding {
                let raw = crate::bigfloat::decimal(&e, sig.max(1) + 6)?;
                round_decimal(raw.trim_end_matches('…'), sig.max(1)).unwrap_or(crate::bigfloat::decimal(&e, sig.max(1))?)
            } else {
                crate::bigfloat::decimal(&e, sig.max(1))?
            };
            let label = format!("{} to {n} {unit}", ptext(&e));
            let mut pods = vec![primary(pod("Result", "Result", text(d)))];
            if !e.is_numeric() {
                pods.insert(0, pod("Exact", "Exact form", math(&e)));
            }
            return answer(label.clone(), label, pods);
        }
    }
    // rounding
    if let Some(m) = cap(re!(r"^round\s+(.+?)\s+to\s+(?:the\s+)?nearest\s+(integer|whole number|ten|tens|hundred|hundreds|thousand|thousands|million|tenth|tenths|hundredth|hundredths|thousandth|thousandths|[\d.]+)$")) {
        let v = ex(&g(&m, 1))?.to_f64_const()?;
        let step = match g(&m, 2).to_lowercase().trim_end_matches('s') {
            "integer" | "whole number" => 1.0,
            "ten" => 10.0,
            "hundred" => 100.0,
            "thousand" => 1000.0,
            "million" => 1e6,
            "tenth" => 0.1,
            "hundredth" => 0.01,
            "thousandth" => 0.001,
            other => other.parse().ok()?,
        };
        let r = (v / step).round() * step;
        let label = format!("{} rounded to the nearest {}", g(&m, 1), g(&m, 2));
        return answer(label.clone(), label, vec![primary(pod("Result", "Result", text(crate::stats::fmt(r))))]);
    }
    // numbers written in another base
    let from_base = cap(re!(r"^(?:convert\s+)?(?:(binary|octal|hex|hexadecimal)\s+(?:number\s+)?)?(0x[0-9a-f]+|0b[01]+|0o[0-7]+|[0-9a-f]+)(?:\s+(?:in\s+)?(binary|octal|hex|hexadecimal|base\s*\d+))?\s+(?:to|in|into|as)\s+(decimal|base\s*10|base ten|denary|binary|octal|hex|hexadecimal|base\s*\d+)$"));
    if let Some(m) = from_base {
        let tok = g(&m, 2).to_lowercase();
        let named = if g(&m, 1).is_empty() { g(&m, 3) } else { g(&m, 1) }.to_lowercase();
        let base_of = |t: &str| -> Option<u32> {
            Some(match t {
                "binary" => 2,
                "octal" => 8,
                "hex" | "hexadecimal" => 16,
                "decimal" | "denary" | "base ten" => 10,
                t => t.trim_start_matches("base").trim().parse().ok()?,
            })
        };
        let (digits, from) = if let Some(h) = tok.strip_prefix("0x") {
            (h.to_string(), 16)
        } else if let Some(b) = tok.strip_prefix("0b") {
            (b.to_string(), 2)
        } else if let Some(o) = tok.strip_prefix("0o") {
            (o.to_string(), 8)
        } else if !named.is_empty() {
            (tok.clone(), base_of(&named)?)
        } else {
            return None;
        };
        let to = base_of(&g(&m, 4).to_lowercase())?;
        if !(2..=36).contains(&from) || !(2..=36).contains(&to) {
            return None;
        }
        let n = nt::from_base(&digits, from)?;
        let out = if to == 10 { n.to_string() } else { nt::to_base(&n, to).to_uppercase() };
        let label = format!("{}₍{from}₎ in base {to}", digits.to_uppercase());
        let mut pods = vec![primary(pod("Result", "Result", text(format!("{out}{}", if to == 10 { String::new() } else { format!(" (base {to})") }))))];
        let rows: Vec<Vec<Content>> = [(2, "binary"), (8, "octal"), (10, "decimal"), (16, "hexadecimal")].iter().map(|(b, name)| vec![text(*name), text(if *b == 10 { n.to_string() } else { nt::to_base(&n, *b).to_uppercase() })]).collect();
        pods.push(pod("Bases", "Other bases", table(None, rows)));
        return answer(label.clone(), label, pods);
    }
    // random numbers, coins and dice
    if let Some(m) = cap(re!(r"^(?:pick\s+|choose\s+|generate\s+|give me\s+)?(?:a\s+)?random\s+(?:number|integer|int)\s+(?:between|from)\s+(-?\d+)\s+(?:and|to)\s+(-?\d+)$")) {
        let (a, b): (i64, i64) = (g(&m, 1).parse().ok()?, g(&m, 2).parse().ok()?);
        let (a, b) = (a.min(b), a.max(b));
        let v = a + (rng() % ((b - a + 1) as u64)) as i64;
        let label = format!("random integer between {a} and {b}");
        return answer(label.clone(), label, vec![primary(pod("Result", "Result", text(v.to_string()))), pod("Note", "Note", text("Each query draws a new value, uniformly distributed over the range."))]);
    }
    if re!(r"^(?:flip|toss)\s+a\s+coin$|^coin\s+(?:flip|toss)$|^heads or tails$").is_match(l) {
        let v = if rng() % 2 == 0 { "heads" } else { "tails" };
        return answer("coin flip", "flip a fair coin", vec![primary(pod("Result", "Result", text(v)))]);
    }
    if let Some(m) = cap(re!(r"^roll\s+(?:(a|an|one|two|three|four|five|six|\d+)\s+)?(?:(\d+)[- ]sided\s+)?(?:die|dice)$|^roll\s+(\d+)d(\d+)$")) {
        let count = match g(&m, 1).as_str() {
            "" | "a" | "an" | "one" => g(&m, 3).parse().unwrap_or(1),
            "two" => 2,
            "three" => 3,
            "four" => 4,
            "five" => 5,
            "six" => 6,
            n => n.parse().ok()?,
        };
        let sides: u64 = if !g(&m, 4).is_empty() { g(&m, 4).parse().ok()? } else { g(&m, 2).parse().unwrap_or(6) };
        if count == 0 || count > 100 || sides < 2 {
            return None;
        }
        let rolls: Vec<u64> = (0..count).map(|_| rng() % sides + 1).collect();
        let label = format!("roll {count} {sides}-sided {}", if count == 1 { "die" } else { "dice" });
        let mut pods = vec![primary(pod("Result", "Result", text(rolls.iter().map(|r| r.to_string()).collect::<Vec<_>>().join(", "))))];
        if count > 1 {
            pods.push(pod("Total", "Total", text(rolls.iter().sum::<u64>().to_string())));
        }
        return answer(label.clone(), label, pods);
    }
    let _ = q;
    None
}

trait FromF64Checked: Sized {
    fn from_f64_checked(v: f64) -> Option<Self>;
}
impl FromF64Checked for BigInt {
    fn from_f64_checked(v: f64) -> Option<BigInt> {
        num_traits::FromPrimitive::from_f64(v)
    }
}

/// Round a positional decimal string to `sig` significant digits (half up).
fn round_decimal(s: &str, sig: usize) -> Option<String> {
    let (neg, body) = match s.strip_prefix('-') {
        Some(r) => (true, r),
        None => (false, s),
    };
    if !body.chars().all(|c| c.is_ascii_digit() || c == '.') || body.matches('.').count() > 1 {
        return None;
    }
    let point = body.find('.').unwrap_or(body.len());
    let mut digits: Vec<u8> = body.bytes().filter(|b| b.is_ascii_digit()).map(|b| b - b'0').collect();
    let first = digits.iter().position(|d| *d != 0)?;
    let keep = first + sig;
    if keep >= digits.len() {
        return Some(s.to_string());
    }
    let up = digits[keep] >= 5;
    digits.truncate(keep);
    let mut point = point;
    if up {
        let mut k = keep;
        loop {
            if k == 0 {
                digits.insert(0, 1);
                point += 1;
                // keep the same number of significant digits
                digits.pop();
                break;
            }
            k -= 1;
            if digits[k] == 9 {
                digits[k] = 0;
            } else {
                digits[k] += 1;
                break;
            }
        }
    }
    // pad integers that lost their low digits
    while digits.len() < point {
        digits.push(0);
    }
    let text: String = digits.iter().map(|d| (b'0' + d) as char).collect();
    let out = if point >= text.len() { text } else { format!("{}.{}", &text[..point], &text[point..]) };
    Some(format!("{}{out}", if neg { "-" } else { "" }))
}
