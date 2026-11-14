//! Algebraic simplification: expansion, cancellation of rational functions,
//! factoring, partial fractions, and trigonometric / logarithmic / radical
//! rewriting.  Symbols are assumed real where a choice of branch matters.

use crate::budget;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::poly::{self, Gens, MPoly, QPoly};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};

// ---------------------------------------------------------------------------
// expansion
// ---------------------------------------------------------------------------

fn expand_product(fs: Vec<Expr>) -> Expr {
    let mut acc: Vec<Expr> = vec![one()];
    for f in fs {
        match f {
            Add(ts) => {
                let mut next = Vec::with_capacity(acc.len() * ts.len());
                for a in &acc {
                    for t in &ts {
                        next.push(mul(vec![a.clone(), t.clone()]));
                    }
                }
                // merge like terms early to keep intermediate sums small
                acc = match add(next) {
                    Add(v) => v,
                    other => vec![other],
                };
            }
            other => {
                acc = acc.into_iter().map(|a| mul(vec![a, other.clone()])).collect();
            }
        }
    }
    let r = add(acc);
    // products of radicals may have produced new sums inside products
    if needs_expansion(&r) {
        expand(&r)
    } else {
        r
    }
}

fn needs_expansion(e: &Expr) -> bool {
    match e {
        Add(v) => v.iter().any(needs_expansion),
        Mul(v) => v.iter().any(|f| matches!(f, Add(_)) || matches!(f, Pow(b, ex) if matches!(**b, Add(_)) && ex.as_i64().map_or(false, |k| k > 1))),
        Pow(b, ex) => matches!(**b, Add(_)) && ex.as_i64().map_or(false, |k| k > 1),
        _ => false,
    }
}

/// Distribute products over sums and expand integer powers of sums.
pub fn expand(e: &Expr) -> Expr {
    budget::tick();
    match e {
        Add(v) => add(v.iter().map(expand).collect()),
        Mul(v) => expand_product(v.iter().map(expand).collect()),
        Pow(b, ex) => {
            let b2 = expand(b);
            let ex2 = expand(ex);
            if let (Add(_), Some(n)) = (&b2, ex2.as_i64()) {
                if n > 1 && n <= 400 {
                    // binary powering
                    let mut result = one();
                    let mut base = b2.clone();
                    let mut k = n;
                    while k > 0 {
                        if k & 1 == 1 {
                            result = expand_product(vec![result, base.clone()]);
                        }
                        k >>= 1;
                        if k > 0 {
                            base = expand_product(vec![base.clone(), base]);
                        }
                    }
                    return result;
                }
                if n < -1 && n >= -400 {
                    return pow(expand(&pow(b2, int(-n))), int(-1));
                }
            }
            let r = pow(b2, ex2);
            if needs_expansion(&r) && r != *e {
                expand(&r)
            } else {
                r
            }
        }
        Func(n, a) => func(n, a.iter().map(expand).collect()),
        List(v) => List(v.iter().map(expand).collect()),
        Rel(op, a, b) => Rel(*op, Box::new(expand(a)), Box::new(expand(b))),
        _ => e.clone(),
    }
}

/// Build a product without re-simplifying (keeps factored forms intact).
pub fn raw_mul(mut fs: Vec<Expr>) -> Expr {
    fs.retain(|f| !f.is_one());
    match fs.len() {
        0 => one(),
        1 => fs.pop().unwrap(),
        _ => Mul(fs),
    }
}

pub fn raw_pow(b: Expr, k: i64) -> Expr {
    if k == 1 {
        b
    } else {
        Pow(Box::new(b), Box::new(int(k)))
    }
}

// ---------------------------------------------------------------------------
// rational functions
// ---------------------------------------------------------------------------

fn radical_atom(den: &Expr) -> Option<Expr> {
    let mut found = None;
    fn walk(e: &Expr, out: &mut Option<Expr>) {
        if out.is_some() {
            return;
        }
        match e {
            Const(K::I) => *out = Some(e.clone()),
            Pow(b, ex) if **ex == rat(1, 2) && b.free_symbols().is_empty() => {
                // innermost radical first
                walk(b, out);
                if out.is_none() {
                    *out = Some(e.clone());
                }
            }
            Add(v) | Mul(v) => v.iter().for_each(|x| walk(x, out)),
            Pow(b, _) => walk(b, out),
            _ => {}
        }
    }
    walk(den, &mut found);
    found
}

/// Remove square roots and `i` from a constant denominator.
fn rationalize_pair(mut num: Expr, mut den: Expr) -> (Expr, Expr) {
    if !den.free_symbols().is_empty() {
        return (num, den);
    }
    for _ in 0..8 {
        let Some(atom) = radical_atom(&den) else { break };
        let conj = den.subs(&atom, &neg(atom.clone()));
        if conj == den {
            break;
        }
        num = expand(&mul(vec![num, conj.clone()]));
        den = expand(&mul(vec![den, conj]));
        if den.is_numeric() {
            break;
        }
    }
    (num, den)
}

/// Generators pre-seeded with the free symbols in alphabetical order, so
/// that the first symbol is the main variable of every polynomial.
fn seeded_gens(e: &Expr) -> Gens {
    Gens(e.free_symbols().into_iter().map(Sym).collect())
}

fn ratfn_parts(e: &Expr) -> (Expr, Expr) {
    let mut gens = seeded_gens(e);
    let mut r = poly::to_ratfn(e, &mut gens);
    let n = gens.0.len();
    r.num = grow(&r.num, n);
    r.den = grow(&r.den, n);
    (poly::mpoly_to_expr(&r.num, &gens), poly::mpoly_to_expr(&r.den, &gens))
}

fn grow(p: &MPoly, n: usize) -> MPoly {
    if p.n == n {
        return p.clone();
    }
    MPoly {
        n,
        t: p.t
            .iter()
            .map(|(m, c)| {
                let mut mm = m.clone();
                mm.resize(n, 0);
                (mm, c.clone())
            })
            .collect(),
    }
}

/// Numerator and denominator of the cancelled rational form.
pub fn numer_denom(e: &Expr) -> (Expr, Expr) {
    let (mut num, mut den) = ratfn_parts(e);
    // algebraic relations between generators (sqrt(2)^2 = 2, i^2 = -1) are
    // applied when converting back; iterate until stable
    for _ in 0..4 {
        if den.is_one() {
            break;
        }
        let again = div(num.clone(), den.clone());
        let (n2, d2) = ratfn_parts(&again);
        if n2 == num && d2 == den {
            break;
        }
        num = n2;
        den = d2;
    }
    let (num, den) = rationalize_pair(num, den);
    if let Num(d) = &den {
        if !d.is_one() && !d.is_zero() {
            return (mul(vec![Num(d.recip()), num]), one());
        }
    }
    (num, den)
}

/// Combine into a single fraction and cancel common factors.
pub fn cancel(e: &Expr) -> Expr {
    match e {
        List(v) => return List(v.iter().map(cancel).collect()),
        Rel(op, a, b) => return Rel(*op, Box::new(cancel(a)), Box::new(cancel(b))),
        Num(_) | Float(_) | Sym(_) | Const(_) => return e.clone(),
        _ => {}
    }
    if e.has(&|x| matches!(x, List(_) | Rel(..))) || e.has(&|x| matches!(x, Const(K::Inf | K::CInf | K::Undef))) {
        return e.clone();
    }
    let (num, den) = numer_denom(e);
    div(num, den)
}

pub fn together(e: &Expr) -> Expr {
    cancel(e)
}

// ---------------------------------------------------------------------------
// factoring
// ---------------------------------------------------------------------------

/// Factor into `(numeric content, [(factor, multiplicity)])`; multiplicities
/// are negative for denominator factors.
pub fn factor_list(e: &Expr) -> (Expr, Vec<(Expr, i64)>) {
    let mut gens = seeded_gens(e);
    let mut r = poly::to_ratfn(e, &mut gens);
    let n = gens.0.len();
    r.num = grow(&r.num, n);
    r.den = grow(&r.den, n);
    let (c1, f1) = poly::factor_mpoly(&r.num);
    let (c2, f2) = poly::factor_mpoly(&r.den);
    let mut out: Vec<(Expr, i64)> = Vec::new();
    for (f, m) in f1 {
        out.push((poly::mpoly_to_expr(&f, &gens), m as i64));
    }
    for (f, m) in f2 {
        out.push((poly::mpoly_to_expr(&f, &gens), -(m as i64)));
    }
    let content = if c2.is_zero() { undef() } else { Num(c1 / c2) };
    (content, out)
}

/// Factored form as a (non-canonical) product expression.
pub fn factor(e: &Expr) -> Expr {
    match e {
        List(v) => return List(v.iter().map(factor).collect()),
        Rel(op, a, b) => return Rel(*op, Box::new(factor(a)), Box::new(factor(b))),
        _ => {}
    }
    let (content, fs) = factor_list(e);
    if fs.is_empty() {
        return content;
    }
    let mut parts = Vec::new();
    if !content.is_one() {
        if content.is_minus_one() && fs.len() == 1 && fs[0].1 == 1 {
            return neg(fs[0].0.clone());
        }
        parts.push(content);
    }
    for (f, m) in fs {
        parts.push(raw_pow(f, m));
    }
    raw_mul(parts)
}

/// Integer factorisation as an expression `2^3 × 3 × 5`.
pub fn factor_integer(n: &BigInt) -> Vec<(BigInt, u32)> {
    crate::numtheory::factor(n)
}

// ---------------------------------------------------------------------------
// partial fractions
// ---------------------------------------------------------------------------

/// Partial fraction decomposition with respect to `x`; `None` if `e` is not a
/// rational function of `x` with rational coefficients.
pub fn apart(e: &Expr, x: &Expr) -> Option<Expr> {
    let (num, den) = numer_denom(e);
    let n = poly::to_qpoly(&num, x)?;
    let d = poly::to_qpoly(&den, x)?;
    if d.degree() == 0 {
        return Some(expand(e));
    }
    let (q, r) = n.divrem(&d);
    let (content, factors) = poly::factor(&d);
    let mut terms = vec![q.to_expr(x)];
    // r / (content * Π p_i^m_i)
    let r = r.scale(&content.recip());
    let full: QPoly = factors.iter().fold(QPoly::one(), |acc, (p, m)| acc.mul(&p.pow(*m)));
    for (p, m) in &factors {
        let pm = p.pow(*m);
        let cofactor = full.div_exact(&pm)?;
        // numerator over p^m: r * cofactor^{-1} mod p^m
        let (g, s, _) = cofactor.xgcd(&pm);
        if !g.is_constant() {
            return None;
        }
        let mut ni = r.mul(&s).rem(&pm);
        // expand in powers of p
        let mut digits = Vec::new();
        for _ in 0..*m {
            let (qq, rr) = ni.divrem(p);
            digits.push(rr);
            ni = qq;
        }
        for (j, a) in digits.iter().enumerate() {
            if a.is_zero() {
                continue;
            }
            let power = (*m as usize - j) as i64;
            // normalise so that the numerator carries the rational content
            terms.push(div(a.to_expr(x), pow(p.to_expr(x), int(power))));
        }
    }
    Some(add(terms))
}

// ---------------------------------------------------------------------------
// collecting
// ---------------------------------------------------------------------------

/// Group by powers of `x`: `a x + b x + c` -> `(a + b) x + c` (raw form).
pub fn collect(e: &Expr, x: &Expr) -> Option<Expr> {
    let cs = poly::coeffs(e, x)?;
    let mut terms = Vec::new();
    for (k, c) in cs.iter().enumerate().rev() {
        if c.is_zero() {
            continue;
        }
        let p = pow(x.clone(), int(k as i64));
        terms.push(if k == 0 { c.clone() } else if c.is_one() { p } else { raw_mul(vec![c.clone(), p]) });
    }
    Some(match terms.len() {
        0 => zero(),
        1 => terms.pop().unwrap(),
        _ => Add(terms),
    })
}

// ---------------------------------------------------------------------------
// trigonometry
// ---------------------------------------------------------------------------

fn trig_fn(name: &str, arg: Expr) -> Expr {
    func(name, vec![arg])
}

fn expand_trig_call(name: &str, arg: &Expr, depth: usize) -> Expr {
    if depth > 24 {
        return trig_fn(name, arg.clone());
    }
    let s = |a: &Expr| expand_trig_call("sin", a, depth + 1);
    let c = |a: &Expr| expand_trig_call("cos", a, depth + 1);
    let (sh, ch) = (matches!(name, "sinh" | "cosh" | "tanh"), name);
    let _ = ch;
    if sh {
        let s = |a: &Expr| expand_trig_call("sinh", a, depth + 1);
        let c = |a: &Expr| expand_trig_call("cosh", a, depth + 1);
        let split = split_angle(arg);
        return match (name, split) {
            ("tanh", _) => s(arg) / c(arg),
            ("sinh", Some((a, b))) => s(&a) * c(&b) + c(&a) * s(&b),
            ("cosh", Some((a, b))) => c(&a) * c(&b) + s(&a) * s(&b),
            _ => trig_fn(name, arg.clone()),
        };
    }
    match (name, split_angle(arg)) {
        ("tan", _) => {
            let (sv, cv) = (s(arg), c(arg));
            if matches!(&sv, Func(n, _) if &**n == "sin") && matches!(&cv, Func(n, _) if &**n == "cos") {
                trig_fn("tan", arg.clone())
            } else {
                sv / cv
            }
        }
        ("sin", Some((a, b))) => s(&a) * c(&b) + c(&a) * s(&b),
        ("cos", Some((a, b))) => c(&a) * c(&b) - s(&a) * s(&b),
        _ => trig_fn(name, arg.clone()),
    }
}

/// Split `a + b` or `n u` (integer n >= 2) into two angles.
fn split_angle(arg: &Expr) -> Option<(Expr, Expr)> {
    match arg {
        Add(ts) => Some((ts[0].clone(), add(ts[1..].to_vec()))),
        Mul(_) => {
            let (c, rest) = arg.coeff_split();
            let n = c.as_i64()?;
            if (2..=16).contains(&n) {
                Some((mul(vec![int(n - 1), rest.clone()]), rest))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Expand trigonometric functions of sums and integer multiples.
pub fn trig_expand(e: &Expr) -> Expr {
    let r = e.map(&|x| match &x {
        Func(n, a) if a.len() == 1 && matches!(&**n, "sin" | "cos" | "tan" | "sinh" | "cosh" | "tanh") => expand_trig_call(n, &a[0], 0),
        _ => x,
    });
    expand(&r)
}

/// Product-to-sum linearisation: products and powers of sines and cosines
/// become sums of sines and cosines of combined angles.
pub fn trig_reduce(e: &Expr) -> Expr {
    let ex = expand(e);
    let mut out = Vec::new();
    for term in ex.terms() {
        out.push(reduce_term(&term));
    }
    add(out)
}

fn reduce_term(term: &Expr) -> Expr {
    let mut coef = Vec::new();
    // list of (is_sin, angle) factors
    let mut trig: Vec<(bool, Expr)> = Vec::new();
    for f in term.factors() {
        let (b, ex) = f.base_exp();
        match (&b, ex.as_i64()) {
            (Func(n, a), Some(k)) if a.len() == 1 && (1..=24).contains(&k) && matches!(&**n, "sin" | "cos") => {
                for _ in 0..k {
                    trig.push((&**n == "sin", a[0].clone()));
                }
            }
            _ => coef.push(f.clone()),
        }
    }
    if trig.len() < 2 {
        return term.clone();
    }
    // running sum of (coefficient, is_sin, angle); start with cos(0) = 1
    let mut acc: Vec<(Expr, bool, Expr)> = vec![(one(), false, zero())];
    for (is_sin, u) in trig {
        let mut next = Vec::with_capacity(acc.len() * 2);
        for (c, a_sin, a) in acc {
            let h = mul(vec![rat(1, 2), c]);
            let plus = add(vec![a.clone(), u.clone()]);
            let minus = sub(a, u.clone());
            match (a_sin, is_sin) {
                (false, false) => {
                    next.push((h.clone(), false, minus));
                    next.push((h, false, plus));
                }
                (false, true) => {
                    next.push((h.clone(), true, plus));
                    next.push((neg(h), true, minus));
                }
                (true, false) => {
                    next.push((h.clone(), true, plus));
                    next.push((h, true, minus));
                }
                (true, true) => {
                    next.push((h.clone(), false, minus));
                    next.push((neg(h), false, plus));
                }
            }
        }
        // merge equal entries to limit growth
        let merged = add(next.iter().map(|(c, s, a)| mul(vec![c.clone(), trig_fn(if *s { "sin" } else { "cos" }, a.clone())])).collect());
        acc = merged
            .terms()
            .into_iter()
            .map(|t| {
                let mut c = Vec::new();
                let mut hit = None;
                for f in t.factors() {
                    match &f {
                        Func(n, a) if hit.is_none() && a.len() == 1 && matches!(&**n, "sin" | "cos") => hit = Some((&**n == "sin", a[0].clone())),
                        _ => c.push(f.clone()),
                    }
                }
                match hit {
                    Some((s, a)) => (mul(c), s, a),
                    None => (mul(c), false, zero()),
                }
            })
            .collect();
    }
    let body = add(acc.into_iter().map(|(c, s, a)| mul(vec![c, trig_fn(if s { "sin" } else { "cos" }, a)])).collect());
    expand(&mul(vec![mul(coef), body]))
}

/// Replace `f(u)^n` (n >= 2) using `f^2 = sign * g^2 + konst`.
fn pythagorean(e: &Expr, from: &str, to: &str, sign: i64, konst: i64) -> Expr {
    e.map(&|x| {
        if let Pow(b, ex) = &x {
            if let (Func(n, a), Some(k)) = (&**b, ex.as_i64()) {
                if &**n == from && a.len() == 1 && k >= 2 {
                    let g2 = square(trig_fn(to, a[0].clone()));
                    let repl = add(vec![int(konst), mul(vec![int(sign), g2])]);
                    let mut r = pow(repl, int(k / 2));
                    if k % 2 == 1 {
                        r = mul(vec![r, (**b).clone()]);
                    }
                    return r;
                }
            }
        }
        x
    })
}

/// sin/cos quotients back to tan.
fn to_tan(e: &Expr) -> Expr {
    e.map(&|x| {
        let Mul(fs) = &x else { return x };
        let mut fs = fs.clone();
        let mut changed = false;
        for (num_name, den_name, out_name) in [("sin", "cos", "tan"), ("sinh", "cosh", "tanh")] {
            loop {
                let mut found = None;
                'search: for (i, f) in fs.iter().enumerate() {
                    let (b, ex) = f.base_exp();
                    let (Func(n, a), Some(p)) = (&b, ex.as_i64()) else { continue };
                    if &**n != num_name || a.len() != 1 || p == 0 {
                        continue;
                    }
                    for (j, g) in fs.iter().enumerate() {
                        let (b2, ex2) = g.base_exp();
                        let (Func(n2, a2), Some(q)) = (&b2, ex2.as_i64()) else { continue };
                        if &**n2 == den_name && a2 == a && p.signum() == -q.signum() {
                            found = Some((i, j, p, q, a[0].clone()));
                            break 'search;
                        }
                    }
                }
                let Some((i, j, p, q, arg)) = found else { break };
                let k = p.abs().min(q.abs()) * p.signum();
                let s = trig_fn(num_name, arg.clone());
                let c = trig_fn(den_name, arg.clone());
                fs[i] = pow(s, int(p - k));
                fs[j] = pow(c, int(q + k));
                fs.push(pow(trig_fn(out_name, arg), int(k)));
                changed = true;
            }
        }
        if changed {
            mul(fs)
        } else {
            x
        }
    })
}

fn has_trig(e: &Expr) -> bool {
    e.has_any_fn(&["sin", "cos", "tan"])
}

fn has_hyper(e: &Expr) -> bool {
    e.has_any_fn(&["sinh", "cosh", "tanh"])
}

fn trig_candidates(e: &Expr, out: &mut Vec<Expr>) {
    let tan_free = e.map(&|x| match &x {
        Func(n, a) if &**n == "tan" && a.len() == 1 => trig_fn("sin", a[0].clone()) / trig_fn("cos", a[0].clone()),
        Func(n, a) if &**n == "tanh" && a.len() == 1 => trig_fn("sinh", a[0].clone()) / trig_fn("cosh", a[0].clone()),
        _ => x,
    });
    let base = cancel(&tan_free);
    let mut forms = vec![base.clone()];
    if has_trig(&base) {
        forms.push(cancel(&pythagorean(&base, "sin", "cos", -1, 1)));
        forms.push(cancel(&pythagorean(&base, "cos", "sin", -1, 1)));
        // multiple angles
        let ex = trig_expand(&base);
        if ex != base {
            let exc = cancel(&ex);
            forms.push(cancel(&pythagorean(&exc, "sin", "cos", -1, 1)));
            forms.push(cancel(&pythagorean(&exc, "cos", "sin", -1, 1)));
            forms.push(exc);
        }
        // Fourier form of numerator and denominator
        let (n, d) = numer_denom(&base);
        let (rn, rd) = (trig_reduce(&n), trig_reduce(&d));
        forms.push(div(rn, rd));
    }
    if has_hyper(&base) {
        forms.push(cancel(&pythagorean(&base, "cosh", "sinh", 1, 1)));
        forms.push(cancel(&pythagorean(&base, "sinh", "cosh", 1, -1)));
        let ex = trig_expand(&base);
        if ex != base {
            let exc = cancel(&ex);
            forms.push(cancel(&pythagorean(&exc, "cosh", "sinh", 1, 1)));
            forms.push(exc);
        }
        // exponential form settles identities such as sinh x + cosh x = e^x
        let exp_form = cancel(&rewrite_exp(&base));
        if !exp_form.has_any_fn(&["sinh", "cosh"]) {
            forms.push(exp_form);
        }
    }
    for f in forms {
        let t = to_tan(&f);
        if t != f {
            out.push(t);
        }
        out.push(f);
    }
}

/// Rewrite hyperbolic functions through exponentials.
pub fn rewrite_exp(e: &Expr) -> Expr {
    e.map(&|x| match &x {
        Func(n, a) if a.len() == 1 => {
            let u = a[0].clone();
            let (p, m) = (exp(u.clone()), exp(neg(u)));
            match &**n {
                "sinh" => (p - m) / int(2),
                "cosh" => (p + m) / int(2),
                "tanh" => (p.clone() - m.clone()) / (p + m),
                _ => x,
            }
        }
        _ => x,
    })
}

/// Rewrite circular functions through complex exponentials.
pub fn rewrite_exp_trig(e: &Expr) -> Expr {
    e.map(&|x| match &x {
        Func(n, a) if a.len() == 1 => {
            let u = a[0].clone();
            let (p, m) = (exp(i() * u.clone()), exp(neg(i() * u)));
            match &**n {
                "sin" => (p - m) / (int(2) * i()),
                "cos" => (p + m) / int(2),
                "tan" => (p.clone() - m.clone()) / (i() * (p + m)),
                _ => x,
            }
        }
        _ => x,
    })
}

// ---------------------------------------------------------------------------
// logarithms, powers, radicals
// ---------------------------------------------------------------------------

/// `ln(a b) -> ln a + ln b`, `ln(a^k) -> k ln a` (positive arguments assumed).
pub fn expand_log(e: &Expr) -> Expr {
    e.map(&|x| match &x {
        Func(n, a) if &**n == "ln" && a.len() == 1 => split_log(&a[0]).unwrap_or(x),
        _ => x,
    })
}

fn split_log(arg: &Expr) -> Option<Expr> {
    match arg {
        Num(r) if r.is_integer() && r.is_positive() => {
            if r.numer().bits() > 80 {
                return None;
            }
            let fs = crate::numtheory::factor(r.numer());
            if fs.len() <= 1 {
                return None;
            }
            Some(add(fs.into_iter().map(|(p, k)| int(k as i64) * Func("ln".into(), vec![big(p)])).collect()))
        }
        Mul(fs) => {
            let mut terms = Vec::new();
            let mut sign_neg = false;
            for f in fs {
                match f {
                    Num(r) if r.is_negative() => {
                        sign_neg = true;
                        if !(-r.clone()).is_one() {
                            terms.push(expand_log(&ln(Num(-r.clone()))));
                        }
                    }
                    _ => terms.push(expand_log(&ln(f.clone()))),
                }
            }
            if sign_neg {
                // keep the sign inside one logarithm: ln(-x y) = ln(-x) + ln(y)
                return None;
            }
            Some(add(terms))
        }
        Pow(b, ex) if !ex.has_const(K::I) => Some(mul(vec![(**ex).clone(), expand_log(&ln((**b).clone()))])),
        _ => None,
    }
}

/// `a ln x + b ln y -> ln(x^a y^b)` for rational a, b.
pub fn combine_log(e: &Expr) -> Expr {
    e.map(&|x| {
        let Add(ts) = &x else { return x };
        let mut logs = Vec::new();
        let mut rest = Vec::new();
        for t in ts {
            let (c, r) = t.coeff_split();
            match (&c, &r) {
                (Num(_), Func(n, a)) if &**n == "ln" && a.len() == 1 => logs.push(pow(a[0].clone(), c.clone())),
                _ => rest.push(t.clone()),
            }
        }
        if logs.len() < 2 {
            return x;
        }
        rest.push(ln(cancel(&mul(logs))));
        add(rest)
    })
}

/// Power rules that need the "real variable" assumption:
/// `(x^2)^(1/2) = |x|`, `|x|^2 = x^2`, `sqrt(x)^2`-style clean-ups.
pub fn power_simplify(e: &Expr) -> Expr {
    e.map(&|x| {
        if let Pow(b, ex) = &x {
            // |u|^(even) -> u^(even)
            if let (Func(n, a), Some(k)) = (&**b, ex.as_i64()) {
                if &**n == "abs" && k % 2 == 0 && !a[0].has_const(K::I) {
                    return pow(a[0].clone(), (**ex).clone());
                }
            }
            if let (Pow(b2, e2), Num(q)) = (&**b, &**ex) {
                if let Some(k) = e2.as_i64() {
                    if k % 2 == 0 && !q.is_integer() && !b2.has_const(K::I) {
                        // (u^(2m))^q = |u|^(2 m q)
                        let total = Rat::from_integer(BigInt::from(k)) * q;
                        if total.is_integer() && total.numer().is_even() {
                            return pow((**b2).clone(), Num(total));
                        }
                        return pow(func("abs", vec![(**b2).clone()]), Num(total));
                    }
                    if k % 2 != 0 && q.denom().is_odd() {
                        // odd roots of odd powers are single-valued on the reals
                        return pow((**b2).clone(), Num(Rat::from_integer(BigInt::from(k)) * q));
                    }
                }
            }
            // perfect squares under a root: sqrt(x^2 + 2x + 1) = |x + 1|
            if let (Add(_), Num(q)) = (&**b, &**ex) {
                if *q.denom() == BigInt::from(2) && b.free_symbols().len() == 1 {
                    let (content, fs) = factor_list(b);
                    if let Num(c) = &content {
                        if c.is_positive() && !fs.is_empty() && fs.iter().all(|(_, m)| *m % 2 == 0) {
                            let inner = mul(fs.iter().map(|(f, m)| pow(func("abs", vec![f.clone()]), int(*m / 2))).collect());
                            return pow(mul(vec![pow(content.clone(), rat(1, 2)), inner]), big(q.numer().clone()));
                        }
                    }
                }
            }
        }
        x
    })
}

/// `sqrt(a + b sqrt(c))` -> `sqrt(d) + sqrt(e)` when possible.
pub fn denest(e: &Expr) -> Expr {
    e.map(&|x| {
        if let Pow(b, ex) = &x {
            if **ex == rat(1, 2) {
                if let Some(r) = denest_sqrt(b) {
                    return r;
                }
            }
        }
        x
    })
}

fn denest_sqrt(inner: &Expr) -> Option<Expr> {
    let Add(ts) = inner else { return None };
    if ts.len() != 2 || !inner.free_symbols().is_empty() {
        return None;
    }
    // a + b sqrt(c)
    let (a, rad) = match (&ts[0], &ts[1]) {
        (Num(a), other) => (a.clone(), other.clone()),
        (other, Num(a)) => (a.clone(), other.clone()),
        _ => return None,
    };
    // b^2 c is the square of the radical term
    let sq = expand(&square(rad.clone()));
    let Num(b2c) = sq else { return None };
    let disc = &a * &a - &b2c;
    if disc.is_negative() {
        return None;
    }
    let root = rat_sqrt(&disc)?;
    let two = Rat::from_integer(BigInt::from(2));
    let (d, e2) = ((&a + &root) / &two, (&a - &root) / &two);
    if d.is_negative() || e2.is_negative() {
        return None;
    }
    let sign_negative = crate::eval::eval_f64(&rad, &[])? < 0.0;
    let (s1, s2) = (sqrt(Num(d)), sqrt(Num(e2)));
    Some(if sign_negative { s1 - s2 } else { s1 + s2 })
}

pub fn rat_sqrt(r: &Rat) -> Option<Rat> {
    if r.is_negative() {
        return None;
    }
    let (n, d) = (r.numer().sqrt(), r.denom().sqrt());
    if &n * &n == *r.numer() && &d * &d == *r.denom() {
        Some(Rat::new(n, d))
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// the main entry point
// ---------------------------------------------------------------------------

fn score(e: &Expr) -> (usize, usize) {
    // prefer fewer nodes; among equals prefer fewer function calls / radicals
    let mut funcs = 0usize;
    count_special(e, &mut funcs);
    (e.size() + funcs, funcs)
}

fn count_special(e: &Expr, n: &mut usize) {
    match e {
        Func(_, a) => {
            *n += 1;
            a.iter().for_each(|x| count_special(x, n));
        }
        Add(v) | Mul(v) | List(v) => v.iter().for_each(|x| count_special(x, n)),
        Pow(b, ex) => {
            if !ex.is_integer() {
                *n += 1;
            }
            if ex.is_negative_term() && !matches!(**b, Func(..)) {
                *n += 1;
                // radicals and i in denominators are not "simple"
                if b.has(&|x| matches!(x, Const(K::I)) || matches!(x, Pow(_, q) if !q.is_integer())) {
                    *n += 4;
                }
            }
            count_special(b, n);
            count_special(ex, n);
        }
        Rel(_, a, b) => {
            count_special(a, n);
            count_special(b, n);
        }
        _ => {}
    }
}

fn simplify_args(e: &Expr) -> Expr {
    match e {
        Add(v) => add(v.iter().map(simplify_args).collect()),
        Mul(v) => mul(v.iter().map(simplify_args).collect()),
        Pow(b, ex) => pow(simplify_args(b), simplify(ex)),
        Func(n, a) => {
            let holder = n.chars().next().map_or(false, |c| c.is_uppercase()) && n.len() > 2;
            if holder {
                e.clone()
            } else {
                func(n, a.iter().map(simplify).collect())
            }
        }
        _ => e.clone(),
    }
}

/// Pull the rational content out of sums inside products:
/// `e^x (-x/2 - 1/2)` -> `-(e^x (x + 1))/2`.
pub fn pull_content(e: &Expr) -> Expr {
    match e {
        Add(v) => add(v.iter().map(pull_content).collect()),
        Mul(v) => {
            let mut coef = Rat::one();
            let mut fs = Vec::with_capacity(v.len());
            for f in v {
                let f = pull_content(f);
                match &f {
                    Add(ts) => {
                        let (g, sign) = sum_content(ts);
                        if g.is_one() && !sign {
                            fs.push(f);
                        } else {
                            let k = if sign { -g.clone() } else { g.clone() };
                            coef *= k.clone();
                            let inv = Num(k.recip());
                            fs.push(add(ts.iter().map(|t| mul(vec![inv.clone(), t.clone()])).collect()));
                        }
                    }
                    _ => fs.push(f),
                }
            }
            fs.push(Num(coef));
            raw_mul_keep(fs)
        }
        Pow(b, ex) => {
            let base = pull_content(b);
            if let (Add(ts), Some(r)) = (&base, ex.as_rat()) {
                if !r.is_integer() {
                    let (g, sign) = sum_content(ts);
                    if !g.is_one() && !sign {
                        let inv = Num(g.recip());
                        let rest = add(ts.iter().map(|t| mul(vec![inv.clone(), t.clone()])).collect());
                        return mul(vec![pow(Num(g), (**ex).clone()), pow(rest, (**ex).clone())]);
                    }
                }
            }
            pow(base, (**ex).clone())
        }
        Func(n, a) => func(n, a.iter().map(pull_content).collect()),
        _ => e.clone(),
    }
}

/// Factor common factors (and numeric content) out of sums, recursively:
/// `-e^x x - e^x` -> `-e^x (x + 1)`.
pub fn factor_terms(e: &Expr) -> Expr {
    match e {
        Add(v) => {
            let ts: Vec<Expr> = v.iter().map(factor_terms).collect();
            // candidate common bases with their minimal integer exponent
            let first = ts[0].factors();
            let mut common: Vec<(Expr, Int)> = Vec::new();
            for f in &first {
                if f.is_numeric() {
                    continue;
                }
                let (b, k) = int_base_exp(f);
                if k <= Int::zero() {
                    continue;
                }
                let mut m = k;
                let mut ok = true;
                for t in &ts[1..] {
                    let found = t.factors().iter().find_map(|g| {
                        let (gb, gk) = int_base_exp(g);
                        if gb == b {
                            Some(gk)
                        } else {
                            None
                        }
                    });
                    match found {
                        Some(j) if j > Int::zero() => m = m.min(j),
                        _ => {
                            ok = false;
                            break;
                        }
                    }
                }
                if ok && !common.iter().any(|(c, _)| *c == b) {
                    common.push((b, m));
                }
            }
            if common.is_empty() {
                return pull_content(&add(ts));
            }
            let cf: Vec<Expr> = common.iter().map(|(b, k)| pow(b.clone(), big(k.clone()))).collect();
            let inv = mul(cf.iter().map(|f| pow(f.clone(), int(-1))).collect());
            let rest = add(ts.iter().map(|t| mul(vec![t.clone(), inv.clone()])).collect());
            let mut fs = cf;
            fs.push(rest);
            pull_content(&raw_mul_keep(fs))
        }
        Mul(v) => pull_content(&mul(v.iter().map(factor_terms).collect())),
        Pow(b, ex) => pow(factor_terms(b), (**ex).clone()),
        Func(n, a) => func(n, a.iter().map(factor_terms).collect()),
        _ => e.clone(),
    }
}

/// `b^k` with integer k as `(b, k)`; anything else as `(self, 1)`.
fn int_base_exp(f: &Expr) -> (Expr, Int) {
    if let Pow(b, k) = f {
        if let Some(k) = k.as_int() {
            return ((**b).clone(), k.clone());
        }
    }
    (f.clone(), Int::one())
}

/// Polynomial in its symbols (no functions, radicals or denominators)?
fn is_plain_poly(e: &Expr) -> bool {
    match e {
        Num(_) | Sym(_) => true,
        Add(v) | Mul(v) => v.iter().all(is_plain_poly),
        Pow(b, k) => matches!(k.as_i64(), Some(n) if n >= 0) && is_plain_poly(b),
        _ => false,
    }
}

/// Multiply without letting the numeric coefficient distribute back into a
/// lone sum.
fn raw_mul_keep(fs: Vec<Expr>) -> Expr {
    let m = mul(fs.clone());
    match &m {
        Add(_) if fs.iter().filter(|f| !f.is_numeric()).count() == 1 => {
            let c: Vec<Expr> = fs.iter().filter(|f| f.is_numeric()).cloned().collect();
            let rest: Vec<Expr> = fs.into_iter().filter(|f| !f.is_numeric()).collect();
            let c = mul(c);
            if c.is_one() {
                rest[0].clone()
            } else {
                Mul(vec![c, rest[0].clone()])
            }
        }
        _ => m,
    }
}

/// Rational gcd of the numeric coefficients of a sum and whether the leading
/// (highest-ordered) term is negative.
fn sum_content(ts: &[Expr]) -> (Rat, bool) {
    let mut num = Int::zero();
    let mut den = Int::one();
    for t in ts {
        let (c, _) = t.coeff_split();
        let Some(r) = c.as_rat() else { return (Rat::one(), false) };
        num = num.gcd(r.numer());
        den = den.lcm(r.denom());
    }
    if num.is_zero() {
        return (Rat::one(), false);
    }
    let all_neg = ts.iter().all(|t| t.is_negative_term());
    (Rat::new(num, den), all_neg)
}

/// General-purpose simplification: tries several rewrites and keeps the
/// smallest result.
pub fn simplify(e: &Expr) -> Expr {
    budget::tick();
    match e {
        Num(_) | Float(_) | Sym(_) | Const(_) => return e.clone(),
        List(v) => return List(v.iter().map(simplify).collect()),
        Rel(op, a, b) => return crate::funcs::rel(*op, simplify(a), simplify(b)),
        _ => {}
    }
    if e.has(&|x| matches!(x, List(_) | Rel(..))) {
        return match e {
            Func(n, a) => func(n, a.iter().map(simplify).collect()),
            Add(v) => add(v.iter().map(simplify).collect()),
            Mul(v) => mul(v.iter().map(simplify).collect()),
            _ => e.clone(),
        };
    }
    let e1 = simplify_args(e);
    let mut best = e1.clone();
    let mut best_score = score(&best);
    let consider = |c: Expr, best: &mut Expr, best_score: &mut (usize, usize)| {
        if c.is_undef() && !best.is_undef() {
            return;
        }
        let s = score(&c);
        if s < *best_score {
            *best = c;
            *best_score = s;
        }
    };
    let cands = budget::attempt(budget::remaining().min(4.0) * 0.5, || {
        let mut out = Vec::new();
        let c1 = cancel(&e1);
        let p1 = power_simplify(&c1);
        if p1 != c1 {
            out.push(cancel(&p1));
        }
        if c1.has(&|x| matches!(x, Pow(b, ex) if **ex == rat(1, 2) && matches!(**b, Add(_)))) {
            let d1 = denest(&c1);
            if d1 != c1 {
                out.push(cancel(&d1));
            }
        }
        if has_trig(&c1) || has_hyper(&c1) {
            if let Some(v) = budget::attempt(budget::remaining() * 0.6, || {
                let mut t = Vec::new();
                trig_candidates(&c1, &mut t);
                t
            }) {
                out.extend(v);
            }
        }
        if c1.has_fn("ln") {
            let el = cancel(&expand_log(&c1));
            let cl = combine_log(&c1);
            out.push(combine_log(&el));
            out.push(el);
            out.push(cl);
        }
        for base in [&e1, &c1] {
            if is_plain_poly(&expand(base)) {
                continue;
            }
            let pc = factor_terms(base);
            if pc != *base {
                out.push(pc);
            }
        }
        out.push(c1);
        out
    });
    for c in cands.unwrap_or_default() {
        consider(c, &mut best, &mut best_score);
    }
    best
}

/// Decide whether an expression is identically zero.  `Some(true)` is
/// certain (symbolically proven); `Some(false)` when a numeric sample is
/// clearly non-zero; `None` when undecided.
pub fn is_zero(e: &Expr) -> Option<bool> {
    if e.is_zero() {
        return Some(true);
    }
    if e.is_numeric() {
        return Some(false);
    }
    let s = simplify(e);
    if s.is_zero() {
        return Some(true);
    }
    // numeric probing at a few sample points
    let syms: Vec<Name> = s.free_symbols().into_iter().collect();
    let names: Vec<&str> = syms.iter().map(|n| &**n).collect();
    let prog = crate::eval::Program::compile(&s, &names)?;
    let mut all_small = true;
    let mut any = false;
    for k in 0..6 {
        let vals: Vec<num_complex::Complex64> = (0..names.len()).map(|j| num_complex::Complex64::new(0.37 + 0.61 * k as f64 + 0.23 * j as f64, 0.0)).collect();
        let z = prog.eval_c(&vals);
        if z.re.is_nan() || z.im.is_nan() {
            continue;
        }
        any = true;
        if z.norm() > 1e-9 {
            return Some(false);
        }
        all_small &= z.norm() < 1e-11;
    }
    if any && all_small {
        // numerically zero everywhere we looked but not proven
        return None;
    }
    None
}

/// Numerically check that `e` vanishes at random points (heuristic identity test).
pub fn numerically_zero(e: &Expr) -> bool {
    let syms: Vec<Name> = e.free_symbols().into_iter().collect();
    let names: Vec<&str> = syms.iter().map(|n| &**n).collect();
    let Some(prog) = crate::eval::Program::compile(e, &names) else { return false };
    let mut ok = 0;
    for k in 0..10 {
        let vals: Vec<num_complex::Complex64> = (0..names.len()).map(|j| num_complex::Complex64::new(0.31 + 0.47 * k as f64 + 0.19 * j as f64, 0.0)).collect();
        let z = prog.eval_c(&vals);
        if z.re.is_nan() || z.im.is_nan() {
            continue;
        }
        if z.norm() > 1e-9 * (1.0 + vals.iter().map(|v| v.norm()).sum::<f64>()) {
            return false;
        }
        ok += 1;
    }
    ok >= 3
}

/// Are two expressions equal (symbolically, else numerically)?
pub fn equal(a: &Expr, b: &Expr) -> bool {
    if a == b {
        return true;
    }
    let d = sub(a.clone(), b.clone());
    match is_zero(&d) {
        Some(v) => v,
        None => numerically_zero(&d),
    }
}

/// Convert every exact number to floating point and evaluate constants.
pub fn evalf(e: &Expr) -> Expr {
    if e.free_symbols().is_empty() && !e.is_list() && !matches!(e, Rel(..)) {
        if let Some(z) = crate::eval::const_c64(e) {
            if z.im.abs() <= 1e-14 * (1.0 + z.re.abs()) {
                return float(z.re);
            }
            if z.re.abs() <= 1e-14 * z.im.abs() {
                return mul(vec![float(z.im), i()]);
            }
            return add(vec![float(z.re), mul(vec![float(z.im), i()])]);
        }
    }
    match e {
        Num(r) => float(rat_to_f64(r)),
        Const(K::Pi) => float(std::f64::consts::PI),
        Const(K::E) => float(std::f64::consts::E),
        Const(K::EulerGamma) => float(crate::special::EULER_GAMMA),
        Const(K::Golden) => float(crate::special::GOLDEN),
        Const(K::Catalan) => float(crate::special::CATALAN),
        Add(v) => add(v.iter().map(evalf).collect()),
        Mul(v) => mul(v.iter().map(evalf).collect()),
        Pow(b, ex) => {
            // keep small integer exponents exact: x^2, not x^2.0
            let ex2 = if ex.is_integer() { (**ex).clone() } else { evalf(ex) };
            pow(evalf(b), ex2)
        }
        Func(n, a) => func(n, a.iter().map(evalf).collect()),
        List(v) => List(v.iter().map(evalf).collect()),
        Rel(op, a, b) => Rel(*op, Box::new(evalf(a)), Box::new(evalf(b))),
        _ => e.clone(),
    }
}

/// Greatest common divisor of two polynomial expressions.
pub fn poly_gcd(a: &Expr, b: &Expr) -> Expr {
    let mut gens = Gens::default();
    let (Some(pa), Some(pb)) = (poly::to_mpoly(a, &mut gens), poly::to_mpoly(b, &mut gens)) else { return one() };
    let n = gens.0.len();
    let g = grow(&pa, n).gcd(&grow(&pb, n));
    poly::mpoly_to_expr(&g, &gens)
}

pub fn to_i64(e: &Expr) -> Option<i64> {
    e.as_int().and_then(|n| n.to_i64())
}
