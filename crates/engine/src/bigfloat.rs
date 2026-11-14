//! Arbitrary-precision evaluation of constant expressions (astro-float),
//! used for long decimal expansions such as π to 50 digits.

use crate::expr::Expr::*;
use crate::expr::*;
use astro_float::{BigFloat, Consts, Radix, RoundingMode};
use num_traits::{Signed, ToPrimitive};

const RM: RoundingMode = RoundingMode::ToEven;

struct Ctx {
    p: usize,
    cc: Consts,
}

fn from_int(n: &Int, c: &mut Ctx) -> BigFloat {
    BigFloat::parse(&n.to_string(), Radix::Dec, c.p, RM, &mut c.cc)
}

fn ev(e: &Expr, c: &mut Ctx) -> Option<BigFloat> {
    crate::budget::tick();
    let p = c.p;
    let r = match e {
        Num(r) => {
            let n = from_int(r.numer(), c);
            let d = from_int(r.denom(), c);
            n.div(&d, p, RM)
        }
        Float(f) => BigFloat::from_f64(f.0, p),
        Const(K::Pi) => c.cc.pi(p, RM),
        Const(K::E) => c.cc.e(p, RM),
        Const(K::Golden) => {
            let five = BigFloat::from_f64(5.0, p);
            five.sqrt(p, RM).add(&BigFloat::from_f64(1.0, p), p, RM).div(&BigFloat::from_f64(2.0, p), p, RM)
        }
        Const(K::EulerGamma) => BigFloat::parse("0.57721566490153286060651209008240243104215933593992359880576723488486772677766467", Radix::Dec, p, RM, &mut c.cc),
        Const(K::Catalan) => BigFloat::parse("0.91596559417721901505460351493238411077414937428167213426649811962176301977625476947", Radix::Dec, p, RM, &mut c.cc),
        Add(v) => {
            let mut acc = BigFloat::from_f64(0.0, p);
            for t in v {
                acc = acc.add(&ev(t, c)?, p, RM);
            }
            acc
        }
        Mul(v) => {
            let mut acc = BigFloat::from_f64(1.0, p);
            for t in v {
                acc = acc.mul(&ev(t, c)?, p, RM);
            }
            acc
        }
        Pow(b, ex) => {
            let base = ev(b, c)?;
            if let Some(k) = ex.as_i64() {
                if k.unsigned_abs() <= 100_000 {
                    let r = base.powi(k.unsigned_abs() as usize, p, RM);
                    return Some(if k < 0 { BigFloat::from_f64(1.0, p).div(&r, p, RM) } else { r });
                }
            }
            if let Some(r) = ex.as_rat() {
                if r.denom().to_i64() == Some(2) {
                    if base.is_negative() {
                        return None;
                    }
                    let s = base.sqrt(p, RM);
                    let k = r.numer().to_i64()?;
                    let r2 = s.powi(k.unsigned_abs() as usize, p, RM);
                    return Some(if k < 0 { BigFloat::from_f64(1.0, p).div(&r2, p, RM) } else { r2 });
                }
                if r.denom().to_i64() == Some(3) && r.numer().abs().to_i64() == Some(1) {
                    let cb = base.cbrt(p, RM);
                    return Some(if r.is_negative() { BigFloat::from_f64(1.0, p).div(&cb, p, RM) } else { cb });
                }
            }
            if base.is_negative() {
                return None;
            }
            let e2 = ev(ex, c)?;
            base.pow(&e2, p, RM, &mut c.cc)
        }
        Func(name, args) if args.len() == 1 => {
            let x = ev(&args[0], c)?;
            let cc = &mut c.cc;
            match &**name {
                "sin" => x.sin(p, RM, cc),
                "cos" => x.cos(p, RM, cc),
                "tan" => x.tan(p, RM, cc),
                "asin" => x.asin(p, RM, cc),
                "acos" => x.acos(p, RM, cc),
                "atan" => x.atan(p, RM, cc),
                "sinh" => x.sinh(p, RM, cc),
                "cosh" => x.cosh(p, RM, cc),
                "tanh" => x.tanh(p, RM, cc),
                "asinh" => x.asinh(p, RM, cc),
                "acosh" => x.acosh(p, RM, cc),
                "atanh" => x.atanh(p, RM, cc),
                "ln" => {
                    if !x.is_positive() {
                        return None;
                    }
                    x.ln(p, RM, cc)
                }
                "abs" => x.abs(),
                "cbrt" => x.cbrt(p, RM),
                _ => return None,
            }
        }
        _ => return None,
    };
    if r.is_nan() || r.is_inf() {
        None
    } else {
        Some(r)
    }
}

/// Decimal expansion of a real constant expression to `digits` significant
/// digits (truncated, as WolframAlpha shows it).
pub fn decimal(e: &Expr, digits: usize) -> Option<String> {
    if !e.free_symbols().is_empty() || e.has_const(K::I) {
        return None;
    }
    let bits = (digits as f64 * 3.33) as usize + 64;
    let mut c = Ctx { p: bits, cc: Consts::new().ok()? };
    let v = crate::budget::attempt_opt(crate::budget::remaining().min(2.0), || ev(e, &mut c))?;
    let s = v.format(Radix::Dec, RM, &mut c.cc).ok()?;
    let out = positional(&s, digits)?;
    // cross-check against f64 to avoid branch mistakes
    if let Some(f) = e.to_f64_const() {
        let back: f64 = out.trim_end_matches('…').parse().ok()?;
        if (back - f).abs() > 1e-9 * (1.0 + f.abs()) {
            return None;
        }
    }
    Some(out)
}

/// Turn astro-float's `d.ddddde±n` output into positional notation.
fn positional(s: &str, digits: usize) -> Option<String> {
    let (neg, s) = match s.strip_prefix('-') {
        Some(r) => (true, r),
        None => (false, s),
    };
    let (mant, exp) = s.split_once(['e', 'E'])?;
    let exp: i64 = exp.parse().ok()?;
    let mut ds: String = mant.chars().filter(|c| c.is_ascii_digit()).collect();
    // astro-float prints 0.ddd style mantissas; normalise to d.ddd
    let mut exp = exp;
    if mant.starts_with("0.") || mant.starts_with('.') {
        let lead = ds.chars().skip(1).take_while(|c| *c == '0').count();
        ds = ds[1 + lead..].to_string();
        exp -= 1 + lead as i64;
    } else if let Some(dot) = mant.find('.') {
        exp += dot as i64 - 1;
    }
    let ds: String = ds.trim_start_matches('0').chars().take(digits).collect();
    if ds.is_empty() {
        return Some("0".into());
    }
    let out = if exp >= 0 {
        let e = exp as usize;
        if e + 1 >= ds.len() {
            if e > 60 {
                format!("{}.{}×10^{}", &ds[..1], &ds[1..], e)
            } else {
                format!("{}{}", ds, "0".repeat(e + 1 - ds.len()))
            }
        } else {
            format!("{}.{}", &ds[..=e], &ds[e + 1..])
        }
    } else {
        let z = (-exp - 1) as usize;
        if z > 20 {
            format!("{}.{}×10^{}", &ds[..1], &ds[1..], exp)
        } else {
            format!("0.{}{}", "0".repeat(z), ds)
        }
    };
    Some(if neg { format!("-{out}") } else { out })
}
