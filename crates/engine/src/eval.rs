//! Numeric evaluation of expressions: a small stack machine that can be run
//! over real (`f64`) or complex values.  Programs are cheap to build and are
//! shared across threads when evaluating grids.

use crate::expr::{Expr, RelOp, K};
use crate::special as sp;
use num_complex::Complex64 as C;
use num_traits::{ToPrimitive, Zero};
use std::f64::consts::PI;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Fun {
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
    Asinh,
    Acosh,
    Atanh,
    Ln,
    Abs,
    Sign,
    Floor,
    Ceil,
    Round,
    Frac,
    Gamma,
    LGamma,
    Factorial,
    Erf,
    Erfc,
    Erfi,
    Erfinv,
    Zeta,
    Heaviside,
    Dirac,
    Min,
    Max,
    Mod,
    Atan2,
    Binomial,
    Re,
    Im,
    Conj,
    Arg,
    Sinc,
    Si,
    Ci,
    Ei,
    Li,
    LambertW,
    BesselJ,
    BesselY,
    BesselI,
    BesselK,
    Digamma,
    Beta,
    FresnelS,
    FresnelC,
    AiryAi,
    AiryBi,
    EllipticK,
    EllipticE,
    Cbrt,
    Surd,
    And,
    Or,
    Not,
    Fib,
    Harmonic,
    Polylog,
    Gcd,
    Lcm,
    Log,
}

impl Fun {
    pub fn from_name(name: &str) -> Option<Fun> {
        use Fun::*;
        Some(match name {
            "sin" => Sin,
            "cos" => Cos,
            "tan" => Tan,
            "asin" => Asin,
            "acos" => Acos,
            "atan" => Atan,
            "sinh" => Sinh,
            "cosh" => Cosh,
            "tanh" => Tanh,
            "asinh" => Asinh,
            "acosh" => Acosh,
            "atanh" => Atanh,
            "ln" => Ln,
            "abs" => Abs,
            "sign" => Sign,
            "floor" => Floor,
            "ceil" => Ceil,
            "round" => Round,
            "frac" => Frac,
            "gamma" => Gamma,
            "lgamma" => LGamma,
            "factorial" => Factorial,
            "erf" => Erf,
            "erfc" => Erfc,
            "erfi" => Erfi,
            "erfinv" => Erfinv,
            "zeta" => Zeta,
            "heaviside" => Heaviside,
            "dirac" => Dirac,
            "min" => Min,
            "max" => Max,
            "mod" => Mod,
            "atan2" => Atan2,
            "binomial" => Binomial,
            "re" => Re,
            "im" => Im,
            "conj" => Conj,
            "arg" => Arg,
            "sinc" => Sinc,
            "Si" => Si,
            "Ci" => Ci,
            "Ei" => Ei,
            "li" => Li,
            "W" => LambertW,
            "besselj" => BesselJ,
            "bessely" => BesselY,
            "besseli" => BesselI,
            "besselk" => BesselK,
            "digamma" => Digamma,
            "beta" => Beta,
            "fresnels" => FresnelS,
            "fresnelc" => FresnelC,
            "airyai" => AiryAi,
            "airybi" => AiryBi,
            "ellipticK" => EllipticK,
            "ellipticE" => EllipticE,
            "cbrt" => Cbrt,
            "surd" => Surd,
            "and" => And,
            "or" => Or,
            "not" => Not,
            "fibonacci" => Fib,
            "harmonic" => Harmonic,
            "polylog" => Polylog,
            "gcd" => Gcd,
            "lcm" => Lcm,
            "log" => Log,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, Debug)]
enum Op {
    Num(f64),
    Imag,
    Var(u16),
    Add(u16),
    Mul(u16),
    Pow,
    PowI(i32),
    PowRat(i32, u32),
    Exp,
    Call(Fun, u8),
    Rel(RelOp),
}

#[derive(Clone, Debug)]
pub struct Program {
    ops: Vec<Op>,
    pub nvars: usize,
    /// Use the real branch for odd roots of negative numbers.
    pub real_roots: bool,
}

impl Program {
    pub fn compile(e: &Expr, vars: &[&str]) -> Option<Program> {
        let mut ops = Vec::new();
        compile_into(e, vars, &mut ops)?;
        Some(Program { ops, nvars: vars.len(), real_roots: false })
    }

    pub fn with_real_roots(mut self) -> Program {
        self.real_roots = true;
        self
    }

    pub fn eval(&self, vars: &[f64]) -> f64 {
        let mut stack = Vec::with_capacity(16);
        self.eval_with(vars, &mut stack)
    }

    pub fn eval1(&self, x: f64) -> f64 {
        self.eval(&[x])
    }

    pub fn eval_with(&self, vars: &[f64], st: &mut Vec<f64>) -> f64 {
        st.clear();
        for op in &self.ops {
            match *op {
                Op::Num(v) => st.push(v),
                Op::Imag => st.push(f64::NAN),
                Op::Var(k) => st.push(vars[k as usize]),
                Op::Add(n) => {
                    let n = n as usize;
                    let at = st.len() - n;
                    let s: f64 = st[at..].iter().sum();
                    st.truncate(at);
                    st.push(s);
                }
                Op::Mul(n) => {
                    let n = n as usize;
                    let at = st.len() - n;
                    let s: f64 = st[at..].iter().product();
                    st.truncate(at);
                    st.push(s);
                }
                Op::Pow => {
                    let e = st.pop().unwrap();
                    let b = st.pop().unwrap();
                    st.push(real_pow(b, e));
                }
                Op::PowI(k) => {
                    let b = st.pop().unwrap();
                    st.push(b.powi(k));
                }
                Op::PowRat(p, q) => {
                    let b = st.pop().unwrap();
                    let v = if b < 0.0 {
                        if self.real_roots && q % 2 == 1 {
                            let m = (-b).powf(p as f64 / q as f64);
                            if p % 2 == 0 {
                                m
                            } else {
                                -m
                            }
                        } else {
                            f64::NAN
                        }
                    } else {
                        b.powf(p as f64 / q as f64)
                    };
                    st.push(v);
                }
                Op::Exp => {
                    let x = st.pop().unwrap();
                    st.push(x.exp());
                }
                Op::Call(f, n) => {
                    let n = n as usize;
                    let at = st.len() - n;
                    let v = real_fn(f, &st[at..]);
                    st.truncate(at);
                    st.push(v);
                }
                Op::Rel(r) => {
                    let b = st.pop().unwrap();
                    let a = st.pop().unwrap();
                    let t = match r {
                        RelOp::Eq => (a - b).abs() <= 1e-12 * (1.0 + a.abs().max(b.abs())),
                        RelOp::Ne => a != b,
                        RelOp::Lt => a < b,
                        RelOp::Le => a <= b,
                        RelOp::Gt => a > b,
                        RelOp::Ge => a >= b,
                    };
                    st.push(if a.is_nan() || b.is_nan() { f64::NAN } else if t { 1.0 } else { 0.0 });
                }
            }
        }
        st.pop().unwrap_or(f64::NAN)
    }

    /// Translate to a WGSL `f32` expression over variables `v0, v1, ...`.
    /// `None` when the program uses a function without a GPU implementation.
    /// Helper functions referenced by the expression are defined in
    /// [`WGSL_PRELUDE`].
    pub fn to_wgsl(&self) -> Option<String> {
        fn lit(v: f64) -> String {
            if v.is_nan() {
                "nan_f()".into()
            } else if v.is_infinite() {
                if v > 0.0 { "inf_f()".into() } else { "(-inf_f())".into() }
            } else {
                let s = format!("{:e}", v as f32);
                format!("f32({s})")
            }
        }
        let mut st: Vec<String> = Vec::new();
        for op in &self.ops {
            match *op {
                Op::Num(v) => st.push(lit(v)),
                Op::Imag => return None,
                Op::Var(k) => st.push(format!("v{k}")),
                Op::Add(n) | Op::Mul(n) => {
                    let at = st.len().checked_sub(n as usize)?;
                    let parts: Vec<String> = st.drain(at..).collect();
                    let sep = if matches!(op, Op::Add(_)) { " + " } else { " * " };
                    st.push(format!("({})", parts.join(sep)));
                }
                Op::Pow => {
                    let e = st.pop()?;
                    let b = st.pop()?;
                    st.push(format!("rpow({b}, {e})"));
                }
                Op::PowI(k) => {
                    let b = st.pop()?;
                    st.push(format!("powi({b}, {k})"));
                }
                Op::PowRat(p, q) => {
                    let b = st.pop()?;
                    let real = if self.real_roots { 1 } else { 0 };
                    st.push(format!("powrat({b}, {p}, {q}, {real})"));
                }
                Op::Exp => {
                    let x = st.pop()?;
                    st.push(format!("exp({x})"));
                }
                Op::Call(f, n) => {
                    let at = st.len().checked_sub(n as usize)?;
                    let a: Vec<String> = st.drain(at..).collect();
                    let one = |name: &str| -> Option<String> { if a.len() == 1 { Some(format!("{name}({})", a[0])) } else { None } };
                    let s = match f {
                        Fun::Sin => one("sin")?,
                        Fun::Cos => one("cos")?,
                        Fun::Tan => one("tan")?,
                        Fun::Asin => one("asin")?,
                        Fun::Acos => one("acos")?,
                        Fun::Atan => one("atan")?,
                        Fun::Sinh => one("sinh")?,
                        Fun::Cosh => one("cosh")?,
                        Fun::Tanh => one("tanh")?,
                        Fun::Asinh => one("asinh")?,
                        Fun::Acosh => one("acosh")?,
                        Fun::Atanh => one("atanh")?,
                        Fun::Ln => one("ln_f")?,
                        Fun::Abs => one("abs")?,
                        Fun::Sign => one("sign")?,
                        Fun::Floor => one("floor")?,
                        Fun::Ceil => one("ceil")?,
                        Fun::Round => one("round")?,
                        Fun::Frac => one("fract")?,
                        Fun::Cbrt => one("cbrt_f")?,
                        Fun::Erf => one("erf_f")?,
                        Fun::Erfc => format!("(1.0 - erf_f({}))", a.first()?),
                        Fun::Heaviside => one("heav")?,
                        Fun::Sinc => one("sinc_f")?,
                        Fun::Gamma => one("gamma_f")?,
                        Fun::Factorial => format!("gamma_f({} + 1.0)", a.first()?),
                        Fun::Min if a.len() >= 2 => a[1..].iter().fold(a[0].clone(), |acc, x| format!("min({acc}, {x})")),
                        Fun::Max if a.len() >= 2 => a[1..].iter().fold(a[0].clone(), |acc, x| format!("max({acc}, {x})")),
                        Fun::Mod if a.len() == 2 => format!("modf_({}, {})", a[0], a[1]),
                        Fun::Atan2 if a.len() == 2 => format!("atan2({}, {})", a[0], a[1]),
                        Fun::Log if a.len() == 2 => format!("(ln_f({}) / ln_f({}))", a[1], a[0]),
                        Fun::Re => a.first()?.clone(),
                        _ => return None,
                    };
                    st.push(s);
                }
                Op::Rel(r) => {
                    let b = st.pop()?;
                    let a = st.pop()?;
                    let o = match r {
                        RelOp::Eq => "==",
                        RelOp::Ne => "!=",
                        RelOp::Lt => "<",
                        RelOp::Le => "<=",
                        RelOp::Gt => ">",
                        RelOp::Ge => ">=",
                    };
                    st.push(format!("select(0.0, 1.0, {a} {o} {b})"));
                }
            }
        }
        if st.len() == 1 {
            st.pop()
        } else {
            None
        }
    }

    pub fn eval_c(&self, vars: &[C]) -> C {
        let mut st: Vec<C> = Vec::with_capacity(16);
        self.eval_c_with(vars, &mut st)
    }

    pub fn eval_c_with(&self, vars: &[C], st: &mut Vec<C>) -> C {
        st.clear();
        let nan = C::new(f64::NAN, f64::NAN);
        for op in &self.ops {
            match *op {
                Op::Num(v) => st.push(C::new(v, 0.0)),
                Op::Imag => st.push(C::new(0.0, 1.0)),
                Op::Var(k) => st.push(vars[k as usize]),
                Op::Add(n) => {
                    let at = st.len() - n as usize;
                    let s: C = st[at..].iter().sum();
                    st.truncate(at);
                    st.push(s);
                }
                Op::Mul(n) => {
                    let at = st.len() - n as usize;
                    let s: C = st[at..].iter().product();
                    st.truncate(at);
                    st.push(s);
                }
                Op::Pow => {
                    let e = st.pop().unwrap();
                    let b = st.pop().unwrap();
                    st.push(complex_pow(b, e));
                }
                Op::PowI(k) => {
                    let b = st.pop().unwrap();
                    st.push(b.powi(k));
                }
                Op::PowRat(p, q) => {
                    let b = st.pop().unwrap();
                    st.push(complex_pow(b, C::new(p as f64 / q as f64, 0.0)));
                }
                Op::Exp => {
                    let x = st.pop().unwrap();
                    st.push(x.exp());
                }
                Op::Call(f, n) => {
                    let at = st.len() - n as usize;
                    let v = complex_fn(f, &st[at..]);
                    st.truncate(at);
                    st.push(v);
                }
                Op::Rel(r) => {
                    let b = st.pop().unwrap();
                    let a = st.pop().unwrap();
                    let t = match r {
                        RelOp::Eq => (a - b).norm() <= 1e-12 * (1.0 + a.norm().max(b.norm())),
                        RelOp::Ne => a != b,
                        RelOp::Lt => a.re < b.re,
                        RelOp::Le => a.re <= b.re,
                        RelOp::Gt => a.re > b.re,
                        RelOp::Ge => a.re >= b.re,
                    };
                    st.push(if a.is_nan() || b.is_nan() { nan } else { C::new(if t { 1.0 } else { 0.0 }, 0.0) });
                }
            }
        }
        st.pop().unwrap_or(nan)
    }
}

fn real_pow(b: f64, e: f64) -> f64 {
    if b == 0.0 && e == 0.0 {
        return 1.0;
    }
    b.powf(e)
}

fn complex_pow(b: C, e: C) -> C {
    if e.im == 0.0 {
        if e.re == 0.0 {
            return C::new(1.0, 0.0);
        }
        if b.is_zero() {
            return if e.re > 0.0 { C::new(0.0, 0.0) } else { C::new(f64::INFINITY, 0.0) };
        }
        if e.re == e.re.trunc() && e.re.abs() < 1e6 {
            return b.powi(e.re as i32);
        }
        if b.im == 0.0 && b.re > 0.0 {
            return C::new(b.re.powf(e.re), 0.0);
        }
        return b.powf(e.re);
    }
    if b.is_zero() {
        return if e.re > 0.0 { C::new(0.0, 0.0) } else { C::new(f64::NAN, f64::NAN) };
    }
    b.powc(e)
}

fn compile_into(e: &Expr, vars: &[&str], ops: &mut Vec<Op>) -> Option<()> {
    match e {
        Expr::Num(r) => ops.push(Op::Num(crate::expr::rat_to_f64(r))),
        Expr::Float(f) => ops.push(Op::Num(f.0)),
        Expr::Sym(s) => {
            let k = vars.iter().position(|v| *v == &**s)?;
            ops.push(Op::Var(k as u16));
        }
        Expr::Const(c) => ops.push(match c {
            K::Pi => Op::Num(PI),
            K::E => Op::Num(std::f64::consts::E),
            K::I => Op::Imag,
            K::Inf => Op::Num(f64::INFINITY),
            K::CInf | K::Undef => Op::Num(f64::NAN),
            K::EulerGamma => Op::Num(sp::EULER_GAMMA),
            K::Golden => Op::Num(sp::GOLDEN),
            K::Catalan => Op::Num(sp::CATALAN),
            K::True => Op::Num(1.0),
            K::False => Op::Num(0.0),
        }),
        Expr::Add(v) => {
            for a in v {
                compile_into(a, vars, ops)?;
            }
            ops.push(Op::Add(v.len() as u16));
        }
        Expr::Mul(v) => {
            for a in v {
                compile_into(a, vars, ops)?;
            }
            ops.push(Op::Mul(v.len() as u16));
        }
        Expr::Pow(b, ex) => {
            if matches!(**b, Expr::Const(K::E)) {
                compile_into(ex, vars, ops)?;
                ops.push(Op::Exp);
                return Some(());
            }
            compile_into(b, vars, ops)?;
            if let Expr::Num(q) = &**ex {
                if q.is_integer() {
                    if let Some(k) = q.numer().to_i32() {
                        ops.push(Op::PowI(k));
                        return Some(());
                    }
                } else if let (Some(p), Some(d)) = (q.numer().to_i32(), q.denom().to_u32()) {
                    ops.push(Op::PowRat(p, d));
                    return Some(());
                }
            }
            compile_into(ex, vars, ops)?;
            ops.push(Op::Pow);
        }
        Expr::Func(name, args) => {
            let f = Fun::from_name(name)?;
            if args.len() > 250 {
                return None;
            }
            for a in args {
                compile_into(a, vars, ops)?;
            }
            ops.push(Op::Call(f, args.len() as u8));
        }
        Expr::Rel(r, a, b) => {
            compile_into(a, vars, ops)?;
            compile_into(b, vars, ops)?;
            ops.push(Op::Rel(*r));
        }
        Expr::List(_) => return None,
    }
    Some(())
}

fn round_half_even(x: f64) -> f64 {
    let r = x.round();
    if (x - x.trunc()).abs() == 0.5 && r % 2.0 != 0.0 {
        r - x.signum()
    } else {
        r
    }
}

fn gcd_f(a: f64, b: f64) -> f64 {
    if a != a.trunc() || b != b.trunc() || a.abs() > 9e15 || b.abs() > 9e15 {
        return f64::NAN;
    }
    let (mut a, mut b) = (a.abs() as u64, b.abs() as u64);
    while b != 0 {
        let t = a % b;
        a = b;
        b = t;
    }
    a as f64
}

pub fn real_fn(f: Fun, a: &[f64]) -> f64 {
    use Fun::*;
    let x = a.first().copied().unwrap_or(f64::NAN);
    let y = a.get(1).copied().unwrap_or(f64::NAN);
    match f {
        Sin => x.sin(),
        Cos => x.cos(),
        Tan => x.tan(),
        Asin => x.asin(),
        Acos => x.acos(),
        Atan => x.atan(),
        Sinh => x.sinh(),
        Cosh => x.cosh(),
        Tanh => x.tanh(),
        Asinh => x.asinh(),
        Acosh => x.acosh(),
        Atanh => x.atanh(),
        Ln => {
            if x < 0.0 {
                f64::NAN
            } else {
                x.ln()
            }
        }
        Log => {
            // log(base, x)
            if x <= 0.0 || y < 0.0 {
                f64::NAN
            } else {
                y.ln() / x.ln()
            }
        }
        Abs => x.abs(),
        Sign => {
            if x > 0.0 {
                1.0
            } else if x < 0.0 {
                -1.0
            } else {
                x
            }
        }
        Floor => x.floor(),
        Ceil => x.ceil(),
        Round => round_half_even(x),
        Frac => x - x.floor(),
        Gamma => sp::gamma(x),
        LGamma => sp::lgamma(x),
        Factorial => sp::factorial(x),
        Erf => sp::erf(x),
        Erfc => sp::erfc(x),
        Erfi => sp::erfi(x),
        Erfinv => sp::erfinv(x),
        Zeta => sp::zeta(x),
        Heaviside => {
            if x > 0.0 {
                1.0
            } else if x < 0.0 {
                0.0
            } else if x == 0.0 {
                0.5
            } else {
                f64::NAN
            }
        }
        Dirac => {
            if x == 0.0 {
                f64::INFINITY
            } else if x.is_nan() {
                f64::NAN
            } else {
                0.0
            }
        }
        Min => a.iter().copied().fold(f64::INFINITY, |p, q| if q.is_nan() || p.is_nan() { f64::NAN } else { p.min(q) }),
        Max => a.iter().copied().fold(f64::NEG_INFINITY, |p, q| if q.is_nan() || p.is_nan() { f64::NAN } else { p.max(q) }),
        Mod => {
            if y == 0.0 {
                f64::NAN
            } else {
                x - y * (x / y).floor()
            }
        }
        Atan2 => x.atan2(y),
        Binomial => sp::binomial(x, y),
        Re | Conj => x,
        Im => {
            if x.is_nan() {
                x
            } else {
                0.0
            }
        }
        Arg => {
            if x >= 0.0 {
                0.0
            } else if x < 0.0 {
                PI
            } else {
                f64::NAN
            }
        }
        Sinc => sp::sinc(x),
        Si => sp::si(x),
        Ci => sp::ci(x),
        Ei => sp::ei(x),
        Li => sp::li(x),
        LambertW => {
            if a.len() == 2 {
                sp::lambert_w(y, x as i32)
            } else {
                sp::lambert_w(x, 0)
            }
        }
        BesselJ => sp::bessel_j(x, y),
        BesselY => sp::bessel_y(x, y),
        BesselI => sp::bessel_i(x, y),
        BesselK => sp::bessel_k(x, y),
        Digamma => sp::digamma(x),
        Beta => sp::beta(x, y),
        FresnelS => sp::fresnel(x).0,
        FresnelC => sp::fresnel(x).1,
        AiryAi => sp::airy_ai(x),
        AiryBi => sp::airy_bi(x),
        EllipticK => sp::elliptic_ke(x).0,
        EllipticE => sp::elliptic_ke(x).1,
        Cbrt => x.cbrt(),
        Surd => {
            // surd(x, n): real n-th root
            if y != y.trunc() || y == 0.0 {
                f64::NAN
            } else if x < 0.0 {
                if (y as i64) % 2 != 0 {
                    -(-x).powf(1.0 / y)
                } else {
                    f64::NAN
                }
            } else {
                x.powf(1.0 / y)
            }
        }
        And => {
            if a.iter().any(|v| v.is_nan()) {
                f64::NAN
            } else if a.iter().all(|v| *v != 0.0) {
                1.0
            } else {
                0.0
            }
        }
        Or => {
            if a.iter().any(|v| v.is_nan()) {
                f64::NAN
            } else if a.iter().any(|v| *v != 0.0) {
                1.0
            } else {
                0.0
            }
        }
        Not => {
            if x.is_nan() {
                x
            } else if x == 0.0 {
                1.0
            } else {
                0.0
            }
        }
        Fib => sp::fib(x),
        Harmonic => sp::harmonic(x),
        Polylog => sp::polylog(x, y),
        Gcd => a.iter().copied().fold(0.0, gcd_f),
        Lcm => a.iter().copied().fold(1.0, |p, q| {
            let g = gcd_f(p, q);
            if g == 0.0 {
                0.0
            } else {
                (p * q).abs() / g
            }
        }),
    }
}

pub fn complex_fn(f: Fun, a: &[C]) -> C {
    use Fun::*;
    let nan = C::new(f64::NAN, f64::NAN);
    let z = a.first().copied().unwrap_or(nan);
    let re = |v: f64| C::new(v, 0.0);
    match f {
        Sin => z.sin(),
        Cos => z.cos(),
        Tan => z.tan(),
        Asin => z.asin(),
        Acos => z.acos(),
        Atan => z.atan(),
        Sinh => z.sinh(),
        Cosh => z.cosh(),
        Tanh => z.tanh(),
        Asinh => z.asinh(),
        Acosh => z.acosh(),
        Atanh => z.atanh(),
        Ln => {
            if z.is_zero() {
                re(f64::NEG_INFINITY)
            } else {
                z.ln()
            }
        }
        Log => {
            let w = a.get(1).copied().unwrap_or(nan);
            w.ln() / z.ln()
        }
        Abs => re(z.norm()),
        Sign => {
            if z.is_zero() {
                z
            } else {
                z / z.norm()
            }
        }
        Re => re(z.re),
        Im => re(z.im),
        Conj => z.conj(),
        Arg => re(z.arg()),
        Gamma => {
            if z.im == 0.0 {
                re(sp::gamma(z.re))
            } else {
                sp::cgamma(z)
            }
        }
        Factorial => {
            if z.im == 0.0 {
                re(sp::factorial(z.re))
            } else {
                sp::cgamma(z + 1.0)
            }
        }
        Zeta => {
            if z.im == 0.0 {
                re(sp::zeta(z.re))
            } else {
                sp::czeta(z)
            }
        }
        Erf => sp::cerf(z),
        Erfc => re(1.0) - sp::cerf(z),
        Erfi => {
            // erfi(z) = -i erf(i z)
            let i = C::new(0.0, 1.0);
            -i * sp::cerf(i * z)
        }
        Sinc => {
            if z.is_zero() {
                re(1.0)
            } else {
                z.sin() / z
            }
        }
        Cbrt => {
            if z.im == 0.0 {
                re(z.re.cbrt())
            } else {
                z.powf(1.0 / 3.0)
            }
        }
        Floor => C::new(z.re.floor(), z.im.floor()),
        Ceil => C::new(z.re.ceil(), z.im.ceil()),
        Round => C::new(round_half_even(z.re), round_half_even(z.im)),
        _ => {
            // real-only functions: defined when every argument is real
            if a.iter().all(|w| w.im == 0.0) {
                let r: Vec<f64> = a.iter().map(|w| w.re).collect();
                re(real_fn(f, &r))
            } else {
                nan
            }
        }
    }
}

/// Evaluate with the given real variable bindings.  `None` when the value is
/// not a real number (undefined, complex, unknown symbols or functions).
pub fn eval_f64(e: &Expr, vars: &[(&str, f64)]) -> Option<f64> {
    match e {
        Expr::Num(r) => return Some(crate::expr::rat_to_f64(r)),
        Expr::Float(f) => return Some(f.0),
        _ => {}
    }
    let names: Vec<&str> = vars.iter().map(|v| v.0).collect();
    let vals: Vec<f64> = vars.iter().map(|v| v.1).collect();
    let p = Program::compile(e, &names)?;
    let v = p.eval(&vals);
    if v.is_nan() {
        // the real path fails for e.g. i^2 or (-8)^(1/3)*...; try complex
        let cv: Vec<C> = vals.iter().map(|x| C::new(*x, 0.0)).collect();
        let z = p.eval_c(&cv);
        if z.re.is_finite() && z.im.abs() <= 1e-12 * (1.0 + z.re.abs()) {
            return Some(z.re);
        }
        if z.re.is_infinite() && z.im == 0.0 {
            return Some(z.re);
        }
        return None;
    }
    Some(v)
}

/// Complex evaluation with complex variable bindings.
pub fn eval_c64(e: &Expr, vars: &[(&str, C)]) -> Option<C> {
    let names: Vec<&str> = vars.iter().map(|v| v.0).collect();
    let vals: Vec<C> = vars.iter().map(|v| v.1).collect();
    let p = Program::compile(e, &names)?;
    // prefer the real path when possible: it is more accurate on branch cuts
    if vals.iter().all(|z| z.im == 0.0) {
        let rv: Vec<f64> = vals.iter().map(|z| z.re).collect();
        let v = p.eval(&rv);
        if !v.is_nan() {
            return Some(C::new(v, 0.0));
        }
    }
    let z = p.eval_c(&vals);
    if z.re.is_nan() || z.im.is_nan() {
        None
    } else {
        Some(z)
    }
}

/// Numeric value of a constant expression as a complex number.
pub fn const_c64(e: &Expr) -> Option<C> {
    eval_c64(e, &[])
}

/// Tidy tiny rounding residue: snaps values within a few ulps of an integer.
pub fn chop(x: f64) -> f64 {
    let r = x.round();
    if (x - r).abs() < 1e-12 * (1.0 + x.abs()) && r.abs() < 1e15 {
        if r == 0.0 {
            0.0
        } else {
            r
        }
    } else {
        x
    }
}

/// WGSL helper functions used by [`Program::to_wgsl`].
pub const WGSL_PRELUDE: &str = r#"
fn nan_f() -> f32 { return bitcast<f32>(0x7fc00000u); }
fn inf_f() -> f32 { return bitcast<f32>(0x7f800000u); }
fn ln_f(x: f32) -> f32 { if (x < 0.0) { return nan_f(); } return log(x); }
fn powi(b: f32, k: i32) -> f32 {
    var r = 1.0; var p = b; var n = abs(k);
    loop { if (n == 0) { break; } if ((n & 1) == 1) { r = r * p; } p = p * p; n = n >> 1u; }
    if (k < 0) { return 1.0 / r; }
    return r;
}
fn rpow(b: f32, e: f32) -> f32 {
    if (b > 0.0) { return pow(b, e); }
    if (b == 0.0) { if (e > 0.0) { return 0.0; } if (e == 0.0) { return 1.0; } return inf_f(); }
    if (fract(e) == 0.0) { return powi(b, i32(e)); }
    return nan_f();
}
fn powrat(b: f32, p: i32, q: i32, real_roots: i32) -> f32 {
    let e = f32(p) / f32(q);
    if (b >= 0.0) { return pow(b, e); }
    if (real_roots == 1 && (q % 2) == 1) {
        let m = pow(-b, e);
        if ((p % 2) == 0) { return m; }
        return -m;
    }
    return nan_f();
}
fn cbrt_f(x: f32) -> f32 { return sign(x) * pow(abs(x), 1.0 / 3.0); }
fn heav(x: f32) -> f32 { if (x > 0.0) { return 1.0; } if (x < 0.0) { return 0.0; } return 0.5; }
fn sinc_f(x: f32) -> f32 { if (abs(x) < 1e-6) { return 1.0; } return sin(x) / x; }
fn modf_(a: f32, b: f32) -> f32 { return a - b * floor(a / b); }
fn erf_f(x: f32) -> f32 {
    // Abramowitz-Stegun 7.1.26
    let t = 1.0 / (1.0 + 0.3275911 * abs(x));
    let y = 1.0 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * exp(-x * x);
    return sign(x) * y;
}
fn gamma_f(x0: f32) -> f32 {
    // Lanczos approximation with reflection
    var x = x0;
    if (x < 0.5) {
        let s = sin(3.14159265 * x);
        if (s == 0.0) { return nan_f(); }
        return 3.14159265 / (s * gamma_f_pos(1.0 - x));
    }
    return gamma_f_pos(x);
}
fn gamma_f_pos(z: f32) -> f32 {
    // Lanczos (g = 5, 6 terms)
    var y = z;
    var tmp = z + 5.5;
    tmp = tmp - (z + 0.5) * log(tmp);
    var ser = 1.000000000190015;
    y = y + 1.0; ser = ser + 76.18009172947146 / y;
    y = y + 1.0; ser = ser - 86.50532032941677 / y;
    y = y + 1.0; ser = ser + 24.01409824083091 / y;
    y = y + 1.0; ser = ser - 1.231739572450155 / y;
    y = y + 1.0; ser = ser + 0.001208650973866179 / y;
    y = y + 1.0; ser = ser - 0.000005395239384953 / y;
    return exp(-tmp + log(2.5066282746310005 * ser / z));
}
"#;
