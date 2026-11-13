//! Special functions in double precision (real and, where practical, complex).

use crate::numeric::quad;
use num_complex::Complex64 as C;
use std::f64::consts::PI;

pub const EULER_GAMMA: f64 = 0.577_215_664_901_532_9;
pub const CATALAN: f64 = 0.915_965_594_177_219;
pub const GOLDEN: f64 = 1.618_033_988_749_895;

pub fn gamma(x: f64) -> f64 {
    if x <= 0.0 && x == x.floor() {
        return f64::NAN;
    }
    if x > 171.7 {
        return f64::INFINITY;
    }
    statrs::function::gamma::gamma(x)
}

pub fn lgamma(x: f64) -> f64 {
    if x <= 0.0 && x == x.floor() {
        return f64::INFINITY;
    }
    statrs::function::gamma::ln_gamma(x)
}

pub fn digamma(x: f64) -> f64 {
    if x <= 0.0 && x == x.floor() {
        return f64::NAN;
    }
    statrs::function::gamma::digamma(x)
}

pub fn beta(a: f64, b: f64) -> f64 {
    if a > 0.0 && b > 0.0 {
        (lgamma(a) + lgamma(b) - lgamma(a + b)).exp()
    } else {
        gamma(a) * gamma(b) / gamma(a + b)
    }
}

pub fn erf(x: f64) -> f64 {
    statrs::function::erf::erf(x)
}
pub fn erfc(x: f64) -> f64 {
    statrs::function::erf::erfc(x)
}
pub fn erfinv(x: f64) -> f64 {
    statrs::function::erf::erf_inv(x)
}

pub fn erfi(x: f64) -> f64 {
    // all-positive Maclaurin series
    let ax = x.abs();
    if ax > 26.0 {
        return f64::INFINITY.copysign(x);
    }
    let mut term = ax;
    let mut sum = ax;
    let x2 = ax * ax;
    for k in 1..2000 {
        term *= x2 / k as f64;
        let add = term / (2 * k + 1) as f64;
        sum += add;
        if add < sum * 1e-17 {
            break;
        }
    }
    (2.0 / PI.sqrt() * sum).copysign(x)
}

pub fn factorial(x: f64) -> f64 {
    gamma(x + 1.0)
}

pub fn binomial(n: f64, k: f64) -> f64 {
    if k < 0.0 && k == k.floor() {
        return 0.0;
    }
    if n == n.floor() && k == k.floor() && n >= 0.0 {
        if k > n {
            return 0.0;
        }
        let k = k.min(n - k);
        let mut r = 1.0;
        let mut i = 0.0;
        while i < k {
            r = r * (n - i) / (i + 1.0);
            i += 1.0;
        }
        return r.round();
    }
    gamma(n + 1.0) / (gamma(k + 1.0) * gamma(n - k + 1.0))
}

/// Complex gamma function (Lanczos approximation, g = 7).
pub fn cgamma(z: C) -> C {
    const G: f64 = 7.0;
    const P: [f64; 9] = [
        0.999_999_999_999_809_9,
        676.520_368_121_885_1,
        -1_259.139_216_722_402_8,
        771.323_428_777_653_1,
        -176.615_029_162_140_6,
        12.507_343_278_686_905,
        -0.138_571_095_265_720_12,
        9.984_369_578_019_572e-6,
        1.505_632_735_149_311_6e-7,
    ];
    if z.re < 0.5 {
        let s = (PI * z).sin();
        return C::new(PI, 0.0) / (s * cgamma(C::new(1.0, 0.0) - z));
    }
    let z = z - 1.0;
    let mut x = C::new(P[0], 0.0);
    for (i, p) in P.iter().enumerate().skip(1) {
        x += *p / (z + i as f64);
    }
    let t = z + G + 0.5;
    (2.0 * PI).sqrt() * t.powc(z + 0.5) * (-t).exp() * x
}

/// Riemann zeta function for complex argument (Borwein's algorithm plus the
/// functional equation).
pub fn czeta(s: C) -> C {
    if (s - 1.0).norm() < 1e-14 {
        return C::new(f64::INFINITY, 0.0);
    }
    if s.re < 0.0 {
        let one_minus = C::new(1.0, 0.0) - s;
        return C::new(2.0, 0.0).powc(s) * C::new(PI, 0.0).powc(s - 1.0) * (PI * s / 2.0).sin() * cgamma(one_minus) * czeta(one_minus);
    }
    let n = (48.0 + 1.1 * s.im.abs()).min(220.0) as usize;
    // d_k = n * sum_{i=0}^{k} (n+i-1)! 4^i / ((n-i)! (2i)!)
    let mut d = vec![0.0f64; n + 1];
    let mut term = 1.0 / n as f64; // i = 0: (n-1)!/n! = 1/n
    let mut acc = term;
    d[0] = n as f64 * acc;
    for i in 1..=n {
        // ratio term_i / term_{i-1}
        term *= 4.0 * ((n + i - 1) as f64) * ((n - i + 1) as f64) / (((2 * i) * (2 * i - 1)) as f64);
        acc += term;
        d[i] = n as f64 * acc;
    }
    let dn = d[n];
    let mut sum = C::new(0.0, 0.0);
    for k in 0..n {
        let sign = if k % 2 == 0 { 1.0 } else { -1.0 };
        sum += sign * (d[k] - dn) / C::new((k + 1) as f64, 0.0).powc(s);
    }
    let denom = dn * (C::new(1.0, 0.0) - C::new(2.0, 0.0).powc(C::new(1.0, 0.0) - s));
    -sum / denom
}

pub fn zeta(s: f64) -> f64 {
    if s == 1.0 {
        return f64::INFINITY;
    }
    if s < 0.0 && s == s.floor() && (s as i64) % 2 == 0 {
        return 0.0;
    }
    czeta(C::new(s, 0.0)).re
}

/// Complex error function.
pub fn cerf(z: C) -> C {
    if z.im == 0.0 {
        return C::new(erf(z.re), 0.0);
    }
    if z.norm() < 3.5 {
        let mut term = z;
        let mut sum = z;
        let z2 = z * z;
        for k in 1..400 {
            term *= -z2 / k as f64;
            let add = term / (2 * k + 1) as f64;
            sum += add;
            if add.norm() < 1e-17 * sum.norm() {
                break;
            }
        }
        return 2.0 / PI.sqrt() * sum;
    }
    if z.re < 0.0 {
        return -cerf(-z);
    }
    // continued fraction for erfc
    let mut f = z;
    for k in (1..80).rev() {
        f = z + (k as f64 / 2.0) / f;
    }
    C::new(1.0, 0.0) - (-z * z).exp() / (PI.sqrt() * f)
}

/// Exponential integral E1 for complex argument with positive real part or
/// non-zero imaginary part (modified Lentz continued fraction).
fn ce1(z: C) -> C {
    if z.norm() < 2.0 {
        // power series: -γ - ln z - Σ (-z)^k / (k k!)
        let mut sum = C::new(0.0, 0.0);
        let mut term = C::new(1.0, 0.0);
        for k in 1..200 {
            term *= -z / k as f64;
            let add = term / k as f64;
            sum += add;
            if add.norm() < 1e-17 * sum.norm().max(1e-300) {
                break;
            }
        }
        return -EULER_GAMMA - z.ln() - sum;
    }
    let tiny = 1e-300;
    let mut b = z + 1.0;
    let mut c = C::new(1.0 / tiny, 0.0);
    let mut d = 1.0 / b;
    let mut h = d;
    for i in 1..500 {
        let a = -((i * i) as f64);
        b += 2.0;
        d = 1.0 / (a * d + b);
        c = b + a / c;
        let del = c * d;
        h *= del;
        if (del - 1.0).norm() < 1e-16 {
            break;
        }
    }
    h * (-z).exp()
}

pub fn ei(x: f64) -> f64 {
    if x == 0.0 {
        return f64::NEG_INFINITY;
    }
    if x < 0.0 {
        return -ce1(C::new(-x, 0.0)).re;
    }
    if x < 40.0 {
        let mut sum = 0.0;
        let mut term = 1.0;
        for k in 1..500 {
            term *= x / k as f64;
            let add = term / k as f64;
            sum += add;
            if add < 1e-17 * sum {
                break;
            }
        }
        EULER_GAMMA + x.ln() + sum
    } else {
        let mut sum = 1.0;
        let mut term = 1.0;
        for k in 1..60 {
            let next = term * k as f64 / x;
            if next > term {
                break;
            }
            term = next;
            sum += term;
        }
        x.exp() / x * sum
    }
}

pub fn li(x: f64) -> f64 {
    if x < 0.0 {
        return f64::NAN;
    }
    if x == 0.0 {
        return 0.0;
    }
    ei(x.ln())
}

pub fn si(x: f64) -> f64 {
    let ax = x.abs();
    let v = if ax < 2.0 {
        let mut term = ax;
        let mut sum = ax;
        let x2 = ax * ax;
        for k in 1..100 {
            term *= -x2 / ((2 * k) as f64 * (2 * k + 1) as f64);
            let add = term / (2 * k + 1) as f64;
            sum += add;
            if add.abs() < 1e-17 {
                break;
            }
        }
        sum
    } else {
        PI / 2.0 + ce1(C::new(0.0, ax)).im
    };
    v.copysign(x)
}

pub fn ci(x: f64) -> f64 {
    if x <= 0.0 {
        return if x == 0.0 { f64::NEG_INFINITY } else { f64::NAN };
    }
    if x < 2.0 {
        let mut term = 1.0;
        let mut sum = 0.0;
        let x2 = x * x;
        for k in 1..100 {
            term *= -x2 / ((2 * k - 1) as f64 * (2 * k) as f64);
            let add = term / (2 * k) as f64;
            sum += add;
            if add.abs() < 1e-17 {
                break;
            }
        }
        EULER_GAMMA + x.ln() + sum
    } else {
        -ce1(C::new(0.0, x)).re
    }
}

/// Lambert W, principal branch (`branch = 0`) or lower branch (`branch = -1`).
pub fn lambert_w(x: f64, branch: i32) -> f64 {
    let em1 = -(-1.0f64).exp();
    if x < em1 - 1e-15 {
        return f64::NAN;
    }
    if (x - em1).abs() < 1e-15 {
        return -1.0;
    }
    let mut w = if branch == 0 {
        if x < 1.0 {
            let p = (2.0 * (std::f64::consts::E * x + 1.0)).max(0.0).sqrt();
            -1.0 + p - p * p / 3.0 + 11.0 / 72.0 * p * p * p
        } else {
            let l = x.ln();
            l - l.ln().max(-1.0)
        }
    } else {
        if x >= 0.0 {
            return f64::NAN;
        }
        if x > -0.1 {
            let l = (-x).ln();
            l - (-l).ln()
        } else {
            let p = -(2.0 * (std::f64::consts::E * x + 1.0)).max(0.0).sqrt();
            -1.0 + p - p * p / 3.0 + 11.0 / 72.0 * p * p * p
        }
    };
    for _ in 0..60 {
        let ew = w.exp();
        let f = w * ew - x;
        let denom = ew * (w + 1.0) - (w + 2.0) * f / (2.0 * w + 2.0);
        if denom == 0.0 || !denom.is_finite() {
            break;
        }
        let dw = f / denom;
        w -= dw;
        if dw.abs() < 1e-15 * (1.0 + w.abs()) {
            break;
        }
    }
    w
}

fn periodic_trapz(f: &dyn Fn(f64) -> f64, n: usize) -> f64 {
    // ∫_0^π f, where f extends to an even 2π-periodic function
    let h = PI / n as f64;
    let mut s = 0.5 * (f(0.0) + f(PI));
    for k in 1..n {
        s += f(h * k as f64);
    }
    s * h
}

pub fn bessel_j(nu: f64, x: f64) -> f64 {
    if x == 0.0 {
        return if nu == 0.0 { 1.0 } else if nu > 0.0 { 0.0 } else { f64::NAN };
    }
    let is_int = nu == nu.floor();
    if x < 0.0 {
        return if is_int { if (nu as i64) % 2 == 0 { bessel_j(nu, -x) } else { -bessel_j(nu, -x) } } else { f64::NAN };
    }
    if is_int {
        let n = (64.0 + 2.0 * (x + nu.abs())) as usize;
        return periodic_trapz(&|t| (nu * t - x * t.sin()).cos(), n) / PI;
    }
    let a = quad(&|t| (nu * t - x * t.sin()).cos(), 0.0, PI, 1e-13).value / PI;
    let b = quad(&|t| (-x * t.sinh() - nu * t).exp(), 0.0, f64::INFINITY, 1e-13).value;
    a - (nu * PI).sin() / PI * b
}

pub fn bessel_y(nu: f64, x: f64) -> f64 {
    if x <= 0.0 {
        return if x == 0.0 { f64::NEG_INFINITY } else { f64::NAN };
    }
    let a = quad(&|t| (x * t.sin() - nu * t).sin(), 0.0, PI, 1e-13).value / PI;
    let c = (nu * PI).cos();
    let b = quad(&|t| ((nu * t).exp() + c * (-nu * t).exp()) * (-x * t.sinh()).exp(), 0.0, f64::INFINITY, 1e-13).value / PI;
    a - b
}

pub fn bessel_i(nu: f64, x: f64) -> f64 {
    if x == 0.0 {
        return if nu == 0.0 { 1.0 } else if nu > 0.0 { 0.0 } else { f64::NAN };
    }
    let is_int = nu == nu.floor();
    if x < 0.0 {
        return if is_int { if (nu as i64) % 2 == 0 { bessel_i(nu, -x) } else { -bessel_i(nu, -x) } } else { f64::NAN };
    }
    if x > 700.0 {
        return f64::INFINITY;
    }
    if is_int {
        let n = (64.0 + 2.0 * (x + nu.abs())) as usize;
        return periodic_trapz(&|t| (x * t.cos()).exp() * (nu * t).cos(), n) / PI;
    }
    let a = quad(&|t| (x * t.cos()).exp() * (nu * t).cos(), 0.0, PI, 1e-13).value / PI;
    let b = quad(&|t| (-x * t.cosh() - nu * t).exp(), 0.0, f64::INFINITY, 1e-13).value;
    a - (nu * PI).sin() / PI * b
}

pub fn bessel_k(nu: f64, x: f64) -> f64 {
    if x <= 0.0 {
        return if x == 0.0 { f64::INFINITY } else { f64::NAN };
    }
    quad(&|t| (-x * t.cosh()).exp() * (nu * t).cosh(), 0.0, f64::INFINITY, 1e-13).value
}

fn airy_series(x: f64) -> (f64, f64) {
    // Ai = c1 f - c2 g, Bi = sqrt(3) (c1 f + c2 g)
    let c1 = 0.355_028_053_887_817_24;
    let c2 = 0.258_819_403_792_806_8;
    let x3 = x * x * x;
    let (mut f, mut g) = (1.0, x);
    let (mut tf, mut tg) = (1.0, x);
    for k in 1..200 {
        let k3 = 3.0 * k as f64;
        tf *= x3 / ((k3 - 1.0) * k3);
        tg *= x3 / (k3 * (k3 + 1.0));
        f += tf;
        g += tg;
        if tf.abs() < 1e-17 * f.abs().max(1e-300) && tg.abs() < 1e-17 * g.abs().max(1e-300) {
            break;
        }
    }
    (c1 * f - c2 * g, 3f64.sqrt() * (c1 * f + c2 * g))
}

pub fn airy_ai(x: f64) -> f64 {
    if x.abs() <= 5.0 {
        return airy_series(x).0;
    }
    if x > 0.0 {
        let z = 2.0 / 3.0 * x.powf(1.5);
        (x / 3.0).sqrt() / PI * bessel_k(1.0 / 3.0, z)
    } else {
        let ax = -x;
        let z = 2.0 / 3.0 * ax.powf(1.5);
        ax.sqrt() / 3.0 * (bessel_j(1.0 / 3.0, z) + bessel_j(-1.0 / 3.0, z))
    }
}

pub fn airy_bi(x: f64) -> f64 {
    if x.abs() <= 5.0 {
        return airy_series(x).1;
    }
    if x > 0.0 {
        let z = 2.0 / 3.0 * x.powf(1.5);
        (x / 3.0).sqrt() * (bessel_i(-1.0 / 3.0, z) + bessel_i(1.0 / 3.0, z))
    } else {
        let ax = -x;
        let z = 2.0 / 3.0 * ax.powf(1.5);
        (ax / 3.0).sqrt() * (bessel_j(-1.0 / 3.0, z) - bessel_j(1.0 / 3.0, z))
    }
}

/// Fresnel integrals `(S(x), C(x))` with the `π t² / 2` convention.
pub fn fresnel(x: f64) -> (f64, f64) {
    let ax = x.abs();
    let (s, c) = if ax < 1.8 {
        let t = PI / 2.0 * ax * ax;
        // S = Σ (-1)^n t^(2n+1) x / ((2n+1)! (4n+3)), C = Σ (-1)^n t^(2n) x / ((2n)! (4n+1))
        let (mut s, mut c) = (0.0, 0.0);
        let mut term = 1.0; // t^m / m!
        for m in 0..200 {
            if m > 0 {
                term *= t / m as f64;
            }
            let sign = if (m / 2) % 2 == 0 { 1.0 } else { -1.0 };
            if m % 2 == 0 {
                c += sign * term * ax / (2 * m + 1) as f64;
            } else {
                s += sign * term * ax / (2 * m + 1) as f64;
            }
            if term < 1e-18 {
                break;
            }
        }
        (s, c)
    } else if ax < 6.0 {
        (
            quad(&|t| (PI / 2.0 * t * t).sin(), 0.0, ax, 1e-13).value,
            quad(&|t| (PI / 2.0 * t * t).cos(), 0.0, ax, 1e-13).value,
        )
    } else {
        let px2 = PI * ax * ax;
        let f = (1.0 - 3.0 / (px2 * px2) + 105.0 / px2.powi(4)) / (PI * ax);
        let g = (1.0 - 15.0 / (px2 * px2) + 945.0 / px2.powi(4)) / (PI * PI * ax * ax * ax);
        let (sn, cs) = (px2 / 2.0).sin_cos();
        (0.5 - f * cs - g * sn, 0.5 + f * sn - g * cs)
    };
    (s.copysign(x), c.copysign(x))
}

/// Complete elliptic integrals `K(m)` and `E(m)` (parameter `m = k²`).
pub fn elliptic_ke(m: f64) -> (f64, f64) {
    if m > 1.0 {
        return (f64::NAN, f64::NAN);
    }
    if m == 1.0 {
        return (f64::INFINITY, 1.0);
    }
    let mut a = 1.0;
    let mut b = (1.0 - m).sqrt();
    let mut c2sum = m / 2.0;
    let mut pow2 = 0.5;
    for _ in 0..40 {
        let an = 0.5 * (a + b);
        let c = 0.5 * (a - b);
        b = (a * b).sqrt();
        a = an;
        pow2 *= 2.0;
        c2sum += pow2 * c * c;
        if c.abs() < 1e-16 {
            break;
        }
    }
    let k = PI / (2.0 * a);
    (k, k * (1.0 - c2sum))
}

pub fn sinc(x: f64) -> f64 {
    if x == 0.0 {
        1.0
    } else {
        x.sin() / x
    }
}

pub fn harmonic(x: f64) -> f64 {
    digamma(x + 1.0) + EULER_GAMMA
}

/// Fibonacci numbers extended to real arguments.
pub fn fib(x: f64) -> f64 {
    let phi = GOLDEN;
    let v = (phi.powf(x) - (PI * x).cos() * phi.powf(-x)) / 5f64.sqrt();
    if x == x.floor() {
        v.round()
    } else {
        v
    }
}

/// Polylogarithm Li_s(x) for |x| <= 1 by direct summation (slow near 1).
pub fn polylog(s: f64, x: f64) -> f64 {
    if x.abs() > 1.0 {
        return f64::NAN;
    }
    if x == 1.0 {
        return zeta(s);
    }
    let mut sum = 0.0;
    let mut p = 1.0;
    for k in 1..200_000 {
        p *= x;
        let add = p / (k as f64).powf(s);
        sum += add;
        if add.abs() < 1e-17 * sum.abs().max(1e-300) {
            break;
        }
    }
    sum
}
