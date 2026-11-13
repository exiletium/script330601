//! Numerical kernels: quadrature, root finding, minimisation, ODE stepping.

use crate::budget;

const XGK: [f64; 8] = [
    0.991455371120812639206854697526329,
    0.949107912342758524526189684047851,
    0.864864423359769072789712788640926,
    0.741531185599394439863864773280788,
    0.586087235467691130294144838258730,
    0.405845151377397166906606412076961,
    0.207784955007898467600689403773245,
    0.0,
];
const WGK: [f64; 8] = [
    0.022935322010529224963732008058970,
    0.063092092629978553290700663189204,
    0.104790010322250183839876322541518,
    0.140653259715525918745189590510238,
    0.169004726639267902826583426598550,
    0.190350578064785409913256402421014,
    0.204432940075298892414161999234649,
    0.209482141084727828012999174891714,
];
const WG: [f64; 4] = [
    0.129484966168869693270611432679082,
    0.279705391489276667901467771423780,
    0.381830050505118944950369775488975,
    0.417959183673469387755102040816327,
];

fn gk15(f: &dyn Fn(f64) -> f64, a: f64, b: f64) -> (f64, f64) {
    let c = 0.5 * (a + b);
    let h = 0.5 * (b - a);
    let fc = f(c);
    let mut k = fc * WGK[7];
    let mut g = fc * WG[3];
    for j in 0..7 {
        let dx = h * XGK[j];
        let s = f(c - dx) + f(c + dx);
        k += WGK[j] * s;
        if j % 2 == 1 {
            g += WG[j / 2] * s;
        }
    }
    (k * h, ((k - g) * h).abs())
}

/// Result of a numerical integration.
#[derive(Clone, Copy, Debug)]
pub struct Quad {
    pub value: f64,
    pub error: f64,
    pub converged: bool,
}

/// Globally adaptive Gauss–Kronrod (7, 15) quadrature on a finite interval.
pub fn quad_finite(f: &dyn Fn(f64) -> f64, a: f64, b: f64, rel_tol: f64) -> Quad {
    if a == b {
        return Quad { value: 0.0, error: 0.0, converged: true };
    }
    let clean = |x: f64| {
        let v = f(x);
        if v.is_finite() {
            v
        } else {
            f64::NAN
        }
    };
    // start with a few panels so that narrow features are not missed
    let n0 = 8;
    let mut parts: Vec<(f64, f64, f64, f64)> = Vec::new();
    for k in 0..n0 {
        let x0 = a + (b - a) * k as f64 / n0 as f64;
        let x1 = a + (b - a) * (k + 1) as f64 / n0 as f64;
        let (v, e) = gk15(&clean, x0, x1);
        parts.push((x0, x1, v, e));
    }
    let mut iterations = 0;
    loop {
        let total: f64 = parts.iter().map(|p| p.2).sum();
        let err: f64 = parts.iter().map(|p| p.3).sum();
        if total.is_nan() {
            return Quad { value: f64::NAN, error: f64::INFINITY, converged: false };
        }
        if err <= rel_tol * total.abs().max(1e-300) || err < 1e-14 {
            return Quad { value: total, error: err, converged: true };
        }
        if parts.len() > 4000 {
            return Quad { value: total, error: err, converged: err <= 1e-6 * total.abs().max(1e-12) };
        }
        iterations += 1;
        if iterations % 64 == 0 {
            budget::check();
        }
        // bisect the worst interval
        let (idx, _) = parts.iter().enumerate().fold((0, -1.0), |acc, (i, p)| if p.3 > acc.1 { (i, p.3) } else { acc });
        let (x0, x1, _, _) = parts.swap_remove(idx);
        let m = 0.5 * (x0 + x1);
        if m == x0 || m == x1 {
            parts.push((x0, x1, 0.0, 0.0));
            let total: f64 = parts.iter().map(|p| p.2).sum();
            return Quad { value: total, error: f64::INFINITY, converged: false };
        }
        let (v1, e1) = gk15(&clean, x0, m);
        let (v2, e2) = gk15(&clean, m, x1);
        parts.push((x0, m, v1, e1));
        parts.push((m, x1, v2, e2));
    }
}

/// Integrate over any real interval; infinite limits are mapped to finite ones.
pub fn quad(f: &dyn Fn(f64) -> f64, a: f64, b: f64, rel_tol: f64) -> Quad {
    if a > b {
        let q = quad(f, b, a, rel_tol);
        return Quad { value: -q.value, ..q };
    }
    match (a.is_infinite(), b.is_infinite()) {
        (false, false) => quad_finite(f, a, b, rel_tol),
        (false, true) => {
            let g = |t: f64| {
                let u = 1.0 - t;
                f(a + t / u) / (u * u)
            };
            quad_finite(&g, 0.0, 1.0, rel_tol)
        }
        (true, false) => {
            let g = |t: f64| {
                let u = 1.0 - t;
                f(b - t / u) / (u * u)
            };
            quad_finite(&g, 0.0, 1.0, rel_tol)
        }
        (true, true) => {
            let g = |t: f64| {
                let u = 1.0 - t * t;
                f(t / u) * (1.0 + t * t) / (u * u)
            };
            quad_finite(&g, -1.0, 1.0, rel_tol)
        }
    }
}

/// Integration with known interior break points (singularities, kinks).
pub fn quad_breaks(f: &dyn Fn(f64) -> f64, a: f64, b: f64, breaks: &[f64], rel_tol: f64) -> Quad {
    let mut pts: Vec<f64> = breaks.iter().copied().filter(|x| *x > a && *x < b).collect();
    pts.sort_by(|x, y| x.total_cmp(y));
    pts.dedup();
    let mut edges = vec![a];
    edges.extend(pts);
    edges.push(b);
    let mut out = Quad { value: 0.0, error: 0.0, converged: true };
    for w in edges.windows(2) {
        let q = quad(f, w[0], w[1], rel_tol);
        out.value += q.value;
        out.error += q.error;
        out.converged &= q.converged;
    }
    out
}

/// Brent's method on a bracketing interval.
pub fn brent(f: &dyn Fn(f64) -> f64, mut a: f64, mut b: f64, tol: f64) -> Option<f64> {
    let mut fa = f(a);
    let mut fb = f(b);
    if !fa.is_finite() || !fb.is_finite() {
        return None;
    }
    if fa == 0.0 {
        return Some(a);
    }
    if fb == 0.0 {
        return Some(b);
    }
    if fa.signum() == fb.signum() {
        return None;
    }
    let (mut c, mut fc) = (a, fa);
    let mut d = b - a;
    let mut e = d;
    for _ in 0..200 {
        if fb.signum() == fc.signum() {
            c = a;
            fc = fa;
            d = b - a;
            e = d;
        }
        if fc.abs() < fb.abs() {
            a = b;
            b = c;
            c = a;
            fa = fb;
            fb = fc;
            fc = fa;
        }
        let tol1 = 2.0 * f64::EPSILON * b.abs() + 0.5 * tol;
        let xm = 0.5 * (c - b);
        if xm.abs() <= tol1 || fb == 0.0 {
            return Some(b);
        }
        if e.abs() >= tol1 && fa.abs() > fb.abs() {
            let s = fb / fa;
            let (mut p, mut q);
            if a == c {
                p = 2.0 * xm * s;
                q = 1.0 - s;
            } else {
                let q0 = fa / fc;
                let r = fb / fc;
                p = s * (2.0 * xm * q0 * (q0 - r) - (b - a) * (r - 1.0));
                q = (q0 - 1.0) * (r - 1.0) * (s - 1.0);
            }
            if p > 0.0 {
                q = -q;
            }
            p = p.abs();
            if 2.0 * p < (3.0 * xm * q - (tol1 * q).abs()).min((e * q).abs()) {
                e = d;
                d = p / q;
            } else {
                d = xm;
                e = d;
            }
        } else {
            d = xm;
            e = d;
        }
        a = b;
        fa = fb;
        b += if d.abs() > tol1 { d } else { tol1.copysign(xm) };
        fb = f(b);
        if !fb.is_finite() {
            return None;
        }
    }
    Some(b)
}

/// Real roots of `f` on `[a, b]`: sign-change scan refined with Brent, plus
/// touching roots (even multiplicity) found through local minima of `|f|`.
pub fn find_roots(f: &dyn Fn(f64) -> f64, a: f64, b: f64, samples: usize) -> Vec<f64> {
    let mut roots: Vec<f64> = Vec::new();
    let n = samples.max(16);
    let h = (b - a) / n as f64;
    let xs: Vec<f64> = (0..=n).map(|k| a + h * k as f64).collect();
    let ys: Vec<f64> = xs.iter().map(|x| f(*x)).collect();
    let scale = {
        let mut v: Vec<f64> = ys.iter().filter(|y| y.is_finite()).map(|y| y.abs()).collect();
        v.sort_by(|p, q| p.total_cmp(q));
        v.get(v.len() / 2).copied().unwrap_or(1.0).max(1e-12)
    };
    let push = |r: f64, roots: &mut Vec<f64>| {
        if !roots.iter().any(|x| (x - r).abs() <= 1e-9 * (1.0 + r.abs())) {
            roots.push(r);
        }
    };
    for k in 0..n {
        let (y0, y1) = (ys[k], ys[k + 1]);
        if !y0.is_finite() || !y1.is_finite() {
            continue;
        }
        if y0 == 0.0 {
            push(xs[k], &mut roots);
            continue;
        }
        if y0.signum() != y1.signum() && y1 != 0.0 {
            if let Some(r) = brent(f, xs[k], xs[k + 1], 1e-14) {
                // reject sign changes across poles
                let fr = f(r).abs();
                if fr.is_finite() && fr <= 1e-6 * scale.max(y0.abs().min(y1.abs())) + 1e-9 {
                    push(r, &mut roots);
                }
            }
        }
    }
    if ys[n] == 0.0 {
        push(xs[n], &mut roots);
    }
    // even-multiplicity roots: local minima of |f| that touch zero
    for k in 1..n {
        let (y0, y1, y2) = (ys[k - 1].abs(), ys[k].abs(), ys[k + 1].abs());
        if y0.is_finite() && y1.is_finite() && y2.is_finite() && y1 <= y0 && y1 <= y2 && y1 < 0.05 * scale {
            let g = |x: f64| f(x).abs();
            let (xm, fm) = golden_min(&g, xs[k - 1], xs[k + 1], 1e-13);
            if fm < 1e-9 * scale.max(1.0) {
                // polish with Newton on the derivative-free secant of f
                push(xm, &mut roots);
            }
        }
    }
    roots.sort_by(|p, q| p.total_cmp(q));
    // snap near-integers and tiny values
    for r in roots.iter_mut() {
        let nearest = r.round();
        if (*r - nearest).abs() < 1e-10 && f(nearest).abs() <= f(*r).abs() {
            *r = nearest;
        }
    }
    roots.dedup_by(|p, q| (*p - *q).abs() <= 1e-8 * (1.0 + q.abs()));
    roots
}

/// Golden-section minimisation; returns `(argmin, min)`.
pub fn golden_min(f: &dyn Fn(f64) -> f64, mut a: f64, mut b: f64, tol: f64) -> (f64, f64) {
    let gr = 0.5 * (5f64.sqrt() - 1.0);
    let mut c = b - gr * (b - a);
    let mut d = a + gr * (b - a);
    let (mut fc, mut fd) = (f(c), f(d));
    for _ in 0..200 {
        if (b - a).abs() <= tol * (1.0 + c.abs() + d.abs()) {
            break;
        }
        if fc < fd {
            b = d;
            d = c;
            fd = fc;
            c = b - gr * (b - a);
            fc = f(c);
        } else {
            a = c;
            c = d;
            fc = fd;
            d = a + gr * (b - a);
            fd = f(d);
        }
    }
    let x = 0.5 * (a + b);
    (x, f(x))
}

/// Newton iteration in several variables with a finite-difference Jacobian.
pub fn newton_system(f: &dyn Fn(&[f64]) -> Vec<f64>, x0: &[f64], iters: usize) -> Option<Vec<f64>> {
    let n = x0.len();
    let mut x = x0.to_vec();
    for _ in 0..iters {
        let fx = f(&x);
        if fx.len() != n || fx.iter().any(|v| !v.is_finite()) {
            return None;
        }
        let norm: f64 = fx.iter().map(|v| v * v).sum::<f64>().sqrt();
        if norm < 1e-13 {
            return Some(x);
        }
        let mut jac = vec![vec![0.0; n]; n];
        for j in 0..n {
            let h = 1e-7 * (1.0 + x[j].abs());
            let mut xp = x.clone();
            xp[j] += h;
            let fp = f(&xp);
            for i in 0..n {
                jac[i][j] = (fp[i] - fx[i]) / h;
            }
        }
        let rhs: Vec<f64> = fx.iter().map(|v| -v).collect();
        let dx = solve_linear(jac, rhs)?;
        // damped update
        let mut lambda = 1.0;
        let mut improved = false;
        for _ in 0..12 {
            let xn: Vec<f64> = x.iter().zip(&dx).map(|(a, b)| a + lambda * b).collect();
            let fnn = f(&xn);
            let nn: f64 = fnn.iter().map(|v| v * v).sum::<f64>().sqrt();
            if nn.is_finite() && nn < norm {
                x = xn;
                improved = true;
                break;
            }
            lambda *= 0.5;
        }
        if !improved {
            return None;
        }
    }
    let fx = f(&x);
    let norm: f64 = fx.iter().map(|v| v * v).sum::<f64>().sqrt();
    if norm < 1e-9 {
        Some(x)
    } else {
        None
    }
}

/// Gaussian elimination with partial pivoting.
pub fn solve_linear(mut a: Vec<Vec<f64>>, mut b: Vec<f64>) -> Option<Vec<f64>> {
    let n = b.len();
    for col in 0..n {
        let piv = (col..n).max_by(|i, j| a[*i][col].abs().total_cmp(&a[*j][col].abs()))?;
        if a[piv][col].abs() < 1e-300 {
            return None;
        }
        a.swap(col, piv);
        b.swap(col, piv);
        for r in col + 1..n {
            let m = a[r][col] / a[col][col];
            if m != 0.0 {
                for c in col..n {
                    a[r][c] -= m * a[col][c];
                }
                b[r] -= m * b[col];
            }
        }
    }
    let mut x = vec![0.0; n];
    for r in (0..n).rev() {
        let mut s = b[r];
        for c in r + 1..n {
            s -= a[r][c] * x[c];
        }
        x[r] = s / a[r][r];
    }
    Some(x)
}

/// Adaptive Dormand–Prince RK45 integration of `y' = f(t, y)`.
/// Returns the sampled trajectory `(t, y)`.
pub fn ode45(f: &dyn Fn(f64, &[f64]) -> Vec<f64>, t0: f64, y0: &[f64], t1: f64, max_steps: usize) -> Vec<(f64, Vec<f64>)> {
    const C: [f64; 7] = [0.0, 0.2, 0.3, 0.8, 8.0 / 9.0, 1.0, 1.0];
    const A: [[f64; 6]; 7] = [
        [0.0; 6],
        [0.2, 0.0, 0.0, 0.0, 0.0, 0.0],
        [3.0 / 40.0, 9.0 / 40.0, 0.0, 0.0, 0.0, 0.0],
        [44.0 / 45.0, -56.0 / 15.0, 32.0 / 9.0, 0.0, 0.0, 0.0],
        [19372.0 / 6561.0, -25360.0 / 2187.0, 64448.0 / 6561.0, -212.0 / 729.0, 0.0, 0.0],
        [9017.0 / 3168.0, -355.0 / 33.0, 46732.0 / 5247.0, 49.0 / 176.0, -5103.0 / 18656.0, 0.0],
        [35.0 / 384.0, 0.0, 500.0 / 1113.0, 125.0 / 192.0, -2187.0 / 6784.0, 11.0 / 84.0],
    ];
    const B5: [f64; 7] = [35.0 / 384.0, 0.0, 500.0 / 1113.0, 125.0 / 192.0, -2187.0 / 6784.0, 11.0 / 84.0, 0.0];
    const B4: [f64; 7] = [5179.0 / 57600.0, 0.0, 7571.0 / 16695.0, 393.0 / 640.0, -92097.0 / 339200.0, 187.0 / 2100.0, 1.0 / 40.0];
    let n = y0.len();
    let dir = if t1 >= t0 { 1.0 } else { -1.0 };
    let span = (t1 - t0).abs();
    let mut t = t0;
    let mut y = y0.to_vec();
    let mut h = dir * span / 200.0;
    let mut out = vec![(t, y.clone())];
    let (rtol, atol) = (1e-8, 1e-10);
    let mut steps = 0;
    while (t1 - t) * dir > 1e-14 * span.max(1.0) && steps < max_steps {
        steps += 1;
        if steps % 256 == 0 {
            budget::check();
        }
        if (t + h - t1) * dir > 0.0 {
            h = t1 - t;
        }
        let mut k: Vec<Vec<f64>> = Vec::with_capacity(7);
        for s in 0..7 {
            let mut ys = y.clone();
            for (j, kj) in k.iter().enumerate() {
                let a = A[s][j];
                if a != 0.0 {
                    for i in 0..n {
                        ys[i] += h * a * kj[i];
                    }
                }
            }
            k.push(f(t + C[s] * h, &ys));
        }
        let mut y5 = y.clone();
        let mut err: f64 = 0.0;
        for i in 0..n {
            let mut d5 = 0.0;
            let mut d4 = 0.0;
            for s in 0..7 {
                d5 += B5[s] * k[s][i];
                d4 += B4[s] * k[s][i];
            }
            y5[i] += h * d5;
            let sc = atol + rtol * y[i].abs().max(y5[i].abs());
            err = err.max((h * (d5 - d4)).abs() / sc);
        }
        if !err.is_finite() {
            h *= 0.25;
            if h.abs() < 1e-14 * span.max(1.0) {
                break;
            }
            continue;
        }
        if err <= 1.0 {
            t += h;
            y = y5;
            out.push((t, y.clone()));
            if y.iter().any(|v| !v.is_finite() || v.abs() > 1e12) {
                break;
            }
        }
        let fac = (0.9 * err.max(1e-10).powf(-0.2)).clamp(0.2, 5.0);
        h *= fac;
        if h.abs() < 1e-14 * span.max(1.0) {
            break;
        }
    }
    out
}

/// Central finite-difference derivative with Richardson extrapolation.
pub fn nderiv(f: &dyn Fn(f64) -> f64, x: f64) -> f64 {
    let h = 1e-3 * (1.0 + x.abs());
    let d = |h: f64| (f(x + h) - f(x - h)) / (2.0 * h);
    let d1 = d(h);
    let d2 = d(h / 2.0);
    (4.0 * d2 - d1) / 3.0
}

/// Numerical limit by Richardson-style sequence acceleration.
/// `dir` is +1 (from above), -1 (from below).
pub fn nlimit(f: &dyn Fn(f64) -> f64, a: f64, dir: f64) -> Option<f64> {
    let mut vals = Vec::new();
    if a.is_infinite() {
        let mut x = 16.0f64;
        for _ in 0..40 {
            let v = f(a.signum() * x);
            if !v.is_finite() {
                break;
            }
            vals.push(v);
            x *= 2.0;
        }
    } else {
        let mut h = 0.25 * dir * (1.0 + a.abs() * 0.1);
        for _ in 0..40 {
            let v = f(a + h);
            if !v.is_finite() {
                break;
            }
            vals.push(v);
            h *= 0.5;
        }
    }
    if vals.len() < 8 {
        return None;
    }
    let n = vals.len();
    let last = vals[n - 1];
    let d1 = (vals[n - 1] - vals[n - 2]).abs();
    let d2 = (vals[n - 2] - vals[n - 3]).abs();
    if last.abs() > 1e12 && vals[n - 1].abs() > vals[n - 2].abs() {
        return Some(if last > 0.0 { f64::INFINITY } else { f64::NEG_INFINITY });
    }
    if d1 <= 1e-9 * (1.0 + last.abs()) || (d1 < d2 * 0.75 && d1 < 1e-4 * (1.0 + last.abs())) {
        // Aitken extrapolation
        let (a0, a1, a2) = (vals[n - 3], vals[n - 2], vals[n - 1]);
        let den = a2 - 2.0 * a1 + a0;
        let ext = if den.abs() > 1e-300 { a2 - (a2 - a1) * (a2 - a1) / den } else { a2 };
        return Some(if ext.is_finite() { ext } else { last });
    }
    None
}
