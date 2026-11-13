//! Polynomial arithmetic over the rationals: dense univariate ([`QPoly`]) and
//! sparse multivariate ([`MPoly`]) polynomials, GCDs, square-free
//! decomposition, root finding and factorisation.

use crate::budget;
use crate::expr::{self, Expr, Rat};
use num_bigint::BigInt;
use num_complex::Complex64;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};
use std::collections::BTreeMap;

fn ri(n: i64) -> Rat {
    Rat::from_integer(BigInt::from(n))
}

// ===========================================================================
// dense univariate polynomials
// ===========================================================================

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QPoly(pub Vec<Rat>);

impl QPoly {
    pub fn new(mut c: Vec<Rat>) -> QPoly {
        while c.last().map_or(false, |x| x.is_zero()) {
            c.pop();
        }
        QPoly(c)
    }
    pub fn zero() -> QPoly {
        QPoly(vec![])
    }
    pub fn one() -> QPoly {
        QPoly(vec![Rat::one()])
    }
    pub fn x() -> QPoly {
        QPoly(vec![Rat::zero(), Rat::one()])
    }
    pub fn constant(r: Rat) -> QPoly {
        QPoly::new(vec![r])
    }
    pub fn from_ints(c: &[i64]) -> QPoly {
        QPoly::new(c.iter().map(|v| ri(*v)).collect())
    }
    pub fn is_zero(&self) -> bool {
        self.0.is_empty()
    }
    pub fn degree(&self) -> usize {
        self.0.len().saturating_sub(1)
    }
    pub fn lc(&self) -> Rat {
        self.0.last().cloned().unwrap_or_else(Rat::zero)
    }
    pub fn coeff(&self, k: usize) -> Rat {
        self.0.get(k).cloned().unwrap_or_else(Rat::zero)
    }
    pub fn is_constant(&self) -> bool {
        self.0.len() <= 1
    }

    pub fn add(&self, o: &QPoly) -> QPoly {
        let n = self.0.len().max(o.0.len());
        QPoly::new((0..n).map(|k| self.coeff(k) + o.coeff(k)).collect())
    }
    pub fn sub(&self, o: &QPoly) -> QPoly {
        let n = self.0.len().max(o.0.len());
        QPoly::new((0..n).map(|k| self.coeff(k) - o.coeff(k)).collect())
    }
    pub fn neg(&self) -> QPoly {
        QPoly(self.0.iter().map(|c| -c).collect())
    }
    pub fn scale(&self, r: &Rat) -> QPoly {
        QPoly::new(self.0.iter().map(|c| c * r).collect())
    }
    pub fn mul(&self, o: &QPoly) -> QPoly {
        if self.is_zero() || o.is_zero() {
            return QPoly::zero();
        }
        let mut out = vec![Rat::zero(); self.0.len() + o.0.len() - 1];
        for (i, a) in self.0.iter().enumerate() {
            if a.is_zero() {
                continue;
            }
            for (j, b) in o.0.iter().enumerate() {
                out[i + j] += a * b;
            }
            budget::tick();
        }
        QPoly::new(out)
    }
    pub fn pow(&self, n: u32) -> QPoly {
        let mut r = QPoly::one();
        for _ in 0..n {
            r = r.mul(self);
        }
        r
    }
    pub fn divrem(&self, d: &QPoly) -> (QPoly, QPoly) {
        assert!(!d.is_zero(), "division by zero polynomial");
        if self.0.len() < d.0.len() {
            return (QPoly::zero(), self.clone());
        }
        let mut r = self.0.clone();
        let dl = d.lc();
        let dd = d.degree();
        let mut q = vec![Rat::zero(); self.0.len() - d.0.len() + 1];
        for k in (0..q.len()).rev() {
            let c = &r[k + dd] / &dl;
            if !c.is_zero() {
                for j in 0..=dd {
                    let t = &c * &d.0[j];
                    r[k + j] -= t;
                }
            }
            q[k] = c;
            budget::tick();
        }
        (QPoly::new(q), QPoly::new(r))
    }
    pub fn rem(&self, d: &QPoly) -> QPoly {
        self.divrem(d).1
    }
    pub fn div_exact(&self, d: &QPoly) -> Option<QPoly> {
        let (q, r) = self.divrem(d);
        if r.is_zero() {
            Some(q)
        } else {
            None
        }
    }
    pub fn monic(&self) -> QPoly {
        if self.is_zero() {
            return self.clone();
        }
        self.scale(&self.lc().recip())
    }
    pub fn gcd(&self, o: &QPoly) -> QPoly {
        let (mut a, mut b) = (self.clone(), o.clone());
        while !b.is_zero() {
            let r = a.rem(&b);
            a = b;
            b = r.primitive();
        }
        a.monic()
    }
    /// Extended Euclid: `(g, s, t)` with `s*self + t*o = g` (g monic).
    pub fn xgcd(&self, o: &QPoly) -> (QPoly, QPoly, QPoly) {
        let (mut r0, mut r1) = (self.clone(), o.clone());
        let (mut s0, mut s1) = (QPoly::one(), QPoly::zero());
        let (mut t0, mut t1) = (QPoly::zero(), QPoly::one());
        while !r1.is_zero() {
            let (q, r) = r0.divrem(&r1);
            r0 = std::mem::replace(&mut r1, r);
            let s = s0.sub(&q.mul(&s1));
            s0 = std::mem::replace(&mut s1, s);
            let t = t0.sub(&q.mul(&t1));
            t0 = std::mem::replace(&mut t1, t);
        }
        if r0.is_zero() {
            return (r0, s0, t0);
        }
        let inv = r0.lc().recip();
        (r0.scale(&inv), s0.scale(&inv), t0.scale(&inv))
    }
    pub fn derivative(&self) -> QPoly {
        QPoly::new(self.0.iter().enumerate().skip(1).map(|(k, c)| c * ri(k as i64)).collect())
    }
    pub fn eval(&self, x: &Rat) -> Rat {
        let mut acc = Rat::zero();
        for c in self.0.iter().rev() {
            acc = acc * x + c;
        }
        acc
    }
    pub fn eval_f64(&self, x: f64) -> f64 {
        self.0.iter().rev().fold(0.0, |acc, c| acc * x + expr::rat_to_f64(c))
    }
    pub fn eval_c(&self, x: Complex64) -> Complex64 {
        self.0.iter().rev().fold(Complex64::new(0.0, 0.0), |acc, c| acc * x + expr::rat_to_f64(c))
    }
    pub fn compose(&self, inner: &QPoly) -> QPoly {
        let mut acc = QPoly::zero();
        for c in self.0.iter().rev() {
            acc = acc.mul(inner).add(&QPoly::constant(c.clone()));
        }
        acc
    }
    /// Rational content (positive) such that `self / content` has coprime
    /// integer coefficients.
    pub fn content(&self) -> Rat {
        if self.is_zero() {
            return Rat::one();
        }
        let g = self.0.iter().fold(BigInt::zero(), |g, c| g.gcd(c.numer()));
        let l = self.0.iter().fold(BigInt::one(), |l, c| l.lcm(c.denom()));
        Rat::new(g, l)
    }
    /// Primitive integer polynomial with positive leading coefficient.
    pub fn primitive(&self) -> QPoly {
        if self.is_zero() {
            return self.clone();
        }
        let mut c = self.content();
        if self.lc().is_negative() {
            c = -c;
        }
        self.scale(&c.recip())
    }
    pub fn int_coeffs(&self) -> Vec<BigInt> {
        self.0.iter().map(|c| c.to_integer()).collect()
    }

    /// Square-free decomposition (Yun): `self = c * Π f_i^i`.
    pub fn square_free(&self) -> Vec<(QPoly, u32)> {
        let mut out = Vec::new();
        if self.degree() == 0 {
            return out;
        }
        let f = self.primitive();
        let df = f.derivative();
        let g = f.gcd(&df);
        let mut c = f.div_exact(&g).unwrap();
        let mut d = df.div_exact(&g).unwrap().sub(&c.derivative());
        let mut k = 1;
        while c.degree() > 0 {
            let p = c.gcd(&d);
            c = c.div_exact(&p).unwrap();
            d = d.div_exact(&p).unwrap().sub(&c.derivative());
            if p.degree() > 0 {
                out.push((p.primitive(), k));
            }
            k += 1;
            if k > 10_000 {
                break;
            }
        }
        out
    }

    pub fn to_expr(&self, x: &Expr) -> Expr {
        let mut terms = Vec::new();
        for (k, c) in self.0.iter().enumerate() {
            if !c.is_zero() {
                terms.push(expr::mul(vec![Expr::Num(c.clone()), expr::pow(x.clone(), expr::int(k as i64))]));
            }
        }
        expr::add(terms)
    }

    /// All complex roots (with multiplicity) in double precision.
    pub fn roots_c(&self) -> Vec<Complex64> {
        let mut out = Vec::new();
        if self.degree() == 0 {
            return out;
        }
        for (f, m) in self.square_free() {
            for r in aberth(&f) {
                for _ in 0..m {
                    out.push(r);
                }
            }
        }
        out
    }
}

/// Aberth–Ehrlich simultaneous iteration for a square-free polynomial.
fn aberth(p: &QPoly) -> Vec<Complex64> {
    let n = p.degree();
    if n == 0 {
        return vec![];
    }
    // scale coefficients to avoid overflow
    let lc = expr::rat_to_f64(&p.lc());
    let c: Vec<f64> = p.0.iter().map(|x| expr::rat_to_f64(x) / lc).collect();
    if n == 1 {
        return vec![Complex64::new(-c[0], 0.0)];
    }
    if n == 2 {
        let disc = Complex64::new(c[1] * c[1] - 4.0 * c[0], 0.0).sqrt();
        // numerically stable quadratic formula
        let q = if c[1] >= 0.0 { -(Complex64::new(c[1], 0.0) + disc) / 2.0 } else { -(Complex64::new(c[1], 0.0) - disc) / 2.0 };
        if q.norm() == 0.0 {
            return vec![Complex64::new(0.0, 0.0); 2];
        }
        return vec![q, Complex64::new(c[0], 0.0) / q];
    }
    let eval = |z: Complex64| -> (Complex64, Complex64) {
        let mut v = Complex64::new(0.0, 0.0);
        let mut d = Complex64::new(0.0, 0.0);
        for k in (0..=n).rev() {
            d = d * z + v;
            v = v * z + c[k];
        }
        (v, d)
    };
    let radius = 1.0 + c[..n].iter().fold(0.0f64, |m, x| m.max(x.abs()));
    let radius = radius.min(c[..n].iter().enumerate().map(|(k, x)| (x.abs() * n as f64).powf(1.0 / (n - k) as f64)).fold(0.0f64, f64::max).max(1e-3));
    let mut z: Vec<Complex64> = (0..n).map(|k| Complex64::from_polar(radius * 0.9, 2.0 * std::f64::consts::PI * k as f64 / n as f64 + 0.37)).collect();
    for iter in 0..400 {
        let mut moved = 0.0f64;
        for k in 0..n {
            let (v, d) = eval(z[k]);
            if v.norm() == 0.0 {
                continue;
            }
            let ratio = if d.norm() == 0.0 { Complex64::new(1e-3, 1e-3) } else { v / d };
            let mut s = Complex64::new(0.0, 0.0);
            for j in 0..n {
                if j != k {
                    let diff = z[k] - z[j];
                    if diff.norm() > 0.0 {
                        s += 1.0 / diff;
                    }
                }
            }
            let step = ratio / (1.0 - ratio * s);
            if step.re.is_finite() && step.im.is_finite() {
                z[k] -= step;
                moved = moved.max(step.norm() / (1.0 + z[k].norm()));
            }
        }
        if moved < 1e-15 {
            break;
        }
        if iter % 32 == 31 {
            budget::check();
        }
    }
    // Newton polish and clean-up
    for zk in z.iter_mut() {
        for _ in 0..3 {
            let (v, d) = eval(*zk);
            if d.norm() > 0.0 {
                let step = v / d;
                if step.norm() < 1e-6 * (1.0 + zk.norm()) {
                    *zk -= step;
                }
            }
        }
        if zk.im.abs() < 1e-10 * (1.0 + zk.re.abs()) {
            zk.im = 0.0;
        }
        if zk.re.abs() < 1e-10 * (1.0 + zk.im.abs()) {
            zk.re = 0.0;
        }
    }
    z.sort_by(|a, b| a.re.total_cmp(&b.re).then(a.im.total_cmp(&b.im)));
    z
}

/// Exact rational roots with multiplicities.
pub fn rational_roots(p: &QPoly) -> Vec<(Rat, u32)> {
    let mut out: Vec<(Rat, u32)> = Vec::new();
    if p.degree() == 0 {
        return out;
    }
    for (f, m) in p.square_free() {
        let lc = f.lc().to_integer();
        let mut g = f.clone();
        // zero root
        if g.coeff(0).is_zero() {
            out.push((Rat::zero(), m));
            g = g.div_exact(&QPoly::x()).unwrap();
        }
        if g.degree() == 0 {
            continue;
        }
        if g.degree() == 1 {
            out.push((-g.coeff(0) / g.coeff(1), m));
            continue;
        }
        let lcf = expr::rat_to_f64(&g.lc());
        for r in aberth(&g) {
            if r.im != 0.0 || !lcf.is_finite() {
                continue;
            }
            let approx = r.re * lcf;
            if !approx.is_finite() || approx.abs() > 1e15 {
                continue;
            }
            let Some(num) = Rat::from_float(approx.round()) else { continue };
            let cand = num / Rat::from_integer(lc.clone());
            if g.eval(&cand).is_zero() && !out.iter().any(|(x, _)| *x == cand) {
                out.push((cand, m));
            }
        }
    }
    out.sort();
    out
}

/// Factor a square-free primitive integer polynomial into irreducibles over Q.
fn factor_square_free(f: &QPoly) -> Vec<QPoly> {
    let mut out = Vec::new();
    let mut g = f.clone();
    if g.degree() == 0 {
        return out;
    }
    if g.coeff(0).is_zero() {
        out.push(QPoly::x());
        g = g.div_exact(&QPoly::x()).unwrap();
    }
    // linear factors
    for (r, _) in rational_roots(&g) {
        let lin = QPoly::new(vec![-Rat::from_integer(r.numer().clone()), Rat::from_integer(r.denom().clone())]);
        if let Some(q) = g.div_exact(&lin) {
            out.push(lin);
            g = q;
        }
    }
    g = g.primitive();
    if g.degree() <= 3 {
        // no rational roots left: degree 2 and 3 are irreducible
        if g.degree() > 0 {
            out.push(g);
        }
        out.sort_by(|a, b| a.degree().cmp(&b.degree()).then_with(|| a.0.cmp(&b.0)));
        return out;
    }
    // recombination of numeric roots
    let roots = aberth(&g);
    let mut units: Vec<Vec<Complex64>> = Vec::new();
    let mut used = vec![false; roots.len()];
    for a in 0..roots.len() {
        if used[a] {
            continue;
        }
        used[a] = true;
        if roots[a].im == 0.0 {
            units.push(vec![roots[a]]);
            continue;
        }
        // find the conjugate
        let mut best = None;
        for b in 0..roots.len() {
            if !used[b] {
                let d = (roots[b] - roots[a].conj()).norm();
                if best.map_or(true, |(_, bd)| d < bd) {
                    best = Some((b, d));
                }
            }
        }
        match best {
            Some((b, d)) if d < 1e-6 * (1.0 + roots[a].norm()) => {
                used[b] = true;
                units.push(vec![roots[a], roots[b]]);
            }
            _ => units.push(vec![roots[a]]),
        }
    }
    let try_subset = |g: &QPoly, subset: &[usize], units: &[Vec<Complex64>]| -> Option<QPoly> {
        let lc = expr::rat_to_f64(&g.lc());
        let mut poly = vec![Complex64::new(lc, 0.0)];
        for &u in subset {
            for r in &units[u] {
                let mut next = vec![Complex64::new(0.0, 0.0); poly.len() + 1];
                for (k, c) in poly.iter().enumerate() {
                    next[k + 1] += *c;
                    next[k] -= *c * *r;
                }
                poly = next;
            }
        }
        let mut coeffs = Vec::with_capacity(poly.len());
        for c in &poly {
            let scale = 1.0 + c.norm();
            if c.im.abs() > 1e-5 * scale || (c.re - c.re.round()).abs() > 1e-5 * scale.min(1e4) || c.re.abs() > 1e15 {
                return None;
            }
            coeffs.push(Rat::from_float(c.re.round())?);
        }
        let cand = QPoly::new(coeffs).primitive();
        if cand.degree() == 0 || cand.degree() == g.degree() {
            return None;
        }
        g.div_exact(&cand).map(|_| cand)
    };
    let mut alive: Vec<usize> = (0..units.len()).collect();
    let mut size = 1;
    let mut tested = 0usize;
    'outer: while size * 2 <= alive.len() && alive.len() <= 26 {
        // iterate over combinations of `size` units among `alive`
        let mut idx: Vec<usize> = (0..size).collect();
        loop {
            let subset: Vec<usize> = idx.iter().map(|k| alive[*k]).collect();
            let deg: usize = subset.iter().map(|u| units[*u].len()).sum();
            if deg * 2 <= g.degree() || size * 2 == alive.len() {
                tested += 1;
                if tested % 256 == 0 {
                    budget::check();
                }
                if tested > 2_000_000 {
                    break 'outer;
                }
                if let Some(fac) = try_subset(&g, &subset, &units) {
                    g = g.div_exact(&fac).unwrap().primitive();
                    out.push(fac);
                    alive.retain(|u| !subset.contains(u));
                    continue 'outer;
                }
            }
            // next combination
            let mut k = size;
            loop {
                if k == 0 {
                    size += 1;
                    continue 'outer;
                }
                k -= 1;
                if idx[k] != k + alive.len() - size {
                    break;
                }
            }
            idx[k] += 1;
            for j in k + 1..size {
                idx[j] = idx[j - 1] + 1;
            }
        }
    }
    if g.degree() > 0 {
        out.push(g);
    }
    out.sort_by(|a, b| a.degree().cmp(&b.degree()).then_with(|| a.0.cmp(&b.0)));
    out
}

/// Factorisation over Q: `p = content * Π f_i^{m_i}` with primitive integer
/// irreducible factors.
pub fn factor(p: &QPoly) -> (Rat, Vec<(QPoly, u32)>) {
    if p.is_zero() {
        return (Rat::zero(), vec![]);
    }
    let mut content = p.content();
    if p.lc().is_negative() {
        content = -content;
    }
    let mut out = Vec::new();
    for (f, m) in p.square_free() {
        for g in factor_square_free(&f) {
            out.push((g, m));
        }
    }
    out.sort_by(|a, b| a.0.degree().cmp(&b.0.degree()).then_with(|| a.0 .0.cmp(&b.0 .0)));
    (content, out)
}

/// Cyclotomic-style helper: is the polynomial irreducible over Q?
pub fn is_irreducible(p: &QPoly) -> bool {
    let (_, fs) = factor(p);
    fs.len() == 1 && fs[0].1 == 1
}

// ===========================================================================
// sparse multivariate polynomials
// ===========================================================================

pub type Mono = Vec<u16>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MPoly {
    pub n: usize,
    pub t: BTreeMap<Mono, Rat>,
}

impl MPoly {
    pub fn zero(n: usize) -> MPoly {
        MPoly { n, t: BTreeMap::new() }
    }
    pub fn constant(n: usize, r: Rat) -> MPoly {
        let mut p = MPoly::zero(n);
        if !r.is_zero() {
            p.t.insert(vec![0; n], r);
        }
        p
    }
    pub fn one(n: usize) -> MPoly {
        MPoly::constant(n, Rat::one())
    }
    pub fn var(n: usize, k: usize) -> MPoly {
        let mut m = vec![0; n];
        m[k] = 1;
        let mut p = MPoly::zero(n);
        p.t.insert(m, Rat::one());
        p
    }
    pub fn is_zero(&self) -> bool {
        self.t.is_empty()
    }
    pub fn as_constant(&self) -> Option<Rat> {
        match self.t.len() {
            0 => Some(Rat::zero()),
            1 => {
                let (m, c) = self.t.iter().next().unwrap();
                if m.iter().all(|e| *e == 0) {
                    Some(c.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    pub fn is_one(&self) -> bool {
        self.as_constant().map_or(false, |c| c.is_one())
    }
    fn add_term(&mut self, m: Mono, c: Rat) {
        if c.is_zero() {
            return;
        }
        match self.t.entry(m) {
            std::collections::btree_map::Entry::Occupied(mut e) => {
                *e.get_mut() += c;
                if e.get().is_zero() {
                    e.remove();
                }
            }
            std::collections::btree_map::Entry::Vacant(e) => {
                e.insert(c);
            }
        }
    }
    pub fn add(&self, o: &MPoly) -> MPoly {
        let mut r = self.clone();
        for (m, c) in &o.t {
            r.add_term(m.clone(), c.clone());
        }
        r
    }
    pub fn sub(&self, o: &MPoly) -> MPoly {
        let mut r = self.clone();
        for (m, c) in &o.t {
            r.add_term(m.clone(), -c);
        }
        r
    }
    pub fn neg(&self) -> MPoly {
        MPoly { n: self.n, t: self.t.iter().map(|(m, c)| (m.clone(), -c)).collect() }
    }
    pub fn scale(&self, r: &Rat) -> MPoly {
        if r.is_zero() {
            return MPoly::zero(self.n);
        }
        MPoly { n: self.n, t: self.t.iter().map(|(m, c)| (m.clone(), c * r)).collect() }
    }
    pub fn mul(&self, o: &MPoly) -> MPoly {
        let mut r = MPoly::zero(self.n);
        if self.t.len() * o.t.len() > 4_000_000 {
            budget::check();
        }
        for (m1, c1) in &self.t {
            for (m2, c2) in &o.t {
                let m: Mono = m1.iter().zip(m2).map(|(a, b)| a + b).collect();
                r.add_term(m, c1 * c2);
            }
            budget::tick();
        }
        r
    }
    pub fn pow(&self, k: u32) -> MPoly {
        let mut result = MPoly::one(self.n);
        let mut base = self.clone();
        let mut k = k;
        while k > 0 {
            if k & 1 == 1 {
                result = result.mul(&base);
            }
            k >>= 1;
            if k > 0 {
                base = base.mul(&base);
            }
        }
        result
    }
    pub fn degree(&self, v: usize) -> u16 {
        self.t.keys().map(|m| m[v]).max().unwrap_or(0)
    }
    pub fn total_degree(&self) -> u32 {
        self.t.keys().map(|m| m.iter().map(|e| *e as u32).sum()).max().unwrap_or(0)
    }
    pub fn lead(&self) -> Option<(&Mono, &Rat)> {
        self.t.iter().next_back()
    }
    /// Exact division; `None` when `d` does not divide `self`.
    pub fn div_exact(&self, d: &MPoly) -> Option<MPoly> {
        if d.is_zero() {
            return None;
        }
        if let Some(c) = d.as_constant() {
            return Some(self.scale(&c.recip()));
        }
        let (dm, dc) = d.lead().map(|(m, c)| (m.clone(), c.clone()))?;
        let mut rem = self.clone();
        let mut q = MPoly::zero(self.n);
        let mut guard = 0usize;
        while let Some((m, c)) = rem.lead().map(|(m, c)| (m.clone(), c.clone())) {
            if m.iter().zip(&dm).any(|(a, b)| a < b) {
                return None;
            }
            let qm: Mono = m.iter().zip(&dm).map(|(a, b)| a - b).collect();
            let qc = &c / &dc;
            for (m2, c2) in &d.t {
                let mm: Mono = qm.iter().zip(m2).map(|(a, b)| a + b).collect();
                rem.add_term(mm, -(&qc * c2));
            }
            q.add_term(qm, qc);
            guard += 1;
            if guard % 64 == 0 {
                budget::tick();
            }
            if guard > 2_000_000 {
                return None;
            }
        }
        Some(q)
    }
    /// Coefficients as a polynomial in variable `v` (index = power of `v`).
    pub fn coeffs_in(&self, v: usize) -> Vec<MPoly> {
        let d = self.degree(v) as usize;
        let mut out = vec![MPoly::zero(self.n); d + 1];
        for (m, c) in &self.t {
            let k = m[v] as usize;
            let mut mm = m.clone();
            mm[v] = 0;
            out[k].t.insert(mm, c.clone());
        }
        out
    }
    pub fn from_coeffs_in(n: usize, v: usize, coeffs: &[MPoly]) -> MPoly {
        let mut out = MPoly::zero(n);
        for (k, c) in coeffs.iter().enumerate() {
            for (m, r) in &c.t {
                let mut mm = m.clone();
                mm[v] += k as u16;
                out.add_term(mm, r.clone());
            }
        }
        out
    }
    /// Rational content with the sign of the leading term.
    pub fn content(&self) -> Rat {
        if self.is_zero() {
            return Rat::one();
        }
        let g = self.t.values().fold(BigInt::zero(), |g, c| g.gcd(c.numer()));
        let l = self.t.values().fold(BigInt::one(), |l, c| l.lcm(c.denom()));
        let c = Rat::new(g, l);
        if self.lead().unwrap().1.is_negative() {
            -c
        } else {
            c
        }
    }
    /// Primitive integer polynomial with positive leading coefficient.
    pub fn primitive(&self) -> MPoly {
        if self.is_zero() {
            return self.clone();
        }
        self.scale(&self.content().recip())
    }
    pub fn derivative(&self, v: usize) -> MPoly {
        let mut out = MPoly::zero(self.n);
        for (m, c) in &self.t {
            if m[v] > 0 {
                let mut mm = m.clone();
                mm[v] -= 1;
                out.add_term(mm, c * ri(m[v] as i64));
            }
        }
        out
    }
    pub fn to_qpoly(&self, v: usize) -> Option<QPoly> {
        let mut c = vec![Rat::zero(); self.degree(v) as usize + 1];
        for (m, r) in &self.t {
            if m.iter().enumerate().any(|(k, e)| k != v && *e != 0) {
                return None;
            }
            c[m[v] as usize] = r.clone();
        }
        Some(QPoly::new(c))
    }
    pub fn from_qpoly(n: usize, v: usize, p: &QPoly) -> MPoly {
        let mut out = MPoly::zero(n);
        for (k, c) in p.0.iter().enumerate() {
            if !c.is_zero() {
                let mut m = vec![0; n];
                m[v] = k as u16;
                out.t.insert(m, c.clone());
            }
        }
        out
    }
    pub fn used_vars(&self) -> Vec<usize> {
        (0..self.n).filter(|v| self.degree(*v) > 0).collect()
    }

    fn prem(&self, b: &MPoly, v: usize) -> MPoly {
        // pseudo-remainder in variable v
        let mut a = self.clone();
        let db = b.degree(v);
        let lb = b.coeffs_in(v).pop().unwrap();
        loop {
            let da = a.degree(v);
            if a.is_zero() || da < db {
                return a;
            }
            let la = a.coeffs_in(v).pop().unwrap();
            let mut shift = vec![0u16; self.n];
            shift[v] = da - db;
            let mut xs = MPoly::zero(self.n);
            xs.t.insert(shift, Rat::one());
            a = lb.mul(&a).sub(&la.mul(&xs).mul(b));
            budget::tick();
        }
    }

    pub fn gcd(&self, o: &MPoly) -> MPoly {
        budget::tick();
        if self.is_zero() {
            return o.primitive();
        }
        if o.is_zero() {
            return self.primitive();
        }
        if self.as_constant().is_some() || o.as_constant().is_some() {
            return MPoly::one(self.n);
        }
        // univariate fast path
        let (ua, ub) = (self.used_vars(), o.used_vars());
        if ua.len() == 1 && ua == ub {
            let v = ua[0];
            let g = self.to_qpoly(v).unwrap().gcd(&o.to_qpoly(v).unwrap());
            return MPoly::from_qpoly(self.n, v, &g.primitive());
        }
        // a variable present in only one operand: reduce to its content
        for v in 0..self.n {
            let (da, db) = (self.degree(v), o.degree(v));
            if da > 0 && db == 0 {
                let c = self.coeffs_in(v).into_iter().filter(|c| !c.is_zero()).fold(MPoly::zero(self.n), |g, c| g.gcd(&c));
                return c.gcd(o);
            }
            if db > 0 && da == 0 {
                let c = o.coeffs_in(v).into_iter().filter(|c| !c.is_zero()).fold(MPoly::zero(self.n), |g, c| g.gcd(&c));
                return self.gcd(&c);
            }
        }
        let v = *ua.iter().min_by_key(|v| self.degree(**v).min(o.degree(**v))).unwrap();
        let content = |p: &MPoly| p.coeffs_in(v).into_iter().filter(|c| !c.is_zero()).fold(MPoly::zero(p.n), |g, c| g.gcd(&c));
        let (ca, cb) = (content(self), content(o));
        let gc = ca.gcd(&cb);
        let mut a = self.div_exact(&ca).unwrap_or_else(|| self.clone());
        let mut b = o.div_exact(&cb).unwrap_or_else(|| o.clone());
        if a.degree(v) < b.degree(v) {
            std::mem::swap(&mut a, &mut b);
        }
        loop {
            let r = a.prem(&b, v);
            if r.is_zero() {
                break;
            }
            if r.degree(v) == 0 {
                return gc.primitive();
            }
            let cr = content(&r);
            a = b;
            b = r.div_exact(&cr).unwrap_or(r).primitive();
        }
        gc.mul(&b).primitive()
    }
}

/// Factor a multivariate polynomial over Q.  Returns the rational content
/// and irreducible (as far as could be determined) factors with multiplicity.
pub fn factor_mpoly(p: &MPoly) -> (Rat, Vec<(MPoly, u32)>) {
    let n = p.n;
    if p.is_zero() {
        return (Rat::zero(), vec![]);
    }
    let content = p.content();
    let mut f = p.scale(&content.recip());
    let mut out: Vec<(MPoly, u32)> = Vec::new();
    // monomial content
    for v in 0..n {
        let low = f.t.keys().map(|m| m[v]).min().unwrap_or(0);
        if low > 0 {
            out.push((MPoly::var(n, v), low as u32));
            f = MPoly { n, t: f.t.iter().map(|(m, c)| (m.iter().enumerate().map(|(k, e)| if k == v { e - low } else { *e }).collect(), c.clone())).collect() };
        }
    }
    let used = f.used_vars();
    if used.is_empty() {
        return (content, out);
    }
    if used.len() == 1 {
        let v = used[0];
        let (c2, fs) = factor(&f.to_qpoly(v).unwrap());
        for (g, m) in fs {
            out.push((MPoly::from_qpoly(n, v, &g), m));
        }
        return (content * c2, out);
    }
    // square-free split with respect to the first used variable
    let v0 = used[0];
    let g = f.gcd(&f.derivative(v0));
    let mut pieces: Vec<(MPoly, u32)> = Vec::new();
    if g.as_constant().is_none() {
        // f = (f/g) * g ; recurse on both parts and merge multiplicities
        let h = f.div_exact(&g).unwrap();
        for part in [h, g] {
            let (_, fs) = factor_mpoly(&part);
            for (q, m) in fs {
                match pieces.iter_mut().find(|(x, _)| *x == q) {
                    Some(slot) => slot.1 += m,
                    None => pieces.push((q, m)),
                }
            }
        }
    } else {
        for q in kronecker_factor(&f) {
            match pieces.iter_mut().find(|(x, _)| *x == q) {
                Some(slot) => slot.1 += 1,
                None => pieces.push((q, 1)),
            }
        }
    }
    out.extend(pieces);
    // deterministic order: by degree, then size, then "x - y" before "x + y"
    let sign_key = |p: &MPoly| -> Vec<i8> { p.t.values().rev().map(|c| if c.is_negative() { 0 } else { 1 }).collect() };
    out.sort_by(|a, b| a.0.total_degree().cmp(&b.0.total_degree()).then_with(|| a.0.t.len().cmp(&b.0.t.len())).then_with(|| sign_key(&a.0).cmp(&sign_key(&b.0))));
    (content, out)
}

/// Kronecker substitution: reduce to a univariate problem and recombine.
fn kronecker_factor(f: &MPoly) -> Vec<MPoly> {
    let n = f.n;
    let used = f.used_vars();
    let d = used.iter().map(|v| f.degree(*v) as u64).max().unwrap_or(0) + 1;
    let total: u64 = (0..used.len() as u32).map(|k| (d - 1) * d.pow(k)).sum();
    if total > 80 {
        return vec![f.clone()];
    }
    let weight = |m: &Mono| -> usize { used.iter().enumerate().map(|(k, v)| m[*v] as u64 * d.pow(k as u32)).sum::<u64>() as usize };
    let mut uni = vec![Rat::zero(); total as usize + 1];
    for (m, c) in &f.t {
        uni[weight(m)] += c;
    }
    let up = QPoly::new(uni);
    let (_, ufs) = factor(&up);
    let mut list: Vec<QPoly> = Vec::new();
    for (g, m) in ufs {
        for _ in 0..m {
            list.push(g.clone());
        }
    }
    if list.len() <= 1 || list.len() > 16 {
        return vec![f.clone()];
    }
    let back = |q: &QPoly| -> MPoly {
        let mut out = MPoly::zero(n);
        for (k, c) in q.0.iter().enumerate() {
            if c.is_zero() {
                continue;
            }
            let mut m = vec![0u16; n];
            let mut kk = k as u64;
            for v in &used {
                m[*v] = (kk % d) as u16;
                kk /= d;
            }
            out.add_term(m, c.clone());
        }
        out
    };
    let mut result = Vec::new();
    let mut rest = f.clone();
    let mut alive: Vec<usize> = (0..list.len()).collect();
    let mut size = 1;
    'outer: while size * 2 <= alive.len() {
        let mut idx: Vec<usize> = (0..size).collect();
        loop {
            budget::tick();
            let mut prod = QPoly::one();
            for k in &idx {
                prod = prod.mul(&list[alive[*k]]);
            }
            for cand in [back(&prod), back(&prod.neg())] {
                if cand.as_constant().is_none() && cand.total_degree() < rest.total_degree() {
                    if let Some(q) = rest.div_exact(&cand) {
                        result.push(cand.primitive());
                        rest = q;
                        let chosen: Vec<usize> = idx.iter().map(|k| alive[*k]).collect();
                        alive.retain(|u| !chosen.contains(u));
                        continue 'outer;
                    }
                }
            }
            let mut k = size;
            loop {
                if k == 0 {
                    size += 1;
                    continue 'outer;
                }
                k -= 1;
                if idx[k] != k + alive.len() - size {
                    break;
                }
            }
            idx[k] += 1;
            for j in k + 1..size {
                idx[j] = idx[j - 1] + 1;
            }
        }
    }
    if rest.as_constant().is_none() {
        result.push(rest.primitive());
    }
    result
}

// ===========================================================================
// conversion between expressions and polynomials
// ===========================================================================

/// The list of "generators" – sub-expressions treated as indeterminates.
#[derive(Clone, Debug, Default)]
pub struct Gens(pub Vec<Expr>);

impl Gens {
    fn index(&mut self, e: &Expr) -> usize {
        match self.0.iter().position(|g| g == e) {
            Some(k) => k,
            None => {
                self.0.push(e.clone());
                self.0.len() - 1
            }
        }
    }
}

/// A power product of generators with integer exponents: the image of a
/// non-polynomial factor such as `sqrt(2)`, `e^(2x)` or `sin(x)`.
fn atom_gens(e: &Expr, gens: &mut Gens, out: &mut Vec<(usize, i64)>) {
    match e {
        Expr::Pow(b, ex) => {
            // split sums in the exponent: b^(u + v) = b^u b^v
            if let Expr::Add(ts) = &**ex {
                for t in ts {
                    atom_gens(&expr::pow((**b).clone(), t.clone()), gens, out);
                }
                return;
            }
            let (c, rest) = ex.coeff_split();
            if let Expr::Num(q) = &c {
                // b^(p/q * rest): generator b^(rest/q), power p
                let (p, qd) = (q.numer().clone(), q.denom().clone());
                if let Some(pi) = p.to_i64() {
                    if rest.is_one() && qd.is_one() {
                        // integer power of a non-polynomial base
                        let k = gens.index(b);
                        out.push((k, pi));
                        return;
                    }
                    let g = Expr::Pow(b.clone(), Box::new(expr::mul(vec![Expr::Num(Rat::new(BigInt::one(), qd)), rest])));
                    let k = gens.index(&g);
                    out.push((k, pi));
                    return;
                }
            }
            let k = gens.index(e);
            out.push((k, 1));
        }
        _ => {
            let k = gens.index(e);
            out.push((k, 1));
        }
    }
}

/// Rational function in the generators: numerator / denominator.
#[derive(Clone, Debug)]
pub struct RatFn {
    pub num: MPoly,
    pub den: MPoly,
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

impl RatFn {
    fn align(&mut self, n: usize) {
        self.num = grow(&self.num, n);
        self.den = grow(&self.den, n);
    }
    pub fn reduce(mut self) -> RatFn {
        if self.num.is_zero() {
            self.den = MPoly::one(self.num.n);
            return self;
        }
        if self.den.as_constant().is_none() {
            let g = self.num.gcd(&self.den);
            if g.as_constant().is_none() {
                self.num = self.num.div_exact(&g).expect("gcd divides numerator");
                self.den = self.den.div_exact(&g).expect("gcd divides denominator");
            }
        }
        // make the denominator primitive with positive leading coefficient
        let c = self.den.content();
        if !c.is_one() {
            let inv = c.recip();
            self.num = self.num.scale(&inv);
            self.den = self.den.scale(&inv);
        }
        self
    }
}

/// Convert an expression to a rational function over generators.
/// Generators are discovered on the fly and appended to `gens`.
pub fn to_ratfn(e: &Expr, gens: &mut Gens) -> RatFn {
    budget::tick();
    let n0 = gens.0.len();
    let konst = |r: Rat, n: usize| RatFn { num: MPoly::constant(n, r), den: MPoly::one(n) };
    match e {
        Expr::Num(r) => konst(r.clone(), n0),
        Expr::Add(v) => {
            let mut parts: Vec<RatFn> = v.iter().map(|t| to_ratfn(t, gens)).collect();
            let n = gens.0.len();
            parts.iter_mut().for_each(|p| p.align(n));
            let mut acc = konst(Rat::zero(), n);
            for p in parts {
                if p.den == acc.den {
                    acc.num = acc.num.add(&p.num);
                } else {
                    let g = acc.den.gcd(&p.den);
                    let (da, db) = (acc.den.div_exact(&g).unwrap(), p.den.div_exact(&g).unwrap());
                    acc.num = acc.num.mul(&db).add(&p.num.mul(&da));
                    acc.den = da.mul(&p.den);
                }
            }
            acc.reduce()
        }
        Expr::Mul(v) => {
            let mut parts: Vec<RatFn> = v.iter().map(|t| to_ratfn(t, gens)).collect();
            let n = gens.0.len();
            parts.iter_mut().for_each(|p| p.align(n));
            let mut acc = konst(Rat::one(), n);
            for p in parts {
                acc.num = acc.num.mul(&p.num);
                acc.den = acc.den.mul(&p.den);
            }
            acc.reduce()
        }
        Expr::Pow(b, ex) => {
            if let Some(k) = ex.as_i64() {
                if k.abs() <= 4000 && matches!(**b, Expr::Add(_) | Expr::Mul(_) | Expr::Sym(_)) {
                    let mut base = to_ratfn(b, gens);
                    let n = gens.0.len();
                    base.align(n);
                    let (num, den) = (base.num.pow(k.unsigned_abs() as u32), base.den.pow(k.unsigned_abs() as u32));
                    return if k >= 0 { RatFn { num, den } } else { RatFn { num: den, den: num }.reduce() };
                }
            }
            let mut atoms = Vec::new();
            atom_gens(e, gens, &mut atoms);
            let n = gens.0.len();
            let (mut num, mut den) = (MPoly::one(n), MPoly::one(n));
            for (k, p) in atoms {
                let g = MPoly::var(n, k).pow(p.unsigned_abs() as u32);
                if p >= 0 {
                    num = num.mul(&g);
                } else {
                    den = den.mul(&g);
                }
            }
            RatFn { num, den }
        }
        _ => {
            let k = gens.index(e);
            let n = gens.0.len();
            RatFn { num: MPoly::var(n, k), den: MPoly::one(n) }
        }
    }
}

pub fn mpoly_to_expr(p: &MPoly, gens: &Gens) -> Expr {
    let mut terms = Vec::with_capacity(p.t.len());
    for (m, c) in &p.t {
        let mut fs = vec![Expr::Num(c.clone())];
        for (k, e) in m.iter().enumerate() {
            if *e > 0 {
                fs.push(expr::pow(gens.0[k].clone(), expr::int(*e as i64)));
            }
        }
        terms.push(expr::mul(fs));
    }
    expr::add(terms)
}

/// Polynomial in the given generator list (must already contain every
/// generator that occurs); `None` when the expression is not a polynomial.
pub fn to_mpoly(e: &Expr, gens: &mut Gens) -> Option<MPoly> {
    let r = to_ratfn(e, gens);
    let c = r.den.as_constant()?;
    Some(grow(&r.num, gens.0.len()).scale(&c.recip()))
}

/// Coefficients of `e` as a polynomial in `x` (index = power); the
/// coefficients may contain other symbols.  `None` when `e` is not polynomial
/// in `x`.
pub fn coeffs(e: &Expr, x: &Expr) -> Option<Vec<Expr>> {
    let ex = crate::simplify::expand(e);
    let mut out: Vec<Vec<Expr>> = Vec::new();
    for term in ex.terms() {
        let mut deg = 0usize;
        let mut rest = Vec::new();
        for f in term.factors() {
            if f == *x {
                deg += 1;
            } else if let Expr::Pow(b, k) = &f {
                if **b == *x {
                    let k = k.as_i64()?;
                    if !(0..=10_000).contains(&k) {
                        return None;
                    }
                    deg += k as usize;
                } else if f.has_sym(x) {
                    return None;
                } else {
                    rest.push(f.clone());
                }
            } else if f.has_sym(x) {
                return None;
            } else {
                rest.push(f.clone());
            }
        }
        if out.len() <= deg {
            out.resize(deg + 1, Vec::new());
        }
        out[deg].push(expr::mul(rest));
    }
    Some(out.into_iter().map(expr::add).collect())
}

/// Univariate polynomial with rational coefficients in `x`.
pub fn to_qpoly(e: &Expr, x: &Expr) -> Option<QPoly> {
    let cs = coeffs(e, x)?;
    let rs: Option<Vec<Rat>> = cs.iter().map(|c| c.as_rat().cloned()).collect();
    Some(QPoly::new(rs?))
}

/// Degree of `e` in `x` when polynomial.
pub fn degree(e: &Expr, x: &Expr) -> Option<usize> {
    let c = coeffs(e, x)?;
    let mut d = c.len().saturating_sub(1);
    while d > 0 && c[d].is_zero() {
        d -= 1;
    }
    Some(d)
}
