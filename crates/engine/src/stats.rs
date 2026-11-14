//! Statistics: descriptive statistics, regression and probability
//! distributions.

use crate::engine::*;
use crate::expr::*;
use crate::parse::Parsed;
use crate::plot as pl;
use crate::print::fmt_float;
use crate::simplify as s;

pub fn fmt(v: f64) -> String {
    fmt_float(crate::eval::chop(v), 6)
}

#[derive(Clone, Debug)]
pub struct Summary {
    pub n: usize,
    pub sum: f64,
    pub mean: f64,
    pub median: f64,
    pub min: f64,
    pub max: f64,
    pub var_sample: f64,
    pub var_pop: f64,
    pub q1: f64,
    pub q3: f64,
    pub modes: Vec<f64>,
}

fn quantile(sorted: &[f64], q: f64) -> f64 {
    let n = sorted.len();
    if n == 1 {
        return sorted[0];
    }
    let h = (n - 1) as f64 * q;
    let lo = h.floor() as usize;
    let hi = h.ceil() as usize;
    sorted[lo] + (h - lo as f64) * (sorted[hi] - sorted[lo])
}

pub fn summarize(xs: &[f64]) -> Option<Summary> {
    if xs.is_empty() || xs.iter().any(|v| !v.is_finite()) {
        return None;
    }
    let n = xs.len();
    let mut v = xs.to_vec();
    v.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let sum: f64 = v.iter().sum();
    let mean = sum / n as f64;
    let ss: f64 = v.iter().map(|x| (x - mean) * (x - mean)).sum();
    // modes
    let mut counts: Vec<(f64, usize)> = Vec::new();
    for x in &v {
        match counts.last_mut() {
            Some((y, c)) if y == x => *c += 1,
            _ => counts.push((*x, 1)),
        }
    }
    let maxc = counts.iter().map(|c| c.1).max().unwrap_or(1);
    let modes = if maxc > 1 { counts.iter().filter(|c| c.1 == maxc).map(|c| c.0).collect() } else { vec![] };
    Some(Summary {
        n,
        sum,
        mean,
        median: quantile(&v, 0.5),
        min: v[0],
        max: v[n - 1],
        var_sample: if n > 1 { ss / (n - 1) as f64 } else { 0.0 },
        var_pop: ss / n as f64,
        q1: quantile(&v, 0.25),
        q3: quantile(&v, 0.75),
        modes,
    })
}

/// Pods for a list of numbers.
pub fn list_pods(p: &Parsed, v: &[Expr]) -> Option<Vec<Pod>> {
    let xs: Vec<f64> = v.iter().map(|e| e.to_f64_const()).collect::<Option<_>>()?;
    let st = summarize(&xs)?;
    let mut pods = vec![crate::engine::math::input_pod(&p.d)];
    // exact mean / median for rational data
    let exact = v.iter().all(|e| e.as_rat().is_some());
    let mean_c = if exact { math(&s::simplify(&div(add(v.to_vec()), int(st.n as i64)))) } else { text(fmt(st.mean)) };
    let mut rows = vec![vec![text("mean"), mean_c], vec![text("median"), text(fmt(st.median))]];
    if !st.modes.is_empty() {
        rows.push(vec![text(if st.modes.len() == 1 { "mode" } else { "modes" }), text(st.modes.iter().map(|m| fmt(*m)).collect::<Vec<_>>().join(", "))]);
    }
    rows.push(vec![text("standard deviation"), text(fmt(st.var_sample.sqrt()))]);
    rows.push(vec![text("variance"), text(fmt(st.var_sample))]);
    pods.push(primary(pod("Statistics", "Basic statistics", table(None, rows))));
    pods.push(pod(
        "Summary",
        "Summary",
        table(
            None,
            vec![
                vec![text("count"), text(st.n.to_string())],
                vec![text("sum"), if exact { math(&s::simplify(&add(v.to_vec()))) } else { text(fmt(st.sum)) }],
                vec![text("minimum"), text(fmt(st.min))],
                vec![text("lower quartile"), text(fmt(st.q1))],
                vec![text("upper quartile"), text(fmt(st.q3))],
                vec![text("maximum"), text(fmt(st.max))],
                vec![text("range"), text(fmt(st.max - st.min))],
                vec![text("population standard deviation"), text(fmt(st.var_pop.sqrt()))],
            ],
        ),
    ));
    let mut sorted = xs.clone();
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
    if st.n <= 40 {
        pods.push(pod("Sorted", "Sorted data", text(sorted.iter().map(|x| fmt(*x)).collect::<Vec<_>>().join(", "))));
    }
    // histogram
    let bins = ((st.n as f64).sqrt().ceil() as usize).clamp(3, 20);
    let w = ((st.max - st.min) / bins as f64).max(1e-12);
    let mut counts = vec![0usize; bins];
    for x in &xs {
        let k = (((x - st.min) / w) as usize).min(bins - 1);
        counts[k] += 1;
    }
    let bars: Vec<(String, f64)> = counts.iter().enumerate().map(|(k, c)| (format!("{}–{}", fmt(st.min + w * k as f64), fmt(st.min + w * (k + 1) as f64)), *c as f64)).collect();
    pods.push(pod("Histogram", "Histogram", plot(pl::bars(bars))));
    Some(pods)
}

// ---------------------------------------------------------------------------
// regression
// ---------------------------------------------------------------------------

/// Least-squares polynomial fit of the given degree; returns coefficients
/// (lowest first) and R².
pub fn polyfit(pts: &[[f64; 2]], deg: usize) -> Option<(Vec<f64>, f64)> {
    let n = deg + 1;
    if pts.len() < n {
        return None;
    }
    let mut a = vec![vec![0.0; n]; n];
    let mut b = vec![0.0; n];
    for p in pts {
        let mut pw = vec![1.0; 2 * n];
        for k in 1..2 * n {
            pw[k] = pw[k - 1] * p[0];
        }
        for i in 0..n {
            for j in 0..n {
                a[i][j] += pw[i + j];
            }
            b[i] += pw[i] * p[1];
        }
    }
    let c = crate::numeric::solve_linear(a, b)?;
    let mean = pts.iter().map(|p| p[1]).sum::<f64>() / pts.len() as f64;
    let ss_tot: f64 = pts.iter().map(|p| (p[1] - mean).powi(2)).sum();
    let ss_res: f64 = pts.iter().map(|p| (p[1] - c.iter().rev().fold(0.0, |acc, k| acc * p[0] + k)).powi(2)).sum();
    let r2 = if ss_tot > 0.0 { 1.0 - ss_res / ss_tot } else { 1.0 };
    Some((c, r2))
}

pub fn fit_pods(pts: &[[f64; 2]], kind: &str) -> Option<Vec<Pod>> {
    let x = sym("x");
    let tidy = |v: f64| crate::calculus::limit::tidy_float(v);
    let (model, r2) = match kind {
        "quadratic" => {
            let (c, r2) = polyfit(pts, 2)?;
            (add(vec![mul(vec![tidy(c[2]), square(x.clone())]), mul(vec![tidy(c[1]), x.clone()]), tidy(c[0])]), r2)
        }
        "exponential" => {
            if pts.iter().any(|p| p[1] <= 0.0) {
                return None;
            }
            let lp: Vec<[f64; 2]> = pts.iter().map(|p| [p[0], p[1].ln()]).collect();
            let (c, r2) = polyfit(&lp, 1)?;
            (mul(vec![tidy(c[0].exp()), exp(mul(vec![tidy(c[1]), x.clone()]))]), r2)
        }
        _ => {
            let (c, r2) = polyfit(pts, 1)?;
            (add(vec![mul(vec![tidy(c[1]), x.clone()]), tidy(c[0])]), r2)
        }
    };
    let mut pods = vec![primary(pod("Fit", &format!("Least-squares {kind} fit"), math(&model)))];
    pods.push(pod("R2", "Coefficient of determination", text(format!("R² = {}", fmt(r2)))));
    pods.push(pod("FitPlot", "Plot of data and fit", plot(pl::scatter(pts.to_vec(), "data", Some(&model)))));
    Some(pods)
}

// ---------------------------------------------------------------------------
// distributions
// ---------------------------------------------------------------------------

pub enum Dist {
    Normal(f64, f64),
    Binomial(u64, f64),
    Poisson(f64),
    Exponential(f64),
    Uniform(f64, f64),
    StudentT(f64),
    ChiSquared(f64),
}

impl Dist {
    pub fn name(&self) -> String {
        match self {
            Dist::Normal(m, s) => format!("normal distribution (μ = {}, σ = {})", fmt(*m), fmt(*s)),
            Dist::Binomial(n, p) => format!("binomial distribution (n = {n}, p = {})", fmt(*p)),
            Dist::Poisson(l) => format!("Poisson distribution (λ = {})", fmt(*l)),
            Dist::Exponential(l) => format!("exponential distribution (λ = {})", fmt(*l)),
            Dist::Uniform(a, b) => format!("uniform distribution on [{}, {}]", fmt(*a), fmt(*b)),
            Dist::StudentT(v) => format!("Student's t distribution (ν = {})", fmt(*v)),
            Dist::ChiSquared(v) => format!("χ² distribution (ν = {})", fmt(*v)),
        }
    }
    fn discrete(&self) -> bool {
        matches!(self, Dist::Binomial(..) | Dist::Poisson(_))
    }
    pub fn mean_var(&self) -> (f64, f64) {
        match *self {
            Dist::Normal(m, s) => (m, s * s),
            Dist::Binomial(n, p) => (n as f64 * p, n as f64 * p * (1.0 - p)),
            Dist::Poisson(l) => (l, l),
            Dist::Exponential(l) => (1.0 / l, 1.0 / (l * l)),
            Dist::Uniform(a, b) => ((a + b) / 2.0, (b - a).powi(2) / 12.0),
            Dist::StudentT(v) => (0.0, if v > 2.0 { v / (v - 2.0) } else { f64::INFINITY }),
            Dist::ChiSquared(v) => (v, 2.0 * v),
        }
    }
    pub fn cdf(&self, x: f64) -> f64 {
        use statrs::distribution::*;
        match *self {
            Dist::Normal(m, s) => Normal::new(m, s).map(|d| d.cdf(x)).unwrap_or(f64::NAN),
            Dist::Binomial(n, p) => {
                if x < 0.0 {
                    0.0
                } else {
                    Binomial::new(p, n).map(|d| d.cdf(x.floor() as u64)).unwrap_or(f64::NAN)
                }
            }
            Dist::Poisson(l) => {
                if x < 0.0 {
                    0.0
                } else {
                    Poisson::new(l).map(|d| d.cdf(x.floor() as u64)).unwrap_or(f64::NAN)
                }
            }
            Dist::Exponential(l) => Exp::new(l).map(|d| d.cdf(x)).unwrap_or(f64::NAN),
            Dist::Uniform(a, b) => Uniform::new(a, b).map(|d| d.cdf(x)).unwrap_or(f64::NAN),
            Dist::StudentT(v) => StudentsT::new(0.0, 1.0, v).map(|d| d.cdf(x)).unwrap_or(f64::NAN),
            Dist::ChiSquared(v) => ChiSquared::new(v).map(|d| d.cdf(x)).unwrap_or(f64::NAN),
        }
    }
    pub fn pdf(&self, x: f64) -> f64 {
        use statrs::distribution::*;
        match *self {
            Dist::Normal(m, s) => Normal::new(m, s).map(|d| d.pdf(x)).unwrap_or(f64::NAN),
            Dist::Binomial(n, p) => {
                if x < 0.0 || x.fract() != 0.0 {
                    0.0
                } else {
                    Binomial::new(p, n).map(|d| d.pmf(x as u64)).unwrap_or(f64::NAN)
                }
            }
            Dist::Poisson(l) => {
                if x < 0.0 || x.fract() != 0.0 {
                    0.0
                } else {
                    Poisson::new(l).map(|d| d.pmf(x as u64)).unwrap_or(f64::NAN)
                }
            }
            Dist::Exponential(l) => Exp::new(l).map(|d| d.pdf(x)).unwrap_or(f64::NAN),
            Dist::Uniform(a, b) => Uniform::new(a, b).map(|d| d.pdf(x)).unwrap_or(f64::NAN),
            Dist::StudentT(v) => StudentsT::new(0.0, 1.0, v).map(|d| d.pdf(x)).unwrap_or(f64::NAN),
            Dist::ChiSquared(v) => ChiSquared::new(v).map(|d| d.pdf(x)).unwrap_or(f64::NAN),
        }
    }
}

/// Pods describing a distribution, optionally with a probability query
/// P(lo < X < hi).
pub fn dist_pods(d: &Dist, query: Option<(f64, f64)>) -> Vec<Pod> {
    let (m, v) = d.mean_var();
    let mut pods = Vec::new();
    if let Some((lo, hi)) = query {
        let pr = if d.discrete() {
            // inclusive bounds for discrete variables
            d.cdf(hi) - if lo.is_finite() { d.cdf(lo - 1.0) } else { 0.0 }
        } else {
            d.cdf(hi) - d.cdf(lo)
        };
        let label = match (lo.is_finite(), hi.is_finite()) {
            (false, true) => format!("P(X ≤ {})", fmt(hi)),
            (true, false) => format!("P(X ≥ {})", fmt(lo)),
            (true, true) if lo == hi => format!("P(X = {})", fmt(lo)),
            _ => format!("P({} ≤ X ≤ {})", fmt(lo), fmt(hi)),
        };
        pods.push(primary(pod("Probability", "Probability", text(format!("{label} ≈ {}", fmt_float(pr, 8))))));
    }
    pods.push(pod(
        "Properties",
        "Distribution properties",
        table(None, vec![vec![text("mean"), text(fmt(m))], vec![text("standard deviation"), text(fmt(v.sqrt()))], vec![text("variance"), text(fmt(v))]]),
    ));
    // pdf plot
    let sd = v.sqrt().max(1e-9);
    let (a, b) = match d {
        Dist::Uniform(a, b) => (a - 0.2 * (b - a), b + 0.2 * (b - a)),
        Dist::Exponential(l) => (0.0, 5.0 / l),
        Dist::ChiSquared(k) => (0.0, k + 5.0 * (2.0 * k).sqrt()),
        _ => (m - 4.0 * sd, m + 4.0 * sd),
    };
    if d.discrete() {
        let lo = a.floor().max(0.0) as i64;
        let hi = b.ceil() as i64;
        let vals: Vec<(String, f64)> = (lo..=hi.min(lo + 60)).map(|k| (k.to_string(), d.pdf(k as f64))).collect();
        pods.push(pod("PMF", "Probability mass function", plot(pl::bars(vals))));
    } else {
        let n = 400;
        let pts: Vec<[f64; 2]> = (0..n).map(|k| a + (b - a) * k as f64 / (n - 1) as f64).map(|x| [x, d.pdf(x)]).collect();
        let mut p = pl::scatter(vec![], "", None);
        p.kind = pl::PlotKind::Cartesian;
        p.series = vec![pl::Series { label: "PDF".into(), segments: vec![pts], points: false }];
        p.x_range = (a, b);
        let ymax = (0..n).map(|k| d.pdf(a + (b - a) * k as f64 / (n - 1) as f64)).fold(0.0, f64::max);
        p.y_range = (0.0, ymax * 1.1 + 1e-12);
        if let Some((lo, hi)) = query {
            p.spans.push(pl::Span { lo: lo.max(a), hi: hi.min(b), lo_closed: true, hi_closed: true });
        }
        pods.push(pod("PDF", "Probability density function", plot(p)));
    }
    pods
}

#[allow(dead_code)]
fn unused(_: &Expr) {}
