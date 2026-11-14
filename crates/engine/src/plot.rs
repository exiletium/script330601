//! Plot data: sampling of curves, parametric and polar plots, implicit
//! curves (marching squares), surfaces and contour plots, plus SVG export.
//! The GUI draws [`Plot`] values directly and re-samples them on zoom.

use crate::compute::{self, Device};
use crate::eval::Program;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::print::text;
use serde::Serialize;

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum PlotKind {
    Cartesian,
    Parametric,
    Polar,
    Implicit,
    Surface,
    Contour,
    NumberLine,
    Scatter,
    Bars,
    Fractal,
}

#[derive(Clone, Debug, Serialize)]
pub struct Series {
    pub label: String,
    /// Connected pieces; breaks at discontinuities.
    pub segments: Vec<Vec<[f64; 2]>>,
    /// Draw as points instead of lines.
    pub points: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct Grid {
    pub nx: usize,
    pub ny: usize,
    pub x: (f64, f64),
    pub y: (f64, f64),
    pub z: Vec<f32>,
    pub z_range: (f64, f64),
}

/// A highlighted interval or point on a number line.
#[derive(Clone, Debug, Serialize)]
pub struct Span {
    pub lo: f64,
    pub hi: f64,
    pub lo_closed: bool,
    pub hi_closed: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct Plot {
    pub kind: PlotKind,
    pub x_range: (f64, f64),
    pub y_range: (f64, f64),
    pub series: Vec<Series>,
    pub grid: Option<Grid>,
    pub spans: Vec<Span>,
    /// Expressions (plain text) for re-sampling, with their variables.
    pub exprs: Vec<String>,
    pub vars: Vec<String>,
    pub x_label: String,
    pub y_label: String,
    pub device: Device,
    /// Equal axis scaling (circles look round).
    pub equal_aspect: bool,
}

impl Plot {
    fn new(kind: PlotKind) -> Plot {
        Plot {
            kind,
            x_range: (-1.0, 1.0),
            y_range: (-1.0, 1.0),
            series: vec![],
            grid: None,
            spans: vec![],
            exprs: vec![],
            vars: vec![],
            x_label: "x".into(),
            y_label: "y".into(),
            device: Device::Cpu,
            equal_aspect: false,
        }
    }
}

fn name_of(x: &Expr) -> String {
    match x {
        Sym(n) => n.to_string(),
        _ => "x".into(),
    }
}

// ---------------------------------------------------------------------------
// 1-D curves
// ---------------------------------------------------------------------------

/// Sample `f` on [a, b], splitting at discontinuities.
pub fn sample_curve(p: &Program, a: f64, b: f64, n: usize) -> (Vec<Vec<[f64; 2]>>, Device) {
    let xs: Vec<f64> = (0..n).map(|k| a + (b - a) * k as f64 / (n - 1) as f64).collect();
    let (ys, dev) = compute::eval_points(p, &xs);
    // robust scale for jump detection
    let mut finite: Vec<f64> = ys.iter().cloned().filter(|v| v.is_finite()).collect();
    finite.sort_by(|p, q| p.partial_cmp(q).unwrap());
    let span = if finite.len() > 10 {
        let lo = finite[finite.len() / 20];
        let hi = finite[finite.len() - 1 - finite.len() / 20];
        (hi - lo).abs().max(1e-12)
    } else {
        1.0
    };
    let mut segs: Vec<Vec<[f64; 2]>> = Vec::new();
    let mut cur: Vec<[f64; 2]> = Vec::new();
    for k in 0..n {
        let (x, y) = (xs[k], ys[k]);
        if !y.is_finite() {
            if cur.len() > 1 {
                segs.push(std::mem::take(&mut cur));
            } else {
                cur.clear();
            }
            continue;
        }
        if let Some(&[px, py]) = cur.last() {
            let jump = (y - py).abs();
            if jump > 0.4 * span {
                // check the midpoint: a true jump/asymptote has a midpoint
                // value outside the endpoints' range
                let xm = 0.5 * (px + x);
                let ym = p.eval1(xm);
                let between = ym.is_finite() && ym >= py.min(y) - 1e-9 * span && ym <= py.max(y) + 1e-9 * span;
                if !between {
                    segs.push(std::mem::take(&mut cur));
                }
            }
        }
        cur.push([x, y]);
    }
    if cur.len() > 1 {
        segs.push(cur);
    } else if cur.len() == 1 {
        segs.push(cur);
    }
    (segs, dev)
}

/// y-range covering the bulk of the data (outliers near asymptotes clipped).
fn y_window(series: &[Series]) -> (f64, f64) {
    let mut ys: Vec<f64> = series.iter().flat_map(|s| s.segments.iter().flatten().map(|p| p[1])).filter(|v| v.is_finite()).collect();
    if ys.is_empty() {
        return (-1.0, 1.0);
    }
    ys.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let n = ys.len();
    let (mut lo, mut hi) = (ys[0], ys[n - 1]);
    let (q1, q9) = (ys[n / 50], ys[n - 1 - n / 50]);
    let core = (q9 - q1).abs().max(1e-9);
    if hi - lo > 8.0 * core {
        lo = q1 - 0.6 * core;
        hi = q9 + 0.6 * core;
    }
    if (hi - lo).abs() < 1e-9 {
        let c = lo;
        let d = if c.abs() > 1e-9 { c.abs() * 0.5 } else { 1.0 };
        return (c - d, c + d);
    }
    let pad = 0.06 * (hi - lo);
    (lo - pad, hi + pad)
}

/// Choose a good x-window for plotting `f` (a function of `x`).
pub fn choose_range(f: &Expr, x: &Expr) -> (f64, f64) {
    let name = name_of(x);
    let Some(p) = Program::compile(f, &[&name]) else { return (-10.0, 10.0) };
    let mut pts: Vec<f64> = Vec::new();
    // roots and extrema found numerically in a broad window
    let fx = |t: f64| p.eval1(t);
    for r in crate::numeric::find_roots(&fx, -50.0, 50.0, 2000) {
        pts.push(r);
    }
    let d = crate::calculus::diff(f, x);
    if let Some(dp) = Program::compile(&d, &[&name]) {
        for r in crate::numeric::find_roots(&|t| dp.eval1(t), -50.0, 50.0, 2000) {
            pts.push(r);
        }
    }
    // domain edges and singularities
    let mut last_ok: Option<bool> = None;
    for k in 0..=2000 {
        let t = -50.0 + 0.05 * k as f64;
        let ok = fx(t).is_finite();
        if let Some(l) = last_ok {
            if l != ok {
                pts.push(t);
            }
        }
        last_ok = Some(ok);
    }
    let periodic = f.has_any_fn(&["sin", "cos", "tan"]);
    pts.retain(|v| v.is_finite());
    pts.sort_by(|a, b| a.partial_cmp(b).unwrap());
    if periodic && pts.len() > 12 {
        // a few periods around the origin
        return (-2.0 * std::f64::consts::PI, 2.0 * std::f64::consts::PI);
    }
    if pts.is_empty() {
        return (-10.0, 10.0);
    }
    // keep interesting points near the origin cluster
    let lo = pts.first().cloned().unwrap().max(-50.0);
    let hi = pts.last().cloned().unwrap().min(50.0);
    let width = (hi - lo).max(2.0);
    let (mut a, mut b) = (lo - 0.4 * width, hi + 0.4 * width);
    if a > -1.0 && lo >= -1e-9 && !fx(-1.0).is_finite() {
        a = lo - 0.1 * width; // domain starts here (e.g. sqrt, log)
    }
    if b - a < 4.0 {
        let c = 0.5 * (a + b);
        a = c - 2.0;
        b = c + 2.0;
    }
    (nice(a, false), nice(b, true))
}

fn nice(v: f64, up: bool) -> f64 {
    let m = 10f64.powf(v.abs().log10().floor() - 1.0).max(0.1);
    if up {
        (v / m).ceil() * m
    } else {
        (v / m).floor() * m
    }
}

/// Plot one or more functions of x.
pub fn cartesian(fs: &[Expr], x: &Expr, range: Option<(f64, f64)>) -> Option<Plot> {
    let name = name_of(x);
    let (a, b) = range.unwrap_or_else(|| {
        let mut lo = f64::INFINITY;
        let mut hi = f64::NEG_INFINITY;
        for f in fs {
            let (p, q) = choose_range(f, x);
            lo = lo.min(p);
            hi = hi.max(q);
        }
        (lo, hi)
    });
    let mut plot = Plot::new(PlotKind::Cartesian);
    plot.x_label = name.clone();
    for f in fs {
        let p = Program::compile(f, &[&name])?.with_real_roots();
        let (segs, dev) = sample_curve(&p, a, b, 1200);
        plot.device = dev;
        plot.series.push(Series { label: text(f), segments: segs, points: false });
        plot.exprs.push(text(f));
    }
    plot.vars = vec![name];
    plot.x_range = (a, b);
    plot.y_range = y_window(&plot.series);
    Some(plot)
}

/// Re-sample a plot for a new x-window (used by the GUI on zoom/pan).
pub fn resample(plot: &Plot, a: f64, b: f64) -> Option<Plot> {
    match plot.kind {
        PlotKind::Cartesian => {
            let x = sym(plot.vars.first()?);
            let fs: Vec<Expr> = plot.exprs.iter().filter_map(|s| crate::parse::parse_expr(s).ok()).collect();
            let mut p = cartesian(&fs, &x, Some((a, b)))?;
            p.y_range = plot.y_range;
            Some(p)
        }
        _ => None,
    }
}

pub fn parametric(fx: &Expr, fy: &Expr, t: &Expr, range: (f64, f64)) -> Option<Plot> {
    let name = name_of(t);
    let px = Program::compile(fx, &[&name])?.with_real_roots();
    let py = Program::compile(fy, &[&name])?.with_real_roots();
    let n = 2000;
    let ts: Vec<f64> = (0..n).map(|k| range.0 + (range.1 - range.0) * k as f64 / (n - 1) as f64).collect();
    let (xs, dev) = compute::eval_points(&px, &ts);
    let (ys, _) = compute::eval_points(&py, &ts);
    let mut segs = Vec::new();
    let mut cur = Vec::new();
    for k in 0..n {
        if xs[k].is_finite() && ys[k].is_finite() {
            cur.push([xs[k], ys[k]]);
        } else if !cur.is_empty() {
            segs.push(std::mem::take(&mut cur));
        }
    }
    if !cur.is_empty() {
        segs.push(cur);
    }
    let mut plot = Plot::new(PlotKind::Parametric);
    plot.device = dev;
    plot.series.push(Series { label: format!("({}, {})", text(fx), text(fy)), segments: segs, points: false });
    fit_both(&mut plot);
    plot.equal_aspect = true;
    plot.exprs = vec![text(fx), text(fy)];
    plot.vars = vec![name];
    Some(plot)
}

pub fn polar(r: &Expr, theta: &Expr, range: (f64, f64)) -> Option<Plot> {
    let fx = mul(vec![r.clone(), f1("cos", theta.clone())]);
    let fy = mul(vec![r.clone(), f1("sin", theta.clone())]);
    let mut p = parametric(&fx, &fy, theta, range)?;
    p.kind = PlotKind::Polar;
    p.series[0].label = format!("r = {}", text(r));
    Some(p)
}

fn fit_both(plot: &mut Plot) {
    let pts: Vec<[f64; 2]> = plot.series.iter().flat_map(|s| s.segments.iter().flatten().cloned()).collect();
    if pts.is_empty() {
        return;
    }
    let (mut x0, mut x1, mut y0, mut y1) = (f64::INFINITY, f64::NEG_INFINITY, f64::INFINITY, f64::NEG_INFINITY);
    for p in &pts {
        x0 = x0.min(p[0]);
        x1 = x1.max(p[0]);
        y0 = y0.min(p[1]);
        y1 = y1.max(p[1]);
    }
    let px = 0.08 * (x1 - x0).max(1e-9);
    let py = 0.08 * (y1 - y0).max(1e-9);
    plot.x_range = (x0 - px, x1 + px);
    plot.y_range = (y0 - py, y1 + py);
}

// ---------------------------------------------------------------------------
// implicit curves and surfaces
// ---------------------------------------------------------------------------

/// Curve F(x, y) = 0 by marching squares.
pub fn implicit(f: &Expr, x: &Expr, y: &Expr, xr: (f64, f64), yr: (f64, f64)) -> Option<Plot> {
    let (xn, yn) = (name_of(x), name_of(y));
    let p = Program::compile(f, &[&xn, &yn])?.with_real_roots();
    let n = 400;
    let (z, dev) = compute::eval_grid(&p, xr.0, xr.1, n, yr.0, yr.1, n);
    let dx = (xr.1 - xr.0) / (n - 1) as f64;
    let dy = (yr.1 - yr.0) / (n - 1) as f64;
    let at = |i: usize, j: usize| z[j * n + i];
    let mut segs: Vec<Vec<[f64; 2]>> = Vec::new();
    // typical magnitude for rejecting sign changes across poles
    let mut mags: Vec<f64> = z.iter().filter(|v| v.is_finite()).map(|v| v.abs()).collect();
    mags.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let typical = if mags.is_empty() { 1.0 } else { mags[mags.len() / 2].max(1e-12) };
    for j in 0..n - 1 {
        for i in 0..n - 1 {
            let c = [at(i, j), at(i + 1, j), at(i + 1, j + 1), at(i, j + 1)];
            if c.iter().any(|v| !v.is_finite()) {
                continue;
            }
            if c.iter().all(|v| v.abs() > 50.0 * typical) {
                continue;
            }
            let corners = [[xr.0 + dx * i as f64, yr.0 + dy * j as f64], [xr.0 + dx * (i + 1) as f64, yr.0 + dy * j as f64], [xr.0 + dx * (i + 1) as f64, yr.0 + dy * (j + 1) as f64], [xr.0 + dx * i as f64, yr.0 + dy * (j + 1) as f64]];
            let mut cross = Vec::new();
            for e in 0..4 {
                let (a, b) = (c[e], c[(e + 1) % 4]);
                if (a < 0.0) != (b < 0.0) {
                    let t = a / (a - b);
                    let (p0, p1) = (corners[e], corners[(e + 1) % 4]);
                    cross.push([p0[0] + t * (p1[0] - p0[0]), p0[1] + t * (p1[1] - p0[1])]);
                }
            }
            if cross.len() == 2 {
                segs.push(vec![cross[0], cross[1]]);
            } else if cross.len() == 4 {
                segs.push(vec![cross[0], cross[1]]);
                segs.push(vec![cross[2], cross[3]]);
            }
        }
    }
    let mut plot = Plot::new(PlotKind::Implicit);
    plot.device = dev;
    plot.series.push(Series { label: format!("{} = 0", text(f)), segments: join_segments(segs), points: false });
    plot.x_range = xr;
    plot.y_range = yr;
    plot.x_label = xn.clone();
    plot.y_label = yn.clone();
    plot.equal_aspect = true;
    plot.exprs = vec![text(f)];
    plot.vars = vec![xn, yn];
    Some(plot)
}

/// Join 2-point segments into polylines where endpoints coincide.
fn join_segments(segs: Vec<Vec<[f64; 2]>>) -> Vec<Vec<[f64; 2]>> {
    use std::collections::HashMap;
    let key = |p: [f64; 2]| ((p[0] * 1e7).round() as i64, (p[1] * 1e7).round() as i64);
    let mut ends: HashMap<(i64, i64), Vec<usize>> = HashMap::new();
    for (k, s) in segs.iter().enumerate() {
        ends.entry(key(s[0])).or_default().push(k);
        ends.entry(key(s[1])).or_default().push(k);
    }
    let mut used = vec![false; segs.len()];
    let mut out = Vec::new();
    for start in 0..segs.len() {
        if used[start] {
            continue;
        }
        used[start] = true;
        let mut line: std::collections::VecDeque<[f64; 2]> = segs[start].iter().cloned().collect();
        for forward in [true, false] {
            loop {
                let tip = if forward { *line.back().unwrap() } else { *line.front().unwrap() };
                let next = ends.get(&key(tip)).and_then(|v| v.iter().find(|&&k| !used[k]).cloned());
                let Some(k) = next else { break };
                used[k] = true;
                let s = &segs[k];
                let other = if key(s[0]) == key(tip) { s[1] } else { s[0] };
                if forward {
                    line.push_back(other);
                } else {
                    line.push_front(other);
                }
            }
        }
        out.push(line.into_iter().collect());
    }
    out
}

/// z = f(x, y) sampled on a grid (for 3-D surfaces and contour/density plots).
pub fn surface(f: &Expr, x: &Expr, y: &Expr, xr: (f64, f64), yr: (f64, f64), n: usize, kind: PlotKind) -> Option<Plot> {
    let (xn, yn) = (name_of(x), name_of(y));
    let p = Program::compile(f, &[&xn, &yn])?.with_real_roots();
    let (z, dev) = compute::eval_grid(&p, xr.0, xr.1, n, yr.0, yr.1, n);
    let mut fin: Vec<f64> = z.iter().cloned().filter(|v| v.is_finite()).collect();
    if fin.is_empty() {
        return None;
    }
    fin.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let m = fin.len();
    let (mut lo, mut hi) = (fin[0], fin[m - 1]);
    let (q1, q9) = (fin[m / 100], fin[m - 1 - m / 100]);
    if hi - lo > 10.0 * (q9 - q1).max(1e-12) {
        lo = q1;
        hi = q9;
    }
    if hi - lo < 1e-12 {
        lo -= 1.0;
        hi += 1.0;
    }
    let mut plot = Plot::new(kind);
    plot.device = dev;
    plot.grid = Some(Grid { nx: n, ny: n, x: xr, y: yr, z: z.iter().map(|v| *v as f32).collect(), z_range: (lo, hi) });
    plot.x_range = xr;
    plot.y_range = yr;
    plot.x_label = xn.clone();
    plot.y_label = yn.clone();
    plot.exprs = vec![text(f)];
    plot.vars = vec![xn, yn];
    plot.series.push(Series { label: text(f), segments: vec![], points: false });
    if plot.kind == PlotKind::Contour {
        // a handful of level curves
        let levels = 9;
        for k in 1..levels {
            let c = lo + (hi - lo) * k as f64 / levels as f64;
            let shifted = sub(f.clone(), float(c));
            if let Some(ip) = implicit(&shifted, x, y, xr, yr) {
                plot.series.push(Series { label: crate::print::fmt_float(c, 3), segments: ip.series[0].segments.clone(), points: false });
            }
        }
        plot.equal_aspect = true;
    }
    Some(plot)
}

/// Escape-time fractal image.
pub fn fractal(julia: Option<(f64, f64)>, w: usize, h: usize) -> Plot {
    let (cx, scale) = if julia.is_some() { (0.0, 3.2 / w as f64) } else { (-0.6, 3.2 / w as f64) };
    let (vals, dev) = compute::fractal(cx, 0.0, scale, w, h, 400, julia);
    let mut plot = Plot::new(PlotKind::Fractal);
    plot.device = dev;
    let hw = scale * w as f64 / 2.0;
    let hh = scale * h as f64 / 2.0;
    plot.x_range = (cx - hw, cx + hw);
    plot.y_range = (-hh, hh);
    let maxv = vals.iter().cloned().fold(0.0f32, f32::max) as f64;
    plot.grid = Some(Grid { nx: w, ny: h, x: plot.x_range, y: plot.y_range, z: vals, z_range: (0.0, maxv.max(1.0)) });
    plot.x_label = "Re".into();
    plot.y_label = "Im".into();
    plot.equal_aspect = true;
    plot
}

/// Number line with highlighted intervals (inequality solutions).
pub fn number_line(spans: Vec<Span>) -> Plot {
    let mut plot = Plot::new(PlotKind::NumberLine);
    let mut pts: Vec<f64> = spans.iter().flat_map(|s| [s.lo, s.hi]).filter(|v| v.is_finite()).collect();
    if pts.is_empty() {
        pts = vec![-1.0, 1.0];
    }
    let lo = pts.iter().cloned().fold(f64::INFINITY, f64::min);
    let hi = pts.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let w = (hi - lo).max(2.0);
    plot.x_range = (lo - 0.5 * w, hi + 0.5 * w);
    plot.y_range = (-1.0, 1.0);
    plot.spans = spans;
    plot
}

pub fn scatter(points: Vec<[f64; 2]>, label: &str, fit: Option<&Expr>) -> Plot {
    let mut plot = Plot::new(PlotKind::Scatter);
    plot.series.push(Series { label: label.into(), segments: vec![points], points: true });
    fit_both(&mut plot);
    if let Some(f) = fit {
        if let Some(p) = Program::compile(f, &["x"]) {
            let (segs, _) = sample_curve(&p, plot.x_range.0, plot.x_range.1, 400);
            plot.series.push(Series { label: text(f), segments: segs, points: false });
        }
    }
    plot
}

pub fn bars(values: Vec<(String, f64)>) -> Plot {
    let mut plot = Plot::new(PlotKind::Bars);
    let pts: Vec<[f64; 2]> = values.iter().enumerate().map(|(k, (_, v))| [k as f64, *v]).collect();
    let hi = values.iter().map(|v| v.1).fold(0.0, f64::max);
    let lo = values.iter().map(|v| v.1).fold(0.0, f64::min);
    plot.series.push(Series { label: values.iter().map(|v| v.0.clone()).collect::<Vec<_>>().join("\u{1f}"), segments: vec![pts], points: true });
    plot.x_range = (-0.75, values.len() as f64 - 0.25);
    plot.y_range = (lo * 1.1, hi * 1.1 + 1e-12);
    plot
}

// ---------------------------------------------------------------------------
// SVG export
// ---------------------------------------------------------------------------

pub const SERIES_LIGHT: [&str; 8] = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948"];
pub const SERIES_DARK: [&str; 8] = ["#3987e5", "#d95926", "#199e70", "#c98500", "#d55181", "#008300", "#9085e9", "#e66767"];

/// Tick positions ("nice" numbers).
pub fn ticks(lo: f64, hi: f64, target: usize) -> Vec<f64> {
    let span = (hi - lo).abs();
    if !span.is_finite() || span <= 0.0 {
        return vec![];
    }
    let raw = span / target.max(1) as f64;
    let mag = 10f64.powf(raw.log10().floor());
    let step = [1.0, 2.0, 5.0, 10.0].iter().map(|m| m * mag).find(|s| span / s <= target as f64 * 1.5).unwrap_or(10.0 * mag);
    let mut v = (lo / step).ceil() * step;
    let mut out = Vec::new();
    while v <= hi + 1e-9 * step && out.len() < 100 {
        out.push(if v.abs() < 1e-12 * step { 0.0 } else { v });
        v += step;
    }
    out
}

/// Standalone SVG rendering of a 2-D plot.
pub fn to_svg(plot: &Plot, dark: bool, width: f64, height: f64) -> String {
    let (bg, fg, muted, grid) = if dark { ("#1a1a19", "#f0efec", "#c3c2b7", "#2e2e2c") } else { ("#fcfcfb", "#0b0b0b", "#52514e", "#e4e3df") };
    let colors = if dark { SERIES_DARK } else { SERIES_LIGHT };
    let (ml, mr, mt, mb) = (52.0, 16.0, 14.0, 34.0);
    let (pw, ph) = (width - ml - mr, height - mt - mb);
    let (x0, x1) = plot.x_range;
    let (y0, y1) = plot.y_range;
    let sx = |x: f64| ml + (x - x0) / (x1 - x0) * pw;
    let sy = |y: f64| mt + (1.0 - (y - y0) / (y1 - y0)) * ph;
    let mut s = format!("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{width}\" height=\"{height}\" viewBox=\"0 0 {width} {height}\" font-family=\"Inter, Segoe UI, sans-serif\" font-size=\"11\">");
    s += &format!("<rect width=\"100%\" height=\"100%\" fill=\"{bg}\"/>");
    for t in ticks(x0, x1, 8) {
        s += &format!("<line x1=\"{0:.1}\" y1=\"{1:.1}\" x2=\"{0:.1}\" y2=\"{2:.1}\" stroke=\"{grid}\" stroke-width=\"1\"/>", sx(t), mt, mt + ph);
        s += &format!("<text x=\"{:.1}\" y=\"{:.1}\" fill=\"{muted}\" text-anchor=\"middle\">{}</text>", sx(t), mt + ph + 16.0, crate::print::fmt_float(t, 4));
    }
    if plot.kind != PlotKind::NumberLine {
        for t in ticks(y0, y1, 6) {
            s += &format!("<line x1=\"{0:.1}\" y1=\"{1:.1}\" x2=\"{2:.1}\" y2=\"{1:.1}\" stroke=\"{grid}\" stroke-width=\"1\"/>", ml, sy(t), ml + pw);
            s += &format!("<text x=\"{:.1}\" y=\"{:.1}\" fill=\"{muted}\" text-anchor=\"end\">{}</text>", ml - 6.0, sy(t) + 4.0, crate::print::fmt_float(t, 4));
        }
    }
    // axes through the origin when visible
    if x0 < 0.0 && x1 > 0.0 && plot.kind != PlotKind::NumberLine {
        s += &format!("<line x1=\"{0:.1}\" y1=\"{1:.1}\" x2=\"{0:.1}\" y2=\"{2:.1}\" stroke=\"{muted}\" stroke-width=\"1\"/>", sx(0.0), mt, mt + ph);
    }
    if y0 < 0.0 && y1 > 0.0 {
        s += &format!("<line x1=\"{0:.1}\" y1=\"{1:.1}\" x2=\"{2:.1}\" y2=\"{1:.1}\" stroke=\"{muted}\" stroke-width=\"1\"/>", ml, sy(0.0), ml + pw);
    }
    s += &format!("<clipPath id=\"c\"><rect x=\"{ml}\" y=\"{mt}\" width=\"{pw}\" height=\"{ph}\"/></clipPath><g clip-path=\"url(#c)\">");
    for (k, ser) in plot.series.iter().enumerate() {
        let col = colors[k % colors.len()];
        for seg in &ser.segments {
            if ser.points {
                for p in seg {
                    s += &format!("<circle cx=\"{:.2}\" cy=\"{:.2}\" r=\"3\" fill=\"{col}\"/>", sx(p[0]), sy(p[1]));
                }
            } else if seg.len() >= 2 {
                let d: Vec<String> = seg.iter().map(|p| format!("{:.2},{:.2}", sx(p[0]), sy(p[1]).clamp(-1e4, 1e4))).collect();
                s += &format!("<polyline fill=\"none\" stroke=\"{col}\" stroke-width=\"2\" stroke-linejoin=\"round\" points=\"{}\"/>", d.join(" "));
            }
        }
    }
    for sp in &plot.spans {
        let col = colors[0];
        let (a, b) = (sx(sp.lo.max(x0)), sx(sp.hi.min(x1)));
        let y = sy(0.0);
        if (sp.hi - sp.lo).abs() > 0.0 {
            s += &format!("<line x1=\"{a:.1}\" y1=\"{y:.1}\" x2=\"{b:.1}\" y2=\"{y:.1}\" stroke=\"{col}\" stroke-width=\"4\"/>");
        }
        for (v, closed) in [(sp.lo, sp.lo_closed), (sp.hi, sp.hi_closed)] {
            if v.is_finite() {
                let fill = if closed { col } else { bg };
                s += &format!("<circle cx=\"{:.1}\" cy=\"{y:.1}\" r=\"5\" fill=\"{fill}\" stroke=\"{col}\" stroke-width=\"2\"/>", sx(v));
            }
        }
    }
    s += "</g>";
    // legend for two or more series
    let labelled: Vec<&Series> = plot.series.iter().filter(|s| !s.label.is_empty()).collect();
    if labelled.len() >= 2 && plot.kind != PlotKind::Contour {
        for (k, ser) in labelled.iter().enumerate() {
            let y = mt + 14.0 + 16.0 * k as f64;
            s += &format!("<rect x=\"{:.1}\" y=\"{:.1}\" width=\"14\" height=\"3\" fill=\"{}\"/>", ml + 10.0, y - 4.0, colors[k % colors.len()]);
            s += &format!("<text x=\"{:.1}\" y=\"{y:.1}\" fill=\"{fg}\">{}</text>", ml + 30.0, xml_escape(&ser.label));
        }
    }
    s += "</svg>";
    s
}

fn xml_escape(s: &str) -> String {
    s.replace('&', "&amp;").replace('<', "&lt;").replace('>', "&gt;")
}
