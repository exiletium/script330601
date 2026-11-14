//! Compute devices.  Dense numeric work (plot grids, surfaces, implicit
//! curves, fractals, Monte Carlo) goes through [`eval_grid`] and friends,
//! which run on the GPU when one is selected and available, and on all CPU
//! cores (rayon) otherwise.

use crate::eval::Program;
use rayon::prelude::*;
use serde::Serialize;
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::OnceLock;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum Device {
    Cpu,
    Gpu,
}

/// Preferred device: 0 = auto, 1 = cpu, 2 = gpu.
static PREF: AtomicU8 = AtomicU8::new(0);

pub fn set_preference(p: &str) {
    let v = match p.to_ascii_lowercase().as_str() {
        "cpu" => 1,
        "gpu" => 2,
        _ => 0,
    };
    PREF.store(v, Ordering::Relaxed);
}

pub fn preference() -> &'static str {
    match PREF.load(Ordering::Relaxed) {
        1 => "cpu",
        2 => "gpu",
        _ => "auto",
    }
}

/// Information about the available hardware.
#[derive(Clone, Debug, Serialize)]
pub struct DeviceInfo {
    pub cpu_threads: usize,
    pub gpu: Option<String>,
    pub gpu_backend: Option<String>,
}

pub fn info() -> DeviceInfo {
    static INFO: OnceLock<DeviceInfo> = OnceLock::new();
    INFO.get_or_init(|| {
        #[cfg(feature = "gpu")]
        let (gpu, backend) = match crate::gpu::adapter_info() {
            Some((n, b)) => (Some(n), Some(b)),
            None => (None, None),
        };
        #[cfg(not(feature = "gpu"))]
        let (gpu, backend) = (None, None);
        DeviceInfo { cpu_threads: rayon::current_num_threads(), gpu, gpu_backend: backend }
    })
    .clone()
}

/// The device that dense work will use right now.
pub fn active() -> Device {
    match PREF.load(Ordering::Relaxed) {
        1 => Device::Cpu,
        _ => {
            if gpu_available() {
                Device::Gpu
            } else {
                Device::Cpu
            }
        }
    }
}

pub fn gpu_available() -> bool {
    #[cfg(feature = "gpu")]
    {
        if std::env::var("NOESIS_NO_GPU").is_ok() {
            return false;
        }
        crate::gpu::available()
    }
    #[cfg(not(feature = "gpu"))]
    false
}

pub fn device_label() -> String {
    let i = info();
    match active() {
        Device::Gpu => format!("GPU · {}", i.gpu.unwrap_or_else(|| "unknown".into())),
        Device::Cpu => format!("CPU · {} threads", i.cpu_threads),
    }
}

/// Evaluate a one-variable program at many points.
pub fn eval_points(p: &Program, xs: &[f64]) -> (Vec<f64>, Device) {
    #[cfg(feature = "gpu")]
    if xs.len() >= 4096 && active() == Device::Gpu {
        if let Some(v) = crate::gpu::eval_1d(p, xs) {
            return (v, Device::Gpu);
        }
    }
    let out = xs
        .par_chunks(512)
        .flat_map_iter(|chunk| {
            let mut st = Vec::with_capacity(32);
            chunk.iter().map(move |&x| p.eval_with(&[x], &mut st)).collect::<Vec<_>>()
        })
        .collect();
    (out, Device::Cpu)
}

/// Evaluate a two-variable program on an `nx × ny` grid (row-major in y).
pub fn eval_grid(p: &Program, x0: f64, x1: f64, nx: usize, y0: f64, y1: f64, ny: usize) -> (Vec<f64>, Device) {
    #[cfg(feature = "gpu")]
    if nx * ny >= 16_384 && active() == Device::Gpu {
        if let Some(v) = crate::gpu::eval_2d(p, x0, x1, nx, y0, y1, ny) {
            return (v, Device::Gpu);
        }
    }
    let dx = if nx > 1 { (x1 - x0) / (nx - 1) as f64 } else { 0.0 };
    let dy = if ny > 1 { (y1 - y0) / (ny - 1) as f64 } else { 0.0 };
    let out: Vec<f64> = (0..ny)
        .into_par_iter()
        .flat_map_iter(|j| {
            let y = y0 + dy * j as f64;
            let mut st = Vec::with_capacity(32);
            (0..nx).map(move |i| p.eval_with(&[x0 + dx * i as f64, y], &mut st)).collect::<Vec<_>>()
        })
        .collect();
    (out, Device::Cpu)
}

/// Escape-time fractal (Mandelbrot when `julia` is None).
pub fn fractal(cx: f64, cy: f64, scale: f64, w: usize, h: usize, max_iter: u32, julia: Option<(f64, f64)>) -> (Vec<f32>, Device) {
    #[cfg(feature = "gpu")]
    if active() == Device::Gpu {
        if let Some(v) = crate::gpu::fractal(cx, cy, scale, w, h, max_iter, julia) {
            return (v, Device::Gpu);
        }
    }
    let out = (0..h)
        .into_par_iter()
        .flat_map_iter(|j| {
            (0..w).map(move |i| {
                let px = cx + (i as f64 - w as f64 / 2.0) * scale;
                let py = cy - (j as f64 - h as f64 / 2.0) * scale;
                let (mut zr, mut zi, cr, ci) = match julia {
                    Some((a, b)) => (px, py, a, b),
                    None => (0.0, 0.0, px, py),
                };
                let mut k = 0;
                while k < max_iter && zr * zr + zi * zi <= 256.0 {
                    let t = zr * zr - zi * zi + cr;
                    zi = 2.0 * zr * zi + ci;
                    zr = t;
                    k += 1;
                }
                if k >= max_iter {
                    -1.0
                } else {
                    // smooth iteration count
                    let m = (zr * zr + zi * zi).sqrt().ln().ln() / std::f64::consts::LN_2;
                    (k as f64 + 1.0 - m) as f32
                }
            })
        })
        .collect();
    (out, Device::Cpu)
}

/// Monte Carlo estimate of ∫ f over a box, with standard error.
pub fn monte_carlo(p: &Program, lo: &[f64], hi: &[f64], samples: usize) -> (f64, f64, Device) {
    let dim = lo.len();
    let vol: f64 = lo.iter().zip(hi).map(|(a, b)| b - a).product();
    // Halton low-discrepancy sequence + random shift (deterministic)
    let primes = [2u64, 3, 5, 7, 11, 13, 17, 19];
    let chunks = 64usize;
    let per = samples / chunks + 1;
    let partial: Vec<(f64, f64, usize)> = (0..chunks)
        .into_par_iter()
        .map(|c| {
            let mut st = Vec::with_capacity(32);
            let mut pt = vec![0.0; dim];
            let (mut s, mut s2, mut n) = (0.0, 0.0, 0usize);
            for k in 0..per {
                let idx = (c * per + k + 1) as u64;
                for d in 0..dim {
                    let mut f = 1.0;
                    let mut r = 0.0;
                    let mut i = idx;
                    let b = primes[d % primes.len()];
                    while i > 0 {
                        f /= b as f64;
                        r += f * (i % b) as f64;
                        i /= b;
                    }
                    pt[d] = lo[d] + (hi[d] - lo[d]) * r;
                }
                let v = p.eval_with(&pt, &mut st);
                if v.is_finite() {
                    s += v;
                    s2 += v * v;
                    n += 1;
                }
            }
            (s, s2, n)
        })
        .collect();
    let (s, s2, n) = partial.iter().fold((0.0, 0.0, 0usize), |a, b| (a.0 + b.0, a.1 + b.1, a.2 + b.2));
    if n == 0 {
        return (f64::NAN, f64::NAN, Device::Cpu);
    }
    let mean = s / n as f64;
    let var = (s2 / n as f64 - mean * mean).max(0.0);
    (mean * vol, vol * (var / n as f64).sqrt(), Device::Cpu)
}

/// A quick throughput benchmark: evaluations per second on each device.
pub fn benchmark() -> Vec<(String, f64)> {
    let e = crate::parse::p("sin(x) * cos(y) + exp(-(x^2 + y^2)/4) * sqrt(abs(x*y) + 1)");
    let prog = Program::compile(&e, &["x", "y"]).unwrap();
    let n = 1024;
    let mut out = Vec::new();
    let saved = PREF.load(Ordering::Relaxed);
    PREF.store(1, Ordering::Relaxed);
    let t = std::time::Instant::now();
    let _ = eval_grid(&prog, -5.0, 5.0, n, -5.0, 5.0, n);
    out.push((format!("CPU ({} threads)", rayon::current_num_threads()), (n * n) as f64 / t.elapsed().as_secs_f64()));
    if gpu_available() {
        PREF.store(2, Ordering::Relaxed);
        let _ = eval_grid(&prog, -5.0, 5.0, 64, -5.0, 5.0, 64); // warm-up / shader compile
        let t = std::time::Instant::now();
        let (_, dev) = eval_grid(&prog, -5.0, 5.0, n, -5.0, 5.0, n);
        if dev == Device::Gpu {
            out.push((format!("GPU ({})", info().gpu.unwrap_or_default()), (n * n) as f64 / t.elapsed().as_secs_f64()));
        }
    }
    PREF.store(saved, Ordering::Relaxed);
    out
}
