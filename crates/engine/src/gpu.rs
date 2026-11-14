//! GPU compute through wgpu (Vulkan, Metal, DX12 or OpenGL, chosen at run
//! time).  Expressions are compiled to WGSL compute shaders; everything runs
//! in `f32`.  Every entry point returns `None` when no adapter is present or
//! the expression cannot be translated, so callers fall back to the CPU.

use crate::eval::{Program, WGSL_PRELUDE};
use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};
use wgpu::util::DeviceExt;

struct Gpu {
    device: wgpu::Device,
    queue: wgpu::Queue,
    name: String,
    backend: String,
    pipelines: Mutex<HashMap<String, std::sync::Arc<wgpu::ComputePipeline>>>,
}

fn gpu() -> Option<&'static Gpu> {
    static G: OnceLock<Option<Gpu>> = OnceLock::new();
    G.get_or_init(|| {
        // initialising drivers can panic on broken setups; never let that escape
        std::panic::catch_unwind(init).ok().flatten()
    })
    .as_ref()
}

fn init() -> Option<Gpu> {
    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor::default());
    let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
        power_preference: wgpu::PowerPreference::HighPerformance,
        force_fallback_adapter: false,
        compatible_surface: None,
    }))?;
    let info = adapter.get_info();
    // software rasterisers are slower than our own CPU path
    if info.device_type == wgpu::DeviceType::Cpu && std::env::var("NOESIS_ALLOW_SOFTWARE_GPU").is_err() {
        return None;
    }
    let (device, queue) = pollster::block_on(adapter.request_device(
        &wgpu::DeviceDescriptor {
            label: Some("noesis"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::downlevel_defaults().using_resolution(adapter.limits()),
            memory_hints: wgpu::MemoryHints::Performance,
        },
        None,
    ))
    .ok()?;
    Some(Gpu { device, queue, name: info.name.clone(), backend: format!("{:?}", info.backend), pipelines: Mutex::new(HashMap::new()) })
}

pub fn available() -> bool {
    gpu().is_some()
}

pub fn adapter_info() -> Option<(String, String)> {
    gpu().map(|g| (g.name.clone(), g.backend.clone()))
}

fn pipeline(g: &Gpu, src: &str) -> Option<std::sync::Arc<wgpu::ComputePipeline>> {
    if let Some(p) = g.pipelines.lock().ok()?.get(src) {
        return Some(p.clone());
    }
    g.device.push_error_scope(wgpu::ErrorFilter::Validation);
    let module = g.device.create_shader_module(wgpu::ShaderModuleDescriptor { label: Some("noesis-kernel"), source: wgpu::ShaderSource::Wgsl(src.into()) });
    let p = g.device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
        label: Some("noesis-kernel"),
        layout: None,
        module: &module,
        entry_point: Some("main"),
        compilation_options: Default::default(),
        cache: None,
    });
    if pollster::block_on(g.device.pop_error_scope()).is_some() {
        return None;
    }
    let p = std::sync::Arc::new(p);
    let mut cache = g.pipelines.lock().ok()?;
    if cache.len() > 64 {
        cache.clear();
    }
    cache.insert(src.to_string(), p.clone());
    Some(p)
}

/// Run a kernel with one uniform block of params and one f32 output buffer.
fn run(g: &Gpu, src: &str, params: &[f32], n: usize, groups: (u32, u32)) -> Option<Vec<f32>> {
    let pipe = pipeline(g, src)?;
    let size = (n * 4) as u64;
    if size > g.device.limits().max_storage_buffer_binding_size as u64 {
        return None;
    }
    let mut p = params.to_vec();
    while p.len() % 4 != 0 {
        p.push(0.0);
    }
    let ubuf = g.device.create_buffer_init(&wgpu::util::BufferInitDescriptor { label: None, contents: bytemuck::cast_slice(&p), usage: wgpu::BufferUsages::UNIFORM });
    let out = g.device.create_buffer(&wgpu::BufferDescriptor { label: None, size, usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC, mapped_at_creation: false });
    let read = g.device.create_buffer(&wgpu::BufferDescriptor { label: None, size, usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST, mapped_at_creation: false });
    let layout = pipe.get_bind_group_layout(0);
    let bind = g.device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &layout,
        entries: &[wgpu::BindGroupEntry { binding: 0, resource: ubuf.as_entire_binding() }, wgpu::BindGroupEntry { binding: 1, resource: out.as_entire_binding() }],
    });
    let mut enc = g.device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
    {
        let mut pass = enc.begin_compute_pass(&wgpu::ComputePassDescriptor { label: None, timestamp_writes: None });
        pass.set_pipeline(&pipe);
        pass.set_bind_group(0, &bind, &[]);
        pass.dispatch_workgroups(groups.0, groups.1, 1);
    }
    enc.copy_buffer_to_buffer(&out, 0, &read, 0, size);
    g.queue.submit(Some(enc.finish()));
    let slice = read.slice(..);
    let (tx, rx) = std::sync::mpsc::channel();
    slice.map_async(wgpu::MapMode::Read, move |r| {
        let _ = tx.send(r);
    });
    g.device.poll(wgpu::Maintain::Wait);
    rx.recv().ok()?.ok()?;
    let data = slice.get_mapped_range();
    let v: Vec<f32> = bytemuck::cast_slice(&data).to_vec();
    drop(data);
    read.unmap();
    Some(v)
}

fn guarded<T>(f: impl FnOnce() -> Option<T>) -> Option<T> {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)).ok().flatten()
}

/// f(x) at the given points.
pub fn eval_1d(p: &Program, xs: &[f64]) -> Option<Vec<f64>> {
    let g = gpu()?;
    let body = p.to_wgsl()?;
    // xs are uniformly spaced in practice; pass start/step and verify
    let n = xs.len();
    if n < 2 {
        return None;
    }
    let step = (xs[n - 1] - xs[0]) / (n - 1) as f64;
    if xs.iter().enumerate().any(|(k, x)| (x - (xs[0] + step * k as f64)).abs() > 1e-9 * (1.0 + x.abs())) {
        return None;
    }
    let src = format!(
        "struct P {{ x0: f32, dx: f32, n: f32, pad: f32 }}\n@group(0) @binding(0) var<uniform> prm: P;\n@group(0) @binding(1) var<storage, read_write> out: array<f32>;\n{WGSL_PRELUDE}\nfn f(v0: f32) -> f32 {{ return {body}; }}\n@compute @workgroup_size(256)\nfn main(@builtin(global_invocation_id) id: vec3<u32>) {{\n  let i = id.x;\n  if (f32(i) >= prm.n) {{ return; }}\n  out[i] = f(prm.x0 + prm.dx * f32(i));\n}}\n"
    );
    guarded(|| {
        let v = run(g, &src, &[xs[0] as f32, step as f32, n as f32, 0.0], n, ((n as u32).div_ceil(256), 1))?;
        Some(v.into_iter().map(|x| x as f64).collect())
    })
}

/// f(x, y) on a regular grid, row-major in y.
pub fn eval_2d(p: &Program, x0: f64, x1: f64, nx: usize, y0: f64, y1: f64, ny: usize) -> Option<Vec<f64>> {
    let g = gpu()?;
    if p.nvars != 2 {
        return None;
    }
    let body = p.to_wgsl()?;
    let dx = if nx > 1 { (x1 - x0) / (nx - 1) as f64 } else { 0.0 };
    let dy = if ny > 1 { (y1 - y0) / (ny - 1) as f64 } else { 0.0 };
    let src = format!(
        "struct P {{ x0: f32, dx: f32, y0: f32, dy: f32, nx: u32, ny: u32, pad0: u32, pad1: u32 }}\n@group(0) @binding(0) var<uniform> prm: P;\n@group(0) @binding(1) var<storage, read_write> out: array<f32>;\n{WGSL_PRELUDE}\nfn f(v0: f32, v1: f32) -> f32 {{ return {body}; }}\n@compute @workgroup_size(16, 16)\nfn main(@builtin(global_invocation_id) id: vec3<u32>) {{\n  if (id.x >= prm.nx || id.y >= prm.ny) {{ return; }}\n  out[id.y * prm.nx + id.x] = f(prm.x0 + prm.dx * f32(id.x), prm.y0 + prm.dy * f32(id.y));\n}}\n"
    );
    let params: [f32; 8] = [x0 as f32, dx as f32, y0 as f32, dy as f32, f32::from_bits(nx as u32), f32::from_bits(ny as u32), 0.0, 0.0];
    guarded(|| {
        let v = run(g, &src, &params, nx * ny, ((nx as u32).div_ceil(16), (ny as u32).div_ceil(16)))?;
        Some(v.into_iter().map(|x| x as f64).collect())
    })
}

/// Smooth escape-time values (−1 inside the set).
pub fn fractal(cx: f64, cy: f64, scale: f64, w: usize, h: usize, max_iter: u32, julia: Option<(f64, f64)>) -> Option<Vec<f32>> {
    let g = gpu()?;
    let src = r#"
struct P { cx: f32, cy: f32, scale: f32, jr: f32, ji: f32, julia: u32, w: u32, h: u32, iters: u32, pad0: u32, pad1: u32, pad2: u32 }
@group(0) @binding(0) var<uniform> prm: P;
@group(0) @binding(1) var<storage, read_write> out: array<f32>;
@compute @workgroup_size(16, 16)
fn main(@builtin(global_invocation_id) id: vec3<u32>) {
  if (id.x >= prm.w || id.y >= prm.h) { return; }
  let px = prm.cx + (f32(id.x) - f32(prm.w) / 2.0) * prm.scale;
  let py = prm.cy - (f32(id.y) - f32(prm.h) / 2.0) * prm.scale;
  var zr = 0.0; var zi = 0.0; var cr = px; var ci = py;
  if (prm.julia == 1u) { zr = px; zi = py; cr = prm.jr; ci = prm.ji; }
  var k = 0u;
  loop {
    if (k >= prm.iters || zr * zr + zi * zi > 256.0) { break; }
    let t = zr * zr - zi * zi + cr;
    zi = 2.0 * zr * zi + ci;
    zr = t;
    k = k + 1u;
  }
  var v = -1.0;
  if (k < prm.iters) {
    let m = log(log(sqrt(zr * zr + zi * zi))) / 0.6931472;
    v = f32(k) + 1.0 - m;
  }
  out[id.y * prm.w + id.x] = v;
}
"#;
    let (jr, ji, j) = match julia {
        Some((a, b)) => (a as f32, b as f32, 1u32),
        None => (0.0, 0.0, 0u32),
    };
    let params: [f32; 12] = [cx as f32, cy as f32, scale as f32, jr, ji, f32::from_bits(j), f32::from_bits(w as u32), f32::from_bits(h as u32), f32::from_bits(max_iter), 0.0, 0.0, 0.0];
    guarded(|| run(g, src, &params, w * h, ((w as u32).div_ceil(16), (h as u32).div_ceil(16))))
}
