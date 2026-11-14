//! Interactive plot widget.

use super::theme::Palette;
use eframe::egui::{self, pos2, vec2, Color32, FontId, Pos2, Rect, Sense, Shape, Stroke, StrokeKind, Ui};
use noesis::plot::{ticks, Plot, PlotKind};
use std::collections::HashMap;

/// Per-plot interaction state (kept across frames).
#[derive(Clone)]
pub struct PlotState {
    pub view: Option<(f64, f64, f64, f64)>,
    pub yaw: f32,
    pub pitch: f32,
    pub resampled: Option<Plot>,
    pub texture: Option<egui::TextureHandle>,
    pub last_resample: (f64, f64),
}

impl Default for PlotState {
    fn default() -> Self {
        PlotState { view: None, yaw: 0.7, pitch: 0.55, resampled: None, texture: None, last_resample: (f64::NAN, f64::NAN) }
    }
}

pub type States = HashMap<egui::Id, PlotState>;

fn fmt_tick(v: f64) -> String {
    noesis::print::fmt_float(v, 4)
}

/// Sequential colour scale (dark-to-light blue/teal) for heights and densities.
fn ramp(t: f32, dark: bool) -> Color32 {
    let t = t.clamp(0.0, 1.0);
    let stops: [(f32, [f32; 3]); 5] = if dark {
        [(0.0, [0.10, 0.13, 0.30]), (0.3, [0.16, 0.36, 0.72]), (0.6, [0.10, 0.62, 0.55]), (0.85, [0.80, 0.70, 0.20]), (1.0, [0.98, 0.94, 0.70])]
    } else {
        [(0.0, [0.12, 0.20, 0.50]), (0.3, [0.16, 0.47, 0.84]), (0.6, [0.11, 0.69, 0.48]), (0.85, [0.93, 0.63, 0.0]), (1.0, [0.99, 0.90, 0.55])]
    };
    for w in stops.windows(2) {
        let ((t0, c0), (t1, c1)) = (w[0], w[1]);
        if t <= t1 {
            let u = (t - t0) / (t1 - t0);
            let c: Vec<u8> = (0..3).map(|k| ((c0[k] + (c1[k] - c0[k]) * u) * 255.0) as u8).collect();
            return Color32::from_rgb(c[0], c[1], c[2]);
        }
    }
    Color32::WHITE
}

pub fn show(ui: &mut Ui, id: egui::Id, plot: &Plot, pal: &Palette, states: &mut States) {
    let width = ui.available_width().min(760.0).max(240.0);
    let height = match plot.kind {
        PlotKind::NumberLine => 90.0,
        PlotKind::Surface => (width * 0.62).min(440.0),
        _ => (width * 0.58).min(420.0),
    };
    let (rect, resp) = ui.allocate_exact_size(vec2(width, height), Sense::click_and_drag());
    let st = states.entry(id).or_default();
    let painter = ui.painter_at(rect);
    painter.rect_filled(rect, 10.0, pal.plot_bg);
    painter.rect_stroke(rect, 10.0, Stroke::new(1.0_f32, pal.border), StrokeKind::Inside);
    match plot.kind {
        PlotKind::Surface => surface(ui, rect, &resp, plot, pal, st),
        PlotKind::Fractal | PlotKind::Contour => {
            image_plot(ui, rect, plot, pal, st);
            if plot.kind == PlotKind::Contour {
                cartesian(ui, rect, &resp, plot, pal, st, true);
            }
        }
        PlotKind::NumberLine => number_line(ui, rect, plot, pal),
        PlotKind::Bars => bars(ui, rect, &resp, plot, pal),
        _ => cartesian(ui, rect, &resp, plot, pal, st, false),
    }
    if plot.device == noesis::compute::Device::Gpu {
        painter.text(rect.right_top() + vec2(-10.0, 8.0), egui::Align2::RIGHT_TOP, "GPU", FontId::proportional(10.5), pal.faint);
    }
}

struct Frame2 {
    plot_rect: Rect,
    x: (f64, f64),
    y: (f64, f64),
}

impl Frame2 {
    fn sx(&self, x: f64) -> f32 {
        self.plot_rect.left() + ((x - self.x.0) / (self.x.1 - self.x.0)) as f32 * self.plot_rect.width()
    }
    fn sy(&self, y: f64) -> f32 {
        self.plot_rect.bottom() - ((y - self.y.0) / (self.y.1 - self.y.0)) as f32 * self.plot_rect.height()
    }
    fn inv(&self, p: Pos2) -> (f64, f64) {
        let x = self.x.0 + ((p.x - self.plot_rect.left()) / self.plot_rect.width()) as f64 * (self.x.1 - self.x.0);
        let y = self.y.0 + ((self.plot_rect.bottom() - p.y) / self.plot_rect.height()) as f64 * (self.y.1 - self.y.0);
        (x, y)
    }
}

fn axes_frame(rect: Rect, x: (f64, f64), y: (f64, f64)) -> Frame2 {
    let plot_rect = Rect::from_min_max(pos2(rect.left() + 46.0, rect.top() + 12.0), pos2(rect.right() - 14.0, rect.bottom() - 26.0));
    Frame2 { plot_rect, x, y }
}

fn draw_axes(painter: &egui::Painter, f: &Frame2, pal: &Palette, labels: (&str, &str)) {
    let font = FontId::proportional(11.0);
    for t in ticks(f.x.0, f.x.1, 8) {
        let x = f.sx(t);
        painter.line_segment([pos2(x, f.plot_rect.top()), pos2(x, f.plot_rect.bottom())], Stroke::new(1.0_f32, pal.grid));
        painter.text(pos2(x, f.plot_rect.bottom() + 5.0), egui::Align2::CENTER_TOP, fmt_tick(t), font.clone(), pal.muted);
    }
    for t in ticks(f.y.0, f.y.1, 6) {
        let y = f.sy(t);
        painter.line_segment([pos2(f.plot_rect.left(), y), pos2(f.plot_rect.right(), y)], Stroke::new(1.0_f32, pal.grid));
        painter.text(pos2(f.plot_rect.left() - 6.0, y), egui::Align2::RIGHT_CENTER, fmt_tick(t), font.clone(), pal.muted);
    }
    let axis = Stroke::new(1.2_f32, pal.faint);
    if f.x.0 < 0.0 && f.x.1 > 0.0 {
        let x = f.sx(0.0);
        painter.line_segment([pos2(x, f.plot_rect.top()), pos2(x, f.plot_rect.bottom())], axis);
    }
    if f.y.0 < 0.0 && f.y.1 > 0.0 {
        let y = f.sy(0.0);
        painter.line_segment([pos2(f.plot_rect.left(), y), pos2(f.plot_rect.right(), y)], axis);
    }
    painter.text(pos2(f.plot_rect.right(), f.plot_rect.bottom() - 4.0), egui::Align2::RIGHT_BOTTOM, labels.0, FontId::new(12.0, super::theme::math_italic()), pal.muted);
    painter.text(pos2(f.plot_rect.left() + 4.0, f.plot_rect.top() + 2.0), egui::Align2::LEFT_TOP, labels.1, FontId::new(12.0, super::theme::math_italic()), pal.muted);
}

fn cartesian(ui: &mut Ui, rect: Rect, resp: &egui::Response, plot0: &Plot, pal: &Palette, st: &mut PlotState, overlay: bool) {
    let (mut x0, mut x1, mut y0, mut y1) = st.view.unwrap_or((plot0.x_range.0, plot0.x_range.1, plot0.y_range.0, plot0.y_range.1));
    // keep equal aspect when requested
    let f0 = axes_frame(rect, (x0, x1), (y0, y1));
    if plot0.equal_aspect && st.view.is_none() {
        let px = (x1 - x0) / f0.plot_rect.width() as f64;
        let py = (y1 - y0) / f0.plot_rect.height() as f64;
        if px > py {
            let c = 0.5 * (y0 + y1);
            let h = px * f0.plot_rect.height() as f64 / 2.0;
            y0 = c - h;
            y1 = c + h;
        } else {
            let c = 0.5 * (x0 + x1);
            let w = py * f0.plot_rect.width() as f64 / 2.0;
            x0 = c - w;
            x1 = c + w;
        }
    }
    let mut f = axes_frame(rect, (x0, x1), (y0, y1));
    // interaction: drag to pan, scroll to zoom, double-click to reset
    if !overlay {
        if resp.dragged() {
            let d = resp.drag_delta();
            let dx = -(d.x as f64) / f.plot_rect.width() as f64 * (x1 - x0);
            let dy = (d.y as f64) / f.plot_rect.height() as f64 * (y1 - y0);
            st.view = Some((x0 + dx, x1 + dx, y0 + dy, y1 + dy));
        }
        if resp.hovered() {
            let scroll = ui.input(|i| i.smooth_scroll_delta.y + (i.zoom_delta() - 1.0) * 200.0);
            if scroll.abs() > 0.1 {
                if let Some(p) = resp.hover_pos() {
                    let (cx, cy) = f.inv(p);
                    let k = (-(scroll as f64) / 400.0).exp();
                    st.view = Some((cx + (x0 - cx) * k, cx + (x1 - cx) * k, cy + (y0 - cy) * k, cy + (y1 - cy) * k));
                }
            }
        }
        if resp.double_clicked() {
            st.view = None;
            st.resampled = None;
        }
        if let Some(v) = st.view {
            f = axes_frame(rect, (v.0, v.1), (v.2, v.3));
            // re-sample curves when the x-window changed noticeably
            if plot0.kind == PlotKind::Cartesian && !resp.dragged() && ((v.0 - st.last_resample.0).abs() > 1e-9 * (1.0 + v.0.abs()) || (v.1 - st.last_resample.1).abs() > 1e-9 * (1.0 + v.1.abs())) {
                st.last_resample = (v.0, v.1);
                st.resampled = noesis::plot::resample(plot0, v.0, v.1);
            }
        }
    }
    let plot = if plot0.kind == PlotKind::Cartesian { st.resampled.as_ref().unwrap_or(plot0) } else { plot0 };
    let painter = ui.painter_at(rect);
    if !overlay {
        draw_axes(&painter, &f, pal, (&plot.x_label, &plot.y_label));
    }
    let clip = painter.with_clip_rect(f.plot_rect);
    // shaded spans (definite integrals)
    for sp in &plot.spans {
        if let Some(ser) = plot.series.first() {
            for seg in &ser.segments {
                let pts: Vec<&[f64; 2]> = seg.iter().filter(|p| p[0] >= sp.lo && p[0] <= sp.hi).collect();
                if pts.len() < 2 {
                    continue;
                }
                let base = f.sy(0.0f64.clamp(f.y.0, f.y.1));
                let fill = pal.series(0).gamma_multiply(0.22);
                for w in pts.windows(2) {
                    let (a, b) = (w[0], w[1]);
                    let quad = vec![pos2(f.sx(a[0]), base), pos2(f.sx(a[0]), f.sy(a[1])), pos2(f.sx(b[0]), f.sy(b[1])), pos2(f.sx(b[0]), base)];
                    clip.add(Shape::convex_polygon(quad, fill, Stroke::NONE));
                }
            }
        }
    }
    for (k, ser) in plot.series.iter().enumerate() {
        let col = if overlay { pal.text.gamma_multiply(0.55) } else { pal.series(k) };
        for seg in &ser.segments {
            if ser.points {
                for p in seg {
                    clip.circle_filled(pos2(f.sx(p[0]), f.sy(p[1])), 3.5, col);
                }
            } else if seg.len() >= 2 {
                let pts: Vec<Pos2> = seg.iter().map(|p| pos2(f.sx(p[0]), f.sy(p[1]).clamp(-1e5, 1e5))).collect();
                clip.add(Shape::line(pts, Stroke::new(if overlay { 1.0_f32 } else { 2.0_f32 }, col)));
            }
        }
    }
    // legend for two or more labelled series
    let labelled: Vec<(usize, &str)> = plot.series.iter().enumerate().filter(|(_, s)| !s.label.is_empty()).map(|(k, s)| (k, s.label.as_str())).collect();
    if labelled.len() >= 2 && !overlay {
        let mut y = f.plot_rect.top() + 8.0;
        for (k, label) in labelled.iter().take(6) {
            let x = f.plot_rect.left() + 10.0;
            painter.rect_filled(Rect::from_min_size(pos2(x, y + 5.0), vec2(14.0, 3.0)), 1.0, pal.series(*k));
            let short: String = label.chars().take(40).collect();
            painter.text(pos2(x + 20.0, y), egui::Align2::LEFT_TOP, short, FontId::proportional(11.5), pal.text);
            y += 16.0;
        }
    }
    // hover readout
    if let Some(p) = resp.hover_pos() {
        if f.plot_rect.contains(p) && !overlay {
            let (hx, hy) = f.inv(p);
            let mut label = format!("({}, {})", fmt_tick(hx), fmt_tick(hy));
            if plot.kind == PlotKind::Cartesian {
                if let Some(ser) = plot.series.first() {
                    let nearest = ser.segments.iter().flatten().min_by(|a, b| (a[0] - hx).abs().partial_cmp(&(b[0] - hx).abs()).unwrap());
                    if let Some(n) = nearest {
                        label = format!("x = {}   y = {}", fmt_tick(n[0]), fmt_tick(n[1]));
                        clip.circle_filled(pos2(f.sx(n[0]), f.sy(n[1])), 4.0, pal.series(0));
                    }
                }
            }
            painter.text(pos2(f.plot_rect.right() - 6.0, f.plot_rect.top() + 4.0), egui::Align2::RIGHT_TOP, label, FontId::proportional(11.5), pal.muted);
        }
    }
}

fn number_line(ui: &mut Ui, rect: Rect, plot: &Plot, pal: &Palette) {
    let painter = ui.painter_at(rect);
    let (x0, x1) = plot.x_range;
    let left = rect.left() + 24.0;
    let right = rect.right() - 24.0;
    let y = rect.center().y - 6.0;
    let sx = |x: f64| left + ((x - x0) / (x1 - x0)) as f32 * (right - left);
    painter.line_segment([pos2(left, y), pos2(right, y)], Stroke::new(1.5_f32, pal.muted));
    for t in ticks(x0, x1, 10) {
        let x = sx(t);
        painter.line_segment([pos2(x, y - 4.0), pos2(x, y + 4.0)], Stroke::new(1.0_f32, pal.muted));
        painter.text(pos2(x, y + 8.0), egui::Align2::CENTER_TOP, fmt_tick(t), FontId::proportional(11.0), pal.muted);
    }
    let col = pal.series(0);
    for sp in &plot.spans {
        let a = if sp.lo.is_finite() { sx(sp.lo) } else { left };
        let b = if sp.hi.is_finite() { sx(sp.hi) } else { right };
        if b > a {
            painter.line_segment([pos2(a, y), pos2(b, y)], Stroke::new(5.0_f32, col));
        }
        for (v, closed) in [(sp.lo, sp.lo_closed), (sp.hi, sp.hi_closed)] {
            if v.is_finite() {
                let c = pos2(sx(v), y);
                painter.circle_filled(c, 6.0, if closed { col } else { pal.plot_bg });
                painter.circle_stroke(c, 6.0, Stroke::new(2.0_f32, col));
            }
        }
    }
}

fn bars(ui: &mut Ui, rect: Rect, resp: &egui::Response, plot: &Plot, pal: &Palette) {
    let painter = ui.painter_at(rect);
    let Some(ser) = plot.series.first() else { return };
    let labels: Vec<&str> = ser.label.split('\u{1f}').collect();
    let pts = ser.segments.first().cloned().unwrap_or_default();
    let f = axes_frame(rect, plot.x_range, (plot.y_range.0.min(0.0), plot.y_range.1));
    for t in ticks(f.y.0, f.y.1, 5) {
        let y = f.sy(t);
        painter.line_segment([pos2(f.plot_rect.left(), y), pos2(f.plot_rect.right(), y)], Stroke::new(1.0_f32, pal.grid));
        painter.text(pos2(f.plot_rect.left() - 6.0, y), egui::Align2::RIGHT_CENTER, fmt_tick(t), FontId::proportional(11.0), pal.muted);
    }
    let n = pts.len().max(1);
    let bw = (f.plot_rect.width() / n as f32 * 0.7).min(60.0);
    let hover = resp.hover_pos();
    for (k, p) in pts.iter().enumerate() {
        let cx = f.sx(p[0]);
        let r = Rect::from_min_max(pos2(cx - bw / 2.0, f.sy(p[1].max(0.0))), pos2(cx + bw / 2.0, f.sy(0.0f64.max(f.y.0))));
        let hot = hover.is_some_and(|h| r.expand(2.0).contains(h));
        painter.rect_filled(r, 3.0, if hot { pal.accent_soft } else { pal.series(0) });
        if n <= 16 {
            if let Some(l) = labels.get(k) {
                painter.text(pos2(cx, f.plot_rect.bottom() + 5.0), egui::Align2::CENTER_TOP, *l, FontId::proportional(10.0), pal.muted);
            }
        }
        if hot {
            painter.text(pos2(cx, r.top() - 4.0), egui::Align2::CENTER_BOTTOM, format!("{} : {}", labels.get(k).unwrap_or(&""), fmt_tick(p[1])), FontId::proportional(11.5), pal.text);
        }
    }
}

fn image_plot(ui: &mut Ui, rect: Rect, plot: &Plot, pal: &Palette, st: &mut PlotState) {
    let Some(g) = &plot.grid else { return };
    if st.texture.is_none() {
        let (lo, hi) = g.z_range;
        let mut rgba = Vec::with_capacity(g.nx * g.ny * 4);
        for j in (0..g.ny).rev() {
            for i in 0..g.nx {
                let z = g.z[j * g.nx + i] as f64;
                let c = if plot.kind == PlotKind::Fractal {
                    if z < 0.0 {
                        if pal.dark {
                            Color32::from_rgb(8, 8, 10)
                        } else {
                            Color32::from_rgb(20, 20, 30)
                        }
                    } else {
                        ramp(((z.max(1.0)).ln() / hi.max(2.0).ln()) as f32, pal.dark)
                    }
                } else if !z.is_finite() {
                    pal.plot_bg
                } else {
                    ramp(((z - lo) / (hi - lo)) as f32, pal.dark).gamma_multiply(0.85)
                };
                rgba.extend_from_slice(&[c.r(), c.g(), c.b(), 255]);
            }
        }
        let img = egui::ColorImage::from_rgba_unmultiplied([g.nx, g.ny], &rgba);
        st.texture = Some(ui.ctx().load_texture(format!("plot-{:p}", plot), img, egui::TextureOptions::LINEAR));
    }
    if let Some(tex) = &st.texture {
        let f = axes_frame(rect, plot.x_range, plot.y_range);
        let target = if plot.kind == PlotKind::Fractal { rect.shrink(6.0) } else { f.plot_rect };
        ui.painter_at(rect).image(tex.id(), target, Rect::from_min_max(pos2(0.0, 0.0), pos2(1.0, 1.0)), Color32::WHITE);
    }
}

fn surface(ui: &mut Ui, rect: Rect, resp: &egui::Response, plot: &Plot, pal: &Palette, st: &mut PlotState) {
    let Some(g) = &plot.grid else { return };
    if resp.dragged() {
        let d = resp.drag_delta();
        st.yaw += d.x * 0.01;
        st.pitch = (st.pitch + d.y * 0.01).clamp(0.05, 1.5);
    }
    if resp.double_clicked() {
        st.yaw = 0.7;
        st.pitch = 0.55;
    }
    let (lo, hi) = g.z_range;
    let painter = ui.painter_at(rect);
    let (cy, sy) = (st.yaw.cos(), st.yaw.sin());
    let (cp, sp) = (st.pitch.cos(), st.pitch.sin());
    let scale = rect.width().min(rect.height() * 1.3) * 0.36;
    let center = rect.center() + vec2(0.0, rect.height() * 0.06);
    // normalised coordinates in [-1, 1]^2 x [-0.6, 0.6]
    let proj = |u: f32, v: f32, w: f32| -> (Pos2, f32) {
        let x = u * cy - v * sy;
        let y = u * sy + v * cy;
        let depth = y * cp + w * sp;
        let sy2 = -w * cp + y * sp;
        (pos2(center.x + x * scale, center.y + sy2 * scale * 0.8), depth)
    };
    let nx = g.nx;
    let ny = g.ny;
    let zn = |i: usize, j: usize| -> Option<f32> {
        let z = g.z[j * nx + i] as f64;
        if !z.is_finite() {
            return None;
        }
        Some((((z.clamp(lo, hi) - lo) / (hi - lo).max(1e-12)) as f32 - 0.5) * 1.1)
    };
    let uv = |i: usize, j: usize| (2.0 * i as f32 / (nx - 1) as f32 - 1.0, 2.0 * j as f32 / (ny - 1) as f32 - 1.0);
    let mut quads: Vec<(f32, [Pos2; 4], Color32)> = Vec::with_capacity(nx * ny);
    for j in 0..ny - 1 {
        for i in 0..nx - 1 {
            let (Some(a), Some(b), Some(c), Some(d)) = (zn(i, j), zn(i + 1, j), zn(i + 1, j + 1), zn(i, j + 1)) else { continue };
            let corners = [(i, j, a), (i + 1, j, b), (i + 1, j + 1, c), (i, j + 1, d)];
            let mut pts = [Pos2::ZERO; 4];
            let mut depth = 0.0;
            for (k, (ii, jj, z)) in corners.iter().enumerate() {
                let (u, v) = uv(*ii, *jj);
                let (p, dd) = proj(u, v, *z);
                pts[k] = p;
                depth += dd;
            }
            let t = (a + b + c + d) / 4.0 / 1.1 + 0.5;
            // simple shading from the surface normal
            let (u0, v0) = uv(i, j);
            let (u1, v1) = uv(i + 1, j + 1);
            let nxv = -(b - a) / (u1 - u0);
            let nyv = -(d - a) / (v1 - v0);
            let shade = (0.75 + 0.25 * ((nxv * 0.4 + nyv * 0.6 + 1.0) / (1.0 + (nxv * nxv + nyv * nyv).sqrt()))).clamp(0.45, 1.05);
            let base = ramp(t, pal.dark);
            let col = Color32::from_rgb((base.r() as f32 * shade).min(255.0) as u8, (base.g() as f32 * shade).min(255.0) as u8, (base.b() as f32 * shade).min(255.0) as u8);
            quads.push((depth, pts, col));
        }
    }
    quads.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap_or(std::cmp::Ordering::Equal));
    let mut mesh = egui::Mesh::default();
    let edge = pal.plot_bg.gamma_multiply(0.35);
    for (_, pts, col) in &quads {
        let base = mesh.vertices.len() as u32;
        for p in pts {
            mesh.colored_vertex(*p, *col);
        }
        mesh.add_triangle(base, base + 1, base + 2);
        mesh.add_triangle(base, base + 2, base + 3);
    }
    painter.add(Shape::mesh(mesh));
    if nx <= 80 {
        for (_, pts, _) in quads.iter().step_by(1) {
            painter.add(Shape::closed_line(pts.to_vec(), Stroke::new(0.4_f32, edge)));
        }
    }
    // bounding box floor and axis labels
    let floor: Vec<Pos2> = [(-1.0, -1.0), (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0)].iter().map(|(u, v)| proj(*u, *v, -0.6).0).collect();
    painter.add(Shape::closed_line(floor, Stroke::new(1.0_f32, pal.faint)));
    let font = FontId::proportional(11.0);
    let (px, _) = proj(1.15, -1.0, -0.6);
    let (py, _) = proj(-1.0, 1.15, -0.6);
    painter.text(px, egui::Align2::CENTER_CENTER, &plot.x_label, font.clone(), pal.muted);
    painter.text(py, egui::Align2::CENTER_CENTER, &plot.y_label, font.clone(), pal.muted);
    painter.text(rect.left_bottom() + vec2(10.0, -8.0), egui::Align2::LEFT_BOTTOM, format!("z from {} to {}  ·  drag to rotate", fmt_tick(lo), fmt_tick(hi)), font, pal.faint);
}
