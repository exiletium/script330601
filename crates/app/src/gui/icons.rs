//! Small vector icons drawn with the painter (no image assets needed).

use eframe::egui::{pos2, vec2, Color32, Painter, Pos2, Rect, Shape, Stroke};

pub fn plus(p: &Painter, c: Pos2, r: f32, col: Color32) {
    let s = Stroke::new(1.6, col);
    p.line_segment([c - vec2(r, 0.0), c + vec2(r, 0.0)], s);
    p.line_segment([c - vec2(0.0, r), c + vec2(0.0, r)], s);
}

pub fn plus_circle(p: &Painter, c: Pos2, r: f32, col: Color32) {
    p.circle_stroke(c, r, Stroke::new(1.4, col));
    plus(p, c, r * 0.5, col);
}

pub fn arrow_up(p: &Painter, c: Pos2, r: f32, col: Color32) {
    let s = Stroke::new(2.0, col);
    p.line_segment([c + vec2(0.0, r), c - vec2(0.0, r)], s);
    p.line_segment([c - vec2(0.0, r), c + vec2(-r * 0.7, -r * 0.3)], s);
    p.line_segment([c - vec2(0.0, r), c + vec2(r * 0.7, -r * 0.3)], s);
}

pub fn stop(p: &Painter, c: Pos2, r: f32, col: Color32) {
    p.rect_filled(Rect::from_center_size(c, vec2(r * 1.3, r * 1.3)), 2.0, col);
}

pub fn gear(p: &Painter, c: Pos2, r: f32, col: Color32) {
    let s = Stroke::new(1.5, col);
    p.circle_stroke(c, r * 0.42, s);
    for k in 0..8 {
        let a = k as f32 * std::f32::consts::TAU / 8.0;
        let d = vec2(a.cos(), a.sin());
        p.line_segment([c + d * r * 0.62, c + d * r], Stroke::new(2.2, col));
    }
    p.circle_stroke(c, r * 0.7, s);
}

pub fn search(p: &Painter, c: Pos2, r: f32, col: Color32) {
    let s = Stroke::new(1.5, col);
    let o = c - vec2(r * 0.2, r * 0.2);
    p.circle_stroke(o, r * 0.55, s);
    p.line_segment([o + vec2(r * 0.4, r * 0.4), c + vec2(r * 0.85, r * 0.85)], s);
}

pub fn sidebar(p: &Painter, c: Pos2, r: f32, col: Color32) {
    let rect = Rect::from_center_size(c, vec2(r * 2.0, r * 1.7));
    p.rect_stroke(rect, 3.0, Stroke::new(1.4, col), eframe::egui::StrokeKind::Inside);
    p.line_segment([pos2(rect.left() + r * 0.7, rect.top()), pos2(rect.left() + r * 0.7, rect.bottom())], Stroke::new(1.4, col));
}

pub fn download(p: &Painter, c: Pos2, r: f32, col: Color32) {
    let s = Stroke::new(1.5, col);
    p.line_segment([c - vec2(0.0, r), c + vec2(0.0, r * 0.4)], s);
    p.line_segment([c + vec2(0.0, r * 0.4), c + vec2(-r * 0.5, -r * 0.1)], s);
    p.line_segment([c + vec2(0.0, r * 0.4), c + vec2(r * 0.5, -r * 0.1)], s);
    p.line_segment([c + vec2(-r, r * 0.9), c + vec2(r, r * 0.9)], s);
}

pub fn copy(p: &Painter, c: Pos2, r: f32, col: Color32) {
    let s = Stroke::new(1.3, col);
    let a = Rect::from_center_size(c + vec2(r * 0.2, r * 0.2), vec2(r * 1.3, r * 1.3));
    let b = Rect::from_center_size(c - vec2(r * 0.25, r * 0.25), vec2(r * 1.3, r * 1.3));
    p.rect_stroke(a, 2.0, s, eframe::egui::StrokeKind::Inside);
    p.rect_stroke(b, 2.0, s, eframe::egui::StrokeKind::Inside);
}

pub fn chevron_down(p: &Painter, c: Pos2, r: f32, col: Color32) {
    let s = Stroke::new(1.4, col);
    p.line_segment([c + vec2(-r, -r * 0.4), c + vec2(0.0, r * 0.4)], s);
    p.line_segment([c + vec2(0.0, r * 0.4), c + vec2(r, -r * 0.4)], s);
}

/// Diamond "mode" glyph.
pub fn diamond(p: &Painter, c: Pos2, r: f32, col: Color32) {
    let pts = vec![c - vec2(0.0, r), c + vec2(r, 0.0), c + vec2(0.0, r), c - vec2(r, 0.0)];
    p.add(Shape::closed_line(pts, Stroke::new(1.3, col)));
}

/// Chip glyph for the compute device.
pub fn chip(p: &Painter, c: Pos2, r: f32, col: Color32) {
    let s = Stroke::new(1.3, col);
    let rect = Rect::from_center_size(c, vec2(r * 1.3, r * 1.3));
    p.rect_stroke(rect, 2.0, s, eframe::egui::StrokeKind::Inside);
    for k in 0..3 {
        let t = -r * 0.4 + k as f32 * r * 0.4;
        p.line_segment([pos2(rect.left() - r * 0.35, c.y + t), pos2(rect.left(), c.y + t)], s);
        p.line_segment([pos2(rect.right(), c.y + t), pos2(rect.right() + r * 0.35, c.y + t)], s);
        p.line_segment([pos2(c.x + t, rect.top() - r * 0.35), pos2(c.x + t, rect.top())], s);
        p.line_segment([pos2(c.x + t, rect.bottom()), pos2(c.x + t, rect.bottom() + r * 0.35)], s);
    }
}

/// Small document glyph used before pod titles.
pub fn doc(p: &Painter, c: Pos2, r: f32, col: Color32) {
    let s = Stroke::new(1.2, col);
    let rect = Rect::from_center_size(c, vec2(r * 1.4, r * 1.8));
    p.rect_stroke(rect, 2.0, s, eframe::egui::StrokeKind::Inside);
    for k in 0..3 {
        let y = rect.top() + r * 0.5 + k as f32 * r * 0.4;
        p.line_segment([pos2(rect.left() + r * 0.3, y), pos2(rect.right() - r * 0.3, y)], s);
    }
}

/// The app mark: a stylised summation sign in a rounded square.
pub fn logo(p: &Painter, c: Pos2, r: f32, accent: Color32, fg: Color32) {
    p.rect_filled(Rect::from_center_size(c, vec2(r * 2.0, r * 2.0)), r * 0.5, accent);
    let s = Stroke::new(r * 0.22, fg);
    let (l, t, rr, b) = (c.x - r * 0.45, c.y - r * 0.55, c.x + r * 0.45, c.y + r * 0.55);
    p.add(Shape::line(vec![pos2(rr, t), pos2(l, t), pos2(c.x + r * 0.05, c.y), pos2(l, b), pos2(rr, b)], s));
}
