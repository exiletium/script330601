//! Typesetting of [`MathNode`] trees with the egui painter: fractions,
//! super/subscripts, radicals, stretchy delimiters and matrices.

use super::theme;
use eframe::egui::{self, pos2, vec2, Color32, FontFamily, FontId, Pos2, Rect, Stroke, Ui};
use noesis::print::MathNode;
use std::sync::Arc;

/// A laid-out box: width, height above the baseline, depth below it.
#[derive(Clone, Copy, Debug, Default)]
struct Metrics {
    w: f32,
    asc: f32,
    desc: f32,
}

enum Item {
    Text { pos: Vec2Rel, galley: Arc<egui::Galley> },
    Line { a: Vec2Rel, b: Vec2Rel, width: f32 },
    Path { pts: Vec<Vec2Rel>, width: f32 },
}

/// Offset relative to the box origin (left edge, baseline).
#[derive(Clone, Copy, Debug)]
struct Vec2Rel(f32, f32);

struct Layout {
    m: Metrics,
    items: Vec<Item>,
}

impl Layout {
    fn shifted(self, dx: f32, dy: f32) -> Vec<Item> {
        self.items
            .into_iter()
            .map(|it| match it {
                Item::Text { pos, galley } => Item::Text { pos: Vec2Rel(pos.0 + dx, pos.1 + dy), galley },
                Item::Line { a, b, width } => Item::Line { a: Vec2Rel(a.0 + dx, a.1 + dy), b: Vec2Rel(b.0 + dx, b.1 + dy), width },
                Item::Path { pts, width } => Item::Path { pts: pts.into_iter().map(|p| Vec2Rel(p.0 + dx, p.1 + dy)).collect(), width },
            })
            .collect()
    }
}

struct Ctx<'a> {
    ui: &'a Ui,
    color: Color32,
}

impl Ctx<'_> {
    fn text(&self, s: &str, size: f32, italic: bool) -> Layout {
        let family = if italic { theme::math_italic() } else { FontFamily::Proportional };
        let font = FontId::new(size, family);
        let galley = self.ui.painter().layout_no_wrap(s.to_string(), font.clone(), self.color);
        let asc = size * 0.78;
        let desc = size * 0.24;
        let w = galley.size().x + if italic { size * 0.06 } else { 0.0 };
        // galley origin is its top-left; place so that the baseline is at y = 0
        let top = -galley.size().y * 0.79;
        Layout { m: Metrics { w, asc, desc }, items: vec![Item::Text { pos: Vec2Rel(0.0, top), galley }] }
    }

    fn layout(&self, n: &MathNode, size: f32) -> Layout {
        match n {
            MathNode::Text(s) if is_big_op(s) => self.big_op(s, size),
            MathNode::Text(s) => {
                // operators get breathing room already from the engine (" + ")
                self.text(s, size, false)
            }
            MathNode::Ident(s) => {
                let italic = s.chars().count() == 1 && s.chars().all(|c| c.is_alphabetic()) && !matches!(s.as_str(), "π" | "∞" | "ℤ" | "ℝ" | "ℂ" | "Δ" | "Γ");
                self.text(s, size, italic)
            }
            MathNode::Row(v) => {
                let mut items = Vec::new();
                let mut x = 0.0;
                let mut m = Metrics::default();
                for c in v {
                    let l = self.layout(c, size);
                    m.asc = m.asc.max(l.m.asc);
                    m.desc = m.desc.max(l.m.desc);
                    let w = l.m.w;
                    items.extend(l.shifted(x, 0.0));
                    x += w;
                }
                m.w = x;
                Layout { m, items }
            }
            MathNode::Frac(a, b) => {
                let s = (size * 0.9).max(10.0);
                let la = self.layout(a, s);
                let lb = self.layout(b, s);
                let pad = size * 0.15;
                let w = la.m.w.max(lb.m.w) + 2.0 * pad;
                let axis = -size * 0.28; // fraction bar height above baseline
                let gap = size * 0.14;
                let mut items = Vec::new();
                let (wa, wb) = (la.m.w, lb.m.w);
                let (da, ab) = (la.m.desc, lb.m.asc);
                let (aa, db) = (la.m.asc, lb.m.desc);
                items.extend(la.shifted((w - wa) / 2.0, axis - gap - da));
                items.extend(lb.shifted((w - wb) / 2.0, axis + gap + ab));
                items.push(Item::Line { a: Vec2Rel(pad * 0.4, axis), b: Vec2Rel(w - pad * 0.4, axis), width: (size * 0.06).max(1.0) });
                Layout { m: Metrics { w, asc: -axis + gap + da + aa, desc: (axis + gap + ab + db).max(0.0) }, items }
            }
            MathNode::Sup(base, sup) => {
                let lb = self.layout(base, size);
                let ls = self.layout(sup, (size * 0.7).max(9.0));
                let raise = lb.m.asc * 0.55 + ls.m.desc * 0.5;
                let (bw, sw) = (lb.m.w, ls.m.w);
                let m = Metrics { w: bw + sw + 1.0, asc: lb.m.asc.max(raise + ls.m.asc), desc: lb.m.desc };
                let mut items = lb.shifted(0.0, 0.0);
                items.extend(ls.shifted(bw + 1.0, -raise));
                Layout { m, items }
            }
            MathNode::Sub(base, sub) => {
                let lb = self.layout(base, size);
                let ls = self.layout(sub, (size * 0.7).max(9.0));
                let drop = size * 0.22 + ls.m.asc * 0.3;
                let (bw, sw) = (lb.m.w, ls.m.w);
                let m = Metrics { w: bw + sw + 1.0, asc: lb.m.asc, desc: lb.m.desc.max(drop + ls.m.desc) };
                let mut items = lb.shifted(0.0, 0.0);
                items.extend(ls.shifted(bw + 1.0, drop));
                Layout { m, items }
            }
            MathNode::SubSup(base, sub, sup) if matches!(&**base, MathNode::Text(t) if t == "∑" || t == "∏") => {
                // display-style limits above and below
                let lb = self.layout(base, size);
                let small = (size * 0.68).max(9.0);
                let lsub = self.layout(sub, small);
                let lsup = self.layout(sup, small);
                let w = lb.m.w.max(lsub.m.w).max(lsup.m.w);
                let gap = size * 0.08;
                let (bw, subw, supw) = (lb.m.w, lsub.m.w, lsup.m.w);
                let (ba, bd) = (lb.m.asc, lb.m.desc);
                let (suba, subd, supa, supd) = (lsub.m.asc, lsub.m.desc, lsup.m.asc, lsup.m.desc);
                let mut items = lb.shifted((w - bw) / 2.0, 0.0);
                items.extend(lsup.shifted((w - supw) / 2.0, -ba - gap - supd));
                items.extend(lsub.shifted((w - subw) / 2.0, bd + gap + suba));
                Layout { m: Metrics { w: w + size * 0.1, asc: ba + gap + supd + supa, desc: bd + gap + suba + subd }, items }
            }
            MathNode::SubSup(base, sub, sup) => {
                let lb = self.layout(base, size);
                let small = (size * 0.68).max(9.0);
                let lsub = self.layout(sub, small);
                let lsup = self.layout(sup, small);
                let raise = lb.m.asc * 0.65 + lsup.m.desc;
                let drop = lb.m.desc + lsub.m.asc * 0.6;
                let bw = lb.m.w;
                let w = bw + lsub.m.w.max(lsup.m.w) + 2.0;
                let m = Metrics { w, asc: lb.m.asc.max(raise + lsup.m.asc), desc: lb.m.desc.max(drop + lsub.m.desc) };
                let mut items = lb.shifted(0.0, 0.0);
                items.extend(lsup.shifted(bw + 2.0, -raise));
                items.extend(lsub.shifted(bw + 2.0, drop));
                Layout { m, items }
            }
            MathNode::Sqrt(inner, index) => {
                let li = self.layout(inner, size);
                let gap = size * 0.12;
                let h = li.m.asc + gap + li.m.desc;
                let hook = size * 0.55;
                let (iw, ia, idesc) = (li.m.w, li.m.asc, li.m.desc);
                let mut idx_w = 0.0;
                let mut items = Vec::new();
                if let Some(ix) = index {
                    let lx = self.layout(ix, (size * 0.6).max(8.0));
                    idx_w = (lx.m.w - hook * 0.4).max(0.0);
                    let lw = lx.m.w;
                    items.extend(lx.shifted(idx_w + hook * 0.4 - lw, -ia * 0.45));
                }
                let x0 = idx_w;
                let top = -(ia + gap);
                let lw = (size * 0.06).max(1.0);
                items.push(Item::Path {
                    pts: vec![Vec2Rel(x0, -h * 0.35 + idesc * 0.5), Vec2Rel(x0 + hook * 0.3, -h * 0.45 + idesc * 0.5), Vec2Rel(x0 + hook * 0.6, idesc), Vec2Rel(x0 + hook, top), Vec2Rel(x0 + hook + iw + size * 0.1, top)],
                    width: lw,
                });
                items.extend(li.shifted(x0 + hook + size * 0.05, 0.0));
                Layout { m: Metrics { w: x0 + hook + iw + size * 0.15, asc: ia + gap + lw, desc: idesc }, items }
            }
            MathNode::Fenced(open, close, inner) => {
                let li = self.layout(inner, size);
                let tall = li.m.asc + li.m.desc > size * 1.25;
                if !tall {
                    let lo = self.text(open, size, false);
                    let lc = self.text(close, size, false);
                    let (ow, iw, cw) = (lo.m.w, li.m.w, lc.m.w);
                    let m = Metrics { w: ow + iw + cw, asc: li.m.asc.max(lo.m.asc), desc: li.m.desc.max(lo.m.desc) };
                    let mut items = lo.shifted(0.0, 0.0);
                    items.extend(li.shifted(ow, 0.0));
                    items.extend(lc.shifted(ow + iw, 0.0));
                    return Layout { m, items };
                }
                // stretchy delimiters drawn as paths
                let (a, d) = (li.m.asc + size * 0.08, li.m.desc + size * 0.08);
                let dw = size * 0.38;
                let lw = (size * 0.065).max(1.0);
                let mut items = Vec::new();
                let delim = |x: f32, kind: &str, left: bool| -> Item {
                    let s = if left { 1.0 } else { -1.0 };
                    let pts = match kind {
                        "[" | "]" => vec![Vec2Rel(x + s * dw * 0.7, -a), Vec2Rel(x + s * dw * 0.15, -a), Vec2Rel(x + s * dw * 0.15, d), Vec2Rel(x + s * dw * 0.7, d)],
                        "|" => vec![Vec2Rel(x + s * dw * 0.4, -a), Vec2Rel(x + s * dw * 0.4, d)],
                        "{" | "}" => vec![Vec2Rel(x + s * dw * 0.75, -a), Vec2Rel(x + s * dw * 0.4, -a + (a + d) * 0.1), Vec2Rel(x + s * dw * 0.4, -a * 0.1), Vec2Rel(x + s * dw * 0.1, (d - a) * 0.5), Vec2Rel(x + s * dw * 0.4, d * 0.1), Vec2Rel(x + s * dw * 0.4, d - (a + d) * 0.1), Vec2Rel(x + s * dw * 0.75, d)],
                        _ => {
                            // parenthesis as a smooth arc
                            let n = 12;
                            (0..=n)
                                .map(|k| {
                                    let t = k as f32 / n as f32;
                                    let y = -a + (a + d) * t;
                                    let bulge = (std::f32::consts::PI * t).sin();
                                    Vec2Rel(x + s * (dw * 0.8 - dw * 0.6 * bulge), y)
                                })
                                .collect()
                        }
                    };
                    Item::Path { pts, width: lw }
                };
                let iw = li.m.w;
                if !open.is_empty() {
                    items.push(delim(0.0, open, true));
                }
                items.extend(li.shifted(dw, 0.0));
                if !close.is_empty() {
                    items.push(delim(dw + iw + dw, close, false));
                }
                Layout { m: Metrics { w: dw * 2.0 + iw, asc: a, desc: d }, items }
            }
            MathNode::Matrix(rows) => {
                let cells: Vec<Vec<Layout>> = rows.iter().map(|r| r.iter().map(|c| self.layout(c, size)).collect()).collect();
                let ncol = cells.iter().map(|r| r.len()).max().unwrap_or(0);
                let mut colw = vec![0.0f32; ncol];
                let mut rowm: Vec<(f32, f32)> = Vec::new();
                for r in &cells {
                    let (mut a, mut d) = (0.0f32, 0.0f32);
                    for (k, c) in r.iter().enumerate() {
                        colw[k] = colw[k].max(c.m.w);
                        a = a.max(c.m.asc);
                        d = d.max(c.m.desc);
                    }
                    rowm.push((a, d));
                }
                let hgap = size * 0.9;
                let vgap = size * 0.35;
                let total_h: f32 = rowm.iter().map(|(a, d)| a + d).sum::<f32>() + vgap * (rowm.len().saturating_sub(1)) as f32;
                let axis = -size * 0.3;
                let mut y = axis - total_h / 2.0;
                let mut items = Vec::new();
                for (ri, r) in cells.into_iter().enumerate() {
                    let (a, d) = rowm[ri];
                    let base = y + a;
                    let mut x = 0.0;
                    for (k, c) in r.into_iter().enumerate() {
                        let w = c.m.w;
                        items.extend(c.shifted(x + (colw[k] - w) / 2.0, base));
                        x += colw[k] + hgap;
                    }
                    y = base + d + vgap;
                }
                let w = colw.iter().sum::<f32>() + hgap * (ncol.saturating_sub(1)) as f32;
                Layout { m: Metrics { w, asc: -(axis - total_h / 2.0), desc: (axis + total_h / 2.0).max(0.0) }, items }
            }
        }
    }
}

fn is_big_op(s: &str) -> bool {
    matches!(s, "∫" | "∑" | "∏" | "∮")
}

impl Ctx<'_> {
    /// A large operator sign, vertically centred on the math axis.
    fn big_op(&self, s: &str, size: f32) -> Layout {
        let big = size * if s == "∫" { 1.75 } else { 1.45 };
        let font = FontId::new(big, FontFamily::Proportional);
        let galley = self.ui.painter().layout_no_wrap(s.to_string(), font, self.color);
        let h = galley.size().y;
        let axis = -size * 0.3;
        let top = axis - h / 2.0;
        let w = galley.size().x + size * 0.05;
        Layout { m: Metrics { w, asc: -top - h * 0.12, desc: (top + h * 0.88).max(0.0) }, items: vec![Item::Text { pos: Vec2Rel(0.0, top), galley }] }
    }
}

/// Draw a math node; returns the response for the allocated rectangle.
pub fn show(ui: &mut Ui, node: &MathNode, size: f32, color: Color32) -> egui::Response {
    let ctx = Ctx { ui, color };
    let l = ctx.layout(node, size);
    let pad = 2.0;
    let desired = vec2(l.m.w + pad, l.m.asc + l.m.desc + pad * 2.0);
    let (rect, resp) = ui.allocate_exact_size(desired, egui::Sense::click());
    if ui.is_rect_visible(rect) {
        let origin = pos2(rect.min.x, rect.min.y + pad + l.m.asc);
        paint(ui, l.items, origin, color);
    }
    resp
}

/// Size the node would occupy (for wrapping decisions).
pub fn measure(ui: &Ui, node: &MathNode, size: f32) -> egui::Vec2 {
    let ctx = Ctx { ui, color: Color32::WHITE };
    let l = ctx.layout(node, size);
    vec2(l.m.w, l.m.asc + l.m.desc)
}

fn paint(ui: &Ui, items: Vec<Item>, origin: Pos2, color: Color32) {
    let p = ui.painter();
    let at = |v: Vec2Rel| pos2(origin.x + v.0, origin.y + v.1);
    for it in items {
        match it {
            Item::Text { pos, galley } => {
                p.galley(at(pos), galley, color);
            }
            Item::Line { a, b, width } => {
                p.line_segment([at(a), at(b)], Stroke::new(width, color));
            }
            Item::Path { pts, width } => {
                let pts: Vec<Pos2> = pts.into_iter().map(at).collect();
                p.add(egui::Shape::line(pts, Stroke::new(width, color)));
            }
        }
    }
}

#[allow(dead_code)]
pub fn bounds(rect: Rect) -> Rect {
    rect
}
