//! Formula knowledge: geometry of common shapes, physics formulas with
//! units, triangle solving, angle conversion and a table of named formulas.

use super::units::{self, Dim};
use crate::engine::*;
use crate::expr::*;
use crate::parse;
use crate::print::text as ptext;
use crate::simplify as s;
use regex::Regex;
use std::sync::OnceLock;

macro_rules! re {
    ($s:expr) => {{
        static R: OnceLock<Regex> = OnceLock::new();
        R.get_or_init(|| Regex::new($s).unwrap())
    }};
}

fn ex(t: &str) -> Option<Expr> {
    parse::parse(t.trim()).ok().map(|p| p.expr)
}

fn fmt(v: f64) -> String {
    crate::stats::fmt(v)
}

fn sci(v: f64) -> String {
    crate::print::fmt_float(v, 6)
}

pub fn handle(q: &Query) -> Option<Answer> {
    let l = q.lower.as_str();
    angles(l).or_else(|| triangle(q)).or_else(|| geometry(l)).or_else(|| physics(q)).or_else(|| named(l))
}

// ---------------------------------------------------------------------------
// geometry
// ---------------------------------------------------------------------------

const NUM: &str = r"(?:\d+(?:\.\d+)?(?:/\d+)?|\.\d+|pi|π|sqrt\(\d+\))";
const UNIT: &str = r"(?:\s*(?:cm|mm|km|m|in|inch|inches|ft|feet|foot|yd|yards?|mi|miles?|meters?|metres?|centimeters?|centimetres?|millimeters?|kilometers?|units?)\b)?";

struct Params {
    vals: Vec<(String, Vec<Expr>)>,
    unit: String,
    bare: Vec<Expr>,
}

impl Params {
    fn get(&self, keys: &[&str]) -> Option<Expr> {
        self.vals.iter().find(|(k, _)| keys.contains(&k.as_str())).and_then(|(_, v)| v.first().cloned())
    }
    fn list(&self, keys: &[&str]) -> Vec<Expr> {
        self.vals.iter().filter(|(k, _)| keys.contains(&k.as_str())).flat_map(|(_, v)| v.iter().cloned()).collect()
    }
}

fn params(l: &str) -> Params {
    static KV: OnceLock<Regex> = OnceLock::new();
    let kv = KV.get_or_init(|| {
        Regex::new(&format!(
            r"\b(radius|radii|diameter|height|altitude|slant height|side length|sides|side|edge length|edges|edge|length|width|breadth|depth|bases|base|legs|leg|hypotenuse|semi-?axes|semi-?major axis|semi-?minor axis|axes|diagonals|diagonal|angle|circumference|perimeter|area|surface area|volume|apothem|major radius|minor radius|r|h|l|w|a|b|s)\s*(?:lengths?\s+)?(?:of|=|is|are|:|equal to)?\s*((?:{NUM}{UNIT}\s*(?:,\s*and|,|and|by|x|×)?\s*)+)"
        ))
        .unwrap()
    });
    static ONE: OnceLock<Regex> = OnceLock::new();
    let one = ONE.get_or_init(|| Regex::new(&format!(r"({NUM})({UNIT})")).unwrap());
    let mut vals = Vec::new();
    let mut unit = String::new();
    let mut covered: Vec<(usize, usize)> = Vec::new();
    for m in kv.captures_iter(l) {
        let key = m[1].replace("semiaxes", "semi-axes").to_string();
        let span = m.get(2).unwrap();
        let mut list = Vec::new();
        for n in one.captures_iter(span.as_str()) {
            if let Some(e) = ex(&n[1].replace('π', "pi")) {
                list.push(e);
            }
            if unit.is_empty() && !n[2].trim().is_empty() {
                unit = n[2].trim().to_string();
            }
        }
        if !list.is_empty() {
            covered.push((span.start(), span.end()));
            vals.push((key, list));
        }
    }
    // numbers not attached to a keyword: "rectangle 4 by 6", "cube 3"
    let mut bare = Vec::new();
    for n in one.captures_iter(l) {
        let at = n.get(0).unwrap().start();
        if covered.iter().any(|(a, b)| at >= *a && at < *b) {
            continue;
        }
        // skip digits glued to letters ("3d")
        if l[..at].chars().last().map_or(false, |c| c.is_alphanumeric()) {
            continue;
        }
        if let Some(e) = ex(&n[1].replace('π', "pi")) {
            bare.push(e);
        }
        if unit.is_empty() && !n[2].trim().is_empty() {
            unit = n[2].trim().to_string();
        }
    }
    Params { vals, unit, bare }
}

/// Evaluate a formula string with single-letter variables.
fn f(formula: &str, vars: &[(&str, &Expr)]) -> Expr {
    let e = ex(formula).unwrap_or_else(undef);
    let pairs: Vec<(Expr, Expr)> = vars.iter().map(|(n, v)| (sym(n), (*v).clone())).collect();
    s::simplify(&e.subs_many(&pairs))
}

struct Prop {
    name: &'static str,
    value: Expr,
    formula: &'static str,
    /// 1 = length, 2 = area, 3 = volume, 0 = angle or pure number
    dim: u8,
}

fn prop(name: &'static str, value: Expr, formula: &'static str, dim: u8) -> Prop {
    Prop { name, value, formula, dim }
}

const POLYGONS: &[(&str, i64)] = &[("pentagon", 5), ("hexagon", 6), ("heptagon", 7), ("septagon", 7), ("octagon", 8), ("nonagon", 9), ("enneagon", 9), ("decagon", 10), ("hendecagon", 11), ("dodecagon", 12), ("icosagon", 20)];

/// All properties of a shape that follow from the given parameters; missing
/// parameters become symbols, so the result is the general formula.
fn shape_props(shape: &str, p: &Params) -> Option<(String, Vec<Prop>)> {
    let or_sym = |v: Option<Expr>, name: &str| v.unwrap_or_else(|| sym(name));
    let half = |e: Expr| s::simplify(&div(e, int(2)));
    let two = |v: &[Expr], k: usize| v.get(k).cloned();
    let dims: Vec<Expr> = {
        let mut v = p.list(&["sides", "side", "length", "width", "breadth", "height", "depth", "edges", "edge", "l", "w", "h"]);
        if v.is_empty() {
            v = p.bare.clone();
        }
        v
    };
    Some(match shape {
        "circle" | "disk" | "disc" | "semicircle" => {
            let r = p
                .get(&["radius", "r"])
                .or_else(|| p.get(&["diameter"]).map(half))
                .or_else(|| p.get(&["circumference", "perimeter"]).map(|c| f("c/(2*pi)", &[("c", &c)])))
                .or_else(|| p.get(&["area"]).map(|a| f("sqrt(a/pi)", &[("a", &a)])))
                .or_else(|| p.bare.first().cloned());
            let r = or_sym(r, "r");
            let v = [("r", &r)];
            if shape == "semicircle" {
                (format!("semicircle | radius {}", ptext(&r)), vec![prop("area", f("pi*r^2/2", &v), "A = π r² / 2", 2), prop("perimeter", f("pi*r + 2*r", &v), "P = π r + 2 r", 1), prop("arc length", f("pi*r", &v), "L = π r", 1)])
            } else {
                (format!("circle | radius {}", ptext(&r)), vec![prop("area", f("pi*r^2", &v), "A = π r²", 2), prop("circumference", f("2*pi*r", &v), "C = 2 π r", 1), prop("diameter", f("2*r", &v), "d = 2 r", 1), prop("radius", r.clone(), "r", 1)])
            }
        }
        "sphere" | "ball" | "hemisphere" => {
            let r = p
                .get(&["radius", "r"])
                .or_else(|| p.get(&["diameter"]).map(half))
                .or_else(|| p.get(&["volume"]).map(|v| f("(3*v/(4*pi))^(1/3)", &[("v", &v)])))
                .or_else(|| p.get(&["surface area", "area"]).map(|a| f("sqrt(a/(4*pi))", &[("a", &a)])))
                .or_else(|| p.bare.first().cloned());
            let r = or_sym(r, "r");
            let v = [("r", &r)];
            if shape == "hemisphere" {
                (format!("hemisphere | radius {}", ptext(&r)), vec![prop("volume", f("2*pi*r^3/3", &v), "V = 2 π r³ / 3", 3), prop("surface area", f("3*pi*r^2", &v), "S = 3 π r² (with the flat face)", 2)])
            } else {
                (format!("sphere | radius {}", ptext(&r)), vec![prop("volume", f("4*pi*r^3/3", &v), "V = 4 π r³ / 3", 3), prop("surface area", f("4*pi*r^2", &v), "S = 4 π r²", 2), prop("diameter", f("2*r", &v), "d = 2 r", 1), prop("circumference", f("2*pi*r", &v), "C = 2 π r (great circle)", 1), prop("radius", r.clone(), "r", 1)])
            }
        }
        "cylinder" => {
            let r = or_sym(p.get(&["radius", "r"]).or_else(|| p.get(&["diameter"]).map(half)).or_else(|| two(&p.bare, 0)), "r");
            let h = or_sym(p.get(&["height", "h", "length"]).or_else(|| two(&p.bare, 1)), "h");
            let v = [("r", &r), ("h", &h)];
            (format!("cylinder | radius {} | height {}", ptext(&r), ptext(&h)), vec![prop("volume", f("pi*r^2*h", &v), "V = π r² h", 3), prop("surface area", f("2*pi*r*(r+h)", &v), "S = 2 π r (r + h)", 2), prop("lateral area", f("2*pi*r*h", &v), "L = 2 π r h", 2), prop("base area", f("pi*r^2", &v), "B = π r²", 2)])
        }
        "cone" => {
            let r = or_sym(p.get(&["radius", "r"]).or_else(|| p.get(&["diameter"]).map(half)).or_else(|| two(&p.bare, 0)), "r");
            let h = or_sym(p.get(&["height", "h"]).or_else(|| two(&p.bare, 1)), "h");
            let v = [("r", &r), ("h", &h)];
            (format!("cone | radius {} | height {}", ptext(&r), ptext(&h)), vec![prop("volume", f("pi*r^2*h/3", &v), "V = π r² h / 3", 3), prop("slant height", f("sqrt(r^2+h^2)", &v), "l = √(r² + h²)", 1), prop("surface area", f("pi*r*(r+sqrt(r^2+h^2))", &v), "S = π r (r + l)", 2), prop("lateral area", f("pi*r*sqrt(r^2+h^2)", &v), "L = π r l", 2)])
        }
        "cube" => {
            let a = or_sym(dims.first().cloned(), "a");
            let v = [("a", &a)];
            (format!("cube | edge {}", ptext(&a)), vec![prop("volume", f("a^3", &v), "V = a³", 3), prop("surface area", f("6*a^2", &v), "S = 6 a²", 2), prop("diagonal", f("a*sqrt(3)", &v), "d = a √3", 1), prop("face diagonal", f("a*sqrt(2)", &v), "a √2", 1)])
        }
        "box" | "cuboid" | "rectangular prism" | "rectangular box" | "prism" => {
            let (a, b, c) = (or_sym(two(&dims, 0), "l"), or_sym(two(&dims, 1), "w"), or_sym(two(&dims, 2), "h"));
            let v = [("l", &a), ("w", &b), ("h", &c)];
            (format!("rectangular box | {} × {} × {}", ptext(&a), ptext(&b), ptext(&c)), vec![prop("volume", f("l*w*h", &v), "V = l w h", 3), prop("surface area", f("2*(l*w+l*h+w*h)", &v), "S = 2 (l w + l h + w h)", 2), prop("diagonal", f("sqrt(l^2+w^2+h^2)", &v), "d = √(l² + w² + h²)", 1)])
        }
        "square" => {
            let a = dims.first().cloned().or_else(|| p.get(&["area"]).map(|x| f("sqrt(x)", &[("x", &x)]))).or_else(|| p.get(&["perimeter"]).map(|x| f("x/4", &[("x", &x)]))).or_else(|| p.get(&["diagonal"]).map(|x| f("x/sqrt(2)", &[("x", &x)])));
            let a = or_sym(a, "a");
            let v = [("a", &a)];
            (format!("square | side {}", ptext(&a)), vec![prop("area", f("a^2", &v), "A = a²", 2), prop("perimeter", f("4*a", &v), "P = 4 a", 1), prop("diagonal", f("a*sqrt(2)", &v), "d = a √2", 1), prop("side", a.clone(), "a", 1)])
        }
        "rectangle" => {
            let (a, b) = (or_sym(two(&dims, 0), "l"), or_sym(two(&dims, 1), "w"));
            let v = [("l", &a), ("w", &b)];
            (format!("rectangle | {} × {}", ptext(&a), ptext(&b)), vec![prop("area", f("l*w", &v), "A = l w", 2), prop("perimeter", f("2*(l+w)", &v), "P = 2 (l + w)", 1), prop("diagonal", f("sqrt(l^2+w^2)", &v), "d = √(l² + w²)", 1)])
        }
        "right triangle" => {
            let legs = {
                let mut v = p.list(&["legs", "leg", "sides", "side", "base", "height", "a", "b"]);
                if v.is_empty() {
                    v = p.bare.clone();
                }
                v
            };
            if let (Some(c), Some(a)) = (p.get(&["hypotenuse"]), legs.first().cloned()) {
                let v = [("c", &c), ("a", &a)];
                let b = f("sqrt(c^2-a^2)", &v);
                (format!("right triangle | hypotenuse {} | leg {}", ptext(&c), ptext(&a)), vec![prop("leg", b.clone(), "b = √(c² − a²)", 1), prop("area", f("a*b/2", &[("a", &a), ("b", &b)]), "A = a b / 2", 2), prop("perimeter", f("a+b+c", &[("a", &a), ("b", &b), ("c", &c)]), "P = a + b + c", 1)])
            } else {
                let (a, b) = (or_sym(two(&legs, 0), "a"), or_sym(two(&legs, 1), "b"));
                let v = [("a", &a), ("b", &b)];
                (format!("right triangle | legs {} and {}", ptext(&a), ptext(&b)), vec![prop("hypotenuse", f("sqrt(a^2+b^2)", &v), "c = √(a² + b²)", 1), prop("area", f("a*b/2", &v), "A = a b / 2", 2), prop("perimeter", f("a+b+sqrt(a^2+b^2)", &v), "P = a + b + c", 1)])
            }
        }
        "equilateral triangle" => {
            let a = or_sym(dims.first().cloned(), "a");
            let v = [("a", &a)];
            (format!("equilateral triangle | side {}", ptext(&a)), vec![prop("area", f("sqrt(3)*a^2/4", &v), "A = √3 a² / 4", 2), prop("perimeter", f("3*a", &v), "P = 3 a", 1), prop("height", f("sqrt(3)*a/2", &v), "h = √3 a / 2", 1), prop("interior angle", int(60), "60°", 0)])
        }
        "triangle" => {
            let sides = {
                let mut v = p.list(&["sides", "side"]);
                if v.is_empty() && p.bare.len() == 3 {
                    v = p.bare.clone();
                }
                v
            };
            if sides.len() == 3 {
                let v = [("a", &sides[0]), ("b", &sides[1]), ("c", &sides[2])];
                let (fa, fb, fc) = (sides[0].to_f64_const()?, sides[1].to_f64_const()?, sides[2].to_f64_const()?);
                if fa + fb <= fc || fa + fc <= fb || fb + fc <= fa {
                    return None;
                }
                let ang = |x: f64, y: f64, z: f64| ((y * y + z * z - x * x) / (2.0 * y * z)).acos().to_degrees();
                let mut out = vec![
                    prop("area", f("sqrt((a+b+c)*(-a+b+c)*(a-b+c)*(a+b-c))/4", &v), "Heron's formula: A = √(s (s − a)(s − b)(s − c)), s = (a + b + c)/2", 2),
                    prop("perimeter", f("a+b+c", &v), "P = a + b + c", 1),
                    prop("angles (degrees)", list(vec![float(ang(fa, fb, fc)), float(ang(fb, fa, fc)), float(ang(fc, fa, fb))]), "law of cosines", 0),
                ];
                let mx = fa.max(fb).max(fc);
                let kind = if (fa * fa + fb * fb + fc * fc - 2.0 * mx * mx).abs() < 1e-9 * mx * mx { "right triangle" } else if fa * fa + fb * fb + fc * fc - 2.0 * mx * mx < 0.0 { "obtuse triangle" } else { "acute triangle" };
                let _ = kind;
                out.push(prop("inradius", f("sqrt((a+b+c)*(-a+b+c)*(a-b+c)*(a+b-c))/(2*(a+b+c))", &v), "r = A / s", 1));
                out.push(prop("circumradius", f("a*b*c/sqrt((a+b+c)*(-a+b+c)*(a-b+c)*(a+b-c))", &v), "R = a b c / (4 A)", 1));
                (format!("triangle | sides {}, {}, {} ({kind})", ptext(&sides[0]), ptext(&sides[1]), ptext(&sides[2])), out)
            } else {
                let b = or_sym(p.get(&["base", "b"]).or_else(|| two(&p.bare, 0)), "b");
                let h = or_sym(p.get(&["height", "altitude", "h"]).or_else(|| two(&p.bare, 1)), "h");
                let v = [("b", &b), ("h", &h)];
                (format!("triangle | base {} | height {}", ptext(&b), ptext(&h)), vec![prop("area", f("b*h/2", &v), "A = b h / 2", 2)])
            }
        }
        "ellipse" => {
            let ax = {
                let mut v = p.list(&["semi-axes", "axes", "semi-major axis", "semimajor axis", "semi-minor axis", "semiminor axis", "radii", "a", "b"]);
                if v.is_empty() {
                    v = p.bare.clone();
                }
                v
            };
            let (a, b) = (or_sym(two(&ax, 0), "a"), or_sym(two(&ax, 1), "b"));
            let v = [("a", &a), ("b", &b)];
            let mut out = vec![prop("area", f("pi*a*b", &v), "A = π a b", 2)];
            if let (Some(fa), Some(fb)) = (a.to_f64_const(), b.to_f64_const()) {
                let hh = ((fa - fb) / (fa + fb)).powi(2);
                out.push(prop("circumference", float(std::f64::consts::PI * (fa + fb) * (1.0 + 3.0 * hh / (10.0 + (4.0 - 3.0 * hh).sqrt()))), "Ramanujan's approximation", 1));
                let (mx, mn) = (fa.max(fb), fa.min(fb));
                out.push(prop("eccentricity", float((1.0 - mn * mn / (mx * mx)).sqrt()), "e = √(1 − b²/a²)", 0));
            }
            (format!("ellipse | semi-axes {} and {}", ptext(&a), ptext(&b)), out)
        }
        "trapezoid" | "trapezium" => {
            let bs = p.list(&["bases", "base", "sides", "a", "b"]);
            let (a, b) = (or_sym(two(&bs, 0).or_else(|| two(&p.bare, 0)), "a"), or_sym(two(&bs, 1).or_else(|| two(&p.bare, 1)), "b"));
            let h = or_sym(p.get(&["height", "h", "altitude"]).or_else(|| two(&p.bare, 2)), "h");
            let v = [("a", &a), ("b", &b), ("h", &h)];
            (format!("trapezoid | bases {} and {} | height {}", ptext(&a), ptext(&b), ptext(&h)), vec![prop("area", f("(a+b)*h/2", &v), "A = (a + b) h / 2", 2)])
        }
        "parallelogram" => {
            let b = or_sym(p.get(&["base", "b"]).or_else(|| two(&p.bare, 0)), "b");
            let h = or_sym(p.get(&["height", "h", "altitude"]).or_else(|| two(&p.bare, 1)), "h");
            (format!("parallelogram | base {} | height {}", ptext(&b), ptext(&h)), vec![prop("area", f("b*h", &[("b", &b), ("h", &h)]), "A = b h", 2)])
        }
        "rhombus" | "kite" => {
            let ds = {
                let mut v = p.list(&["diagonals", "diagonal"]);
                if v.is_empty() {
                    v = p.bare.clone();
                }
                v
            };
            let (a, b) = (or_sym(two(&ds, 0), "p"), or_sym(two(&ds, 1), "q"));
            (format!("{shape} | diagonals {} and {}", ptext(&a), ptext(&b)), vec![prop("area", f("p*q/2", &[("p", &a), ("q", &b)]), "A = p q / 2", 2)])
        }
        "pyramid" | "square pyramid" => {
            let a = or_sym(p.get(&["base", "side", "sides", "edge", "side length", "a"]).or_else(|| two(&p.bare, 0)), "a");
            let h = or_sym(p.get(&["height", "h"]).or_else(|| two(&p.bare, 1)), "h");
            let v = [("a", &a), ("h", &h)];
            (format!("square pyramid | base side {} | height {}", ptext(&a), ptext(&h)), vec![prop("volume", f("a^2*h/3", &v), "V = a² h / 3", 3), prop("slant height", f("sqrt(h^2+a^2/4)", &v), "l = √(h² + a²/4)", 1), prop("surface area", f("a^2+2*a*sqrt(h^2+a^2/4)", &v), "S = a² + 2 a l", 2)])
        }
        "torus" | "donut" | "doughnut" => {
            let big = or_sym(p.get(&["major radius"]).or_else(|| two(&p.list(&["radii", "radius"]), 0)).or_else(|| two(&p.bare, 0)), "R");
            let small = or_sym(p.get(&["minor radius"]).or_else(|| two(&p.list(&["radii", "radius"]), 1)).or_else(|| two(&p.bare, 1)), "r");
            let v = [("R", &big), ("r", &small)];
            (format!("torus | major radius {} | minor radius {}", ptext(&big), ptext(&small)), vec![prop("volume", f("2*pi^2*R*r^2", &v), "V = 2 π² R r²", 3), prop("surface area", f("4*pi^2*R*r", &v), "S = 4 π² R r", 2)])
        }
        "sector" => {
            let r = or_sym(p.get(&["radius", "r"]).or_else(|| two(&p.bare, 0)), "r");
            let t = or_sym(p.get(&["angle"]).or_else(|| two(&p.bare, 1)), "θ");
            let v = [("r", &r), ("t", &t)];
            (format!("circular sector | radius {} | angle {}°", ptext(&r), ptext(&t)), vec![prop("area", f("t/360*pi*r^2", &v), "A = (θ/360°) π r²", 2), prop("arc length", f("t/360*2*pi*r", &v), "L = (θ/360°) 2 π r", 1)])
        }
        _ => {
            // regular polygons
            let n = POLYGONS.iter().find(|(name, _)| *name == shape).map(|(_, n)| *n)?;
            let nn = int(n);
            let a = or_sym(dims.first().cloned(), "a");
            let v = [("n", &nn), ("a", &a)];
            let mut out = vec![
                prop("sum of interior angles", f("(n-2)*180", &v), "(n − 2) · 180°", 0),
                prop("interior angle", f("(n-2)*180/n", &v), "each angle of a regular polygon: (n − 2) · 180° / n", 0),
                prop("exterior angle", f("360/n", &v), "360° / n", 0),
                prop("number of diagonals", f("n*(n-3)/2", &v), "n (n − 3) / 2", 0),
                prop("perimeter", f("n*a", &v), "P = n a", 1),
            ];
            let area = a.to_f64_const().map(|fa| float(n as f64 * fa * fa / (4.0 * (std::f64::consts::PI / n as f64).tan()))).unwrap_or_else(|| mul(vec![float(n as f64 / (4.0 * (std::f64::consts::PI / n as f64).tan())), square(a.clone())]));
            out.push(prop("area", if n == 6 { f("3*sqrt(3)*a^2/2", &v) } else { area }, "A = n a² / (4 tan(π/n))", 2));
            (format!("regular {shape} ({n} sides){}", if a.free_symbols().is_empty() { format!(" | side {}", ptext(&a)) } else { String::new() }), out)
        }
    })
}

fn geometry(l: &str) -> Option<Answer> {
    static SHAPE: OnceLock<Regex> = OnceLock::new();
    let shape_re = SHAPE.get_or_init(|| {
        let polys: Vec<&str> = POLYGONS.iter().map(|(n, _)| *n).collect();
        Regex::new(&format!(r"\b(right(?:[- ]angled?)? triangle|equilateral triangle|rectangular (?:prism|box)|square pyramid|semicircle|hemisphere|circle|disk|disc|sphere|ball|cylinder|cone|cube|cuboid|box|prism|square|rectangle|triangle|ellipse|trapezoid|trapezium|parallelogram|rhombus|kite|pyramid|torus|donut|doughnut|sector|{})\b", polys.join("|"))).unwrap()
    });
    let prop_re = re!(r"\b(surface area|lateral (?:surface )?area|base area|area|volume|perimeter|circumference|face diagonal|diagonals?|hypotenuse|slant height|sum of (?:the )?(?:interior |internal )?angles|interior angles?|internal angles?|exterior angles?|number of diagonals|diameter|radius|height|inradius|circumradius|arc length|side|leg|eccentricity)\b");
    let sm = shape_re.find(l)?;
    let mut shape = sm.as_str().replace("right-angled", "right").replace("right angled", "right").replace("right-angle", "right").replace("right angle ", "right ");
    if shape == "rectangular box" {
        shape = "box".into();
    }
    // reject sentences that merely mention a shape
    if l.split_whitespace().count() > 18 || l.contains("plot") || l.contains("equation") || l.contains("integra") || l.contains("matrix") {
        return None;
    }
    let pm = prop_re.find(l);
    // a property must come before the shape, or the query is just the shape
    let rest_ok = pm.map_or(l.trim() == sm.as_str() || l.trim_start_matches("a ").trim_start_matches("an ") == sm.as_str(), |m| m.start() < sm.start() || l[sm.end()..].contains(m.as_str()));
    if !rest_ok {
        return None;
    }
    // remove the property keyword so "area" is not read as a given parameter
    let given_part = match pm {
        Some(m) if m.start() < sm.start() => &l[sm.end()..],
        _ => l,
    };
    let p = params(given_part);
    let (label, props) = shape_props(&shape, &p)?;
    let want = pm.map(|m| m.as_str().to_string()).map(|w| {
        let w = w.replace("internal", "interior").replace("lateral surface area", "lateral area");
        if w.starts_with("sum of") {
            "sum of interior angles".to_string()
        } else if w == "interior angles" {
            if l.contains("sum") { "sum of interior angles".to_string() } else { "interior angle".to_string() }
        } else if w == "exterior angles" {
            "exterior angle".to_string()
        } else if w == "diagonals" {
            if props.iter().any(|q| q.name == "number of diagonals") { "number of diagonals".to_string() } else { "diagonal".to_string() }
        } else if w == "circumference" && !props.iter().any(|q| q.name == "circumference") {
            "perimeter".to_string()
        } else if w == "perimeter" && !props.iter().any(|q| q.name == "perimeter") {
            "circumference".to_string()
        } else if w == "side" && !props.iter().any(|q| q.name == "side") {
            "leg".to_string()
        } else {
            w
        }
    });
    let unit = p.unit.clone();
    let with_unit = |q: &Prop| -> String {
        let val = ptext(&q.value);
        match (q.dim, unit.is_empty()) {
            (0, _) => {
                if q.name.contains("angle") && !q.name.contains("number") {
                    match &q.value {
                        Expr::List(v) => v.iter().map(|a| format!("{}°", fmt(a.to_f64_const().unwrap_or(0.0)))).collect::<Vec<_>>().join(", "),
                        e => format!("{}°", ptext(e)),
                    }
                } else {
                    val
                }
            }
            (_, true) => val,
            (1, false) => format!("{val} {unit}"),
            (2, false) => format!("{val} {unit}²"),
            _ => format!("{val} {unit}³"),
        }
    };
    let mut pods = vec![pod("Input", "Input interpretation", text(match &want {
        Some(w) => format!("{label} | {w}"),
        None => label.clone(),
    }))];
    let chosen = match &want {
        Some(w) => Some(props.iter().find(|q| q.name == w.as_str() || (w == "interior angle" && q.name == "interior angle"))?),
        None => None,
    };
    if let Some(q) = chosen {
        let mut sub = vec![(String::new(), if q.dim == 0 || !unit.is_empty() { text(with_unit(q)) } else { math(&q.value) })];
        if q.value.free_symbols().is_empty() && !q.value.is_integer() && !q.value.is_list() && !q.value.has_float() {
            if let Some(d) = q.value.to_f64_const() {
                sub.push(("decimal approximation".into(), text(format!("≈ {}", fmt(d)))));
            }
        }
        let mut pd = pod_multi("Result", &capitalize(q.name), sub);
        pd.primary = true;
        pods.push(pd);
        pods.push(pod("Formula", "Formula", text(q.formula)));
    }
    let others: Vec<Vec<Content>> = props.iter().filter(|q| chosen.map_or(true, |c| c.name != q.name)).map(|q| {
        let dec = if q.value.free_symbols().is_empty() && !q.value.is_integer() && !q.value.is_list() && !q.value.has_float() { q.value.to_f64_const().map(|d| format!("  ≈ {}", fmt(d))).unwrap_or_default() } else { String::new() };
        vec![text(q.name), text(format!("{}{dec}", with_unit(q)))]
    }).collect();
    if !others.is_empty() {
        let mut pd = pod("Properties", if chosen.is_some() { "Other properties" } else { "Properties" }, table(None, others));
        pd.primary = chosen.is_none();
        pods.push(pd);
    }
    Some(Answer::new(label, pods))
}

fn capitalize(t: &str) -> String {
    let mut c = t.chars();
    match c.next() {
        Some(first) => first.to_uppercase().collect::<String>() + c.as_str(),
        None => String::new(),
    }
}

// ---------------------------------------------------------------------------
// triangles from sides and angles
// ---------------------------------------------------------------------------

fn triangle(q: &Query) -> Option<Answer> {
    let l = q.lower.as_str();
    if !(l.contains("triangle") || l.contains("law of cosines") || l.contains("law of sines") || l.contains("cosine rule") || l.contains("sine rule")) {
        return None;
    }
    let mut side = [None::<f64>; 3];
    let mut ang = [None::<f64>; 3];
    for m in re!(r"\b(?:(side|angle)\s+)?([abcABC])\s*=\s*([\d.]+)\s*(°|degrees?|deg|radians?|rad)?").captures_iter(&q.clean) {
        let name = m[2].chars().next()?;
        let k = (name.to_ascii_lowercase() as u8 - b'a') as usize;
        let v: f64 = m[3].parse().ok()?;
        let unit = m.get(4).map(|u| u.as_str().to_lowercase()).unwrap_or_default();
        let is_angle = m.get(1).map_or(name.is_ascii_uppercase() || !unit.is_empty(), |w| w.as_str().eq_ignore_ascii_case("angle"));
        if is_angle {
            ang[k] = Some(if unit.starts_with("rad") { v.to_degrees() } else { v });
        } else {
            side[k] = Some(v);
        }
    }
    let given = side.iter().flatten().count() + ang.iter().flatten().count();
    if given < 3 || side.iter().flatten().count() == 0 {
        return None;
    }
    let given_text = (0..3).flat_map(|k| [side[k].map(|v| format!("{} = {}", (b'a' + k as u8) as char, fmt(v))), ang[k].map(|v| format!("{} = {}°", (b'A' + k as u8) as char, fmt(v)))]).flatten().collect::<Vec<_>>().join(", ");
    let mut method = "";
    // fill the third angle when two are known
    let fill_angle = |ang: &mut [Option<f64>; 3]| {
        if ang.iter().flatten().count() == 2 {
            let sum: f64 = ang.iter().flatten().sum();
            for a in ang.iter_mut() {
                if a.is_none() {
                    *a = Some(180.0 - sum);
                }
            }
        }
    };
    for _ in 0..4 {
        fill_angle(&mut ang);
        let ns = side.iter().flatten().count();
        if ns == 3 {
            let (a, b, c) = (side[0]?, side[1]?, side[2]?);
            if a + b <= c || a + c <= b || b + c <= a {
                return Some(Answer::new("triangle", vec![pod("Input", "Input interpretation", text(format!("triangle with {given_text}"))), primary(pod("Result", "Result", text("No such triangle exists: the sides violate the triangle inequality.")))]));
            }
            if ang.iter().any(|x| x.is_none()) {
                if method.is_empty() {
                    method = "law of cosines: cos A = (b² + c² − a²) / (2 b c)";
                }
                ang[0] = Some(((b * b + c * c - a * a) / (2.0 * b * c)).acos().to_degrees());
                ang[1] = Some(((a * a + c * c - b * b) / (2.0 * a * c)).acos().to_degrees());
                ang[2] = Some(180.0 - ang[0]? - ang[1]?);
            }
            break;
        }
        // SAS: two sides and the included angle
        let mut progressed = false;
        for k in 0..3 {
            let (i, j) = ((k + 1) % 3, (k + 2) % 3);
            if let (None, Some(x), Some(y), Some(t)) = (side[k], side[i], side[j], ang[k]) {
                side[k] = Some((x * x + y * y - 2.0 * x * y * t.to_radians().cos()).sqrt());
                method = "law of cosines: c² = a² + b² − 2 a b cos C";
                progressed = true;
            }
        }
        if progressed {
            continue;
        }
        // a side with its opposite angle: law of sines
        if let Some(k) = (0..3).find(|k| side[*k].is_some() && ang[*k].is_some()) {
            let ratio = side[k]? / ang[k]?.to_radians().sin();
            for j in 0..3 {
                match (side[j], ang[j]) {
                    (None, Some(t)) => {
                        side[j] = Some(ratio * t.to_radians().sin());
                        progressed = true;
                    }
                    (Some(x), None) => {
                        let sv = x / ratio;
                        if sv > 1.0 + 1e-12 {
                            return Some(Answer::new("triangle", vec![pod("Input", "Input interpretation", text(format!("triangle with {given_text}"))), primary(pod("Result", "Result", text("No such triangle exists: the law of sines would need a sine greater than 1.")))]));
                        }
                        ang[j] = Some(sv.min(1.0).asin().to_degrees());
                        progressed = true;
                    }
                    _ => {}
                }
            }
            if method.is_empty() {
                method = "law of sines: a / sin A = b / sin B = c / sin C";
            }
        }
        if !progressed {
            break;
        }
    }
    let (a, b, c) = (side[0]?, side[1]?, side[2]?);
    let (aa, bb, cc) = (ang[0]?, ang[1]?, ang[2]?);
    let sp = (a + b + c) / 2.0;
    let area = (sp * (sp - a) * (sp - b) * (sp - c)).max(0.0).sqrt();
    let rows = vec![
        vec![text("a"), text(fmt(a)), text("A"), text(format!("{}°", fmt(aa)))],
        vec![text("b"), text(fmt(b)), text("B"), text(format!("{}°", fmt(bb)))],
        vec![text("c"), text(fmt(c)), text("C"), text(format!("{}°", fmt(cc)))],
    ];
    let mx = aa.max(bb).max(cc);
    let kind = if (mx - 90.0).abs() < 1e-7 { "right" } else if mx > 90.0 { "obtuse" } else { "acute" };
    let pods = vec![
        pod("Input", "Input interpretation", text(format!("solve the triangle with {given_text}"))),
        primary(pod("Result", "Sides and angles", table(Some(vec!["side", "length", "angle", "measure"]), rows))),
        pod("Method", "Method", text(method)),
        pod("Properties", "Properties", table(None, vec![vec![text("area"), text(fmt(area))], vec![text("perimeter"), text(fmt(a + b + c))], vec![text("type"), text(format!("{kind} triangle"))], vec![text("inradius"), text(fmt(area / sp))], vec![text("circumradius"), text(fmt(a * b * c / (4.0 * area)))]])),
    ];
    Some(Answer::new("triangle solution", pods))
}

// ---------------------------------------------------------------------------
// angle conversion
// ---------------------------------------------------------------------------

fn angles(l: &str) -> Option<Answer> {
    if let Some(m) = re!(r"^(?:convert\s+)?(.+?)\s*(?:radians?|rad)?\s+(?:in|to|into|as)\s+(?:degrees?|deg|°)$").captures(l) {
        let e = ex(&m[1])?;
        if !e.free_symbols().is_empty() || m[1].contains("degree") || m[1].contains('°') {
            return None;
        }
        // leave unit conversions like "100 fahrenheit" to the units handler
        if units::parse_quantity(&m[1]).is_some() && !l.contains("rad") {
            return None;
        }
        let deg = s::simplify(&div(mul(vec![e.clone(), int(180)]), pi()));
        let v = deg.to_f64_const()?;
        let mut sub = vec![(String::new(), text(format!("{}°", if deg.has_const(K::Pi) || deg.has(&|n| matches!(n, Expr::Func(..))) { fmt(v) } else { ptext(&deg) })))];
        if deg.is_integer() || deg.as_rat().is_some() {
            let whole = v.abs().floor();
            let minutes = (v.abs() - whole) * 60.0;
            if minutes > 1e-9 {
                sub.push(("degrees, minutes, seconds".into(), text(format!("{}{}° {}′ {}″", if v < 0.0 { "-" } else { "" }, whole, minutes.floor(), fmt((minutes - minutes.floor()) * 60.0)))));
            }
        }
        let mut p = pod_multi("Result", "Result", sub);
        p.primary = true;
        let pods = vec![pod("Input", "Input interpretation", text(format!("convert {} radians to degrees", ptext(&e)))), p, pod("Formula", "Formula", text("degrees = radians × 180/π")), pod("Turns", "Fraction of a full turn", text(fmt(v / 360.0)))];
        return Some(Answer::new(format!("{} radians in degrees", ptext(&e)), pods));
    }
    None
}

// ---------------------------------------------------------------------------
// physics
// ---------------------------------------------------------------------------

const fn dm(l: i8, m: i8, t: i8, i: i8, k: i8) -> Dim {
    [l, m, t, i, k, 0, 0, 0]
}
const LEN: Dim = dm(1, 0, 0, 0, 0);
const MASS: Dim = dm(0, 1, 0, 0, 0);
const TIME: Dim = dm(0, 0, 1, 0, 0);
const SPEED: Dim = dm(1, 0, -1, 0, 0);
const ACC: Dim = dm(1, 0, -2, 0, 0);
const FORCE: Dim = dm(1, 1, -2, 0, 0);
const ENERGY: Dim = dm(2, 1, -2, 0, 0);
const POWER: Dim = dm(2, 1, -3, 0, 0);
const FREQ: Dim = dm(0, 0, -1, 0, 0);
const AREA: Dim = dm(2, 0, 0, 0, 0);
const VOLUME: Dim = dm(3, 0, 0, 0, 0);
const CURRENT: Dim = dm(0, 0, 0, 1, 0);
const VOLT: Dim = dm(2, 1, -3, -1, 0);
const OHM: Dim = dm(2, 1, -3, -2, 0);
const CHARGE: Dim = dm(0, 0, 1, 1, 0);
const TEMP: Dim = dm(0, 0, 0, 0, 1);

const G: f64 = 9.80665;
const C: f64 = 299_792_458.0;
const H: f64 = 6.626_070_15e-34;
const BIG_G: f64 = 6.674_30e-11;
const KE: f64 = 8.987_551_792_3e9;
const EV: f64 = 1.602_176_634e-19;

/// Quantities with units found in the text, in SI.
fn quantities(c: &str) -> Vec<(f64, Dim, String)> {
    let mut out = Vec::new();
    for m in re!(r"(-?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?)\s*([a-zA-Zµμ°Ω][a-zA-Z0-9µμ°Ω/^*·²³]*(?:\s+per\s+[a-zA-Z]+(?:\s+squared)?)?)").captures_iter(c) {
        let Ok(v) = m[1].parse::<f64>() else { continue };
        let raw = m[2].trim_end_matches(['.', ',']);
        // try the longest unit text first ("m/s^2"), then its first word
        let cands = [raw.to_string(), raw.split_whitespace().next().unwrap_or("").to_string()];
        for cand in cands {
            if cand.is_empty() || matches!(cand.to_lowercase().as_str(), "at" | "a" | "in" | "and" | "to" | "of" | "with" | "from" | "for" | "is" | "by" | "on" | "c") {
                continue;
            }
            if let Some(u) = units::parse_unit(&cand) {
                out.push((units::to_si(v, &u), u.dim, format!("{} {}", m[1].to_string(), cand)));
                break;
            }
        }
    }
    out
}

struct Out {
    name: &'static str,
    value: f64,
    unit: &'static str,
}

fn out(name: &'static str, value: f64, unit: &'static str) -> Out {
    Out { name, value, unit }
}

fn take(qs: &[(f64, Dim, String)], dim: Dim, nth: usize) -> Option<f64> {
    qs.iter().filter(|(_, d, _)| *d == dim).nth(nth).map(|(v, _, _)| *v)
}

fn physics(q: &Query) -> Option<Answer> {
    let l = q.lower.as_str();
    let qs = quantities(&q.clean);
    let t = |dim: Dim, nth: usize| take(&qs, dim, nth);
    let has = |words: &[&str]| words.iter().any(|w| l.contains(w));
    let body = super::astro::find_body(l);
    let (title, formula, outs): (&str, &str, Vec<Out>) = if has(&["kinetic energy"]) {
        let (m, v) = (t(MASS, 0)?, t(SPEED, 0)?);
        ("kinetic energy", "E = ½ m v²", vec![out("kinetic energy", 0.5 * m * v * v, "J"), out("momentum", m * v, "kg m/s")])
    } else if has(&["potential energy"]) {
        let (m, h) = (t(MASS, 0)?, t(LEN, 0)?);
        ("gravitational potential energy", "E = m g h, with g = 9.80665 m/s²", vec![out("potential energy", m * G * h, "J")])
    } else if has(&["momentum"]) && t(MASS, 0).is_some() {
        let (m, v) = (t(MASS, 0)?, t(SPEED, 0)?);
        ("momentum", "p = m v", vec![out("momentum", m * v, "kg m/s"), out("kinetic energy", 0.5 * m * v * v, "J")])
    } else if has(&["photon", "quantum of light"]) {
        if let Some(lam) = t(LEN, 0) {
            ("photon energy", "E = h c / λ", vec![out("energy", H * C / lam, "J"), out("energy", H * C / lam / EV, "eV"), out("frequency", C / lam, "Hz")])
        } else if let Some(fr) = t(FREQ, 0) {
            ("photon energy", "E = h f", vec![out("energy", H * fr, "J"), out("energy", H * fr / EV, "eV"), out("wavelength", C / fr, "m")])
        } else {
            let e = t(ENERGY, 0)?;
            ("photon wavelength", "λ = h c / E", vec![out("wavelength", H * C / e, "m"), out("frequency", e / H, "Hz")])
        }
    } else if has(&["de broglie"]) {
        let (m, v) = (t(MASS, 0)?, t(SPEED, 0)?);
        ("de Broglie wavelength", "λ = h / (m v)", vec![out("wavelength", H / (m * v), "m")])
    } else if has(&["wavelength"]) && t(FREQ, 0).is_some() {
        let fr = t(FREQ, 0)?;
        ("wavelength of an electromagnetic wave", "λ = c / f", vec![out("wavelength", C / fr, "m"), out("period", 1.0 / fr, "s"), out("photon energy", H * fr / EV, "eV")])
    } else if has(&["frequency"]) && t(LEN, 0).is_some() && !has(&["pendulum"]) {
        let lam = t(LEN, 0)?;
        ("frequency of an electromagnetic wave", "f = c / λ", vec![out("frequency", C / lam, "Hz"), out("photon energy", H * C / lam / EV, "eV")])
    } else if has(&["mc^2", "mc²", "mass energy", "mass-energy", "rest energy", "energy equivalent"]) {
        let m = t(MASS, 0)?;
        ("mass–energy equivalence", "E = m c²", vec![out("energy", m * C * C, "J"), out("energy", m * C * C / 3.6e6, "kWh"), out("TNT equivalent", m * C * C / 4.184e15, "megatons")])
    } else if has(&["free fall", "freefall", "dropped from", "fall from", "falling from", "falls from", "drop from"]) {
        if let Some(h) = t(LEN, 0) {
            ("free fall from rest (no air resistance)", "t = √(2 h / g),  v = √(2 g h)", vec![out("time to fall", (2.0 * h / G).sqrt(), "s"), out("impact speed", (2.0 * G * h).sqrt(), "m/s"), out("impact speed", (2.0 * G * h).sqrt() * 3.6, "km/h")])
        } else {
            let tm = t(TIME, 0)?;
            ("free fall from rest (no air resistance)", "d = ½ g t²,  v = g t", vec![out("distance fallen", 0.5 * G * tm * tm, "m"), out("speed", G * tm, "m/s")])
        }
    } else if has(&["pendulum"]) {
        let len = t(LEN, 0)?;
        ("simple pendulum (small swings)", "T = 2π √(L / g)", vec![out("period", 2.0 * std::f64::consts::PI * (len / G).sqrt(), "s"), out("frequency", 1.0 / (2.0 * std::f64::consts::PI * (len / G).sqrt()), "Hz")])
    } else if has(&["ohm's law", "ohms law", "ohm law"]) || (has(&["voltage", "current", "resistance"]) && qs.iter().filter(|(_, d, _)| [VOLT, OHM, CURRENT].contains(d)).count() == 2) {
        match (t(VOLT, 0), t(CURRENT, 0), t(OHM, 0)) {
            (Some(v), None, Some(r)) => ("Ohm's law", "I = V / R", vec![out("current", v / r, "A"), out("power", v * v / r, "W")]),
            (Some(v), Some(i), None) => ("Ohm's law", "R = V / I", vec![out("resistance", v / i, "Ω"), out("power", v * i, "W")]),
            (None, Some(i), Some(r)) => ("Ohm's law", "V = I R", vec![out("voltage", i * r, "V"), out("power", i * i * r, "W")]),
            _ => return None,
        }
    } else if has(&["escape velocity", "escape speed"]) {
        let (m, r) = match body {
            Some(b) => (b.mass, b.radius),
            None => (t(MASS, 0)?, t(LEN, 0)?),
        };
        let v = (2.0 * BIG_G * m / r).sqrt();
        ("escape velocity", "v = √(2 G M / R)", vec![out("escape velocity", v, "m/s"), out("escape velocity", v / 1000.0, "km/s"), out("escape velocity", v * 3.6, "km/h")])
    } else if has(&["schwarzschild"]) {
        let m = match body {
            Some(b) => b.mass,
            None => t(MASS, 0)?,
        };
        ("Schwarzschild radius", "r = 2 G M / c²", vec![out("Schwarzschild radius", 2.0 * BIG_G * m / (C * C), "m")])
    } else if has(&["gravitational force", "force of gravity between", "gravitational attraction", "gravity between"]) {
        let pair = super::astro::find_two_bodies(l);
        let (m1, m2, r) = match pair {
            Some((a, b)) => (a.mass, b.mass, super::astro::separation(a, b)?),
            None => (t(MASS, 0)?, t(MASS, 1)?, t(LEN, 0)?),
        };
        ("Newton's law of gravitation", "F = G m₁ m₂ / r²", vec![out("force", BIG_G * m1 * m2 / (r * r), "N")])
    } else if has(&["coulomb", "electric force", "electrostatic force"]) {
        let (q1, q2, r) = (t(CHARGE, 0)?, t(CHARGE, 1)?, t(LEN, 0)?);
        ("Coulomb's law", "F = k q₁ q₂ / r²", vec![out("force", KE * q1 * q2 / (r * r), "N")])
    } else if has(&["centripetal force"]) {
        let (m, v, r) = (t(MASS, 0)?, t(SPEED, 0)?, t(LEN, 0)?);
        ("centripetal force", "F = m v² / r", vec![out("force", m * v * v / r, "N"), out("acceleration", v * v / r, "m/s²")])
    } else if has(&["centripetal acceleration"]) {
        let (v, r) = (t(SPEED, 0)?, t(LEN, 0)?);
        ("centripetal acceleration", "a = v² / r", vec![out("acceleration", v * v / r, "m/s²"), out("acceleration", v * v / r / G, "g")])
    } else if has(&["weight of", "weight on", "weigh on"]) && t(MASS, 0).is_some() {
        let m = t(MASS, 0)?;
        let (gv, where_) = match body {
            Some(b) => (BIG_G * b.mass / (b.radius * b.radius), b.name),
            None => (G, "earth"),
        };
        let _ = where_;
        let pounds = qs.iter().find(|(_, d, _)| *d == MASS).is_some_and(|(_, _, shown)| shown.to_lowercase().contains("lb") || shown.to_lowercase().contains("pound"));
        let mut v = vec![out("weight", m * gv, "N"), out("weight", m * gv / 4.448_221_615_260_5, "lbf"), out("scale reading (scale calibrated on Earth)", m * gv / G, "kg"), out("scale reading (scale calibrated on Earth)", m * gv / G / 0.453_592_37, "lb"), out("local gravity", gv, "m/s²"), out("local gravity", gv / G, "g")];
        if pounds {
            v.swap(0, 1);
        }
        ("weight", "W = m g", v)
    } else if has(&["force"]) && t(MASS, 0).is_some() && t(ACC, 0).is_some() {
        ("Newton's second law", "F = m a", vec![out("force", t(MASS, 0)? * t(ACC, 0)?, "N")])
    } else if has(&["acceleration"]) && t(FORCE, 0).is_some() && t(MASS, 0).is_some() {
        ("Newton's second law", "a = F / m", vec![out("acceleration", t(FORCE, 0)? / t(MASS, 0)?, "m/s²")])
    } else if has(&["acceleration"]) && t(SPEED, 0).is_some() && t(TIME, 0).is_some() {
        let dv = t(SPEED, 1).map_or(t(SPEED, 0)?, |v2| v2 - t(SPEED, 0).unwrap_or(0.0));
        ("average acceleration", "a = Δv / t", vec![out("acceleration", dv / t(TIME, 0)?, "m/s²"), out("acceleration", dv / t(TIME, 0)? / G, "g")])
    } else if has(&["density"]) && t(MASS, 0).is_some() && t(VOLUME, 0).is_some() {
        let d = t(MASS, 0)? / t(VOLUME, 0)?;
        ("density", "ρ = m / V", vec![out("density", d, "kg/m³"), out("density", d / 1000.0, "g/cm³")])
    } else if has(&["pressure"]) && t(FORCE, 0).is_some() && t(AREA, 0).is_some() {
        let p = t(FORCE, 0)? / t(AREA, 0)?;
        ("pressure", "P = F / A", vec![out("pressure", p, "Pa"), out("pressure", p / 101_325.0, "atm")])
    } else if has(&["work done", "work "]) && t(FORCE, 0).is_some() && t(LEN, 0).is_some() {
        ("work", "W = F d", vec![out("work", t(FORCE, 0)? * t(LEN, 0)?, "J")])
    } else if has(&["power"]) && t(ENERGY, 0).is_some() && t(TIME, 0).is_some() {
        ("power", "P = E / t", vec![out("power", t(ENERGY, 0)? / t(TIME, 0)?, "W")])
    } else if has(&["power"]) && t(VOLT, 0).is_some() && t(CURRENT, 0).is_some() {
        ("electric power", "P = V I", vec![out("power", t(VOLT, 0)? * t(CURRENT, 0)?, "W")])
    } else if has(&["energy"]) && t(POWER, 0).is_some() && t(TIME, 0).is_some() {
        let e = t(POWER, 0)? * t(TIME, 0)?;
        ("energy", "E = P t", vec![out("energy", e, "J"), out("energy", e / 3.6e6, "kWh")])
    } else if has(&["average speed", "speed of", "speed if", "how fast", "velocity of"]) && t(LEN, 0).is_some() && t(TIME, 0).is_some() {
        let v = t(LEN, 0)? / t(TIME, 0)?;
        ("average speed", "v = d / t", vec![out("speed", v, "m/s"), out("speed", v * 3.6, "km/h"), out("speed", v / 0.44704, "mph")])
    } else if has(&["how long", "travel time", "time to travel", "time to drive", "time to go", "time to walk", "time to cover"]) && t(LEN, 0).is_some() && t(SPEED, 0).is_some() {
        let tm = t(LEN, 0)? / t(SPEED, 0)?;
        ("travel time", "t = d / v", vec![out("time", tm, "s"), out("time", tm / 60.0, "minutes"), out("time", tm / 3600.0, "hours")])
    } else if has(&["how far", "distance travel", "distance cover"]) && t(SPEED, 0).is_some() && t(TIME, 0).is_some() {
        let d = t(SPEED, 0)? * t(TIME, 0)?;
        ("distance travelled", "d = v t", vec![out("distance", d, "m"), out("distance", d / 1000.0, "km"), out("distance", d / 1609.344, "miles")])
    } else if has(&["lorentz factor", "time dilation", "gamma factor"]) {
        let beta = re!(r"([\d.]+)\s*c\b").captures(l).and_then(|m| m[1].parse::<f64>().ok()).or_else(|| t(SPEED, 0).map(|v| v / C))?;
        if !(0.0..1.0).contains(&beta) {
            return None;
        }
        let gamma = 1.0 / (1.0 - beta * beta).sqrt();
        ("special relativity", "γ = 1 / √(1 − v²/c²)", vec![out("Lorentz factor γ", gamma, ""), out("one second on board lasts, for a stationary observer,", gamma, "s"), out("length contraction factor", 1.0 / gamma, "")])
    } else if has(&["ideal gas", "pv = nrt", "pv=nrt"]) && t(TEMP, 0).is_some() && t(VOLUME, 0).is_some() {
        let n = re!(r"([\d.]+)\s*mol").captures(l).and_then(|m| m[1].parse::<f64>().ok()).unwrap_or(1.0);
        let p = n * 8.314_462_618 * t(TEMP, 0)? / t(VOLUME, 0)?;
        ("ideal gas law", "P = n R T / V", vec![out("pressure", p, "Pa"), out("pressure", p / 101_325.0, "atm")])
    } else {
        return None;
    };
    let given = if let (true, Some((a, b))) = (title.starts_with("Newton's law of grav"), super::astro::find_two_bodies(l)) {
        format!("{} and {}", a.name, b.name)
    } else if qs.is_empty() {
        body.map(|b| b.name.to_string()).unwrap_or_default()
    } else {
        let mut gv = qs.iter().map(|(_, _, shown)| shown.clone()).collect::<Vec<_>>().join(", ");
        if let Some(b) = body {
            gv = format!("{gv}, {}", b.name);
        }
        gv
    };
    let mut pods = vec![pod("Input", "Input interpretation", text(format!("{title} | {given}")))];
    let first = &outs[0];
    let show = |o: &Out| format!("{}{}", sci(o.value), if o.unit.is_empty() { String::new() } else { format!(" {}", o.unit) });
    pods.push(primary(pod("Result", &capitalize(first.name), text(show(first)))));
    pods.push(pod("Formula", "Formula", text(formula)));
    if outs.len() > 1 {
        pods.push(pod("Related", "Related quantities", table(None, outs[1..].iter().map(|o| vec![text(o.name), text(show(o))]).collect())));
    }
    if let Some(u) = units::parse_unit(first.unit) {
        if let Some(tb) = units::conversion_table(units::to_si(first.value, &u), &u.dim, &u.name) {
            pods.push(tb);
        }
    }
    Some(Answer::new(title, pods))
}

// ---------------------------------------------------------------------------
// named formulas
// ---------------------------------------------------------------------------

const NAMED: &[(&[&str], &str, &str, &str)] = &[
    (&["quadratic formula", "quadratic equation formula"], "Quadratic formula", "x = (−b ± √(b² − 4 a c)) / (2 a)", "Solutions of a x² + b x + c = 0 (a ≠ 0). The discriminant b² − 4ac decides whether there are two real roots, one double root or two complex roots."),
    (&["pythagorean theorem", "pythagoras theorem", "pythagoras' theorem", "pythagoras"], "Pythagorean theorem", "a² + b² = c²", "In a right triangle the square on the hypotenuse c equals the sum of the squares on the legs a and b."),
    (&["law of cosines", "cosine rule", "cosine law"], "Law of cosines", "c² = a² + b² − 2 a b cos C", "Relates the three sides of any triangle to one of its angles; with C = 90° it reduces to the Pythagorean theorem."),
    (&["law of sines", "sine rule", "sine law"], "Law of sines", "a / sin A = b / sin B = c / sin C = 2R", "In any triangle the ratio of a side to the sine of its opposite angle is constant and equals the circumdiameter."),
    (&["heron's formula", "herons formula", "heron formula"], "Heron's formula", "A = √(s (s − a)(s − b)(s − c)),  s = (a + b + c) / 2", "Area of a triangle from its three sides."),
    (&["euler's formula", "eulers formula", "euler formula"], "Euler's formula", "e^(i x) = cos x + i sin x", "Links the complex exponential with trigonometry. For polyhedra, Euler's formula is V − E + F = 2."),
    (&["euler's identity", "eulers identity", "euler identity"], "Euler's identity", "e^(i π) + 1 = 0", "The special case x = π of Euler's formula, joining e, i, π, 1 and 0."),
    (&["binomial theorem", "binomial formula", "binomial expansion"], "Binomial theorem", "(a + b)^n = Σ_(k=0)^n C(n, k) a^(n−k) b^k", "C(n, k) = n! / (k! (n − k)!) are the binomial coefficients."),
    (&["distance formula"], "Distance formula", "d = √((x₂ − x₁)² + (y₂ − y₁)²)", "Distance between the points (x₁, y₁) and (x₂, y₂)."),
    (&["midpoint formula"], "Midpoint formula", "M = ((x₁ + x₂)/2, (y₁ + y₂)/2)", "Midpoint of the segment joining (x₁, y₁) and (x₂, y₂)."),
    (&["slope formula"], "Slope formula", "m = (y₂ − y₁) / (x₂ − x₁)", "Slope of the line through (x₁, y₁) and (x₂, y₂)."),
    (&["compound interest formula"], "Compound interest", "A = P (1 + r/n)^(n t)", "P principal, r annual rate, n compounding periods per year, t years. Continuous compounding: A = P e^(r t)."),
    (&["simple interest formula"], "Simple interest", "I = P r t", "P principal, r annual rate, t years."),
    (&["ideal gas law", "ideal gas equation"], "Ideal gas law", "P V = n R T", "R = 8.314462618 J/(mol K)."),
    (&["newton's second law", "newtons second law", "second law of motion"], "Newton's second law", "F = m a", "The net force on a body equals its mass times its acceleration."),
    (&["newton's law of gravitation", "law of universal gravitation", "law of gravitation", "newton's law of universal gravitation"], "Newton's law of universal gravitation", "F = G m₁ m₂ / r²", "G = 6.6743×10⁻¹¹ m³/(kg s²)."),
    (&["coulomb's law", "coulombs law"], "Coulomb's law", "F = k q₁ q₂ / r²", "k = 8.9875517923×10⁹ N m²/C²."),
    (&["ohm's law", "ohms law"], "Ohm's law", "V = I R", "Voltage equals current times resistance; power P = V I = I² R = V²/R."),
    (&["kinetic energy formula", "kinetic energy"], "Kinetic energy", "E = ½ m v²", "Energy of a mass m moving at speed v."),
    (&["mass energy equivalence", "mass-energy equivalence", "e=mc^2", "e = mc^2", "e=mc2"], "Mass–energy equivalence", "E = m c²", "c = 299 792 458 m/s."),
    (&["product rule"], "Product rule", "(f g)′ = f′ g + f g′", "Derivative of a product."),
    (&["quotient rule"], "Quotient rule", "(f / g)′ = (f′ g − f g′) / g²", "Derivative of a quotient."),
    (&["chain rule"], "Chain rule", "(f(g(x)))′ = f′(g(x)) · g′(x)", "Derivative of a composition."),
    (&["power rule"], "Power rule", "d/dx x^n = n x^(n−1)", "Also ∫ x^n dx = x^(n+1)/(n+1) + C for n ≠ −1."),
    (&["integration by parts", "integration by parts formula"], "Integration by parts", "∫ u dv = u v − ∫ v du", "The integral form of the product rule."),
    (&["fundamental theorem of calculus"], "Fundamental theorem of calculus", "∫_a^b f(x) dx = F(b) − F(a),  where F′ = f", "Differentiation and integration are inverse operations."),
    (&["l'hopital's rule", "lhopital's rule", "l'hôpital's rule", "lhopitals rule", "l'hopital rule"], "L'Hôpital's rule", "lim f(x)/g(x) = lim f′(x)/g′(x)", "Valid for the indeterminate forms 0/0 and ∞/∞ when the right-hand limit exists."),
    (&["taylor's theorem", "taylor series formula", "taylor formula"], "Taylor series", "f(x) = Σ_(n=0)^∞ f⁽ⁿ⁾(a) (x − a)^n / n!", "Expansion of a smooth function about x = a."),
    (&["bayes theorem", "bayes' theorem", "bayes's theorem", "bayes rule", "bayes' rule"], "Bayes' theorem", "P(A | B) = P(B | A) P(A) / P(B)", "Updates the probability of A after observing B."),
    (&["de moivre's theorem", "de moivre's formula", "de moivre"], "De Moivre's formula", "(cos x + i sin x)^n = cos(n x) + i sin(n x)", "Powers of complex numbers in polar form."),
    (&["stirling's approximation", "stirling's formula", "stirling formula"], "Stirling's approximation", "n! ≈ √(2 π n) (n / e)^n", "Asymptotic for large n."),
    (&["difference of squares"], "Difference of squares", "a² − b² = (a − b)(a + b)", ""),
    (&["sum of cubes", "difference of cubes"], "Sum and difference of cubes", "a³ ± b³ = (a ± b)(a² ∓ a b + b²)", ""),
    (&["double angle formulas", "double angle formula", "double angle identities"], "Double-angle formulas", "sin 2x = 2 sin x cos x;  cos 2x = cos² x − sin² x;  tan 2x = 2 tan x / (1 − tan² x)", ""),
    (&["half angle formulas", "half angle formula"], "Half-angle formulas", "sin²(x/2) = (1 − cos x)/2;  cos²(x/2) = (1 + cos x)/2", ""),
    (&["pythagorean identity", "pythagorean identities"], "Pythagorean identities", "sin² x + cos² x = 1;  1 + tan² x = sec² x;  1 + cot² x = csc² x", ""),
    (&["schrodinger equation", "schrödinger equation"], "Schrödinger equation", "i ħ ∂ψ/∂t = Ĥ ψ", "Time-dependent form; the stationary form is Ĥ ψ = E ψ."),
    (&["maxwell's equations", "maxwell equations"], "Maxwell's equations", "∇·E = ρ/ε₀;  ∇·B = 0;  ∇×E = −∂B/∂t;  ∇×B = μ₀ J + μ₀ ε₀ ∂E/∂t", "Differential form in SI units."),
    (&["normal distribution formula", "gaussian formula", "normal pdf"], "Normal density", "f(x) = exp(−(x − μ)² / (2 σ²)) / (σ √(2π))", ""),
    (&["standard deviation formula"], "Standard deviation", "s = √(Σ (xᵢ − x̄)² / (n − 1))", "Sample form; the population form divides by n."),
    (&["arithmetic series formula", "sum of arithmetic series", "arithmetic sequence sum"], "Arithmetic series", "S_n = n (a₁ + a_n) / 2", "a_n = a₁ + (n − 1) d."),
    (&["geometric series formula", "sum of geometric series", "geometric sequence sum"], "Geometric series", "S_n = a (1 − r^n) / (1 − r);  S_∞ = a / (1 − r) for |r| < 1", ""),
];

fn named(l: &str) -> Option<Answer> {
    let key = l.trim().trim_start_matches("the ").trim_start_matches("state ").trim_end_matches(" formula?").trim();
    let hit = NAMED.iter().find(|(names, ..)| names.iter().any(|n| *n == key || format!("{n} formula") == key || format!("formula for {n}") == key || format!("formula for the {n}") == key))?;
    let mut pods = vec![pod("Input", "Input interpretation", text(hit.1)), primary(pod("Result", "Formula", text(hit.2)))];
    if !hit.3.is_empty() {
        pods.push(pod("Description", "Description", text(hit.3)));
    }
    Some(Answer::new(hit.1, pods))
}
