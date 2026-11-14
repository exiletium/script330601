//! Chemistry: the periodic table, element properties, chemical formulas
//! and molar masses.

use crate::engine::*;
use crate::print::fmt_float;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::sync::OnceLock;

#[derive(Deserialize, Clone, Debug)]
pub struct Element {
    pub z: u32,
    pub symbol: String,
    pub name: String,
    pub mass: f64,
    pub group: Option<u32>,
    pub period: Option<u32>,
    pub block: Option<String>,
    pub series: Option<String>,
    pub config: Option<String>,
    pub electronegativity: Option<f64>,
    pub melting_k: Option<f64>,
    pub boiling_k: Option<f64>,
    pub density_g_cm3: Option<f64>,
    pub atomic_radius_pm: Option<f64>,
    pub first_ionization_ev: Option<f64>,
    pub electron_affinity_ev: Option<f64>,
    pub discovery_year: Option<serde_json::Value>,
    pub discoverers: Option<String>,
    pub oxidation_states: Option<Vec<i32>>,
    pub abundance_crust_mg_kg: Option<f64>,
    pub is_radioactive: Option<bool>,
    pub name_origin: Option<String>,
    pub uses: Option<String>,
    pub description: Option<String>,
}

pub fn elements() -> &'static Vec<Element> {
    static E: OnceLock<Vec<Element>> = OnceLock::new();
    E.get_or_init(|| serde_json::from_str(include_str!("../../data/elements.json")).unwrap_or_default())
}

pub fn by_symbol(s: &str) -> Option<&'static Element> {
    elements().iter().find(|e| e.symbol == s)
}

pub fn find_element(q: &str) -> Option<&'static Element> {
    let q = q.trim();
    let lq = q.to_lowercase();
    let lq = lq.trim_start_matches("element ").trim_end_matches(" element").trim();
    if let Ok(z) = lq.parse::<u32>() {
        return elements().iter().find(|e| e.z == z);
    }
    let lq = if lq == "aluminum" { "aluminium" } else if lq == "sulphur" { "sulfur" } else if lq == "caesium" { "cesium" } else { lq };
    elements().iter().find(|e| e.name.to_lowercase() == lq).or_else(|| if q.len() <= 2 && q.chars().next()?.is_uppercase() { by_symbol(q) } else { None })
}

/// Common compound names → formulas.
const COMPOUNDS: &[(&str, &str)] = &[
    ("water", "H2O"),
    ("caffeine", "C8H10N4O2"),
    ("glucose", "C6H12O6"),
    ("fructose", "C6H12O6"),
    ("sucrose", "C12H22O11"),
    ("sugar", "C12H22O11"),
    ("table salt", "NaCl"),
    ("salt", "NaCl"),
    ("sodium chloride", "NaCl"),
    ("ethanol", "C2H5OH"),
    ("alcohol", "C2H5OH"),
    ("methanol", "CH3OH"),
    ("methane", "CH4"),
    ("ethane", "C2H6"),
    ("propane", "C3H8"),
    ("butane", "C4H10"),
    ("octane", "C8H18"),
    ("benzene", "C6H6"),
    ("acetone", "C3H6O"),
    ("acetic acid", "CH3COOH"),
    ("vinegar", "CH3COOH"),
    ("aspirin", "C9H8O4"),
    ("paracetamol", "C8H9NO2"),
    ("acetaminophen", "C8H9NO2"),
    ("ibuprofen", "C13H18O2"),
    ("carbon dioxide", "CO2"),
    ("carbon monoxide", "CO"),
    ("ammonia", "NH3"),
    ("ozone", "O3"),
    ("oxygen gas", "O2"),
    ("nitrogen gas", "N2"),
    ("hydrogen gas", "H2"),
    ("hydrogen peroxide", "H2O2"),
    ("sulfuric acid", "H2SO4"),
    ("hydrochloric acid", "HCl"),
    ("nitric acid", "HNO3"),
    ("phosphoric acid", "H3PO4"),
    ("sodium hydroxide", "NaOH"),
    ("baking soda", "NaHCO3"),
    ("sodium bicarbonate", "NaHCO3"),
    ("calcium carbonate", "CaCO3"),
    ("limestone", "CaCO3"),
    ("rust", "Fe2O3"),
    ("iron oxide", "Fe2O3"),
    ("silicon dioxide", "SiO2"),
    ("quartz", "SiO2"),
    ("chlorophyll", "C55H72MgN4O5"),
    ("cholesterol", "C27H46O"),
    ("dopamine", "C8H11NO2"),
    ("serotonin", "C10H12N2O"),
    ("nicotine", "C10H14N2"),
    ("vitamin c", "C6H8O6"),
    ("ascorbic acid", "C6H8O6"),
    ("urea", "CH4N2O"),
    ("glycine", "C2H5NO2"),
    ("adrenaline", "C9H13NO3"),
    ("penicillin", "C16H18N2O4S"),
    ("tnt", "C7H5N3O6"),
    ("copper sulfate", "CuSO4"),
    ("potassium permanganate", "KMnO4"),
    ("chloroform", "CHCl3"),
];

pub fn compound_formula(name: &str) -> Option<&'static str> {
    let l = name.trim().to_lowercase();
    COMPOUNDS.iter().find(|(n, _)| *n == l).map(|(_, f)| *f)
}

/// Parse a chemical formula into element counts.  Supports parentheses,
/// brackets and hydrates: `Ca(OH)2`, `CuSO4·5H2O`, `[Fe(CN)6]3`.
pub fn parse_formula(f: &str) -> Option<BTreeMap<String, u32>> {
    let f = f.trim();
    if f.is_empty() || !f.chars().next()?.is_ascii_alphanumeric() && !"([".contains(f.chars().next()?) {
        return None;
    }
    let mut total = BTreeMap::new();
    for part in f.split(['·', '*', '.']) {
        let part = part.trim();
        // leading multiplier for hydrates
        let digits: String = part.chars().take_while(|c| c.is_ascii_digit()).collect();
        let mult: u32 = if digits.is_empty() { 1 } else { digits.parse().ok()? };
        let body = &part[digits.len()..];
        let (counts, rest) = parse_group(body)?;
        if !rest.is_empty() {
            return None;
        }
        for (k, v) in counts {
            *total.entry(k).or_insert(0) += v * mult;
        }
    }
    if total.is_empty() {
        None
    } else {
        Some(total)
    }
}

fn parse_group(s: &str) -> Option<(BTreeMap<String, u32>, &str)> {
    let mut out: BTreeMap<String, u32> = BTreeMap::new();
    let mut rest = s;
    loop {
        let c = match rest.chars().next() {
            Some(c) => c,
            None => break,
        };
        if c == '(' || c == '[' {
            let (inner, after) = parse_group(&rest[1..])?;
            let after = after.strip_prefix([')', ']'])?;
            let n: String = after.chars().take_while(|c| c.is_ascii_digit()).collect();
            let k: u32 = if n.is_empty() { 1 } else { n.parse().ok()? };
            for (e, v) in inner {
                *out.entry(e).or_insert(0) += v * k;
            }
            rest = &after[n.len()..];
        } else if c == ')' || c == ']' {
            break;
        } else if c.is_ascii_uppercase() {
            let mut sym = c.to_string();
            let mut it = rest[1..].chars();
            let mut used = 1;
            if let Some(d) = it.next() {
                if d.is_ascii_lowercase() {
                    sym.push(d);
                    used += 1;
                }
            }
            by_symbol(&sym)?;
            let after = &rest[used..];
            let n: String = after.chars().take_while(|c| c.is_ascii_digit()).collect();
            let k: u32 = if n.is_empty() { 1 } else { n.parse().ok()? };
            *out.entry(sym).or_insert(0) += k;
            rest = &after[n.len()..];
        } else {
            return None;
        }
    }
    Some((out, rest))
}

pub fn molar_mass(counts: &BTreeMap<String, u32>) -> f64 {
    counts.iter().map(|(e, n)| by_symbol(e).map_or(0.0, |el| el.mass) * *n as f64).sum()
}

fn hill_formula(counts: &BTreeMap<String, u32>) -> String {
    let mut keys: Vec<&String> = counts.keys().collect();
    let has_c = counts.contains_key("C");
    keys.sort_by_key(|k| match (has_c, k.as_str()) {
        (true, "C") => (0, String::new()),
        (true, "H") => (1, String::new()),
        _ => (2, k.to_string()),
    });
    keys.iter().map(|k| if counts[*k] == 1 { k.to_string() } else { format!("{k}{}", subscript(counts[*k])) }).collect()
}

fn subscript(n: u32) -> String {
    n.to_string().chars().map(|c| char::from_u32(0x2080 + c.to_digit(10).unwrap()).unwrap()).collect()
}

fn element_pods(e: &Element) -> Vec<Pod> {
    let mut pods = vec![pod("Input", "Input interpretation", text(format!("{} (chemical element)", e.name)))];
    let mut basic = vec![vec![text("element symbol"), text(e.symbol.clone())], vec![text("atomic number"), text(e.z.to_string())], vec![text("atomic mass"), text(format!("{} u", fmt_float(e.mass, 8)))]];
    if let Some(c) = &e.config {
        basic.push(vec![text("electron configuration"), text(c.clone())]);
    }
    if let (Some(g), Some(p)) = (e.group, e.period) {
        basic.push(vec![text("group, period"), text(format!("{g}, {p}"))]);
    } else if let Some(p) = e.period {
        basic.push(vec![text("period"), text(p.to_string())]);
    }
    if let Some(s) = &e.series {
        basic.push(vec![text("element type"), text(s.to_lowercase())]);
    }
    if let Some(b) = &e.block {
        basic.push(vec![text("block"), text(b.clone())]);
    }
    pods.push(primary(pod("BasicProperties", "Basic elemental properties", table(None, basic))));
    let mut thermo = Vec::new();
    let k2c = |k: f64| format!("{} K ({} °C)", fmt_float(k, 6), fmt_float(k - 273.15, 6));
    if let Some(m) = e.melting_k {
        thermo.push(vec![text("melting point"), text(k2c(m))]);
    }
    if let Some(b) = e.boiling_k {
        thermo.push(vec![text("boiling point"), text(k2c(b))]);
    }
    if let Some(d) = e.density_g_cm3 {
        thermo.push(vec![text("density"), text(format!("{} g/cm³", fmt_float(d, 6)))]);
    }
    if let (Some(m), Some(b)) = (e.melting_k, e.boiling_k) {
        let phase = if 293.15 < m {
            "solid"
        } else if 293.15 < b {
            "liquid"
        } else {
            "gas"
        };
        thermo.push(vec![text("phase at STP"), text(phase)]);
    }
    if !thermo.is_empty() {
        pods.push(pod("Thermo", "Thermodynamic properties", table(None, thermo)));
    }
    let mut atomic = Vec::new();
    if let Some(v) = e.electronegativity {
        atomic.push(vec![text("electronegativity (Pauling)"), text(fmt_float(v, 4))]);
    }
    if let Some(v) = e.first_ionization_ev {
        atomic.push(vec![text("first ionization energy"), text(format!("{} eV", fmt_float(v, 6)))]);
    }
    if let Some(v) = e.electron_affinity_ev {
        atomic.push(vec![text("electron affinity"), text(format!("{} eV", fmt_float(v, 6)))]);
    }
    if let Some(v) = e.atomic_radius_pm {
        atomic.push(vec![text("atomic radius"), text(format!("{} pm", fmt_float(v, 6)))]);
    }
    if let Some(ox) = &e.oxidation_states {
        if !ox.is_empty() {
            atomic.push(vec![text("oxidation states"), text(ox.iter().map(|o| if *o > 0 { format!("+{o}") } else { o.to_string() }).collect::<Vec<_>>().join(", "))]);
        }
    }
    if !atomic.is_empty() {
        pods.push(pod("Atomic", "Atomic properties", table(None, atomic)));
    }
    let mut hist = Vec::new();
    if let Some(y) = &e.discovery_year {
        let y = match y {
            serde_json::Value::Number(n) => n.to_string(),
            serde_json::Value::String(s) => s.clone(),
            _ => String::new(),
        };
        if !y.is_empty() {
            hist.push(vec![text("discovered"), text(y)]);
        }
    }
    if let Some(d) = &e.discoverers {
        if !d.is_empty() {
            hist.push(vec![text("discoverers"), text(d.clone())]);
        }
    }
    if let Some(o) = &e.name_origin {
        if !o.is_empty() {
            hist.push(vec![text("name origin"), text(o.clone())]);
        }
    }
    if !hist.is_empty() {
        pods.push(pod("History", "History", table(None, hist)));
    }
    if let Some(a) = e.abundance_crust_mg_kg {
        pods.push(pod("Abundance", "Abundance in Earth's crust", text(format!("{} mg/kg", fmt_float(a, 6)))));
    }
    if let Some(r) = e.is_radioactive {
        if r {
            pods.push(pod("Radioactivity", "Nuclear properties", text("radioactive (no stable isotopes)")));
        }
    }
    if let Some(u) = &e.uses {
        if !u.is_empty() {
            pods.push(pod("Uses", "Uses", text(u.clone())));
        }
    }
    pods
}

fn compound_pods(label: &str, formula: &str, counts: &BTreeMap<String, u32>) -> Vec<Pod> {
    let mm = molar_mass(counts);
    let mut pods = vec![pod("Input", "Input interpretation", text(format!("{label} | molar mass")))];
    pods.push(primary(pod("Result", "Molar mass", text(format!("{} g/mol", fmt_float(mm, 7))))));
    pods.push(pod("Formula", "Chemical formula", text(format!("{formula}   (Hill formula: {})", hill_formula(counts)))));
    let rows: Vec<Vec<Content>> = counts
        .iter()
        .map(|(e, n)| {
            let el = by_symbol(e);
            let m = el.map_or(0.0, |x| x.mass) * *n as f64;
            vec![text(el.map_or(e.clone(), |x| x.name.clone())), text(e.clone()), text(n.to_string()), text(format!("{} g/mol", fmt_float(m, 6))), text(format!("{}%", fmt_float(100.0 * m / mm, 4)))]
        })
        .collect();
    pods.push(pod("Composition", "Elemental composition", table(Some(vec!["element", "symbol", "atoms", "mass", "mass fraction"]), rows)));
    let atoms: u32 = counts.values().sum();
    pods.push(pod("Counts", "Atom count", text(format!("{atoms} atoms ({} elements)", counts.len()))));
    pods
}

pub fn handle(q: &Query) -> Option<Answer> {
    let l = q.lower.trim();
    // molar mass / molecular weight
    for prefix in ["molar mass of ", "molecular weight of ", "molecular mass of ", "mass of one mole of ", "formula weight of "] {
        if let Some(rest) = l.strip_prefix(prefix) {
            let orig = &q.clean[q.clean.len() - rest.len()..];
            let formula = compound_formula(rest).unwrap_or(orig.trim());
            let counts = parse_formula(formula)?;
            let label = if compound_formula(rest).is_some() { format!("{rest} ({formula})") } else { formula.to_string() };
            return Some(Answer::new(format!("molar mass of {label}"), compound_pods(&label, formula, &counts)));
        }
    }
    // element lookups: "iron", "element 26", "properties of gold", "Fe"
    let el_q = l.trim_start_matches("properties of ").trim_start_matches("the element ").trim_start_matches("chemical element ");
    let orig = &q.clean[q.clean.len() - el_q.len()..];
    if let Some(e) = find_element(orig) {
        if el_q.len() > 2 || q.raw.trim().len() <= 2 || l.starts_with("element") {
            return Some(Answer::new(format!("{} (element)", e.name), element_pods(e)));
        }
    }
    // "atomic mass of carbon", "boiling point of mercury"
    if let Some((prop, el)) = l.split_once(" of ") {
        if let Some(e) = find_element(el) {
            let v = match prop {
                "atomic mass" | "atomic weight" | "mass" => Some(format!("{} u", fmt_float(e.mass, 8))),
                "atomic number" => Some(e.z.to_string()),
                "melting point" => e.melting_k.map(|k| format!("{} K ({} °C)", fmt_float(k, 6), fmt_float(k - 273.15, 6))),
                "boiling point" => e.boiling_k.map(|k| format!("{} K ({} °C)", fmt_float(k, 6), fmt_float(k - 273.15, 6))),
                "density" => e.density_g_cm3.map(|d| format!("{} g/cm³", fmt_float(d, 6))),
                "electronegativity" => e.electronegativity.map(|v| fmt_float(v, 4)),
                "electron configuration" => e.config.clone(),
                "symbol" => Some(e.symbol.clone()),
                _ => None,
            };
            if let Some(v) = v {
                let mut pods = vec![pod("Input", "Input interpretation", text(format!("{} | {prop}", e.name))), primary(pod("Result", "Result", text(v)))];
                pods.extend(element_pods(e).into_iter().skip(1).take(2).map(|mut p| {
                    p.primary = false;
                    p
                }));
                return Some(Answer::new(format!("{prop} of {}", e.name), pods));
            }
        }
    }
    // a bare formula like "H2SO4" or a compound name like "caffeine"
    if let Some(f) = compound_formula(l) {
        let counts = parse_formula(f)?;
        let label = format!("{l} ({f})");
        return Some(Answer::new(label.clone(), compound_pods(&label, f, &counts)));
    }
    let raw = q.clean.trim();
    if raw.len() >= 3 && raw.chars().any(|c| c.is_ascii_digit()) && raw.chars().next()?.is_ascii_uppercase() && !raw.contains(' ') {
        if let Some(counts) = parse_formula(raw) {
            if counts.len() >= 2 || counts.values().any(|v| *v > 1) {
                return Some(Answer::new(raw.to_string(), compound_pods(raw, raw, &counts)));
            }
        }
    }
    None
}
