//! Chemistry calculations: properties of common substances, balancing
//! chemical equations, pH of acid and base solutions, and conversions
//! between mass, moles, molecules and molarity.

use super::chem::{by_symbol, compound_formula, find_element, molar_mass, parse_formula};
use crate::engine::*;
use crate::print::fmt_float;
use regex::Regex;
use std::collections::BTreeMap;
use std::sync::OnceLock;

macro_rules! re {
    ($s:expr) => {{
        static R: OnceLock<Regex> = OnceLock::new();
        R.get_or_init(|| Regex::new($s).unwrap())
    }};
}

const AVOGADRO: f64 = 6.022_140_76e23;

fn n(v: f64) -> String {
    fmt_float(v, 6)
}

/// Formula with subscript digits: `H2SO4` → `H₂SO₄`.
fn pretty(formula: &str) -> String {
    let mut out = String::new();
    let mut prev_alpha = false;
    for ch in formula.chars() {
        if ch.is_ascii_digit() && prev_alpha {
            out.push(char::from_u32(0x2080 + ch as u32 - '0' as u32).unwrap_or(ch));
        } else {
            out.push(ch);
            prev_alpha = ch.is_ascii_alphabetic() || ch == ')' || ch == ']';
            continue;
        }
        prev_alpha = true;
    }
    out
}

/// A substance named in a query: a compound name, a formula or an element.
fn substance(s: &str) -> Option<(String, String, BTreeMap<String, u32>)> {
    let s = s.trim().trim_matches(|c| c == '?' || c == '.');
    let s = s.strip_prefix("the ").unwrap_or(s);
    let lower = s.to_lowercase();
    if let Some(f) = compound_formula(&lower) {
        return Some((format!("{lower} ({})", pretty(f)), f.to_string(), parse_formula(f)?));
    }
    if let Some(sub) = SUBSTANCES.iter().find(|x| x.names.contains(&lower.as_str())).filter(|x| !x.formula.is_empty()) {
        return Some((format!("{lower} ({})", pretty(sub.formula)), sub.formula.to_string(), parse_formula(sub.formula)?));
    }
    if s.chars().next()?.is_ascii_uppercase() && !s.contains(' ') {
        if let Some(c) = parse_formula(s) {
            return Some((pretty(s), s.to_string(), c));
        }
    }
    let e = find_element(s)?;
    let mut c = BTreeMap::new();
    // the common diatomic gases are counted as molecules
    let di = matches!(e.symbol.as_str(), "H" | "N" | "O" | "F" | "Cl" | "Br" | "I") && lower.ends_with(" gas");
    c.insert(e.symbol.clone(), if di { 2 } else { 1 });
    let f = if di { format!("{}2", e.symbol) } else { e.symbol.clone() };
    Some((format!("{} ({})", e.name.to_lowercase(), pretty(&f)), f, c))
}

// ---------------------------------------------------------------------------
// properties of common substances
// ---------------------------------------------------------------------------

struct Substance {
    names: &'static [&'static str],
    formula: &'static str,
    /// melting point, °C
    mp: Option<f64>,
    /// boiling point at 1 atm, °C
    bp: Option<f64>,
    /// density near room temperature, g/cm³
    density: Option<f64>,
    /// specific heat capacity, J/(g·K)
    cp: Option<f64>,
    note: &'static str,
}

const fn s(names: &'static [&'static str], formula: &'static str, mp: Option<f64>, bp: Option<f64>, density: Option<f64>, cp: Option<f64>, note: &'static str) -> Substance {
    Substance { names, formula, mp, bp, density, cp, note }
}

static SUBSTANCES: &[Substance] = &[
    s(&["water", "liquid water", "h2o"], "H2O", Some(0.0), Some(100.0), Some(0.997), Some(4.184), "liquid at room temperature; density is greatest (1.000 g/cm³) near 4 °C"),
    s(&["ice"], "H2O", Some(0.0), Some(100.0), Some(0.917), Some(2.09), "solid water; less dense than the liquid, so it floats"),
    s(&["steam", "water vapor", "water vapour"], "H2O", Some(0.0), Some(100.0), Some(0.000598), Some(2.03), "density at 100 °C and 1 atm"),
    s(&["ethanol", "alcohol", "ethyl alcohol"], "C2H5OH", Some(-114.1), Some(78.37), Some(0.789), Some(2.44), "volatile flammable liquid"),
    s(&["methanol", "methyl alcohol"], "CH3OH", Some(-97.6), Some(64.7), Some(0.792), Some(2.53), "toxic flammable liquid"),
    s(&["acetone"], "C3H6O", Some(-94.7), Some(56.05), Some(0.784), Some(2.16), "common solvent"),
    s(&["benzene"], "C6H6", Some(5.53), Some(80.1), Some(0.876), Some(1.74), "aromatic hydrocarbon"),
    s(&["toluene"], "C7H8", Some(-95.0), Some(110.6), Some(0.867), Some(1.70), "aromatic solvent"),
    s(&["glycerol", "glycerin", "glycerine"], "C3H8O3", Some(17.8), Some(290.0), Some(1.261), Some(2.43), "viscous liquid"),
    s(&["ammonia"], "NH3", Some(-77.73), Some(-33.34), Some(0.00073), Some(2.06), "gas at room temperature (density at 1 atm, 15 °C)"),
    s(&["carbon dioxide", "co2", "dry ice"], "CO2", Some(-56.6), Some(-78.5), Some(0.001977), Some(0.846), "sublimes at −78.5 °C at 1 atm; melts only under pressure (5.1 atm)"),
    s(&["carbon monoxide"], "CO", Some(-205.0), Some(-191.5), Some(0.00114), Some(1.04), "toxic gas"),
    s(&["methane", "natural gas"], "CH4", Some(-182.5), Some(-161.5), Some(0.000657), Some(2.22), "gas at room temperature"),
    s(&["ethane"], "C2H6", Some(-182.8), Some(-88.6), Some(0.001356), Some(1.75), "gas at room temperature"),
    s(&["propane"], "C3H8", Some(-187.7), Some(-42.1), Some(0.00201), Some(1.67), "gas at room temperature"),
    s(&["butane"], "C4H10", Some(-138.3), Some(-0.5), Some(0.00248), Some(1.66), "gas at room temperature"),
    s(&["octane"], "C8H18", Some(-56.8), Some(125.6), Some(0.703), Some(2.22), "component of gasoline"),
    s(&["sodium chloride", "salt", "table salt", "nacl"], "NaCl", Some(801.0), Some(1465.0), Some(2.165), Some(0.853), "ionic solid"),
    s(&["sulfuric acid"], "H2SO4", Some(10.31), Some(337.0), Some(1.83), Some(1.34), "strong diprotic acid"),
    s(&["nitric acid"], "HNO3", Some(-42.0), Some(83.0), Some(1.51), Some(1.72), "strong acid"),
    s(&["hydrochloric acid", "hydrogen chloride"], "HCl", Some(-114.2), Some(-85.05), Some(0.00149), None, "values for hydrogen chloride gas; the acid is its aqueous solution"),
    s(&["acetic acid", "vinegar"], "CH3COOH", Some(16.6), Some(118.1), Some(1.049), Some(2.04), "weak acid (pure, 'glacial')"),
    s(&["hydrogen peroxide"], "H2O2", Some(-0.43), Some(150.2), Some(1.45), Some(2.62), "pure compound"),
    s(&["sodium hydroxide", "lye", "caustic soda"], "NaOH", Some(318.0), Some(1388.0), Some(2.13), Some(1.49), "strong base"),
    s(&["glucose"], "C6H12O6", Some(146.0), None, Some(1.54), Some(1.21), "decomposes above its melting point"),
    s(&["sucrose", "sugar", "table sugar"], "C12H22O11", Some(186.0), None, Some(1.587), Some(1.24), "decomposes (caramelizes) on melting"),
    s(&["calcium carbonate", "limestone", "chalk"], "CaCO3", None, None, Some(2.71), Some(0.82), "decomposes to CaO and CO₂ near 825 °C"),
    s(&["silicon dioxide", "quartz", "silica"], "SiO2", Some(1713.0), Some(2950.0), Some(2.648), Some(0.74), "network solid"),
    s(&["chloroform"], "CHCl3", Some(-63.5), Some(61.2), Some(1.489), Some(0.96), "dense volatile liquid"),
    s(&["caffeine"], "C8H10N4O2", Some(235.0), None, Some(1.23), None, "sublimes near 178 °C"),
    s(&["aspirin"], "C9H8O4", Some(135.0), None, Some(1.40), None, "decomposes near 140 °C"),
    s(&["urea"], "CH4N2O", Some(133.0), None, Some(1.32), Some(1.55), "decomposes on heating"),
    s(&["iron oxide", "rust"], "Fe2O3", Some(1565.0), None, Some(5.24), Some(0.65), "decomposes at high temperature"),
    s(&["ozone"], "O3", Some(-192.2), Some(-112.0), Some(0.002144), Some(0.82), "gas at room temperature"),
    s(&["baking soda", "sodium bicarbonate"], "NaHCO3", None, None, Some(2.20), Some(1.04), "decomposes from about 50 °C"),
    // mixtures and materials (no single formula)
    s(&["air"], "", None, Some(-194.35), Some(0.001204), Some(1.005), "density at 20 °C and 1 atm (1.204 kg/m³)"),
    s(&["seawater", "sea water"], "", Some(-1.9), Some(100.6), Some(1.025), Some(3.99), "typical salinity 35 g/kg"),
    s(&["gasoline", "petrol"], "", None, None, Some(0.745), Some(2.22), "varies with blend (0.71–0.77 g/cm³)"),
    s(&["diesel"], "", None, None, Some(0.84), Some(2.05), "varies with blend"),
    s(&["olive oil", "oil", "vegetable oil"], "", Some(-6.0), None, Some(0.92), Some(1.97), "typical value"),
    s(&["milk"], "", Some(-0.5), Some(100.2), Some(1.03), Some(3.93), "whole milk, typical value"),
    s(&["honey"], "", None, None, Some(1.42), Some(2.3), "typical value"),
    s(&["blood"], "", None, None, Some(1.06), Some(3.6), "human whole blood"),
    s(&["steel"], "", Some(1425.0), None, Some(7.85), Some(0.49), "carbon steel; melting range 1425–1540 °C"),
    s(&["stainless steel"], "", Some(1400.0), None, Some(8.0), Some(0.50), "type 304"),
    s(&["brass"], "", Some(930.0), None, Some(8.5), Some(0.38), "typical value"),
    s(&["bronze"], "", Some(950.0), None, Some(8.8), Some(0.38), "typical value"),
    s(&["concrete"], "", None, None, Some(2.4), Some(0.88), "typical value"),
    s(&["glass"], "", Some(1400.0), None, Some(2.5), Some(0.84), "soda-lime glass; softens rather than melting sharply"),
    s(&["granite"], "", None, None, Some(2.7), Some(0.79), "typical value"),
    s(&["wood", "oak"], "", None, None, Some(0.75), Some(2.0), "oak; wood varies from about 0.4 to 0.9 g/cm³"),
    s(&["diamond"], "C", Some(3550.0), None, Some(3.51), Some(0.51), "converts to graphite before melting at ordinary pressure"),
    s(&["graphite"], "C", Some(3600.0), None, Some(2.26), Some(0.71), "sublimes near 3600 °C"),
    s(&["sand"], "", None, None, Some(1.6), Some(0.83), "dry, loose"),
    s(&["human body", "the human body"], "", None, None, Some(0.985), Some(3.5), "average"),
];

fn find_substance(name: &str) -> Option<&'static Substance> {
    let l = name.trim().to_lowercase();
    let l = l.strip_prefix("the ").unwrap_or(&l);
    let l = l.strip_prefix("pure ").unwrap_or(l);
    SUBSTANCES.iter().find(|x| x.names.contains(&l))
}

fn temp(c: f64) -> String {
    format!("{} °C  ({} K, {} °F)", n(c), n(c + 273.15), n(c * 9.0 / 5.0 + 32.0))
}

fn dens(d: f64) -> String {
    if d < 0.01 {
        format!("{} kg/m³  ({} g/L)", n(d * 1000.0), n(d * 1000.0))
    } else {
        format!("{} g/cm³  ({} kg/m³)", n(d), n(d * 1000.0))
    }
}

fn property_rows(x: &Substance) -> Vec<(&'static str, String)> {
    let mut rows = Vec::new();
    if let Some(v) = x.mp {
        rows.push(("melting point", temp(v)));
    }
    if let Some(v) = x.bp {
        rows.push(("boiling point", temp(v)));
    }
    if let Some(v) = x.density {
        rows.push(("density", dens(v)));
    }
    if let Some(v) = x.cp {
        rows.push(("specific heat capacity", format!("{} J/(g·K)", n(v))));
    }
    rows
}

/// A table of physical properties for a compound name, when it is known.
pub(crate) fn property_pod(name: &str) -> Option<Pod> {
    let x = find_substance(name)?;
    let rows = property_rows(x);
    (!rows.is_empty()).then(|| pod("Physical", "Physical properties", table(None, rows.into_iter().map(|(k, v)| vec![text(k), text(v)]).collect())))
}

fn properties(l: &str) -> Option<Answer> {
    let (prop, name) = if let Some(m) = re!(r"^(?:the\s+)?(boiling point|boiling temperature|melting point|melting temperature|freezing point|freezing temperature|density|specific heat capacity|specific heat|heat capacity)\s+(?:of|for)\s+(.+)$").captures(l) {
        (m[1].to_string(), m[2].to_string())
    } else if let Some(m) = re!(r"^(.+?)\s+(boiling point|melting point|freezing point|density|specific heat capacity|specific heat)$").captures(l) {
        (m[2].to_string(), m[1].to_string())
    } else if let Some(m) = re!(r"^(?:at\s+)?what temperature does\s+(.+?)\s+(boil|melt|freeze)$").captures(l) {
        (match &m[2] { "boil" => "boiling point", _ => "melting point" }.to_string(), m[1].to_string())
    } else {
        return None;
    };
    let x = find_substance(&name)?;
    let key = match prop.as_str() {
        p if p.starts_with("boil") => "boiling point",
        p if p.starts_with("melt") || p.starts_with("freez") => "melting point",
        "density" => "density",
        _ => "specific heat capacity",
    };
    let rows = property_rows(x);
    let (_, value) = rows.iter().find(|(k, _)| *k == key)?;
    let label = if prop.starts_with("freez") { "freezing point" } else { key };
    let title = if x.formula.is_empty() { x.names[0].to_string() } else { format!("{} ({})", x.names[0], pretty(x.formula)) };
    let mut pods = vec![pod("Input", "Input interpretation", text(format!("{title} | {label}"))), primary(pod("Result", label, text(value.clone())))];
    let others: Vec<Vec<Content>> = rows.iter().filter(|(k, _)| *k != key).map(|(k, v)| vec![text(*k), text(v.clone())]).collect();
    if !others.is_empty() {
        pods.push(pod("Physical", "Other properties", table(None, others)));
    }
    if !x.note.is_empty() {
        pods.push(pod("Note", "Note", text(format!("{}. Values at a pressure of 1 atmosphere.", x.note))));
    }
    Some(Answer::new(format!("{label} of {}", x.names[0]), pods))
}

// ---------------------------------------------------------------------------
// balancing equations
// ---------------------------------------------------------------------------

fn gcd(a: i128, b: i128) -> i128 {
    if b == 0 { a.abs() } else { gcd(b, a % b) }
}

#[derive(Clone, Copy, PartialEq)]
struct Q(i128, i128);

impl Q {
    fn new(a: i128, b: i128) -> Q {
        let g = gcd(a, b).max(1);
        let s = if b < 0 { -1 } else { 1 };
        Q(s * a / g, s * b / g)
    }
    fn sub(self, o: Q) -> Q {
        Q::new(self.0 * o.1 - o.0 * self.1, self.1 * o.1)
    }
    fn mul(self, o: Q) -> Q {
        Q::new(self.0 * o.0, self.1 * o.1)
    }
    fn div(self, o: Q) -> Q {
        Q::new(self.0 * o.1, self.1 * o.0)
    }
}

/// Smallest positive integer solution of `M·x = 0` when it is unique up to
/// scale; `Err(k)` gives the dimension of the solution space otherwise.
fn integer_nullvector(mut m: Vec<Vec<Q>>, cols: usize) -> Result<Vec<i128>, usize> {
    let mut pivots: Vec<usize> = Vec::new();
    let mut row = 0;
    for c in 0..cols {
        let Some(p) = (row..m.len()).find(|&r| m[r][c].0 != 0) else { continue };
        m.swap(row, p);
        let pv = m[row][c];
        for k in 0..cols {
            m[row][k] = m[row][k].div(pv);
        }
        for r in 0..m.len() {
            if r != row && m[r][c].0 != 0 {
                let f = m[r][c];
                for k in 0..cols {
                    let t = m[row][k].mul(f);
                    m[r][k] = m[r][k].sub(t);
                }
            }
        }
        pivots.push(c);
        row += 1;
        if row == m.len() {
            break;
        }
    }
    let free: Vec<usize> = (0..cols).filter(|c| !pivots.contains(c)).collect();
    if free.len() != 1 {
        return Err(free.len());
    }
    let f = free[0];
    let mut x = vec![Q(0, 1); cols];
    x[f] = Q(1, 1);
    for (r, &c) in pivots.iter().enumerate() {
        x[c] = Q::new(-m[r][f].0, m[r][f].1);
    }
    let lcm = x.iter().fold(1i128, |l, q| l / gcd(l, q.1) * q.1);
    let mut v: Vec<i128> = x.iter().map(|q| q.0 * (lcm / q.1)).collect();
    let g = v.iter().fold(0, |g, a| gcd(g, *a)).max(1);
    v.iter_mut().for_each(|a| *a /= g);
    Ok(v)
}

fn species(side: &str) -> Option<Vec<(String, BTreeMap<String, u32>)>> {
    let mut out = Vec::new();
    for part in side.split('+') {
        let mut p = part.trim();
        for state in ["(g)", "(l)", "(s)", "(aq)"] {
            p = p.trim_end_matches(state).trim();
        }
        // drop an existing coefficient
        let p = p.trim_start_matches(|c: char| c.is_ascii_digit() || c == ' ');
        if p.is_empty() {
            return None;
        }
        let formula = match compound_formula(&p.to_lowercase()) {
            Some(f) => f.to_string(),
            None if p.chars().next()?.is_ascii_uppercase() && !p.contains(' ') => p.to_string(),
            None => {
                let e = find_element(p)?;
                if matches!(e.symbol.as_str(), "H" | "N" | "O" | "F" | "Cl" | "Br" | "I") { format!("{}2", e.symbol) } else { e.symbol.clone() }
            }
        };
        let counts = parse_formula(&formula)?;
        out.push((formula, counts));
    }
    Some(out)
}

fn balance(c: &str) -> Option<Answer> {
    let body = re!(r"(?i)^(?:balance\s+(?:the\s+)?(?:chemical\s+)?(?:equation\s+|reaction\s+)?:?\s*)?(.+)$").captures(c)?.get(1)?.as_str();
    let explicit = c.to_lowercase().starts_with("balance");
    let arrow = ["-->", "->", "=>", "→", "⟶", "⇌", " yields ", "="].iter().find(|a| body.contains(**a))?;
    let (lhs, rhs) = body.split_once(arrow)?;
    let (left, right) = (species(lhs)?, species(rhs)?);
    if !explicit && left.len() + right.len() < 3 {
        return None;
    }
    let all: Vec<&(String, BTreeMap<String, u32>)> = left.iter().chain(right.iter()).collect();
    let mut elements: Vec<&String> = all.iter().flat_map(|(_, c)| c.keys()).collect();
    elements.sort();
    elements.dedup();
    let matrix: Vec<Vec<Q>> = elements
        .iter()
        .map(|e| all.iter().enumerate().map(|(k, (_, c))| Q(*c.get(*e).unwrap_or(&0) as i128 * if k < left.len() { 1 } else { -1 }, 1)).collect())
        .collect();
    let show = |coef: &[i128]| -> String {
        let side = |sp: &[(String, BTreeMap<String, u32>)], off: usize| sp.iter().enumerate().map(|(k, (f, _))| if coef[off + k] == 1 { pretty(f) } else { format!("{} {}", coef[off + k], pretty(f)) }).collect::<Vec<_>>().join(" + ");
        format!("{} → {}", side(&left, 0), side(&right, left.len()))
    };
    let unbalanced = show(&vec![1; all.len()]);
    let mut pods = vec![pod("Input", "Input interpretation", text(format!("balance {unbalanced}")))];
    match integer_nullvector(matrix, all.len()) {
        Ok(mut v) => {
            if v.iter().all(|a| *a < 0) {
                v.iter_mut().for_each(|a| *a = -*a);
            }
            if v.iter().any(|a| *a <= 0) {
                pods.push(primary(pod("Result", "Result", text("This equation cannot be balanced: no assignment of positive coefficients conserves every element."))));
                return Some(Answer::new("balance chemical equation", pods));
            }
            pods.push(primary(pod("Result", "Balanced equation", text(show(&v)))));
            let rows: Vec<Vec<Content>> = elements
                .iter()
                .map(|e| {
                    let count = |sp: &[(String, BTreeMap<String, u32>)], off: usize| -> i128 { sp.iter().enumerate().map(|(k, (_, c))| v[off + k] * *c.get(*e).unwrap_or(&0) as i128).sum() };
                    vec![text(by_symbol(e).map_or((*e).clone(), |x| format!("{} ({})", x.name, e))), text(count(&left, 0).to_string()), text(count(&right, left.len()).to_string())]
                })
                .collect();
            pods.push(pod("Atoms", "Atom balance", table(Some(vec!["element", "reactant atoms", "product atoms"]), rows)));
            let mass_rows: Vec<Vec<Content>> = all
                .iter()
                .enumerate()
                .map(|(k, (f, c))| {
                    let mm = molar_mass(c);
                    vec![text(pretty(f)), text(if k < left.len() { "reactant" } else { "product" }), text(v[k].to_string()), text(format!("{} g/mol", n(mm))), text(format!("{} g", n(mm * v[k] as f64)))]
                })
                .collect();
            pods.push(pod("Masses", "Stoichiometry", table(Some(vec!["species", "role", "moles", "molar mass", "mass"]), mass_rows)));
            let total: f64 = left.iter().enumerate().map(|(k, (_, c))| molar_mass(c) * v[k] as f64).sum();
            pods.push(pod("Conservation", "Mass conservation", text(format!("{} g of reactants give {} g of products", n(total), n(total)))));
        }
        Err(0) => {
            pods.push(primary(pod("Result", "Result", text("This equation cannot be balanced: the elements on the two sides do not match."))));
        }
        Err(k) => {
            pods.push(primary(pod("Result", "Result", text(format!("The balancing is not unique: there are {k} independent ways to balance these species, so the reaction is a combination of simpler reactions.")))));
        }
    }
    Some(Answer::new("balance chemical equation", pods))
}

// ---------------------------------------------------------------------------
// pH
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
enum Kind {
    StrongAcid(u32),
    Sulfuric,
    StrongBase(u32),
    WeakAcid(f64),
    WeakBase(f64),
}

const ACIDS: &[(&[&str], &str, Kind)] = &[
    (&["hcl", "hydrochloric acid", "hydrogen chloride"], "HCl", Kind::StrongAcid(1)),
    (&["hbr", "hydrobromic acid"], "HBr", Kind::StrongAcid(1)),
    (&["hi", "hydroiodic acid"], "HI", Kind::StrongAcid(1)),
    (&["hno3", "nitric acid"], "HNO3", Kind::StrongAcid(1)),
    (&["hclo4", "perchloric acid"], "HClO4", Kind::StrongAcid(1)),
    (&["hclo3", "chloric acid"], "HClO3", Kind::StrongAcid(1)),
    (&["h2so4", "sulfuric acid", "sulphuric acid"], "H2SO4", Kind::Sulfuric),
    (&["naoh", "sodium hydroxide"], "NaOH", Kind::StrongBase(1)),
    (&["koh", "potassium hydroxide"], "KOH", Kind::StrongBase(1)),
    (&["lioh", "lithium hydroxide"], "LiOH", Kind::StrongBase(1)),
    (&["ca(oh)2", "calcium hydroxide"], "Ca(OH)2", Kind::StrongBase(2)),
    (&["ba(oh)2", "barium hydroxide"], "Ba(OH)2", Kind::StrongBase(2)),
    (&["sr(oh)2", "strontium hydroxide"], "Sr(OH)2", Kind::StrongBase(2)),
    (&["ch3cooh", "acetic acid", "hc2h3o2", "ethanoic acid", "vinegar"], "CH3COOH", Kind::WeakAcid(1.8e-5)),
    (&["hf", "hydrofluoric acid"], "HF", Kind::WeakAcid(6.6e-4)),
    (&["hcooh", "formic acid", "methanoic acid"], "HCOOH", Kind::WeakAcid(1.8e-4)),
    (&["hcn", "hydrocyanic acid", "hydrogen cyanide"], "HCN", Kind::WeakAcid(6.2e-10)),
    (&["hno2", "nitrous acid"], "HNO2", Kind::WeakAcid(4.5e-4)),
    (&["h2co3", "carbonic acid"], "H2CO3", Kind::WeakAcid(4.3e-7)),
    (&["h3po4", "phosphoric acid"], "H3PO4", Kind::WeakAcid(7.5e-3)),
    (&["hclo", "hypochlorous acid"], "HClO", Kind::WeakAcid(3.0e-8)),
    (&["c6h5cooh", "benzoic acid"], "C6H5COOH", Kind::WeakAcid(6.3e-5)),
    (&["h2s", "hydrosulfuric acid", "hydrogen sulfide"], "H2S", Kind::WeakAcid(9.1e-8)),
    (&["lactic acid"], "C3H6O3", Kind::WeakAcid(1.4e-4)),
    (&["citric acid"], "C6H8O7", Kind::WeakAcid(7.4e-4)),
    (&["boric acid", "h3bo3"], "H3BO3", Kind::WeakAcid(5.8e-10)),
    (&["nh3", "ammonia", "nh4oh", "ammonium hydroxide"], "NH3", Kind::WeakBase(1.8e-5)),
    (&["ch3nh2", "methylamine"], "CH3NH2", Kind::WeakBase(4.4e-4)),
    (&["c5h5n", "pyridine"], "C5H5N", Kind::WeakBase(1.7e-9)),
    (&["c6h5nh2", "aniline"], "C6H5NH2", Kind::WeakBase(4.3e-10)),
];

const TYPICAL_PH: &[(&[&str], f64, &str)] = &[
    (&["water", "pure water", "distilled water"], 7.0, "neutral at 25 °C"),
    (&["battery acid"], 0.8, "about 1 M sulfuric acid"),
    (&["stomach acid", "gastric acid"], 1.5, "typically 1.5 to 3.5"),
    (&["lemon juice", "lemon"], 2.2, "typically 2.0 to 2.6"),
    (&["vinegar"], 2.5, "typically 2.4 to 3.4"),
    (&["cola", "coke", "soda"], 2.5, "typically 2.4 to 2.8"),
    (&["orange juice"], 3.5, "typically 3.3 to 4.2"),
    (&["wine"], 3.5, "typically 3 to 4"),
    (&["tomato juice", "tomato"], 4.2, "typically 4.1 to 4.6"),
    (&["coffee", "black coffee"], 5.0, "typically 4.8 to 5.1"),
    (&["rain", "rainwater", "rain water"], 5.6, "unpolluted rain, from dissolved CO₂"),
    (&["urine"], 6.0, "typically 4.5 to 8"),
    (&["milk"], 6.6, "typically 6.5 to 6.8"),
    (&["saliva"], 6.8, "typically 6.2 to 7.6"),
    (&["blood", "human blood"], 7.4, "tightly regulated between 7.35 and 7.45"),
    (&["seawater", "sea water", "ocean water", "the ocean"], 8.1, "surface average"),
    (&["baking soda", "baking soda solution"], 8.3, "dilute solution"),
    (&["soap", "soapy water", "hand soap"], 10.0, "typically 9 to 10"),
    (&["milk of magnesia"], 10.5, "saturated magnesium hydroxide"),
    (&["household ammonia", "ammonia solution"], 11.5, "typically 11 to 12"),
    (&["bleach", "household bleach"], 12.5, "typically 11 to 13"),
    (&["drain cleaner", "lye"], 14.0, "concentrated sodium hydroxide"),
];

fn acidity(ph: f64) -> &'static str {
    match ph {
        p if p < 3.0 => "strongly acidic",
        p if p < 6.5 => "acidic",
        p if p <= 7.5 => "about neutral",
        p if p <= 11.0 => "basic",
        _ => "strongly basic",
    }
}

fn ph_pods(label: String, h: f64, how: Vec<String>, want_poh: bool) -> Answer {
    const KW: f64 = 1.0e-14;
    let (ph, oh) = (-h.log10(), KW / h);
    let poh = 14.0 - ph;
    let mut pods = vec![pod("Input", "Input interpretation", text(label.clone()))];
    let ph_pod = pod("Result", "pH", text(format!("{}  ({})", fmt_float(ph, 4), acidity(ph))));
    let poh_pod = pod("POH", "pOH", text(fmt_float(poh, 4)));
    if want_poh {
        pods.push(primary(poh_pod));
        pods.push(ph_pod);
    } else {
        pods.push(primary(ph_pod));
        pods.push(poh_pod);
    }
    pods.push(pod("Ions", "Ion concentrations", table(None, vec![vec![text("[H⁺]"), text(format!("{} mol/L", fmt_float(h, 4)))], vec![text("[OH⁻]"), text(format!("{} mol/L", fmt_float(oh, 4)))]])));
    if !how.is_empty() {
        pods.push(pod("Method", "Method", text(how.join("\n"))));
    }
    Answer::new(label, pods)
}

fn ph(l: &str) -> Option<Answer> {
    const KW: f64 = 1.0e-14;
    let number = |s: &str| -> Option<f64> {
        let t = s.trim().replace(' ', "").replace("×10^", "e").replace("x10^", "e").replace("*10^", "e");
        t.parse::<f64>().ok().filter(|v| *v > 0.0)
    };
    // from an ion concentration
    if let Some(m) = re!(r"^(ph|poh)\s+(?:of\s+|for\s+|when\s+|if\s+|with\s+)?(?:a\s+)?(?:solution\s+with\s+)?(\[h\+\]|\[h3o\+\]|\[oh-\]|h\+|oh-|hydrogen ion concentration|hydronium concentration|hydroxide concentration|hydroxide ion concentration)\s*(?:=|is|of)?\s*([\d.]+(?:\s*(?:e|[×x*]\s*10\^)\s*-?\d+)?)\s*(?:m|molar|mol/l)?$").captures(l) {
        let c = number(&m[3])?;
        let hydroxide = m[2].contains("oh") || m[2].contains("hydroxide");
        let h = if hydroxide { KW / c } else { c };
        let how = vec![if hydroxide { format!("pOH = −log₁₀[OH⁻] = {},  pH = 14 − pOH", fmt_float(-c.log10(), 4)) } else { "pH = −log₁₀[H⁺]".to_string() }];
        return Some(ph_pods(format!("{} = {} mol/L", if hydroxide { "[OH⁻]" } else { "[H⁺]" }, fmt_float(c, 4)), h, how, &m[1] == "poh"));
    }
    // concentration from a pH
    if let Some(m) = re!(r"^(?:\[h\+\]|\[h3o\+\]|h\+ concentration|hydrogen ion concentration|hydronium concentration|concentration of h\+)\s+(?:of|for|at|when|if)?\s*(?:a\s+)?(?:solution\s+(?:with|of)\s+)?ph\s*(?:=|of|is)?\s*(-?[\d.]+)$").captures(l) {
        let p: f64 = m[1].parse().ok()?;
        let mut a = ph_pods(format!("solution with pH {p}"), 10f64.powf(-p), vec!["[H⁺] = 10^(−pH)".into()], false);
        // lead with the concentration
        if let Some(k) = a.pods.iter().position(|x| x.id == "Ions") {
            a.pods.iter_mut().for_each(|x| x.primary = false);
            a.pods[k].primary = true;
        }
        return Some(a);
    }
    let m = re!(r"^(ph|poh)\s+(?:of\s+)?(?:a\s+|an\s+)?(?:([\d.]+(?:e-?\d+)?)\s*(mm|m|molar|millimolar|mol/l)\s+)?(?:solution\s+of\s+|aqueous\s+)?(.+?)(?:\s+solution)?(?:\s+(?:at|with concentration|of concentration)\s+([\d.]+(?:e-?\d+)?)\s*(mm|m|molar|millimolar|mol/l))?$").captures(l)?;
    let name = m[4].trim();
    let conc = match (m.get(2), m.get(5)) {
        (Some(v), _) => Some(number(v.as_str())? * if matches!(&m[3], "mm" | "millimolar") { 1e-3 } else { 1.0 }),
        (_, Some(v)) => Some(number(v.as_str())? * if matches!(&m[6], "mm" | "millimolar") { 1e-3 } else { 1.0 }),
        _ => None,
    };
    let Some(c) = conc else {
        let (names, value, note) = TYPICAL_PH.iter().find(|(names, ..)| names.contains(&name))?;
        let mut a = ph_pods(format!("{} | typical pH", names[0]), 10f64.powf(-value), vec![], &m[1] == "poh");
        a.pods.push(pod("Note", "Note", text(format!("Typical value: {note}."))));
        return Some(a);
    };
    let (_, formula, kind) = ACIDS.iter().find(|(names, ..)| names.contains(&name))?;
    let f = pretty(formula);
    let (h, how): (f64, Vec<String>) = match *kind {
        Kind::StrongAcid(k) => {
            let a = c * k as f64;
            // include the autoionization of water for very dilute solutions
            let h = (a + (a * a + 4.0 * KW).sqrt()) / 2.0;
            (h, vec![format!("{f} is a strong acid and dissociates completely: [H⁺] = {} mol/L", fmt_float(a, 4)), "pH = −log₁₀[H⁺]".into()])
        }
        Kind::Sulfuric => {
            let ka2 = 1.2e-2;
            let x = (-(c + ka2) + ((c + ka2).powi(2) + 4.0 * ka2 * c).sqrt()) / 2.0;
            (c + x, vec![format!("The first dissociation of {f} is complete; the second has Kₐ₂ = 0.012."), format!("Solving x(c + x)/(c − x) = Kₐ₂ gives x = {} mol/L, so [H⁺] = c + x = {} mol/L", fmt_float(x, 4), fmt_float(c + x, 4))])
        }
        Kind::StrongBase(k) => {
            let b = c * k as f64;
            let oh = (b + (b * b + 4.0 * KW).sqrt()) / 2.0;
            (KW / oh, vec![format!("{f} is a strong base and dissociates completely: [OH⁻] = {} mol/L", fmt_float(b, 4)), "pOH = −log₁₀[OH⁻],  pH = 14 − pOH".into()])
        }
        Kind::WeakAcid(ka) => {
            let x = (-ka + (ka * ka + 4.0 * ka * c).sqrt()) / 2.0;
            (x, vec![format!("{f} is a weak acid with Kₐ = {}", fmt_float(ka, 3)), format!("Solving x²/(c − x) = Kₐ gives [H⁺] = x = {} mol/L  ({}% ionized)", fmt_float(x, 4), fmt_float(100.0 * x / c, 3))])
        }
        Kind::WeakBase(kb) => {
            let x = (-kb + (kb * kb + 4.0 * kb * c).sqrt()) / 2.0;
            (KW / x, vec![format!("{f} is a weak base with K_b = {}", fmt_float(kb, 3)), format!("Solving x²/(c − x) = K_b gives [OH⁻] = x = {} mol/L  ({}% ionized)", fmt_float(x, 4), fmt_float(100.0 * x / c, 3)), "pH = 14 − pOH".into()])
        }
    };
    Some(ph_pods(format!("{} mol/L {f} in water at 25 °C", fmt_float(c, 6)), h, how, &m[1] == "poh"))
}

// ---------------------------------------------------------------------------
// moles, mass, particles, molarity
// ---------------------------------------------------------------------------

fn grams(v: f64, unit: &str) -> f64 {
    match unit.to_lowercase().as_str() {
        "kg" | "kilogram" | "kilograms" => v * 1000.0,
        "mg" | "milligram" | "milligrams" => v / 1000.0,
        _ => v,
    }
}

fn amount_pods(label: String, name: &str, counts: &BTreeMap<String, u32>, moles: f64, lead: &str) -> Answer {
    let mm = molar_mass(counts);
    let atoms: u32 = counts.values().sum();
    let rows = [
        ("Moles", "Amount of substance", format!("{} mol", n(moles))),
        ("Mass", "Mass", format!("{} g", n(moles * mm))),
        ("Molecules", if atoms == 1 { "Number of atoms" } else { "Number of molecules (formula units)" }, n(moles * AVOGADRO)),
    ];
    let mut pods = vec![pod("Input", "Input interpretation", text(label.clone()))];
    for (id, title, value) in &rows {
        let p = pod(id, title, text(value.clone()));
        if *id == lead {
            pods.insert(1, primary(p));
        } else {
            pods.push(p);
        }
    }
    if atoms > 1 {
        let p = pod("Atoms", "Total number of atoms", text(format!("{}  ({atoms} atoms per formula unit)", n(moles * AVOGADRO * atoms as f64))));
        if lead == "Atoms" {
            pods.insert(1, primary(p));
        } else {
            pods.push(p);
        }
    }
    pods.push(pod("MolarMass", "Molar mass", text(format!("{name}: {} g/mol", fmt_float(mm, 7)))));
    pods.push(pod("Method", "Method", text("moles = mass ÷ molar mass;  particles = moles × 6.02214076×10²³ (Avogadro constant)")));
    Answer::new(label, pods)
}

fn amounts(c: &str) -> Option<Answer> {
    const NUM: &str = r"([\d.]+(?:e-?\d+)?)";
    const MASS: &str = r"(g|grams?|kg|kilograms?|mg|milligrams?)";
    // moles (or particles) from a mass
    let r1 = Regex::new(&format!(r"(?i)^(?:how many\s+|number of\s+)?(moles?|mol|molecules|atoms|particles|formula units)\s+(?:are\s+)?(?:there\s+)?(?:in|of|from)\s+{NUM}\s*{MASS}\s+(?:of\s+)?(.+)$")).ok()?;
    let r2 = Regex::new(&format!(r"(?i)^(?:convert\s+)?{NUM}\s*{MASS}\s+(?:of\s+)?(.+?)\s+(?:in|to|into)\s+(moles?|mol|molecules|atoms)$")).ok()?;
    let from_mass = if let Some(m) = r1.captures(c) {
        Some((m[1].to_lowercase(), m[2].parse::<f64>().ok()?, m[3].to_string(), m[4].to_string()))
    } else {
        r2.captures(c).and_then(|m| Some((m[4].to_lowercase(), m[1].parse::<f64>().ok()?, m[2].to_string(), m[3].to_string())))
    };
    if let Some((want, v, unit, what)) = from_mass {
        let (name, _, counts) = substance(&what)?;
        let moles = grams(v, &unit) / molar_mass(&counts);
        let lead = if want.starts_with("mol") && !want.starts_with("molec") { "Moles" } else if want == "atoms" && counts.values().sum::<u32>() > 1 { "Atoms" } else { "Molecules" };
        return Some(amount_pods(format!("{v} {unit} of {name}"), &name, &counts, moles, lead));
    }
    // mass (or particles) from moles
    let r3 = Regex::new(&format!(r"(?i)^(?:how many\s+|number of\s+)?(mass|grams?|weight|molecules|atoms|particles|formula units)\s+(?:are\s+)?(?:there\s+)?(?:of|in)\s+{NUM}\s*(?:mol|moles?)\s+(?:of\s+)?(.+)$")).ok()?;
    let r4 = Regex::new(&format!(r"(?i)^(?:convert\s+)?{NUM}\s*(?:mol|moles?)\s+(?:of\s+)?(.+?)(?:\s+(?:in|to|into)\s+(grams?|g|kg|molecules|atoms|mass))?$")).ok()?;
    let from_moles = if let Some(m) = r3.captures(c) {
        Some((m[1].to_lowercase(), m[2].parse::<f64>().ok()?, m[3].to_string()))
    } else {
        r4.captures(c).and_then(|m| Some((m.get(3).map_or("mass".to_string(), |g| g.as_str().to_lowercase()), m[1].parse::<f64>().ok()?, m[2].to_string())))
    };
    if let Some((want, moles, what)) = from_moles {
        let (name, _, counts) = substance(&what)?;
        let lead = if want == "atoms" && counts.values().sum::<u32>() > 1 { "Atoms" } else if matches!(want.as_str(), "molecules" | "atoms" | "particles" | "formula units") { "Molecules" } else { "Mass" };
        return Some(amount_pods(format!("{moles} mol of {name}"), &name, &counts, moles, lead));
    }
    // molarity
    let r5 = Regex::new(&format!(r"(?i)^(?:molarity|concentration)\s+of\s+{NUM}\s*(g|grams?|kg|mg|mol|moles?)\s+(?:of\s+)?(.+?)\s+(?:dissolved\s+)?in\s+{NUM}\s*(l|ml|liters?|litres?|milliliters?|millilitres?)(?:\s+of\s+(?:water|solution))?$")).ok()?;
    if let Some(m) = r5.captures(c) {
        let (name, _, counts) = substance(&m[3])?;
        let v: f64 = m[1].parse().ok()?;
        let moles = if m[2].to_lowercase().starts_with("mol") { v } else { grams(v, &m[2]) / molar_mass(&counts) };
        let vol: f64 = m[4].parse().ok()?;
        let litres = if m[5].to_lowercase().starts_with('m') { vol / 1000.0 } else { vol };
        let label = format!("{} {} of {name} in {} {}", &m[1], &m[2], &m[4], &m[5]);
        let pods = vec![
            pod("Input", "Input interpretation", text(label.clone())),
            primary(pod("Result", "Molarity", text(format!("{} mol/L", n(moles / litres))))),
            pod("Moles", "Amount of solute", text(format!("{} mol  ({} g)", n(moles), n(moles * molar_mass(&counts))))),
            pod("MolarMass", "Molar mass", text(format!("{name}: {} g/mol", fmt_float(molar_mass(&counts), 7)))),
            pod("Method", "Method", text("molarity = moles of solute ÷ litres of solution")),
        ];
        return Some(Answer::new(label, pods));
    }
    None
}

pub fn handle(q: &Query) -> Option<Answer> {
    let l = q.lower.trim();
    let c = q.clean.trim();
    properties(l).or_else(|| ph(l)).or_else(|| amounts(c)).or_else(|| if l.starts_with("balance") || c.contains("->") || c.contains('→') || c.contains("=>") { balance(c) } else { None })
}
