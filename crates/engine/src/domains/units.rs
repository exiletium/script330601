//! Units of measurement, conversions and physical constants.

use crate::engine::*;
use crate::print::fmt_float;
use regex::Regex;
use std::sync::OnceLock;

/// Exponents of (length, mass, time, current, temperature, amount,
/// luminous intensity, information).
pub type Dim = [i8; 8];

const NONE: Dim = [0; 8];

#[derive(Clone, Debug)]
pub struct Quantity {
    pub value: f64,
    pub dim: Dim,
}

#[derive(Clone, Copy)]
struct UnitDef {
    names: &'static [&'static str],
    factor: f64,
    dim: Dim,
    /// Affine offset (temperatures): SI = (v + offset) * factor.
    offset: f64,
    prefixable: bool,
    long: &'static str,
}

const fn d(l: i8, m: i8, t: i8, i: i8, k: i8, n: i8, j: i8, b: i8) -> Dim {
    [l, m, t, i, k, n, j, b]
}

macro_rules! u {
    ($names:expr, $f:expr, $dim:expr, $long:expr) => {
        UnitDef { names: $names, factor: $f, dim: $dim, offset: 0.0, prefixable: false, long: $long }
    };
    ($names:expr, $f:expr, $dim:expr, $long:expr, p) => {
        UnitDef { names: $names, factor: $f, dim: $dim, offset: 0.0, prefixable: true, long: $long }
    };
}

const L: Dim = d(1, 0, 0, 0, 0, 0, 0, 0);
const M: Dim = d(0, 1, 0, 0, 0, 0, 0, 0);
const T: Dim = d(0, 0, 1, 0, 0, 0, 0, 0);
const AREA: Dim = d(2, 0, 0, 0, 0, 0, 0, 0);
const VOL: Dim = d(3, 0, 0, 0, 0, 0, 0, 0);
const SPEED: Dim = d(1, 0, -1, 0, 0, 0, 0, 0);
const ACC: Dim = d(1, 0, -2, 0, 0, 0, 0, 0);
const FORCE: Dim = d(1, 1, -2, 0, 0, 0, 0, 0);
const ENERGY: Dim = d(2, 1, -2, 0, 0, 0, 0, 0);
const POWER: Dim = d(2, 1, -3, 0, 0, 0, 0, 0);
const PRESSURE: Dim = d(-1, 1, -2, 0, 0, 0, 0, 0);
const FREQ: Dim = d(0, 0, -1, 0, 0, 0, 0, 0);
const CHARGE: Dim = d(0, 0, 1, 1, 0, 0, 0, 0);
const VOLT: Dim = d(2, 1, -3, -1, 0, 0, 0, 0);
const OHM: Dim = d(2, 1, -3, -2, 0, 0, 0, 0);
const TEMP: Dim = d(0, 0, 0, 0, 1, 0, 0, 0);
const BITS: Dim = d(0, 0, 0, 0, 0, 0, 0, 1);

static UNITS: &[UnitDef] = &[
    // length
    u!(&["m", "meter", "meters", "metre", "metres"], 1.0, L, "meters", p),
    u!(&["in", "inch", "inches", "\""], 0.0254, L, "inches"),
    u!(&["ft", "foot", "feet", "'"], 0.3048, L, "feet"),
    u!(&["yd", "yard", "yards"], 0.9144, L, "yards"),
    u!(&["mi", "mile", "miles"], 1609.344, L, "miles"),
    u!(&["nmi", "nautical mile", "nautical miles"], 1852.0, L, "nautical miles"),
    u!(&["au", "astronomical unit", "astronomical units"], 1.495978707e11, L, "astronomical units"),
    u!(&["ly", "light year", "light years", "lightyear", "lightyears", "light-year", "light-years"], 9.4607304725808e15, L, "light years"),
    u!(&["pc", "parsec", "parsecs"], 3.0856775814913673e16, L, "parsecs", p),
    u!(&["å", "angstrom", "angstroms"], 1e-10, L, "ångströms"),
    u!(&["furlong", "furlongs"], 201.168, L, "furlongs"),
    // mass
    u!(&["g", "gram", "grams", "gramme"], 1e-3, M, "grams", p),
    u!(&["t", "tonne", "tonnes", "metric ton", "metric tons"], 1000.0, M, "metric tons"),
    u!(&["lb", "lbs", "pound", "pounds"], 0.45359237, M, "pounds"),
    u!(&["oz", "ounce", "ounces"], 0.028349523125, M, "ounces"),
    u!(&["st", "stone", "stones"], 6.35029318, M, "stone"),
    u!(&["ton", "tons", "short ton", "short tons"], 907.18474, M, "short tons"),
    u!(&["u", "amu", "dalton", "daltons", "da"], 1.66053906660e-27, M, "atomic mass units"),
    u!(&["ct", "carat", "carats"], 2e-4, M, "carats"),
    // time
    u!(&["s", "sec", "secs", "second", "seconds"], 1.0, T, "seconds", p),
    u!(&["min", "mins", "minute", "minutes"], 60.0, T, "minutes"),
    u!(&["h", "hr", "hrs", "hour", "hours"], 3600.0, T, "hours"),
    u!(&["day", "days", "d"], 86400.0, T, "days"),
    u!(&["wk", "week", "weeks"], 604800.0, T, "weeks"),
    u!(&["month", "months"], 2629746.0, T, "months"),
    u!(&["yr", "year", "years", "a"], 31556952.0, T, "years"),
    u!(&["decade", "decades"], 315569520.0, T, "decades"),
    u!(&["century", "centuries"], 3155695200.0, T, "centuries"),
    // area & volume
    u!(&["ha", "hectare", "hectares"], 1e4, AREA, "hectares"),
    u!(&["acre", "acres", "ac"], 4046.8564224, AREA, "acres"),
    u!(&["l", "L", "liter", "liters", "litre", "litres"], 1e-3, VOL, "liters", p),
    u!(&["gal", "gallon", "gallons"], 3.785411784e-3, VOL, "US gallons"),
    u!(&["qt", "quart", "quarts"], 9.46352946e-4, VOL, "US quarts"),
    u!(&["pt", "pint", "pints"], 4.73176473e-4, VOL, "US pints"),
    u!(&["cup", "cups"], 2.365882365e-4, VOL, "US cups"),
    u!(&["floz", "fl oz", "fluid ounce", "fluid ounces"], 2.95735295625e-5, VOL, "US fluid ounces"),
    u!(&["tbsp", "tablespoon", "tablespoons"], 1.478676478125e-5, VOL, "tablespoons"),
    u!(&["tsp", "teaspoon", "teaspoons"], 4.92892159375e-6, VOL, "teaspoons"),
    // speed
    u!(&["mph"], 0.44704, SPEED, "miles per hour"),
    u!(&["kph", "kmh"], 1.0 / 3.6, SPEED, "kilometers per hour"),
    u!(&["kn", "knot", "knots", "kt"], 1852.0 / 3600.0, SPEED, "knots"),
    u!(&["mach"], 340.29, SPEED, "Mach (sea level)"),
    u!(&["c", "speed of light"], 299792458.0, SPEED, "speed of light"),
    // acceleration / force
    u!(&["gee", "g0", "standard gravity"], 9.80665, ACC, "standard gravities"),
    u!(&["n", "newton", "newtons"], 1.0, FORCE, "newtons", p),
    u!(&["lbf", "pound force", "pounds force"], 4.4482216152605, FORCE, "pounds-force"),
    u!(&["dyn", "dyne", "dynes"], 1e-5, FORCE, "dynes"),
    // energy
    u!(&["j", "joule", "joules"], 1.0, ENERGY, "joules", p),
    u!(&["cal", "calorie", "calories"], 4.184, ENERGY, "calories", p),
    u!(&["kcal", "kilocalorie", "kilocalories", "food calorie", "food calories", "cal_food"], 4184.0, ENERGY, "kilocalories"),
    u!(&["ev", "electronvolt", "electronvolts", "electron volt", "electron volts"], 1.602176634e-19, ENERGY, "electronvolts", p),
    u!(&["wh", "watt hour", "watt hours", "watt-hour", "watt-hours"], 3600.0, ENERGY, "watt hours", p),
    u!(&["btu", "btus"], 1055.05585262, ENERGY, "British thermal units"),
    u!(&["erg", "ergs"], 1e-7, ENERGY, "ergs"),
    u!(&["tnt", "ton of tnt", "tons of tnt"], 4.184e9, ENERGY, "tons of TNT"),
    // power
    u!(&["w", "watt", "watts"], 1.0, POWER, "watts", p),
    u!(&["hp", "horsepower"], 745.69987158227022, POWER, "horsepower"),
    // pressure
    u!(&["pa", "pascal", "pascals"], 1.0, PRESSURE, "pascals", p),
    u!(&["bar", "bars"], 1e5, PRESSURE, "bars", p),
    u!(&["atm", "atmosphere", "atmospheres"], 101325.0, PRESSURE, "atmospheres"),
    u!(&["psi"], 6894.757293168, PRESSURE, "pounds per square inch"),
    u!(&["mmhg", "torr"], 133.322387415, PRESSURE, "millimeters of mercury"),
    // electromagnetism and others
    u!(&["hz", "hertz"], 1.0, FREQ, "hertz", p),
    u!(&["rpm"], 1.0 / 60.0, FREQ, "revolutions per minute"),
    u!(&["a", "amp", "amps", "ampere", "amperes"], 1.0, d(0, 0, 0, 1, 0, 0, 0, 0), "amperes", p),
    u!(&["c", "coulomb", "coulombs"], 1.0, CHARGE, "coulombs", p),
    u!(&["v", "volt", "volts"], 1.0, VOLT, "volts", p),
    u!(&["ω", "ohm", "ohms"], 1.0, OHM, "ohms", p),
    u!(&["f", "farad", "farads"], 1.0, d(-2, -1, 4, 2, 0, 0, 0, 0), "farads", p),
    u!(&["t", "tesla", "teslas"], 1.0, d(0, 1, -2, -1, 0, 0, 0, 0), "teslas", p),
    u!(&["mol", "mole", "moles"], 1.0, d(0, 0, 0, 0, 0, 1, 0, 0), "moles", p),
    u!(&["cd", "candela"], 1.0, d(0, 0, 0, 0, 0, 0, 1, 0), "candelas", p),
    // temperature (absolute scales)
    u!(&["k", "kelvin", "kelvins"], 1.0, TEMP, "kelvins", p),
    UnitDef { names: &["°c", "c", "celsius", "degc", "degrees celsius", "degree celsius", "centigrade"], factor: 1.0, dim: TEMP, offset: 273.15, prefixable: false, long: "degrees Celsius" },
    UnitDef { names: &["°f", "f", "fahrenheit", "degf", "degrees fahrenheit", "degree fahrenheit"], factor: 5.0 / 9.0, dim: TEMP, offset: 459.67, prefixable: false, long: "degrees Fahrenheit" },
    UnitDef { names: &["°r", "rankine"], factor: 5.0 / 9.0, dim: TEMP, offset: 0.0, prefixable: false, long: "degrees Rankine" },
    // information
    u!(&["bit", "bits", "b"], 1.0, BITS, "bits", p),
    u!(&["byte", "bytes", "B"], 8.0, BITS, "bytes", p),
    // angles (dimensionless)
    u!(&["rad", "radian", "radians"], 1.0, NONE, "radians"),
    u!(&["deg", "degree", "degrees", "°"], std::f64::consts::PI / 180.0, NONE, "degrees"),
    u!(&["rev", "revolution", "revolutions", "turn", "turns"], std::f64::consts::TAU, NONE, "revolutions"),
    u!(&["percent", "%"], 0.01, NONE, "percent"),
];

const PREFIXES: &[(&str, &str, f64)] = &[
    ("Y", "yotta", 1e24),
    ("Z", "zetta", 1e21),
    ("E", "exa", 1e18),
    ("P", "peta", 1e15),
    ("T", "tera", 1e12),
    ("G", "giga", 1e9),
    ("M", "mega", 1e6),
    ("k", "kilo", 1e3),
    ("h", "hecto", 1e2),
    ("da", "deca", 1e1),
    ("d", "deci", 1e-1),
    ("c", "centi", 1e-2),
    ("m", "milli", 1e-3),
    ("µ", "micro", 1e-6),
    ("u", "micro", 1e-6),
    ("n", "nano", 1e-9),
    ("p", "pico", 1e-12),
    ("f", "femto", 1e-15),
    ("a", "atto", 1e-18),
    ("Ki", "kibi", 1024.0),
    ("Mi", "mebi", 1048576.0),
    ("Gi", "gibi", 1073741824.0),
    ("Ti", "tebi", 1099511627776.0),
];

/// A resolved unit (possibly prefixed).
#[derive(Clone, Debug)]
pub struct Unit {
    pub factor: f64,
    pub dim: Dim,
    pub offset: f64,
    pub name: String,
}

fn lookup_word(w: &str) -> Option<Unit> {
    let lw = w.to_lowercase();
    // exact (case-insensitive for multi-letter names, sensitive for B/b)
    for u in UNITS {
        for n in u.names {
            let hit = if n.len() <= 2 { *n == w || (*n == lw && !matches!(w, "B" | "b" | "M" | "G" | "T" | "P")) } else { *n == lw };
            if hit {
                return Some(Unit { factor: u.factor, dim: u.dim, offset: u.offset, name: u.long.to_string() });
            }
        }
    }
    // prefixed symbols (case-sensitive) and long names
    for (sym, long, f) in PREFIXES {
        for (pre, rest) in [(*sym, w.strip_prefix(sym)), (*long, lw.strip_prefix(long))] {
            let Some(rest) = rest else { continue };
            if rest.is_empty() {
                continue;
            }
            for u in UNITS.iter().filter(|u| u.prefixable) {
                let hit = if pre == *sym { u.names.iter().any(|n| *n == rest || (n.len() > 1 && *n == rest.to_lowercase())) } else { u.names.iter().any(|n| *n == rest) };
                // binary prefixes only for information units
                if hit && (!sym.ends_with('i') || u.dim == BITS) {
                    let lname = if pre == *sym { format!("{long}{}", u.long) } else { format!("{long}{}", u.long) };
                    return Some(Unit { factor: u.factor * f, dim: u.dim, offset: 0.0, name: lname });
                }
            }
        }
    }
    None
}

pub fn singular(name: &str) -> String {
    let (head, tail) = match name.split_once(' ') {
        Some((h, t)) if !h.ends_with('s') => return format!("{h} {}", singular(t)),
        Some((h, t)) => (h.to_string(), format!(" {t}")),
        None => (name.to_string(), String::new()),
    };
    let h = if head == "feet" {
        "foot".to_string()
    } else if let Some(b) = head.strip_suffix("ies") {
        format!("{b}y")
    } else if let Some(b) = head.strip_suffix("ches") {
        format!("{b}ch")
    } else if head.ends_with('s') && !head.ends_with("ss") && head != "hertz" && head != "celsius" {
        head[..head.len() - 1].to_string()
    } else {
        head
    };
    format!("{h}{tail}")
}

/// Temperature reading of single-letter unit names (`C`, `F`).
fn temperature_alt(s: &str) -> Option<Unit> {
    match s.trim() {
        "C" | "c" | "°C" => lookup_word("celsius"),
        "F" | "f" | "°F" => lookup_word("fahrenheit"),
        "K" | "k" => lookup_word("kelvin"),
        _ => None,
    }
}

/// Parse a unit expression: `km/h`, `kg m/s^2`, `m^2`, `N·m`, `square feet`.
pub fn parse_unit(s: &str) -> Option<Unit> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    if let Some(u) = lookup_word(s) {
        return Some(u);
    }
    // natural-language powers and "per"
    let mut t = format!(" {} ", s.to_lowercase().replace(" per ", " / ").replace('·', " ").replace('*', " "));
    for (w, p) in [("square ", "^2"), ("sq ", "^2"), ("cubic ", "^3"), ("cu ", "^3")] {
        while let Some(k) = t.find(w) {
            // move the power after the next word
            let after = &t[k + w.len()..];
            let end = after.find([' ', '/']).unwrap_or(after.len());
            let word = after[..end].to_string();
            t = format!("{}{}{}{}", &t[..k], word, p, &after[end..]);
        }
    }
    // keep original case for symbols when possible
    let src = if t.trim() == s.to_lowercase() { s.to_string() } else { t.trim().to_string() };
    let mut factor = 1.0;
    let mut dim = NONE;
    let mut name_parts = Vec::new();
    let mut sign = 1i8;
    for tok in src.split_inclusive('/') {
        let (body, slash) = match tok.strip_suffix('/') {
            Some(b) => (b, true),
            None => (tok, false),
        };
        for w in body.split_whitespace() {
            let (base, pw) = match w.split_once('^') {
                Some((b, p)) => (b, p.parse::<i8>().ok()?),
                None => {
                    let digits: String = w.chars().rev().take_while(|c| c.is_ascii_digit()).collect();
                    if !digits.is_empty() && digits.len() < w.len() && !w.chars().next()?.is_ascii_digit() {
                        (&w[..w.len() - digits.len()], digits.chars().rev().collect::<String>().parse().ok()?)
                    } else {
                        (w, 1)
                    }
                }
            };
            let u = lookup_word(base)?;
            if u.offset != 0.0 && (pw != 1 || src.contains('/')) {
                return None;
            }
            let p = pw * sign;
            factor *= u.factor.powi(p as i32);
            for k in 0..8 {
                dim[k] += u.dim[k] * p;
            }
            let word = |n: &str, k: i8| -> String {
                match k {
                    1 => n.to_string(),
                    2 => format!("{n} squared"),
                    3 => format!("{n} cubed"),
                    _ => format!("{n}^{k}"),
                }
            };
            if p > 0 {
                name_parts.push(word(&u.name, p));
            } else {
                let single = singular(&u.name);
                name_parts.push(format!("per {}", word(&single, -p)));
            }
        }
        if slash {
            sign = -1;
        }
    }
    if name_parts.is_empty() {
        return None;
    }
    Some(Unit { factor, dim, offset: 0.0, name: name_parts.join(" ") })
}

pub fn to_si(v: f64, u: &Unit) -> f64 {
    (v + u.offset) * u.factor
}

pub fn from_si(v: f64, u: &Unit) -> f64 {
    v / u.factor - u.offset
}

/// Readable SI unit for a dimension vector.
pub fn si_name(dim: &Dim) -> String {
    let named: &[(Dim, &str)] = &[(FORCE, "N"), (ENERGY, "J"), (POWER, "W"), (PRESSURE, "Pa"), (FREQ, "Hz"), (CHARGE, "C"), (VOLT, "V"), (OHM, "Ω")];
    for (dd, n) in named {
        if dd == dim {
            return n.to_string();
        }
    }
    let syms = ["m", "kg", "s", "A", "K", "mol", "cd", "bit"];
    let mut num = Vec::new();
    let mut den = Vec::new();
    for k in 0..8 {
        let p = dim[k];
        if p > 0 {
            num.push(if p == 1 { syms[k].to_string() } else { format!("{}^{}", syms[k], p) });
        } else if p < 0 {
            den.push(if p == -1 { syms[k].to_string() } else { format!("{}^{}", syms[k], -p) });
        }
    }
    match (num.is_empty(), den.is_empty()) {
        (true, true) => String::new(),
        (false, true) => num.join(" "),
        (true, false) => format!("1/{}", den.join(" ")),
        (false, false) => format!("{}/{}", num.join(" "), if den.len() > 1 { format!("({})", den.join(" ")) } else { den[0].clone() }),
    }
}

/// Units shown as "additional conversions" for a dimension.
fn related(dim: &Dim) -> &'static [&'static str] {
    match *dim {
        L => &["m", "km", "cm", "mm", "mi", "ft", "in", "yd", "nmi"],
        M => &["kg", "g", "lb", "oz", "t", "st"],
        T => &["s", "min", "h", "day", "week", "year", "ms"],
        AREA => &["m^2", "km^2", "ft^2", "acre", "ha", "mi^2"],
        VOL => &["L", "mL", "m^3", "gal", "qt", "cup", "floz"],
        SPEED => &["m/s", "km/h", "mph", "kn", "ft/s"],
        ENERGY => &["J", "kJ", "cal", "kcal", "kWh", "eV", "BTU"],
        POWER => &["W", "kW", "hp"],
        PRESSURE => &["Pa", "kPa", "bar", "atm", "psi", "mmHg"],
        TEMP => &["K", "°C", "°F"],
        BITS => &["bit", "B", "kB", "MB", "GB", "KiB", "MiB", "GiB"],
        FORCE => &["N", "kN", "lbf", "dyn"],
        _ => &[],
    }
}

fn re(s: &str) -> Regex {
    Regex::new(s).unwrap()
}

/// Parse "number unit" (also "5 ft 3 in").
pub fn parse_quantity(s: &str) -> Option<(f64, Unit, String)> {
    static R: OnceLock<Regex> = OnceLock::new();
    let r = R.get_or_init(|| re(r"^\s*([-+]?(?:\d[\d,]*\.?\d*|\.\d+)(?:[eE][-+]?\d+)?|[a-z ]+?)\s*(.+?)\s*$"));
    // compound imperial lengths: 5 ft 3 in, 5'3"
    static R2: OnceLock<Regex> = OnceLock::new();
    let r2 = R2.get_or_init(|| re(r#"^\s*(\d+(?:\.\d+)?)\s*(?:ft|feet|foot|')\s*(\d+(?:\.\d+)?)\s*(?:in|inches|inch|")?\s*$"#));
    if let Some(m) = r2.captures(s) {
        let ft: f64 = m[1].parse().ok()?;
        let inch: f64 = m[2].parse().ok()?;
        let u = lookup_word("in")?;
        return Some((ft * 12.0 + inch, u, format!("{ft} ft {inch} in")));
    }
    let m = r.captures(s)?;
    let num_s = m[1].trim();
    let v: f64 = if let Ok(v) = num_s.replace(',', "").parse() {
        v
    } else {
        let words = crate::parse::words_to_numbers(num_s);
        words.trim().parse().ok()?
    };
    let u = parse_unit(&m[2])?;
    let shown = format!("{} {}", fmt_float(v, 10), m[2].trim());
    Some((v, u, shown))
}

// ---------------------------------------------------------------------------
// physical constants
// ---------------------------------------------------------------------------

pub struct Constant {
    pub names: &'static [&'static str],
    pub symbol: &'static str,
    pub value: f64,
    pub unit: &'static str,
    pub exact: bool,
}

pub static CONSTANTS: &[Constant] = &[
    Constant { names: &["speed of light", "speed of light in vacuum", "c"], symbol: "c", value: 299792458.0, unit: "m/s", exact: true },
    Constant { names: &["planck constant", "planck's constant", "plancks constant", "h"], symbol: "h", value: 6.62607015e-34, unit: "J s", exact: true },
    Constant { names: &["reduced planck constant", "hbar", "h bar", "dirac constant"], symbol: "ħ", value: 1.054571817e-34, unit: "J s", exact: false },
    Constant { names: &["gravitational constant", "newton's gravitational constant", "newtonian constant of gravitation", "big g"], symbol: "G", value: 6.67430e-11, unit: "m^3/(kg s^2)", exact: false },
    Constant { names: &["avogadro constant", "avogadro's number", "avogadros number", "avogadro number"], symbol: "N_A", value: 6.02214076e23, unit: "1/mol", exact: true },
    Constant { names: &["boltzmann constant", "boltzmann's constant"], symbol: "k_B", value: 1.380649e-23, unit: "J/K", exact: true },
    Constant { names: &["elementary charge", "electron charge", "charge of an electron"], symbol: "e", value: 1.602176634e-19, unit: "C", exact: true },
    Constant { names: &["electron mass", "mass of an electron", "mass of electron"], symbol: "m_e", value: 9.1093837015e-31, unit: "kg", exact: false },
    Constant { names: &["proton mass", "mass of a proton", "mass of proton"], symbol: "m_p", value: 1.67262192369e-27, unit: "kg", exact: false },
    Constant { names: &["neutron mass", "mass of a neutron", "mass of neutron"], symbol: "m_n", value: 1.67492749804e-27, unit: "kg", exact: false },
    Constant { names: &["vacuum permittivity", "permittivity of free space", "electric constant", "epsilon 0"], symbol: "ε₀", value: 8.8541878128e-12, unit: "F/m", exact: false },
    Constant { names: &["vacuum permeability", "permeability of free space", "magnetic constant", "mu 0"], symbol: "μ₀", value: 1.25663706212e-6, unit: "N/A^2", exact: false },
    Constant { names: &["gas constant", "molar gas constant", "ideal gas constant", "universal gas constant"], symbol: "R", value: 8.314462618, unit: "J/(mol K)", exact: true },
    Constant { names: &["standard gravity", "standard acceleration of gravity", "acceleration due to gravity", "gravity on earth"], symbol: "g", value: 9.80665, unit: "m/s^2", exact: true },
    Constant { names: &["stefan-boltzmann constant", "stefan boltzmann constant"], symbol: "σ", value: 5.670374419e-8, unit: "W/(m^2 K^4)", exact: false },
    Constant { names: &["fine structure constant", "fine-structure constant"], symbol: "α", value: 7.2973525693e-3, unit: "", exact: false },
    Constant { names: &["rydberg constant"], symbol: "R_∞", value: 10973731.568160, unit: "1/m", exact: false },
    Constant { names: &["bohr radius"], symbol: "a₀", value: 5.29177210903e-11, unit: "m", exact: false },
    Constant { names: &["atomic mass unit", "unified atomic mass unit", "dalton"], symbol: "u", value: 1.66053906660e-27, unit: "kg", exact: false },
    Constant { names: &["faraday constant"], symbol: "F", value: 96485.33212, unit: "C/mol", exact: true },
    Constant { names: &["mass of the earth", "earth mass", "mass of earth"], symbol: "M_⊕", value: 5.9722e24, unit: "kg", exact: false },
    Constant { names: &["radius of the earth", "earth radius", "radius of earth"], symbol: "R_⊕", value: 6.3710e6, unit: "m", exact: false },
    Constant { names: &["mass of the sun", "solar mass", "mass of sun", "sun mass"], symbol: "M_☉", value: 1.98847e30, unit: "kg", exact: false },
    Constant { names: &["radius of the sun", "solar radius"], symbol: "R_☉", value: 6.957e8, unit: "m", exact: false },
    Constant { names: &["mass of the moon", "lunar mass", "moon mass"], symbol: "M_☾", value: 7.342e22, unit: "kg", exact: false },
    Constant { names: &["distance to the moon", "earth moon distance", "distance from earth to moon"], symbol: "", value: 3.84399e8, unit: "m", exact: false },
    Constant { names: &["distance to the sun", "earth sun distance", "distance from earth to sun"], symbol: "AU", value: 1.495978707e11, unit: "m", exact: true },
    Constant { names: &["speed of sound", "speed of sound in air"], symbol: "", value: 343.0, unit: "m/s", exact: false },
    Constant { names: &["absolute zero"], symbol: "", value: 0.0, unit: "K", exact: true },
];

pub fn find_constant(q: &str) -> Option<&'static Constant> {
    let q = q.trim().trim_start_matches("the ").trim_start_matches("value of ").trim();
    CONSTANTS.iter().find(|c| c.names.iter().any(|n| *n == q && n.len() > 1))
}

// ---------------------------------------------------------------------------
// handler
// ---------------------------------------------------------------------------

fn fmt_val(v: f64) -> String {
    fmt_float(v, 8)
}

fn conversion_table(si: f64, dim: &Dim, exclude: &str) -> Option<Pod> {
    let rows: Vec<Vec<Content>> = related(dim)
        .iter()
        .filter_map(|u| {
            let unit = parse_unit(u)?;
            if unit.name == exclude {
                return None;
            }
            Some(vec![text(fmt_val(from_si(si, &unit))), text(unit.name.clone())])
        })
        .take(8)
        .collect();
    if rows.is_empty() {
        return None;
    }
    Some(pod("AdditionalConversions", "Additional conversions", table(None, rows)))
}

pub fn handle(q: &Query) -> Option<Answer> {
    let l = q.lower.as_str();
    static CONV: OnceLock<Regex> = OnceLock::new();
    let conv = CONV.get_or_init(|| re(r"^(?:convert\s+)?(.+?)\s+(?:in|to|into|as|in terms of|=\s*\?|->|→)\s+(.+?)$"));
    // constants
    let const_q = l.trim_start_matches("what is ").trim_start_matches("value of ");
    if let Some(m) = conv.captures(const_q) {
        if let (Some(c), Some(u)) = (find_constant(&m[1]), parse_unit(&q.clean[q.clean.len() - m[2].len()..])) {
            let cu = parse_unit(c.unit)?;
            if cu.dim == u.dim {
                let v = from_si(to_si(c.value, &cu), &u);
                let pods = vec![pod("Input", "Input interpretation", text(format!("{} in {}", c.names[0], u.name))), primary(pod("Result", "Result", text(format!("{} {}", fmt_val(v), u.name))))];
                return Some(Answer::new(format!("{} in {}", c.names[0], u.name), pods));
            }
        }
    }
    if let Some(c) = find_constant(const_q) {
        if c.names[0] == "c" || const_q.len() <= 1 {
            return None;
        }
        let mut pods = vec![pod("Input", "Input interpretation", text(format!("{} {}", c.names[0], if c.symbol.is_empty() { String::new() } else { format!("({})", c.symbol) })))];
        let unit = if c.unit.is_empty() { String::new() } else { format!(" {}", c.unit) };
        pods.push(primary(pod("Value", "Value", text(format!("{}{}{}", fmt_float(c.value, 12), unit, if c.exact { " (exact)" } else { "" })))));
        if !c.unit.is_empty() {
            if let Some(cu) = parse_unit(c.unit) {
                if let Some(t) = conversion_table(to_si(c.value, &cu), &cu.dim, &cu.name) {
                    pods.push(t);
                }
            }
        }
        return Some(Answer::new(c.names[0], pods));
    }
    // "feet in a mile", "seconds in a day"
    static HOW: OnceLock<Regex> = OnceLock::new();
    let how = HOW.get_or_init(|| re(r"^(?:how many\s+)?([a-z°/ ]+?)\s+(?:in|per)\s+(?:a|an|one|1)\s+([a-z°/ ]+)$"));
    if let Some(m) = how.captures(l) {
        if let (Some(to), Some(from)) = (parse_unit(&m[1]), parse_unit(&m[2])) {
            if to.dim == from.dim && to.offset == 0.0 {
                let r = from.factor / to.factor;
                let pods = vec![pod("Input", "Input interpretation", text(format!("{} per {}", to.name, singular(&from.name)))), primary(pod("Result", "Result", text(format!("{} {}", fmt_val(r), to.name))))];
                return Some(Answer::new(format!("{} in 1 {}", to.name, singular(&from.name)), pods));
            }
        }
    }
    // conversions: try every "in/to" split point
    static SEP: OnceLock<Regex> = OnceLock::new();
    let sep = SEP.get_or_init(|| re(r"\s+(?:in|to|into|as|in terms of|->|→)\s+"));
    let mut split: Option<(String, String)> = None;
    let _ = sep;
    let words: Vec<(usize, &str)> = q.clean.split_whitespace().map(|w| (w.as_ptr() as usize - q.clean.as_ptr() as usize, w)).collect();
    for (start, w) in &words {
        if !matches!(w.to_lowercase().as_str(), "in" | "to" | "into" | "as" | "->" | "→") {
            continue;
        }
        let (a, b) = (&q.clean[..*start], &q.clean[start + w.len()..]);
        let (a, b) = (a.trim(), b.trim());
        let a = a.trim_start_matches("convert ").trim_start_matches("Convert ");
        if parse_quantity(a).is_some() && parse_unit(b).is_some() {
            split = Some((a.to_string(), b.to_string()));
            break;
        }
    }
    let _ = conv;
    if let Some((qs, us)) = split {
        let (qs, us) = (qs.as_str(), us.as_str());
        if let (Some((v, mut from, shown)), Some(mut to)) = (parse_quantity(qs), parse_unit(us)) {
            if from.dim != to.dim {
                let unit_word = qs.trim_start_matches(|c: char| c.is_ascii_digit() || c == '.' || c == '-' || c == ' ' || c == ',');
                if let (Some(a), Some(b)) = (temperature_alt(unit_word).or(Some(from.clone())), temperature_alt(us).or(Some(to.clone()))) {
                    if a.dim == b.dim {
                        from = a;
                        to = b;
                    }
                }
            }
            if from.dim != to.dim {
                return Some(Answer::new(
                    format!("convert {shown} to {}", to.name),
                    vec![pod("Input", "Input interpretation", text(format!("convert {shown} to {}", to.name))), primary(pod("Result", "Result", text(format!("Incompatible units: {} and {} measure different quantities.", from.name, to.name))))],
                ));
            }
            let si = to_si(v, &from);
            let mut r = from_si(si, &to);
            if r.abs() < 1e-9 * (1.0 + v.abs()) {
                r = 0.0;
            }
            let mut pods = vec![pod("Input", "Input interpretation", text(format!("convert {shown} ({}) to {}", from.name, to.name)))];
            pods.push(primary(pod("Result", "Result", text(format!("{} {}", fmt_val(r), to.name)))));
            if let Some(t) = conversion_table(si, &to.dim, &to.name) {
                pods.push(t);
            }
            if from.offset == 0.0 && to.offset == 0.0 {
                pods.push(pod("Factor", "Conversion factor", text(format!("1 {} = {} {}", singular(&from.name), fmt_val(from.factor / to.factor), to.name))));
            }
            return Some(Answer::new(format!("convert {shown} to {}", to.name), pods));
        }
    }
    // a bare quantity: "60 mph", "5 ft 3 in", "100 °F"
    if let Some((v, u, shown)) = parse_quantity(&q.clean) {
        if u.name.is_empty() || q.clean.split_whitespace().count() > 6 {
            return None;
        }
        // avoid stealing plain math like "2 x" or "3 a"
        let unit_part = q.clean.trim_start_matches(|c: char| c.is_ascii_digit() || c == '.' || c == ',' || c == ' ' || c == '-');
        if unit_part.len() <= 1 && !matches!(unit_part, "m" | "g" | "s" | "K" | "J" | "W" | "N" | "V" | "A" | "L" | "B" | "h") {
            return None;
        }
        let si = to_si(v, &u);
        let mut pods = vec![pod("Input", "Input interpretation", text(format!("{shown} ({})", u.name)))];
        let si_unit = si_name(&u.dim);
        pods.push(primary(pod("Result", "Value in SI units", text(format!("{} {}", fmt_val(si), si_unit)))));
        if let Some(t) = conversion_table(si, &u.dim, &u.name) {
            pods.push(t);
        }
        return Some(Answer::new(format!("{shown}"), pods));
    }
    None
}
