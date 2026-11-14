//! Offline facts: the solar system, notable mountains, rivers and
//! structures, country rankings and approximate economic data.

use super::geo;
use crate::engine::*;
use regex::Regex;
use std::sync::OnceLock;

macro_rules! re {
    ($s:expr) => {{
        static R: OnceLock<Regex> = OnceLock::new();
        R.get_or_init(|| Regex::new($s).unwrap())
    }};
}

pub struct Body {
    pub name: &'static str,
    pub kind: &'static str,
    /// kg
    pub mass: f64,
    /// mean radius, m
    pub radius: f64,
    /// semi-major axis of the orbit, m (around the Sun; the Moon: around Earth)
    pub orbit: f64,
    /// sidereal orbital period, days
    pub period: f64,
    /// sidereal rotation period, hours (negative = retrograde)
    pub rotation: f64,
    /// mean temperature, K (the Sun: effective surface temperature)
    pub temp: f64,
    pub moons: u32,
}

const fn b(name: &'static str, kind: &'static str, mass: f64, radius: f64, orbit: f64, period: f64, rotation: f64, temp: f64, moons: u32) -> Body {
    Body { name, kind, mass, radius, orbit, period, rotation, temp, moons }
}

pub static BODIES: &[Body] = &[
    b("Sun", "star", 1.98847e30, 6.957e8, 0.0, 0.0, 609.12, 5772.0, 0),
    b("Mercury", "planet", 3.3011e23, 2.4397e6, 5.791e10, 87.969, 1407.6, 440.0, 0),
    b("Venus", "planet", 4.8675e24, 6.0518e6, 1.0821e11, 224.701, -5832.5, 737.0, 0),
    b("Earth", "planet", 5.9722e24, 6.371e6, 1.495_978_707e11, 365.256, 23.9345, 288.0, 1),
    b("Moon", "moon of Earth", 7.342e22, 1.7374e6, 3.844e8, 27.3217, 655.72, 250.0, 0),
    b("Mars", "planet", 6.4171e23, 3.3895e6, 2.27956e11, 686.980, 24.6229, 210.0, 2),
    b("Jupiter", "planet", 1.89819e27, 6.9911e7, 7.7857e11, 4332.59, 9.925, 165.0, 95),
    b("Saturn", "planet", 5.6834e26, 5.8232e7, 1.43353e12, 10759.22, 10.656, 134.0, 146),
    b("Uranus", "planet", 8.6810e25, 2.5362e7, 2.87246e12, 30688.5, -17.24, 76.0, 28),
    b("Neptune", "planet", 1.02413e26, 2.4622e7, 4.49506e12, 60182.0, 16.11, 72.0, 16),
    b("Pluto", "dwarf planet", 1.303e22, 1.1883e6, 5.90638e12, 90560.0, -153.2928, 44.0, 5),
];

const BIG_G: f64 = 6.674_30e-11;
const C: f64 = 299_792_458.0;
const AU: f64 = 1.495_978_707e11;

fn sci(v: f64) -> String {
    crate::print::fmt_float(v, 6)
}

fn km(v: f64) -> String {
    let k = v / 1000.0;
    if k >= 1e7 {
        format!("{} km", sci(k))
    } else {
        format!("{} km", group(k.round() as u64))
    }
}

fn group(n: u64) -> String {
    let s = n.to_string();
    let mut out = String::new();
    for (k, ch) in s.chars().enumerate() {
        if k > 0 && (s.len() - k) % 3 == 0 {
            out.push(',');
        }
        out.push(ch);
    }
    out
}

fn body_word(w: &str) -> Option<&'static Body> {
    let w = w.trim().trim_start_matches("the ").trim_start_matches("planet ");
    BODIES.iter().find(|b| b.name.eq_ignore_ascii_case(w) || (b.name == "Moon" && w == "luna") || (b.name == "Sun" && w == "sol"))
}

/// The first solar-system body named in the text.
pub fn find_body(l: &str) -> Option<&'static Body> {
    let mut best: Option<(usize, &'static Body)> = None;
    for body in BODIES {
        let name = body.name.to_lowercase();
        if let Some(m) = Regex::new(&format!(r"\b{name}\b")).ok()?.find(l) {
            if best.map_or(true, |(at, _)| m.start() < at) {
                best = Some((m.start(), body));
            }
        }
    }
    best.map(|(_, b)| b)
}

pub fn find_two_bodies(l: &str) -> Option<(&'static Body, &'static Body)> {
    let mut found: Vec<(usize, &'static Body)> = Vec::new();
    for body in BODIES {
        let name = body.name.to_lowercase();
        if let Some(m) = Regex::new(&format!(r"\b{name}\b")).ok()?.find(l) {
            found.push((m.start(), body));
        }
    }
    found.sort_by_key(|(at, _)| *at);
    (found.len() == 2).then(|| (found[0].1, found[1].1))
}

/// Typical centre-to-centre distance between two bodies (m).
pub fn separation(a: &Body, c: &Body) -> Option<f64> {
    let is = |x: &Body, n: &str| x.name == n;
    if (is(a, "Moon") && is(c, "Earth")) || (is(a, "Earth") && is(c, "Moon")) {
        return Some(3.844e8);
    }
    if is(a, "Sun") {
        return Some(if is(c, "Moon") { AU } else { c.orbit });
    }
    if is(c, "Sun") {
        return Some(if is(a, "Moon") { AU } else { a.orbit });
    }
    None
}

fn gravity(x: &Body) -> f64 {
    BIG_G * x.mass / (x.radius * x.radius)
}

fn duration(days: f64) -> String {
    if days >= 730.0 {
        format!("{} years  ({} days)", sci(days / 365.25), sci(days))
    } else if days >= 2.0 {
        format!("{} days", sci(days))
    } else {
        format!("{} hours", sci(days * 24.0))
    }
}

fn rows(x: &Body) -> Vec<(&'static str, String)> {
    let vol = 4.0 / 3.0 * std::f64::consts::PI * x.radius.powi(3);
    let mut v = vec![
        ("type", x.kind.to_string()),
        ("mass", format!("{} kg{}", sci(x.mass), if x.name == "Earth" { String::new() } else { format!("  ({} Earth masses)", sci(x.mass / 5.9722e24)) })),
        ("mean radius", format!("{}{}", km(x.radius), if x.name == "Earth" { String::new() } else { format!("  ({} Earth radii)", sci(x.radius / 6.371e6)) })),
        ("diameter", km(2.0 * x.radius)),
        ("circumference", km(2.0 * std::f64::consts::PI * x.radius)),
        ("surface area", format!("{} km²", sci(4.0 * std::f64::consts::PI * x.radius * x.radius / 1e6))),
        ("volume", format!("{} km³", sci(vol / 1e9))),
        ("mean density", format!("{} kg/m³", sci(x.mass / vol))),
        ("surface gravity", format!("{} m/s²  ({} g)", sci(gravity(x)), sci(gravity(x) / 9.80665))),
        ("escape velocity", format!("{} km/s", sci((2.0 * BIG_G * x.mass / x.radius).sqrt() / 1000.0))),
        ("rotation period", format!("{}{}", duration(x.rotation.abs() / 24.0), if x.rotation < 0.0 { " (retrograde)" } else { "" })),
        (if x.name == "Sun" { "surface temperature" } else { "mean temperature" }, format!("{} K  ({} °C)", sci(x.temp), sci(x.temp - 273.15))),
    ];
    if x.orbit > 0.0 {
        let around = if x.name == "Moon" { "Earth" } else { "the Sun" };
        v.push((if x.name == "Moon" { "distance from Earth" } else { "distance from the Sun" }, if x.name == "Moon" { km(x.orbit) } else { format!("{}  ({} au)", km(x.orbit), sci(x.orbit / AU)) }));
        v.push(("orbital period", format!("{} around {around}", duration(x.period))));
        v.push(("light travel time", light_time(x.orbit)));
        v.push(("orbital speed", format!("{} km/s", sci(2.0 * std::f64::consts::PI * x.orbit / (x.period * 86400.0) / 1000.0))));
    }
    if x.kind.contains("planet") {
        v.push(("known moons", x.moons.to_string()));
    }
    v
}

fn light_time(d: f64) -> String {
    let s = d / C;
    if s < 120.0 {
        format!("{} seconds", sci(s))
    } else if s < 7200.0 {
        format!("{} minutes", sci(s / 60.0))
    } else {
        format!("{} hours", sci(s / 3600.0))
    }
}

fn body_answer(x: &'static Body, want: Option<&str>) -> Option<Answer> {
    let all = rows(x);
    let mut pods = vec![pod("Input", "Input interpretation", text(match want {
        Some(w) => format!("{} | {w}", x.name),
        None => format!("{} ({})", x.name, x.kind),
    }))];
    if let Some(w) = want {
        let (_, v) = all.iter().find(|(k, _)| *k == w)?;
        pods.push(primary(pod("Result", w, text(v.clone()))));
        pods.push(pod("Properties", "Other properties", table(None, all.iter().filter(|(k, _)| *k != w).map(|(k, v)| vec![text(*k), text(v.clone())]).collect())));
    } else {
        pods.push(primary(pod("Properties", "Properties", table(None, all.iter().map(|(k, v)| vec![text(*k), text(v.clone())]).collect()))));
    }
    Some(Answer::new(x.name, pods))
}

fn solar_system(l: &str) -> Option<Answer> {
    let prop_of = |w: &str| -> Option<&'static str> {
        Some(match w.trim() {
            "mass" | "weight" => "mass",
            "radius" | "mean radius" | "size" => "mean radius",
            "diameter" | "width" => "diameter",
            "circumference" => "circumference",
            "surface area" | "area" => "surface area",
            "volume" => "volume",
            "density" | "mean density" => "mean density",
            "gravity" | "surface gravity" | "gravitational acceleration" | "acceleration due to gravity" | "acceleration of gravity" => "surface gravity",
            "escape velocity" | "escape speed" => "escape velocity",
            "rotation period" | "day length" | "length of day" | "length of a day" | "day" | "sidereal day" => "rotation period",
            "temperature" | "surface temperature" | "mean temperature" | "average temperature" => "temperature",
            "orbital period" | "year length" | "length of year" | "length of a year" | "year" | "period" | "orbit" => "orbital period",
            "orbital speed" | "orbital velocity" | "speed" => "orbital speed",
            "moons" | "number of moons" | "satellites" => "known moons",
            "distance" | "distance from the sun" | "distance to the sun" | "distance from sun" | "semi-major axis" | "semimajor axis" | "orbital radius" | "distance from earth" | "distance to earth" => "distance",
            "type" | "kind" => "type",
            _ => return None,
        })
    };
    let resolve = |x: &'static Body, p: &'static str| -> &'static str {
        match p {
            "temperature" => if x.name == "Sun" { "surface temperature" } else { "mean temperature" },
            "distance" => if x.name == "Moon" { "distance from Earth" } else { "distance from the Sun" },
            other => other,
        }
    };
    // "<property> of|on <body>"
    if let Some(m) = re!(r"^(?:the\s+)?(?:average\s+|mean\s+)?([a-z -]+?)\s+(?:of|on|for|at)\s+(?:the\s+)?(?:planet\s+)?([a-z]+)$").captures(l) {
        if let (Some(p), Some(x)) = (prop_of(&m[1]), body_word(&m[2])) {
            return body_answer(x, Some(resolve(x, p)));
        }
    }
    // "<body> <property>", "<body>'s <property>"
    if let Some(m) = re!(r"^(?:the\s+)?([a-z]+)(?:'s|s')?\s+([a-z -]+)$").captures(l) {
        if let (Some(x), Some(p)) = (body_word(&m[1]), prop_of(&m[2])) {
            return body_answer(x, Some(resolve(x, p)));
        }
    }
    if let Some(m) = re!(r"^(?:moons (?:does|do)|(?:number of )?moons of)\s+(?:the\s+)?([a-z]+)(?:\s+have)?$").captures(l) {
        return body_answer(body_word(&m[1])?, Some("known moons"));
    }
    if let Some(m) = re!(r"^how (long|hot|cold|big|large|heavy|massive|far|far away) is\s+(?:a\s+|the\s+)?(?:(day|year)\s+on\s+)?(?:the\s+)?([a-z]+)(?:\s+from (?:the )?(earth|sun))?$").captures(l) {
        let x = body_word(&m[3])?;
        let p = match (&m[1], m.get(2).map(|g| g.as_str())) {
            ("long", Some("day")) => "rotation period",
            ("long", Some("year")) => "orbital period",
            ("hot" | "cold", _) => "temperature",
            ("big" | "large", _) => "diameter",
            ("heavy" | "massive", _) => "mass",
            ("far" | "far away", _) => {
                if m.get(4).map(|g| g.as_str()) == Some("earth") && x.name != "Moon" || (m.get(4).is_none() && x.kind == "planet") {
                    return distance(BODIES.iter().find(|q| q.name == "Earth")?, x);
                }
                "distance"
            }
            _ => return None,
        };
        return body_answer(x, Some(resolve(x, p)));
    }
    if let Some(m) = re!(r"^(?:the\s+)?distance\s+(?:from|between)\s+(?:the\s+)?([a-z]+)\s+(?:to|and)\s+(?:the\s+)?([a-z]+)$").captures(l) {
        return distance(body_word(&m[1])?, body_word(&m[2])?);
    }
    if let Some(m) = re!(r"^(?:how (?:much|many) (?:would|does|do)\s+)?(?:i weigh|(?:a\s+)?(?:person|someone|something)?\s*(?:of\s+|weighing\s+)?([\d.]+)\s*(kg|kilograms?|lbs?|pounds?)\s+weighs?)\s+on\s+(?:the\s+)?([a-z]+)$").captures(l) {
        let x = body_word(&m[3])?;
        let amount: f64 = m.get(1)?.as_str().parse().ok()?;
        let unit = &m[2];
        let ratio = gravity(x) / gravity(BODIES.iter().find(|q| q.name == "Earth")?);
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("weight on {} of something that weighs {amount} {unit} on Earth", x.name))),
            primary(pod("Result", "Result", text(format!("{} {unit}", sci(amount * ratio))))),
            pod("Ratio", "Surface gravity", text(format!("{} m/s², {} times Earth's", sci(gravity(x)), sci(ratio)))),
        ];
        return Some(Answer::new(format!("weight on {}", x.name), pods));
    }
    // a bare name
    if let Some(x) = body_word(l) {
        if l.trim() != "mercury" {
            return body_answer(x, None);
        }
    }
    if matches!(l.trim(), "planets" | "planets of the solar system" | "solar system" | "list of planets" | "the planets" | "planets in the solar system" | "planets in order") {
        let rows: Vec<Vec<Content>> = BODIES.iter().filter(|x| x.kind == "planet").map(|x| vec![text(x.name), text(format!("{} au", sci(x.orbit / AU))), text(km(2.0 * x.radius)), text(format!("{} Earths", sci(x.mass / 5.9722e24))), text(duration(x.period)), text(x.moons.to_string())]).collect();
        return Some(Answer::new("planets of the solar system", vec![pod("Input", "Input interpretation", text("planets of the solar system")), primary(pod("Result", "Planets", table(Some(vec!["planet", "distance from Sun", "diameter", "mass", "year", "moons"]), rows)))]));
    }
    None
}

fn distance(a: &'static Body, c: &'static Body) -> Option<Answer> {
    if a.name == c.name {
        return None;
    }
    let label = format!("distance between {} and {}", a.name, c.name);
    let mut pods = vec![pod("Input", "Input interpretation", text(label.clone()))];
    if let Some(d) = separation(a, c) {
        pods.push(primary(pod("Result", "Average distance", text(format!("{}  ({} miles{})", km(d), sci(d / 1609.344), if d > 1e10 { format!(", {} au", sci(d / AU)) } else { String::new() })))));
        pods.push(pod("Light", "Light travel time", text(light_time(d))));
    } else {
        // two bodies that both orbit the Sun: the distance varies
        let orbit = |x: &Body| if x.name == "Moon" { AU } else { x.orbit };
        let (ra, rc) = (orbit(a), orbit(c));
        let (near, far) = ((ra - rc).abs(), ra + rc);
        pods.push(primary(pod("Result", "Distance range", text(format!("about {} at closest approach to {} when on opposite sides of the Sun", km(near), km(far))))));
        pods.push(pod("AU", "In astronomical units", text(format!("{} au to {} au", sci(near / AU), sci(far / AU)))));
        pods.push(pod("Light", "Light travel time", text(format!("{} to {}", light_time(near), light_time(far)))));
        pods.push(pod("Note", "Note", text("Both bodies orbit the Sun, so their separation changes continuously; the range uses mean orbital radii.")));
    }
    Some(Answer::new(label, pods))
}

// ---------------------------------------------------------------------------
// landmarks
// ---------------------------------------------------------------------------

/// (names, kind, quantity label, value, unit, note)
const LANDMARKS: &[(&[&str], &str, &str, f64, &str, &str)] = &[
    (&["mount everest", "everest", "mt everest", "mt. everest"], "mountain", "height", 8848.86, "m", "Nepal / China; the highest point on Earth above sea level"),
    (&["k2"], "mountain", "height", 8611.0, "m", "Pakistan / China"),
    (&["kangchenjunga"], "mountain", "height", 8586.0, "m", "Nepal / India"),
    (&["mount kilimanjaro", "kilimanjaro"], "mountain", "height", 5895.0, "m", "Tanzania; highest in Africa"),
    (&["denali", "mount mckinley"], "mountain", "height", 6190.0, "m", "Alaska, USA; highest in North America"),
    (&["aconcagua"], "mountain", "height", 6961.0, "m", "Argentina; highest in South America"),
    (&["mont blanc"], "mountain", "height", 4806.0, "m", "France / Italy; highest in the Alps"),
    (&["mount elbrus", "elbrus"], "mountain", "height", 5642.0, "m", "Russia; highest in Europe"),
    (&["matterhorn"], "mountain", "height", 4478.0, "m", "Switzerland / Italy"),
    (&["mount fuji", "fuji", "mt fuji"], "mountain", "height", 3776.0, "m", "Japan"),
    (&["mauna kea"], "mountain", "height", 4207.0, "m", "Hawaii, USA; over 10,000 m from its base on the sea floor"),
    (&["olympus mons"], "mountain", "height", 21900.0, "m", "Mars; the tallest known volcano in the solar system"),
    (&["burj khalifa"], "building", "height", 828.0, "m", "Dubai; the tallest building in the world"),
    (&["eiffel tower"], "structure", "height", 330.0, "m", "Paris, France"),
    (&["empire state building"], "building", "height", 443.0, "m", "New York, USA (to the tip)"),
    (&["statue of liberty"], "structure", "height", 93.0, "m", "New York, USA (ground to torch)"),
    (&["big ben", "elizabeth tower"], "structure", "height", 96.0, "m", "London, United Kingdom"),
    (&["great pyramid of giza", "great pyramid"], "structure", "height", 138.5, "m", "Egypt (originally about 146.6 m)"),
    (&["nile", "nile river", "river nile"], "river", "length", 6650.0, "km", "Africa"),
    (&["amazon", "amazon river"], "river", "length", 6400.0, "km", "South America; the largest river by discharge"),
    (&["yangtze", "yangtze river"], "river", "length", 6300.0, "km", "China"),
    (&["mississippi", "mississippi river"], "river", "length", 3766.0, "km", "USA"),
    (&["danube", "danube river"], "river", "length", 2850.0, "km", "Europe"),
    (&["rhine", "rhine river"], "river", "length", 1233.0, "km", "Europe"),
    (&["thames", "river thames"], "river", "length", 346.0, "km", "England"),
    (&["great wall of china", "great wall"], "structure", "length", 21196.0, "km", "China (all sections, 2012 survey)"),
    (&["mariana trench", "marianas trench", "challenger deep"], "ocean trench", "depth", 10935.0, "m", "Pacific Ocean; the deepest known point of the oceans"),
    (&["lake baikal", "baikal"], "lake", "depth", 1642.0, "m", "Russia; the deepest lake in the world"),
    (&["grand canyon"], "canyon", "depth", 1857.0, "m", "Arizona, USA"),
    (&["equator"], "circle", "length", 40075.017, "km", "Earth's equatorial circumference"),
];

fn landmarks(l: &str) -> Option<Answer> {
    let m = re!(r"^(?:the\s+)?(?:how (tall|high|long|deep) is|(height|elevation|altitude|length|depth) of)\s+(?:the\s+)?(.+)$").captures(l)?;
    let name = m[3].trim();
    let hit = LANDMARKS.iter().find(|(names, ..)| names.contains(&name))?;
    let (v, unit) = (hit.3, hit.4);
    let (metres, feet_or_miles) = if unit == "m" { (v, format!("{} feet", group((v / 0.3048).round() as u64))) } else { (v * 1000.0, format!("{} miles", group((v / 1.609344).round() as u64))) };
    let _ = metres;
    let shown = if v.fract() == 0.0 { group(v as u64) } else { format!("{v}") };
    let pods = vec![
        pod("Input", "Input interpretation", text(format!("{} | {}", hit.0[0], hit.2))),
        primary(pod("Result", hit.2, text(format!("{shown} {unit}  ({feet_or_miles})")))),
        pod("About", "About", text(format!("{} — {}", hit.1, hit.5))),
    ];
    Some(Answer::new(format!("{} of {}", hit.2, hit.0[0]), pods))
}

// ---------------------------------------------------------------------------
// country rankings and approximate GDP
// ---------------------------------------------------------------------------

/// Nominal GDP in billions of US dollars (rounded, about 2023).
const GDP: &[(&str, f64)] = &[
    ("US", 27360.0), ("CN", 17790.0), ("DE", 4460.0), ("JP", 4210.0), ("IN", 3550.0), ("GB", 3340.0), ("FR", 3030.0), ("IT", 2250.0), ("BR", 2170.0), ("CA", 2140.0),
    ("RU", 2020.0), ("MX", 1790.0), ("AU", 1720.0), ("KR", 1710.0), ("ES", 1580.0), ("ID", 1370.0), ("NL", 1120.0), ("TR", 1110.0), ("SA", 1070.0), ("CH", 885.0),
    ("PL", 811.0), ("AR", 641.0), ("BE", 632.0), ("SE", 593.0), ("IE", 546.0), ("TH", 515.0), ("AT", 516.0), ("IL", 510.0), ("AE", 504.0), ("SG", 501.0),
    ("NO", 486.0), ("BD", 437.0), ("PH", 437.0), ("VN", 430.0), ("DK", 404.0), ("IR", 402.0), ("MY", 400.0), ("EG", 396.0), ("ZA", 378.0), ("CO", 364.0),
    ("NG", 363.0), ("RO", 351.0), ("PK", 338.0), ("CL", 336.0), ("CZ", 331.0), ("FI", 300.0), ("PT", 287.0), ("PE", 267.0), ("NZ", 253.0), ("GR", 238.0),
    ("HU", 212.0), ("UA", 179.0), ("ET", 164.0), ("KE", 107.0),
];

fn money(billions: f64) -> String {
    if billions >= 1000.0 {
        format!("${} trillion", crate::print::fmt_float(billions / 1000.0, 4))
    } else {
        format!("${} billion", crate::print::fmt_float(billions, 4))
    }
}

fn economy(l: &str) -> Option<Answer> {
    if let Some(m) = re!(r"^(?:the\s+)?(?:nominal\s+)?(gdp per capita|gdp|gross domestic product|economy)\s+(?:of|for|in)\s+(?:the\s+)?(.+)$").captures(l) {
        let c = geo::find_country(m[2].trim())?;
        let (_, g) = GDP.iter().find(|(code, _)| *code == c.iso2)?;
        let per = g * 1e9 / c.population as f64;
        let rank = GDP.iter().filter(|(_, v)| v > g).count() + 1;
        let mut pods = vec![pod("Input", "Input interpretation", text(format!("{} | nominal GDP", c.name)))];
        let total = pod("Result", "Gross domestic product", text(format!("≈ {} per year", money(*g))));
        let capita = pod("PerCapita", "GDP per capita", text(format!("≈ ${} per person per year", group(per.round() as u64))));
        if m[1].contains("capita") {
            pods.push(primary(capita));
            pods.push(total);
        } else {
            pods.push(primary(total));
            pods.push(capita);
        }
        pods.push(pod("Rank", "World rank", text(format!("about {rank}{} largest economy", ordinal(rank)))));
        pods.push(pod("Note", "Note", text("Rounded nominal figures for about 2023 from the built-in table; economic data changes every year.")));
        return Some(Answer::new(format!("GDP of {}", c.name), pods));
    }
    if re!(r"^(?:the\s+)?(?:largest|biggest|top|richest)\s+(?:\d+\s+)?(?:economies|economy|countries by gdp|country by gdp)(?:\s+in the world)?$").is_match(l) {
        let rows: Vec<Vec<Content>> = GDP.iter().take(10).enumerate().map(|(k, (code, g))| {
            let name = geo::countries().values().find(|c| c.iso2 == *code).map(|c| c.name.clone()).unwrap_or_else(|| code.to_string());
            vec![text((k + 1).to_string()), text(name), text(money(*g))]
        }).collect();
        return Some(Answer::new("largest economies", vec![pod("Input", "Input interpretation", text("largest economies by nominal GDP")), primary(pod("Result", "Largest economies", table(Some(vec!["rank", "country", "GDP (≈ 2023)"]), rows)))]));
    }
    None
}

fn ordinal(n: usize) -> &'static str {
    match (n % 10, n % 100) {
        (1, 11) | (2, 12) | (3, 13) => "th",
        (1, _) => "st",
        (2, _) => "nd",
        (3, _) => "rd",
        _ => "th",
    }
}

fn rankings(l: &str) -> Option<Answer> {
    let m = re!(r"^(?:the\s+)?(?:(\d+)\s+)?(largest|biggest|smallest|most populous|most populated|least populous|least populated|most densely populated|least densely populated)\s+(?:(\d+)\s+)?(countries|country|nations?|cit(?:y|ies))(?:\s+(?:in|of)\s+(?:the\s+)?(world|europe|asia|africa|north america|south america|oceania))?(?:\s+by\s+(area|population|size|land area))?(?:\s+in the world)?$").captures(l)?;
    let n: usize = m.get(1).or(m.get(3)).and_then(|g| g.as_str().parse().ok()).unwrap_or(if m[4].ends_with('y') || &m[4] == "nation" { 5 } else { 10 }).clamp(1, 50);
    let kind = &m[2];
    let region = m.get(5).map(|g| g.as_str()).filter(|r| *r != "world");
    let by = m.get(6).map(|g| g.as_str());
    if m[4].starts_with("cit") {
        let mut cs: Vec<&geo::City> = geo::cities().iter().collect();
        cs.sort_by(|a, c| c.population.cmp(&a.population));
        if kind.starts_with("small") || kind.starts_with("least") {
            return None;
        }
        let rows: Vec<Vec<Content>> = cs.iter().take(n).enumerate().map(|(k, c)| vec![text((k + 1).to_string()), text(format!("{}, {}", c.name, c.country)), text(group(c.population))]).collect();
        return Some(Answer::new("largest cities", vec![pod("Input", "Input interpretation", text("largest cities by population (city proper, built-in table)")), primary(pod("Result", "Largest cities", table(Some(vec!["rank", "city", "population"]), rows)))]));
    }
    let mut cs: Vec<&geo::Country> = geo::countries().values().filter(|c| c.area_km2 > 0.0 && c.population > 0).collect();
    cs.sort_by(|a, c| a.name.cmp(&c.name));
    cs.dedup_by(|a, c| a.iso2 == c.iso2);
    if let Some(r) = region {
        cs.retain(|c| c.continent.eq_ignore_ascii_case(r));
    }
    let metric: (&str, Box<dyn Fn(&geo::Country) -> f64>) = if kind.contains("densely") {
        ("population density", Box::new(|c: &geo::Country| c.population as f64 / c.area_km2))
    } else if kind.contains("popul") || by == Some("population") {
        ("population", Box::new(|c: &geo::Country| c.population as f64))
    } else {
        ("area", Box::new(|c: &geo::Country| c.area_km2))
    };
    let descending = !(kind.starts_with("small") || kind.starts_with("least"));
    cs.sort_by(|a, c| {
        let (x, y) = ((metric.1)(a), (metric.1)(c));
        if descending { y.partial_cmp(&x) } else { x.partial_cmp(&y) }.unwrap_or(std::cmp::Ordering::Equal)
    });
    let show = |c: &geo::Country| match metric.0 {
        "area" => format!("{} km²", group(c.area_km2.round() as u64)),
        "population" => group(c.population),
        _ => format!("{} people/km²", crate::print::fmt_float(c.population as f64 / c.area_km2, 4)),
    };
    let rows: Vec<Vec<Content>> = cs.iter().take(n).enumerate().map(|(k, c)| vec![text((k + 1).to_string()), text(c.name.clone()), text(show(c))]).collect();
    let label = format!("{kind} countries by {}{}", metric.0, region.map(|r| format!(" in {r}")).unwrap_or_default());
    let first = cs.first()?;
    let mut pods = vec![pod("Input", "Input interpretation", text(label.clone()))];
    pods.push(primary(pod("Result", "Result", text(format!("{} ({})", first.name, show(first))))));
    pods.push(pod("Ranking", "Ranking", table(Some(vec!["rank", "country", metric.0]), rows)));
    Some(Answer::new(label, pods))
}

pub fn handle(q: &Query) -> Option<Answer> {
    let l = q.lower.as_str();
    solar_system(l).or_else(|| landmarks(l)).or_else(|| economy(l)).or_else(|| rankings(l))
}
