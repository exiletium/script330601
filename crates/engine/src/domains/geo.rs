//! Geography: countries, cities, distances and local times.
//! Data: GeoNames (CC BY 4.0) via geonamescache.

use crate::engine::*;
use crate::print::fmt_float;
use serde::Deserialize;
use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Deserialize, Clone, Debug)]
pub struct Country {
    pub name: String,
    pub iso2: String,
    pub iso3: String,
    pub capital: String,
    pub continent: String,
    pub area_km2: f64,
    pub population: u64,
    pub tld: String,
    pub currency_code: String,
    pub currency_name: String,
    pub calling_code: String,
    pub languages: String,
    pub neighbours: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct City {
    pub name: String,
    pub country: String,
    pub lat: f64,
    pub lon: f64,
    pub population: u64,
    pub timezone: String,
    pub capital: bool,
}

pub fn countries() -> &'static HashMap<String, Country> {
    static C: OnceLock<HashMap<String, Country>> = OnceLock::new();
    C.get_or_init(|| serde_json::from_str(include_str!("../../data/countries.json")).unwrap_or_default())
}

pub fn cities() -> &'static Vec<City> {
    static C: OnceLock<Vec<City>> = OnceLock::new();
    C.get_or_init(|| {
        #[derive(Deserialize)]
        struct Raw {
            rows: Vec<(String, String, f64, f64, u64, String, u8, String)>,
        }
        let raw: Raw = serde_json::from_str(include_str!("../../data/cities.json")).unwrap_or(Raw { rows: vec![] });
        raw.rows.into_iter().map(|r| City { name: r.0, country: r.1, lat: r.2, lon: r.3, population: r.4, timezone: r.5, capital: r.6 == 1 }).collect()
    })
}

const ALIASES: &[(&str, &str)] = &[
    ("usa", "US"),
    ("us", "US"),
    ("united states", "US"),
    ("america", "US"),
    ("united states of america", "US"),
    ("uk", "GB"),
    ("britain", "GB"),
    ("great britain", "GB"),
    ("england", "GB"),
    ("russia", "RU"),
    ("south korea", "KR"),
    ("north korea", "KP"),
    ("vietnam", "VN"),
    ("iran", "IR"),
    ("syria", "SY"),
    ("laos", "LA"),
    ("bolivia", "BO"),
    ("venezuela", "VE"),
    ("tanzania", "TZ"),
    ("czech republic", "CZ"),
    ("czechia", "CZ"),
    ("holland", "NL"),
    ("the netherlands", "NL"),
    ("ivory coast", "CI"),
    ("uae", "AE"),
    ("taiwan", "TW"),
    ("moldova", "MD"),
    ("macedonia", "MK"),
];

pub fn find_country(q: &str) -> Option<&'static Country> {
    let l = q.trim().to_lowercase();
    let l = l.trim_start_matches("the ");
    if let Some((_, code)) = ALIASES.iter().find(|(a, _)| *a == l) {
        return countries().get(*code);
    }
    countries().values().find(|c| c.name.to_lowercase() == l || (l.len() == 3 && c.iso3.to_lowercase() == l))
}

pub fn find_city(q: &str) -> Option<&'static City> {
    let l = q.trim().to_lowercase();
    let (name, cc) = match l.split_once(',') {
        Some((n, c)) => (n.trim().to_string(), find_country(c.trim()).map(|c| c.iso2.clone())),
        None => (l.clone(), None),
    };
    let name = match name.as_str() {
        "nyc" | "new york" => "new york city".to_string(),
        "la" => "los angeles".to_string(),
        "sf" | "san fran" => "san francisco".to_string(),
        "dc" | "washington dc" | "washington d.c." => "washington".to_string(),
        "bombay" => "mumbai".to_string(),
        "peking" => "beijing".to_string(),
        other => other.to_string(),
    };
    // most populous match
    cities().iter().filter(|c| c.name.to_lowercase() == name && cc.as_ref().map_or(true, |k| &c.country == k)).max_by_key(|c| c.population)
}

/// Great-circle distance in km (haversine on a spherical Earth).
pub fn distance_km(a: &City, b: &City) -> f64 {
    let r = 6371.0088;
    let (p1, p2) = (a.lat.to_radians(), b.lat.to_radians());
    let dp = p2 - p1;
    let dl = (b.lon - a.lon).to_radians();
    let h = (dp / 2.0).sin().powi(2) + p1.cos() * p2.cos() * (dl / 2.0).sin().powi(2);
    2.0 * r * h.sqrt().asin()
}

fn initial_bearing(a: &City, b: &City) -> f64 {
    let (p1, p2) = (a.lat.to_radians(), b.lat.to_radians());
    let dl = (b.lon - a.lon).to_radians();
    let y = dl.sin() * p2.cos();
    let x = p1.cos() * p2.sin() - p1.sin() * p2.cos() * dl.cos();
    (y.atan2(x).to_degrees() + 360.0) % 360.0
}

fn compass(deg: f64) -> &'static str {
    const D: [&str; 16] = ["N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"];
    D[(((deg + 11.25) % 360.0) / 22.5) as usize]
}

fn fmt_int(n: u64) -> String {
    let s = n.to_string();
    let mut out = String::new();
    for (k, c) in s.chars().enumerate() {
        if k > 0 && (s.len() - k) % 3 == 0 {
            out.push(',');
        }
        out.push(c);
    }
    out
}

fn coords(c: &City) -> String {
    format!("{:.2}° {}, {:.2}° {}", c.lat.abs(), if c.lat >= 0.0 { "N" } else { "S" }, c.lon.abs(), if c.lon >= 0.0 { "E" } else { "W" })
}

pub fn local_time(tz: &str) -> Option<chrono::DateTime<chrono_tz::Tz>> {
    let tz: chrono_tz::Tz = tz.parse().ok()?;
    Some(chrono::Utc::now().with_timezone(&tz))
}

fn country_pods(c: &Country) -> Vec<Pod> {
    let mut pods = vec![pod("Input", "Input interpretation", text(format!("{} (country)", c.name)))];
    let density = c.population as f64 / c.area_km2.max(1e-9);
    pods.push(primary(pod(
        "Facts",
        "Basic facts",
        table(
            None,
            vec![
                vec![text("capital city"), text(c.capital.clone())],
                vec![text("continent"), text(continent(&c.continent))],
                vec![text("population"), text(format!("{} people", fmt_int(c.population)))],
                vec![text("area"), text(format!("{} km² ({} mi²)", fmt_int(c.area_km2 as u64), fmt_int((c.area_km2 / 2.589988) as u64)))],
                vec![text("population density"), text(format!("{} people/km²", fmt_float(density, 4)))],
                vec![text("currency"), text(format!("{} ({})", c.currency_name, c.currency_code))],
                vec![text("calling code"), text(format!("+{}", c.calling_code))],
                vec![text("internet code"), text(c.tld.clone())],
                vec![text("ISO codes"), text(format!("{} / {}", c.iso2, c.iso3))],
            ],
        ),
    )));
    if !c.neighbours.is_empty() {
        let names: Vec<String> = c.neighbours.iter().filter_map(|n| countries().get(n)).map(|n| n.name.clone()).collect();
        pods.push(pod("Borders", "Bordering countries", text(names.join(" | "))));
    }
    if !c.languages.is_empty() {
        pods.push(pod("Languages", "Language codes", text(c.languages.replace(',', ", "))));
    }
    let mut big: Vec<&City> = cities().iter().filter(|x| x.country == c.iso2).collect();
    big.sort_by(|a, b| b.population.cmp(&a.population));
    if !big.is_empty() {
        let rows: Vec<Vec<Content>> = big.iter().take(6).map(|x| vec![text(x.name.clone()), text(fmt_int(x.population))]).collect();
        pods.push(pod("Cities", "Largest cities", table(Some(vec!["city", "population"]), rows)));
        if let Some(t) = local_time(&big[0].timezone) {
            pods.push(pod("Time", &format!("Local time ({})", big[0].name), text(t.format("%-I:%M %p %Z, %A, %B %-d, %Y").to_string())));
        }
    }
    pods
}

fn continent(code: &str) -> String {
    match code {
        "AF" => "Africa",
        "AS" => "Asia",
        "EU" => "Europe",
        "NA" => "North America",
        "SA" => "South America",
        "OC" => "Oceania",
        "AN" => "Antarctica",
        other => other,
    }
    .to_string()
}

fn city_pods(c: &City) -> Vec<Pod> {
    let country = countries().get(&c.country).map(|x| x.name.clone()).unwrap_or_else(|| c.country.clone());
    let mut pods = vec![pod("Input", "Input interpretation", text(format!("{}, {} (city)", c.name, country)))];
    let mut rows = vec![vec![text("country"), text(country)], vec![text("population"), text(format!("{} people", fmt_int(c.population)))], vec![text("coordinates"), text(coords(c))], vec![text("time zone"), text(c.timezone.clone())]];
    if c.capital {
        rows.insert(1, vec![text("status"), text("national capital")]);
    }
    pods.push(primary(pod("Facts", "Basic information", table(None, rows))));
    if let Some(t) = local_time(&c.timezone) {
        pods.push(pod("Time", "Current local time", text(t.format("%-I:%M:%S %p %Z, %A, %B %-d, %Y").to_string())));
    }
    let mut near: Vec<(&City, f64)> = cities().iter().filter(|x| x.name != c.name && x.population > 300_000).map(|x| (x, distance_km(c, x))).filter(|(_, d)| *d < 600.0).collect();
    near.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
    if !near.is_empty() {
        let rows: Vec<Vec<Content>> = near.iter().take(5).map(|(x, d)| vec![text(x.name.clone()), text(format!("{} km", fmt_float(*d, 4)))]).collect();
        pods.push(pod("Nearby", "Nearby cities", table(None, rows)));
    }
    pods
}

pub fn handle(q: &Query) -> Option<Answer> {
    let l = q.lower.trim();
    // distance between cities
    let dist_re = ["distance from ", "distance between ", "how far is ", "how far from "];
    for p in dist_re {
        if let Some(rest) = l.strip_prefix(p) {
            let (a, b) = rest.split_once(" to ").or_else(|| rest.split_once(" and ")).or_else(|| rest.split_once(" from "))?;
            let (ca, cb) = (find_city(a)?, find_city(b)?);
            let d = distance_km(ca, cb);
            let mut pods = vec![pod("Input", "Input interpretation", text(format!("distance from {} to {}", ca.name, cb.name)))];
            pods.push(primary(pod("Result", "Result", text(format!("{} km  ({} miles)", fmt_float(d, 5), fmt_float(d / 1.609344, 5))))));
            let b = initial_bearing(ca, cb);
            pods.push(pod("Direction", "Direction", text(format!("{} ({:.0}° bearing from {})", compass(b), b, ca.name))));
            pods.push(pod(
                "TravelTimes",
                "Approximate travel times",
                table(None, vec![vec![text("aircraft (800 km/h)"), text(fmt_hours(d / 800.0))], vec![text("car (100 km/h)"), text(fmt_hours(d / 100.0))], vec![text("light in fiber (≈200,000 km/s)"), text(format!("{} ms", fmt_float(d / 200.0, 4)))]]),
            ));
            pods.push(pod("Locations", "Locations", table(None, vec![vec![text(ca.name.clone()), text(coords(ca))], vec![text(cb.name.clone()), text(coords(cb))]])));
            return Some(Answer::new(format!("distance {} to {}", ca.name, cb.name), pods));
        }
    }
    // time in a place
    for p in ["time in ", "current time in ", "what time is it in ", "local time in "] {
        if let Some(rest) = l.strip_prefix(p) {
            let (name, tz) = if let Some(c) = find_city(rest) {
                (c.name.clone(), c.timezone.clone())
            } else if let Some(c) = find_country(rest) {
                let big = cities().iter().filter(|x| x.country == c.iso2).max_by_key(|x| x.population)?;
                (c.name.clone(), big.timezone.clone())
            } else {
                return None;
            };
            let t = local_time(&tz)?;
            let pods = vec![pod("Input", "Input interpretation", text(format!("current time in {name}"))), primary(pod("Result", "Result", text(t.format("%-I:%M:%S %p %Z, %A, %B %-d, %Y").to_string()))), pod("TimeZone", "Time zone", text(format!("{tz} (UTC{})", t.format("%:z"))))];
            return Some(Answer::new(format!("time in {name}"), pods));
        }
    }
    // population / capital / area of X
    if let Some((prop, place)) = l.split_once(" of ") {
        let prop = prop.trim_start_matches("the ");
        if let Some(c) = find_country(place) {
            let v = match prop {
                "population" => Some(format!("{} people", fmt_int(c.population))),
                "capital" | "capital city" => Some(c.capital.clone()),
                "area" | "size" => Some(format!("{} km²", fmt_int(c.area_km2 as u64))),
                "currency" => Some(format!("{} ({})", c.currency_name, c.currency_code)),
                "calling code" | "country code" | "dialing code" => Some(format!("+{}", c.calling_code)),
                _ => None,
            };
            if let Some(v) = v {
                let mut pods = vec![pod("Input", "Input interpretation", text(format!("{} | {prop}", c.name))), primary(pod("Result", "Result", text(v)))];
                pods.extend(country_pods(c).into_iter().skip(1).map(|mut p| {
                    p.primary = false;
                    p
                }));
                return Some(Answer::new(format!("{prop} of {}", c.name), pods));
            }
        }
        if prop == "population" {
            if let Some(c) = find_city(place) {
                let pods = vec![pod("Input", "Input interpretation", text(format!("{} | population", c.name))), primary(pod("Result", "Result", text(format!("{} people", fmt_int(c.population)))))];
                return Some(Answer::new(format!("population of {}", c.name), pods));
            }
        }
    }
    if let Some(c) = find_country(l) {
        return Some(Answer::new(format!("{} (country)", c.name), country_pods(c)));
    }
    if l.chars().all(|c| c.is_alphabetic() || c == ' ' || c == ',' || c == '.' || c == '-') && l.len() >= 3 {
        if let Some(c) = find_city(l) {
            if c.population >= 50_000 {
                return Some(Answer::new(format!("{} (city)", c.name), city_pods(c)));
            }
        }
    }
    None
}

fn fmt_hours(h: f64) -> String {
    let total = (h * 60.0).round() as i64;
    let (hh, mm) = (total / 60, total % 60);
    if hh == 0 {
        format!("{mm} min")
    } else {
        format!("{hh} h {mm} min")
    }
}
