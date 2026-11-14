//! Live data from open, key-free web services (optional, `net` feature):
//! weather (Open-Meteo), currency exchange rates (European Central Bank via
//! Frankfurter) and encyclopedia summaries (Wikipedia).
//!
//! Set `NOESIS_OFFLINE=1` (or turn off "online data" in the app) to disable
//! all network access.

use crate::engine::*;
use crate::print::fmt_float;
use regex::Regex;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::OnceLock;

static ONLINE: AtomicBool = AtomicBool::new(true);

pub fn set_online(on: bool) {
    ONLINE.store(on, Ordering::Relaxed);
}

pub fn online() -> bool {
    cfg!(feature = "net") && ONLINE.load(Ordering::Relaxed) && std::env::var("NOESIS_OFFLINE").is_err()
}

macro_rules! re {
    ($s:expr) => {{
        static R: OnceLock<Regex> = OnceLock::new();
        R.get_or_init(|| Regex::new($s).unwrap())
    }};
}

#[cfg(feature = "net")]
fn get_json(url: &str) -> Option<serde_json::Value> {
    static AGENT: OnceLock<ureq::Agent> = OnceLock::new();
    let agent = AGENT.get_or_init(|| ureq::AgentBuilder::new().timeout(std::time::Duration::from_secs(8)).try_proxy_from_env(true).user_agent(concat!("noesis/", env!("CARGO_PKG_VERSION"))).build());
    agent.get(url).call().ok()?.into_json().ok()
}

#[cfg(not(feature = "net"))]
fn get_json(_url: &str) -> Option<serde_json::Value> {
    None
}

fn enc(s: &str) -> String {
    s.bytes()
        .map(|b| match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => (b as char).to_string(),
            b' ' => "%20".into(),
            _ => format!("%{b:02X}"),
        })
        .collect()
}

fn source_pod(source: &str) -> Pod {
    let now = chrono::Local::now().format("%Y-%m-%d %H:%M");
    pod("Source", "Source", text(format!("{source} · retrieved {now}")))
}

// ---------------------------------------------------------------------------
// weather
// ---------------------------------------------------------------------------

fn wmo(code: i64) -> &'static str {
    match code {
        0 => "clear sky",
        1 => "mainly clear",
        2 => "partly cloudy",
        3 => "overcast",
        45 | 48 => "fog",
        51 | 53 | 55 => "drizzle",
        56 | 57 => "freezing drizzle",
        61 => "light rain",
        63 => "rain",
        65 => "heavy rain",
        66 | 67 => "freezing rain",
        71 => "light snow",
        73 => "snow",
        75 => "heavy snow",
        77 => "snow grains",
        80..=82 => "rain showers",
        85 | 86 => "snow showers",
        95 => "thunderstorm",
        96 | 99 => "thunderstorm with hail",
        _ => "unknown",
    }
}

fn locate(place: &str) -> Option<(String, f64, f64)> {
    if let Some(c) = super::geo::find_city(place) {
        let country = super::geo::countries().get(&c.country).map(|x| x.name.clone()).unwrap_or_default();
        return Some((format!("{}, {}", c.name, country), c.lat, c.lon));
    }
    let v = get_json(&format!("https://geocoding-api.open-meteo.com/v1/search?name={}&count=1&language=en&format=json", enc(place.trim())))?;
    let r = v.get("results")?.get(0)?;
    let name = format!("{}, {}", r.get("name")?.as_str()?, r.get("country").and_then(|c| c.as_str()).unwrap_or(""));
    Some((name, r.get("latitude")?.as_f64()?, r.get("longitude")?.as_f64()?))
}

fn weather(q: &Query) -> Option<Answer> {
    let m = re!(r"^(?:the\s+)?(?:current\s+)?(weather|temperature|forecast|weather forecast)\s+(?:today\s+|now\s+|tomorrow\s+)?(?:in|at|for)\s+(.+?)(?:\s+(?:today|now|tomorrow|this week))?$").captures(&q.lower)?;
    let place = m.get(2)?.as_str();
    let (name, lat, lon) = locate(place)?;
    let url = format!(
        "https://api.open-meteo.com/v1/forecast?latitude={lat}&longitude={lon}&current=temperature_2m,apparent_temperature,relative_humidity_2m,weather_code,wind_speed_10m,wind_direction_10m,precipitation&daily=temperature_2m_max,temperature_2m_min,weather_code,precipitation_sum&timezone=auto&forecast_days=7"
    );
    let v = get_json(&url)?;
    let cur = v.get("current")?;
    let f = |k: &str| cur.get(k).and_then(|x| x.as_f64());
    let t = f("temperature_2m")?;
    let mut pods = vec![pod("Input", "Input interpretation", text(format!("weather | {name}")))];
    let cond = wmo(cur.get("weather_code").and_then(|x| x.as_i64()).unwrap_or(-1));
    pods.push(primary(pod("Current", "Latest recorded weather", text(format!("{} °C ({} °F), {cond}", fmt_float(t, 3), fmt_float(t * 9.0 / 5.0 + 32.0, 3))))));
    let mut rows = Vec::new();
    if let Some(a) = f("apparent_temperature") {
        rows.push(vec![text("feels like"), text(format!("{} °C", fmt_float(a, 3)))]);
    }
    if let Some(h) = f("relative_humidity_2m") {
        rows.push(vec![text("relative humidity"), text(format!("{}%", fmt_float(h, 3)))]);
    }
    if let Some(w) = f("wind_speed_10m") {
        let dir = f("wind_direction_10m").map(|d| format!(" from {:.0}°", d)).unwrap_or_default();
        rows.push(vec![text("wind"), text(format!("{} km/h{dir}", fmt_float(w, 3)))]);
    }
    if let Some(p) = f("precipitation") {
        rows.push(vec![text("precipitation (last hour)"), text(format!("{} mm", fmt_float(p, 3)))]);
    }
    pods.push(pod("Details", "Conditions", table(None, rows)));
    if let Some(d) = v.get("daily") {
        let arr = |k: &str| d.get(k).and_then(|x| x.as_array()).cloned().unwrap_or_default();
        let (days, hi, lo, codes) = (arr("time"), arr("temperature_2m_max"), arr("temperature_2m_min"), arr("weather_code"));
        let mut rows = Vec::new();
        let mut his = Vec::new();
        let mut los = Vec::new();
        for k in 0..days.len().min(hi.len()).min(lo.len()) {
            let day = days[k].as_str().unwrap_or("");
            let label = chrono::NaiveDate::parse_from_str(day, "%Y-%m-%d").map(|d| d.format("%a %b %-d").to_string()).unwrap_or_else(|_| day.to_string());
            let (h, l) = (hi[k].as_f64().unwrap_or(f64::NAN), lo[k].as_f64().unwrap_or(f64::NAN));
            his.push([k as f64, h]);
            los.push([k as f64, l]);
            rows.push(vec![text(label), text(format!("{} °C", fmt_float(h, 3))), text(format!("{} °C", fmt_float(l, 3))), text(wmo(codes.get(k).and_then(|c| c.as_i64()).unwrap_or(-1)))]);
        }
        pods.push(pod("Forecast", "Forecast for the next 7 days", table(Some(vec!["day", "high", "low", "conditions"]), rows)));
        if his.len() > 1 {
            let mut p = crate::plot::scatter(vec![], "", None);
            p.kind = crate::plot::PlotKind::Cartesian;
            p.series = vec![crate::plot::Series { label: "high °C".into(), segments: vec![his.clone()], points: false }, crate::plot::Series { label: "low °C".into(), segments: vec![los.clone()], points: false }];
            p.x_range = (0.0, (his.len() - 1) as f64);
            let all: Vec<f64> = his.iter().chain(los.iter()).map(|q| q[1]).filter(|v| v.is_finite()).collect();
            let (mn, mx) = (all.iter().cloned().fold(f64::INFINITY, f64::min), all.iter().cloned().fold(f64::NEG_INFINITY, f64::max));
            p.y_range = (mn - 2.0, mx + 2.0);
            p.x_label = "days from today".into();
            p.y_label = "°C".into();
            pods.push(pod("ForecastPlot", "Temperature forecast", plot(p)));
        }
    }
    pods.push(pod("Location", "Location", text(format!("{name} ({lat:.2}°, {lon:.2}°)"))));
    pods.push(source_pod("Open-Meteo (open-meteo.com), CC BY 4.0"));
    Some(Answer::new(format!("weather in {name}"), pods))
}

// ---------------------------------------------------------------------------
// currency
// ---------------------------------------------------------------------------

const CURRENCIES: &[(&str, &str)] = &[
    ("dollar", "USD"),
    ("dollars", "USD"),
    ("us dollars", "USD"),
    ("euro", "EUR"),
    ("euros", "EUR"),
    ("pound", "GBP"),
    ("pounds", "GBP"),
    ("sterling", "GBP"),
    ("yen", "JPY"),
    ("yuan", "CNY"),
    ("renminbi", "CNY"),
    ("rupee", "INR"),
    ("rupees", "INR"),
    ("ringgit", "MYR"),
    ("won", "KRW"),
    ("franc", "CHF"),
    ("francs", "CHF"),
    ("real", "BRL"),
    ("reais", "BRL"),
    ("peso", "MXN"),
    ("pesos", "MXN"),
    ("baht", "THB"),
    ("rupiah", "IDR"),
    ("lira", "TRY"),
    ("zloty", "PLN"),
    ("krona", "SEK"),
    ("krone", "NOK"),
    ("rand", "ZAR"),
    ("$", "USD"),
    ("€", "EUR"),
    ("£", "GBP"),
    ("¥", "JPY"),
];

const ISO: &[&str] = &["USD", "EUR", "GBP", "JPY", "CNY", "INR", "MYR", "KRW", "CHF", "BRL", "MXN", "THB", "IDR", "TRY", "PLN", "SEK", "NOK", "DKK", "ZAR", "AUD", "CAD", "NZD", "SGD", "HKD", "PHP", "CZK", "HUF", "ILS", "ISK", "RON", "BGN"];

fn currency_code(w: &str) -> Option<&'static str> {
    let l = w.trim().to_lowercase();
    if let Some((_, c)) = CURRENCIES.iter().find(|(n, _)| *n == l) {
        return Some(c);
    }
    ISO.iter().find(|c| c.eq_ignore_ascii_case(&l)).copied()
}

fn currency(q: &Query) -> Option<Answer> {
    let l = q.lower.trim();
    let (amount, from, to) = if let Some(m) = re!(r"^(?:convert\s+)?([$€£¥])?\s*([\d,]*\.?\d+)\s*([a-z$€£¥]+(?:\s+dollars)?)?\s+(?:in|to|into)\s+([a-z€£¥$]+(?:\s+dollars)?)$").captures(l) {
        let amt: f64 = m[2].replace(',', "").parse().ok()?;
        let from = m.get(1).map(|x| x.as_str()).or(m.get(3).map(|x| x.as_str()))?;
        (amt, currency_code(from)?, currency_code(&m[4])?)
    } else if let Some(m) = re!(r"^(?:exchange rate|rate)\s+(?:of\s+|from\s+)?([a-z]{3})\s*(?:to|/|and|-)\s*([a-z]{3})$").captures(l) {
        (1.0, currency_code(&m[1])?, currency_code(&m[2])?)
    } else {
        return None;
    };
    if from == to {
        return None;
    }
    let v = get_json(&format!("https://api.frankfurter.dev/v1/latest?base={from}&symbols={to}"))?;
    let rate = v.get("rates")?.get(to)?.as_f64()?;
    let date = v.get("date").and_then(|d| d.as_str()).unwrap_or("").to_string();
    let mut pods = vec![pod("Input", "Input interpretation", text(format!("convert {} {from} to {to}", fmt_float(amount, 10))))];
    pods.push(primary(pod("Result", "Result", text(format!("{} {to}", fmt_float(amount * rate, 8))))));
    pods.push(pod("Rate", "Exchange rate", text(format!("1 {from} = {} {to}   ·   1 {to} = {} {from}", fmt_float(rate, 6), fmt_float(1.0 / rate, 6)))));
    let rows: Vec<Vec<Content>> = [1.0, 10.0, 100.0, 1000.0].iter().map(|a| vec![text(format!("{} {from}", fmt_float(*a, 6))), text(format!("{} {to}", fmt_float(a * rate, 8)))]).collect();
    pods.push(pod("Table", "Conversions", table(None, rows)));
    // 30-day history
    let start = (chrono::Local::now().date_naive() - chrono::Duration::days(31)).format("%Y-%m-%d");
    if let Some(h) = get_json(&format!("https://api.frankfurter.dev/v1/{start}..?base={from}&symbols={to}")) {
        if let Some(obj) = h.get("rates").and_then(|r| r.as_object()) {
            let mut pts: Vec<[f64; 2]> = obj.iter().enumerate().filter_map(|(k, (_, r))| Some([k as f64, r.get(to)?.as_f64()?])).collect();
            pts.sort_by(|a, b| a[0].partial_cmp(&b[0]).unwrap());
            if pts.len() > 2 {
                let mut p = crate::plot::scatter(vec![], "", None);
                p.kind = crate::plot::PlotKind::Cartesian;
                let (mn, mx) = pts.iter().fold((f64::INFINITY, f64::NEG_INFINITY), |a, q| (a.0.min(q[1]), a.1.max(q[1])));
                p.series = vec![crate::plot::Series { label: format!("{from}/{to}"), segments: vec![pts.clone()], points: false }];
                p.x_range = (0.0, pts.last().unwrap()[0]);
                let pad = (mx - mn).max(1e-9) * 0.15;
                p.y_range = (mn - pad, mx + pad);
                p.x_label = "trading days".into();
                p.y_label = to.into();
                pods.push(pod("History", "Last 30 days", plot(p)));
            }
        }
    }
    pods.push(source_pod(&format!("European Central Bank reference rates via Frankfurter (rates of {date})")));
    Some(Answer::new(format!("{from} to {to}"), pods))
}

// ---------------------------------------------------------------------------
// encyclopedia
// ---------------------------------------------------------------------------

fn encyclopedia(q: &Query) -> Option<Answer> {
    let raw_l = q.raw.trim().trim_end_matches(['?', '.', '!']).to_lowercase();
    let m = re!(r"^(?:who\s+(?:is|was|were|are)|what\s+(?:is|was|are|were)(?:\s+an?)?|what's(?:\s+an?)?|tell me about|define|definition of|information about|info on|wiki|wikipedia)\s+(.+?)$").captures(&raw_l)?;
    lookup(q, m.get(1)?.as_str())
}

/// Last-resort lookup for short word-only inputs nothing else understood.
pub fn fallback(q: &Query) -> Option<Answer> {
    if !online() {
        return None;
    }
    let l = q.lower.trim();
    let words = l.split_whitespace().count();
    if words == 0 || words > 6 || !l.chars().all(|c| c.is_alphabetic() || c == ' ' || c == '-' || c == '\'') {
        return None;
    }
    lookup(q, l.trim_start_matches("about "))
}

fn lookup(q: &Query, topic_in: &str) -> Option<Answer> {
    let topic_l = topic_in.trim().trim_start_matches("the ").trim();
    if topic_l.is_empty() || topic_l.chars().any(|c| "+*/^=<>".contains(c)) || topic_l.chars().next()?.is_ascii_digit() {
        return None;
    }
    // keep the user's capitalisation
    let src = q.raw.trim();
    let topic = match src.to_lowercase().rfind(topic_l) {
        Some(start) if src.is_char_boundary(start) && src.is_char_boundary(start + topic_l.len()) => &src[start..start + topic_l.len()],
        _ => topic_l,
    };
    let title = topic.split_whitespace().map(|w| w.to_string()).collect::<Vec<_>>().join("_");
    let mut v = get_json(&format!("https://en.wikipedia.org/api/rest_v1/page/summary/{}", enc(&title)));
    if v.as_ref().and_then(|x| x.get("extract")).is_none() {
        // search for the best-matching article
        let s = get_json(&format!("https://en.wikipedia.org/w/rest.php/v1/search/title?q={}&limit=1", enc(topic)))?;
        let key = s.get("pages")?.get(0)?.get("key")?.as_str()?.to_string();
        v = get_json(&format!("https://en.wikipedia.org/api/rest_v1/page/summary/{}", enc(&key)));
    }
    let v = v?;
    if v.get("type").and_then(|t| t.as_str()) == Some("disambiguation") {
        return None;
    }
    let extract = v.get("extract")?.as_str()?.to_string();
    let name = v.get("title").and_then(|t| t.as_str()).unwrap_or(topic).to_string();
    let mut pods = vec![pod("Input", "Input interpretation", text(name.clone()))];
    if let Some(d) = v.get("description").and_then(|d| d.as_str()) {
        pods.push(pod("Description", "Description", text(d.to_string())));
    }
    pods.push(primary(pod("Summary", "Summary", text(extract))));
    if let Some(u) = v.get("content_urls").and_then(|c| c.get("desktop")).and_then(|c| c.get("page")).and_then(|p| p.as_str()) {
        pods.push(pod("Link", "Read more", text(u.to_string())));
    }
    pods.push(source_pod("Wikipedia, CC BY-SA 4.0"));
    Some(Answer::new(name, pods))
}

pub fn handle(q: &Query) -> Option<Answer> {
    if !online() {
        return None;
    }
    weather(q).or_else(|| currency(q)).or_else(|| encyclopedia(q))
}
