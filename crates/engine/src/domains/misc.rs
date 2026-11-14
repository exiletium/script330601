//! Dates and times, personal finance and probability.

use crate::engine::*;
use crate::print::fmt_float;
use crate::stats::{dist_pods, Dist};
use chrono::{Datelike, Duration, NaiveDate};
use regex::Regex;
use std::sync::OnceLock;

macro_rules! re {
    ($s:expr) => {{
        static R: OnceLock<Regex> = OnceLock::new();
        R.get_or_init(|| Regex::new($s).unwrap())
    }};
}

// ---------------------------------------------------------------------------
// dates
// ---------------------------------------------------------------------------

pub(crate) fn today() -> NaiveDate {
    chrono::Local::now().date_naive()
}

fn holiday(name: &str, year: i32) -> Option<NaiveDate> {
    let (m, d) = match name {
        "christmas" | "christmas day" | "xmas" => (12, 25),
        "christmas eve" => (12, 24),
        "new year" | "new year's day" | "new years day" | "new years" | "new year's" => (1, 1),
        "new year's eve" | "new years eve" => (12, 31),
        "halloween" => (10, 31),
        "valentine's day" | "valentines day" => (2, 14),
        "independence day" | "july 4th" | "fourth of july" => (7, 4),
        "st patrick's day" | "st. patrick's day" | "saint patrick's day" => (3, 17),
        "pi day" => (3, 14),
        _ => return None,
    };
    NaiveDate::from_ymd_opt(year, m, d)
}

/// Parse many common date formats.
pub fn parse_date(s: &str) -> Option<NaiveDate> {
    let s = s.trim().trim_end_matches('.').trim();
    let l = s.to_lowercase();
    match l.as_str() {
        "today" | "now" => return Some(today()),
        "tomorrow" => return Some(today() + Duration::days(1)),
        "yesterday" => return Some(today() - Duration::days(1)),
        _ => {}
    }
    if let Some(h) = holiday(&l, today().year()) {
        return Some(if h < today() { holiday(&l, today().year() + 1)? } else { h });
    }
    let cleaned = re!(r"(\d)(st|nd|rd|th)\b").replace_all(s, "$1").to_string();
    let fmts = ["%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d.%m.%Y", "%B %d, %Y", "%B %d %Y", "%b %d, %Y", "%b %d %Y", "%d %B %Y", "%d %b %Y", "%B %d,%Y", "%A, %B %d, %Y"];
    for f in fmts {
        if let Ok(d) = NaiveDate::parse_from_str(&cleaned, f) {
            return Some(d);
        }
    }
    // month and day without a year: assume the next occurrence
    for f in ["%B %d", "%b %d", "%d %B", "%d %b"] {
        if let Ok(d) = NaiveDate::parse_from_str(&format!("{cleaned} {}", today().year()), &format!("{f} %Y")) {
            return Some(if d < today() { d.with_year(d.year() + 1)? } else { d });
        }
    }
    None
}

pub(crate) fn date_long(d: NaiveDate) -> String {
    d.format("%A, %B %-d, %Y").to_string()
}

pub(crate) fn span_text(days: i64) -> String {
    let a = days.abs();
    let weeks = a / 7;
    let mut parts = vec![format!("{a} days")];
    if a >= 7 {
        parts.push(format!("{weeks} weeks {} days", a % 7));
    }
    if a >= 365 {
        parts.push(format!("≈ {} years", fmt_float(a as f64 / 365.2425, 5)));
    }
    parts.join("  |  ")
}

fn date_info_pods(d: NaiveDate) -> Vec<Pod> {
    let t = today();
    let diff = (d - t).num_days();
    let rel = if diff == 0 {
        "today".to_string()
    } else if diff > 0 {
        format!("in {}", span_text(diff))
    } else {
        format!("{} ago", span_text(diff))
    };
    let iso = d.iso_week();
    vec![
        primary(pod("Result", "Date", text(date_long(d)))),
        pod("Relative", "Time difference from today", text(rel)),
        pod(
            "Info",
            "Date information",
            table(
                None,
                vec![
                    vec![text("day of the week"), text(d.format("%A").to_string())],
                    vec![text("day of the year"), text(d.ordinal().to_string())],
                    vec![text("ISO week"), text(format!("week {} of {}", iso.week(), iso.year()))],
                    vec![text("leap year"), text(if NaiveDate::from_ymd_opt(d.year(), 2, 29).is_some() { "yes" } else { "no" })],
                    vec![text("Julian day number"), text((d.num_days_from_ce() as i64 + 1_721_425).to_string())],
                ],
            ),
        ),
    ]
}

fn dates(q: &Query) -> Option<Answer> {
    let l = q.lower.trim();
    if let Some(m) = re!(r"^(?:days?|time|weeks?)\s+(?:between|from)\s+(.+?)\s+(?:and|to|until)\s+(.+)$").captures(l) {
        let (a, b) = (parse_date(&m[1])?, parse_date(&m[2])?);
        let days = (b - a).num_days();
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("duration from {} to {}", date_long(a), date_long(b)))),
            primary(pod("Result", "Result", text(span_text(days)))),
            pod("Units", "Unit conversions", table(None, vec![vec![text("hours"), text((days * 24).to_string())], vec![text("minutes"), text((days * 1440).to_string())], vec![text("seconds"), text((days * 86400).to_string())]])),
        ];
        return Some(Answer::new("days between dates", pods));
    }
    if let Some(m) = re!(r"^(?:how many )?days (?:until|till|to|before)\s+(.+)$").captures(l) {
        let d = parse_date(&m[1])?;
        let days = (d - today()).num_days();
        let pods = vec![pod("Input", "Input interpretation", text(format!("days until {}", date_long(d)))), primary(pod("Result", "Result", text(span_text(days))))];
        return Some(Answer::new("days until", pods));
    }
    if let Some(m) = re!(r"^(\d+)\s+(days?|weeks?|months?|years?)\s+(from|after|before|ago)\s*(.*)$").captures(l) {
        let n: i64 = m[1].parse().ok()?;
        let base = if &m[3] == "ago" || m[4].trim().is_empty() || m[4].trim() == "now" { today() } else { parse_date(&m[4])? };
        let sign = if matches!(&m[3], "before" | "ago") { -1 } else { 1 };
        let unit = &m[2];
        let d = if unit.starts_with("day") {
            base + Duration::days(sign * n)
        } else if unit.starts_with("week") {
            base + Duration::weeks(sign * n)
        } else {
            let months = if unit.starts_with("month") { n } else { 12 * n } * sign;
            let total = base.year() as i64 * 12 + base.month0() as i64 + months;
            let (y, mo) = (total.div_euclid(12) as i32, total.rem_euclid(12) as u32 + 1);
            NaiveDate::from_ymd_opt(y, mo, base.day()).or_else(|| NaiveDate::from_ymd_opt(y, mo, 28))?
        };
        let mut pods = vec![pod("Input", "Input interpretation", text(format!("{n} {unit} {} {}", &m[3], date_long(base))))];
        pods.extend(date_info_pods(d));
        return Some(Answer::new("date arithmetic", pods));
    }
    let date_q = l.trim_start_matches("what day is ").trim_start_matches("what day was ").trim_start_matches("day of the week for ").trim_start_matches("day of week ");
    if l == "today" || l == "date" || l == "today's date" || l == "date today" || l == "what day is it" {
        let mut pods = vec![pod("Input", "Input interpretation", text("today's date"))];
        pods.extend(date_info_pods(today()));
        return Some(Answer::new("today", pods));
    }
    if date_q.chars().any(|c| c.is_ascii_digit()) || holiday(date_q, 2000).is_some() {
        if let Some(d) = parse_date(date_q) {
            let mut pods = vec![pod("Input", "Input interpretation", text(date_long(d)))];
            pods.extend(date_info_pods(d));
            return Some(Answer::new(date_long(d), pods));
        }
    }
    None
}

// ---------------------------------------------------------------------------
// finance
// ---------------------------------------------------------------------------

pub(crate) fn money(v: f64) -> String {
    let neg = v < 0.0;
    let cents = (v.abs() * 100.0).round() as u128;
    let (d, c) = (cents / 100, cents % 100);
    let ds = d.to_string();
    let mut out = String::new();
    for (k, ch) in ds.chars().enumerate() {
        if k > 0 && (ds.len() - k) % 3 == 0 {
            out.push(',');
        }
        out.push(ch);
    }
    format!("{}${out}.{c:02}", if neg { "-" } else { "" })
}

pub(crate) fn num(s: &str) -> Option<f64> {
    let t = s.trim().trim_start_matches('$').replace(',', "");
    let (t, mult) = if let Some(x) = t.strip_suffix('k') { (x.to_string(), 1e3) } else if let Some(x) = t.strip_suffix('m') { (x.to_string(), 1e6) } else { (t, 1.0) };
    t.trim().parse::<f64>().ok().map(|v| v * mult)
}

fn finance(q: &Query) -> Option<Answer> {
    let l = q.lower.trim();
    if let Some(m) = re!(r"^(?:compound interest|investment|future value)\s*(?:on|of|for)?\s*\$?([\d,.]+k?)\s*(?:at|with|@)\s*([\d.]+)\s*%(?:\s*(?:per year|annually|apr|interest))?\s*(?:for|over)\s*([\d.]+)\s*years?(?:\s*(?:compounded\s*)?(monthly|quarterly|daily|annually|yearly|continuously))?$").captures(l) {
        let (p, r, t) = (num(&m[1])?, num(&m[2])? / 100.0, num(&m[3])?);
        let n = match m.get(4).map(|x| x.as_str()) {
            Some("monthly") => 12.0,
            Some("quarterly") => 4.0,
            Some("daily") => 365.0,
            Some("continuously") => f64::INFINITY,
            _ => 1.0,
        };
        let fv = if n.is_infinite() { p * (r * t).exp() } else { p * (1.0 + r / n).powf(n * t) };
        let mut rows = Vec::new();
        let years = t.ceil() as usize;
        for y in 0..=years.min(40) {
            let yv = if n.is_infinite() { p * (r * y as f64).exp() } else { p * (1.0 + r / n).powf(n * y as f64) };
            rows.push(vec![text(y.to_string()), text(money(yv))]);
        }
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("{} at {}% for {} years, compounded {}", money(p), fmt_float(r * 100.0, 6), fmt_float(t, 6), m.get(4).map_or("annually", |x| x.as_str())))),
            primary(pod("Result", "Future value", text(money(fv)))),
            pod("Interest", "Interest earned", text(money(fv - p))),
            pod("Growth", "Balance by year", table(Some(vec!["year", "balance"]), rows)),
        ];
        return Some(Answer::new("compound interest", pods));
    }
    if let Some(m) = re!(r"^(?:mortgage|loan|loan payment|monthly payment)\s*(?:on|of|for)?\s*\$?([\d,.]+k?)\s*(?:at|with|@)\s*([\d.]+)\s*%(?:\s*(?:apr|interest))?\s*(?:for|over)\s*([\d.]+)\s*(years?|months?)$").captures(l) {
        let (p, r) = (num(&m[1])?, num(&m[2])? / 100.0 / 12.0);
        let n = if m[4].starts_with("year") { num(&m[3])? * 12.0 } else { num(&m[3])? };
        let pay = if r == 0.0 { p / n } else { p * r / (1.0 - (1.0 + r).powf(-n)) };
        let total = pay * n;
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("loan of {} at {}% APR over {} months", money(p), fmt_float(r * 1200.0, 6), n))),
            primary(pod("Result", "Monthly payment", text(money(pay)))),
            pod("Totals", "Totals", table(None, vec![vec![text("total paid"), text(money(total))], vec![text("total interest"), text(money(total - p))]])),
        ];
        return Some(Answer::new("loan payment", pods));
    }
    if let Some(m) = re!(r"^(?:(\d+(?:\.\d+)?)\s*%\s*tip\s*(?:on|for)\s*\$?([\d,.]+)|tip\s*(?:on|for)\s*\$?([\d,.]+)(?:\s*(?:at)?\s*(\d+(?:\.\d+)?)\s*%)?)$").captures(l) {
        let (pct, bill) = match (m.get(1), m.get(2), m.get(3), m.get(4)) {
            (Some(p), Some(b), _, _) => (num(p.as_str())?, num(b.as_str())?),
            (_, _, Some(b), p) => (p.and_then(|x| num(x.as_str())).unwrap_or(15.0), num(b.as_str())?),
            _ => return None,
        };
        let rows: Vec<Vec<Content>> = [10.0, 15.0, 18.0, 20.0, 25.0].iter().map(|p| vec![text(format!("{p}%")), text(money(bill * p / 100.0)), text(money(bill * (1.0 + p / 100.0)))]).collect();
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("{}% tip on {}", fmt_float(pct, 4), money(bill)))),
            primary(pod("Result", "Tip", text(format!("{}   (total {})", money(bill * pct / 100.0), money(bill * (1.0 + pct / 100.0)))))),
            pod("Table", "Common tip amounts", table(Some(vec!["rate", "tip", "total"]), rows)),
        ];
        return Some(Answer::new("tip", pods));
    }
    None
}

// ---------------------------------------------------------------------------
// probability
// ---------------------------------------------------------------------------

fn kv(l: &str, keys: &[&str]) -> Option<f64> {
    for k in keys {
        let r = Regex::new(&format!(r"(?:^|[\s,(]){}\s*(?:=|of|is)?\s*(-?[\d.]+)", regex::escape(k))).ok()?;
        if let Some(m) = r.captures(l) {
            return m[1].parse().ok();
        }
    }
    None
}

fn probability(q: &Query) -> Option<Answer> {
    let l = q.lower.trim();
    // coin flips / binomial events: "probability of 3 heads in 5 coin flips"
    if let Some(m) = re!(r"^(?:probability|chance) of (?:(at least|at most|exactly)\s+)?(\d+)\s+(?:heads|tails|sixes|successes)\s+in\s+(\d+)\s+(coin (?:flips|tosses)|flips|tosses|(?:die|dice) rolls|rolls|trials)$").captures(l) {
        let k: f64 = m[2].parse().ok()?;
        let n: u64 = m[3].parse().ok()?;
        let p = if m[4].contains("roll") { 1.0 / 6.0 } else { 0.5 };
        let d = Dist::Binomial(n, p);
        let range = match m.get(1).map(|x| x.as_str()) {
            Some("at least") => (k, f64::INFINITY),
            Some("at most") => (f64::NEG_INFINITY, k),
            _ => (k, k),
        };
        let mut pods = vec![pod("Input", "Input interpretation", text(format!("{} | {}", l, d.name())))];
        pods.extend(dist_pods(&d, Some(range)));
        return Some(Answer::new("binomial probability", pods));
    }
    let dist = if l.contains("normal distribution") || l.contains("gaussian") {
        Some(Dist::Normal(kv(l, &["mean", "mu", "μ"]).unwrap_or(0.0), kv(l, &["sd", "standard deviation", "sigma", "σ"]).unwrap_or(1.0)))
    } else if l.contains("binomial distribution") {
        Some(Dist::Binomial(kv(l, &["n", "trials"])? as u64, kv(l, &["p", "probability"])?))
    } else if l.contains("poisson distribution") {
        Some(Dist::Poisson(kv(l, &["lambda", "λ", "mean", "rate"])?))
    } else if l.contains("exponential distribution") {
        Some(Dist::Exponential(kv(l, &["lambda", "λ", "rate"]).unwrap_or(1.0)))
    } else if l.contains("uniform distribution") {
        let a = kv(l, &["a", "min", "from"]).unwrap_or(0.0);
        let b = kv(l, &["b", "max", "to"]).unwrap_or(1.0);
        Some(Dist::Uniform(a, b))
    } else if l.contains("t distribution") || l.contains("student") {
        Some(Dist::StudentT(kv(l, &["df", "degrees of freedom", "nu", "ν"]).unwrap_or(10.0)))
    } else if l.contains("chi squared") || l.contains("chi-squared") || l.contains("χ²") {
        Some(Dist::ChiSquared(kv(l, &["df", "degrees of freedom", "k"]).unwrap_or(2.0)))
    } else {
        None
    };
    let d = dist?;
    // optional probability query: P(X < a), P(a < X < b), P(X > a)
    let query = if let Some(m) = re!(r"p\s*\(\s*(-?[\d.]+)\s*<=?\s*x\s*<=?\s*(-?[\d.]+)\s*\)").captures(l) {
        Some((m[1].parse().ok()?, m[2].parse().ok()?))
    } else if let Some(m) = re!(r"p\s*\(\s*x\s*(<=?|>=?|=)\s*(-?[\d.]+)\s*\)").captures(l) {
        let v: f64 = m[2].parse().ok()?;
        Some(match &m[1] {
            "<" | "<=" => (f64::NEG_INFINITY, v),
            ">" | ">=" => (v, f64::INFINITY),
            _ => (v, v),
        })
    } else {
        None
    };
    let mut pods = vec![pod("Input", "Input interpretation", text(d.name()))];
    pods.extend(dist_pods(&d, query));
    Some(Answer::new(d.name(), pods))
}

fn combinatorics(q: &Query) -> Option<Answer> {
    let l = q.lower.trim();
    if let Some(m) = re!(r"^(?:number of )?(permutations|combinations|arrangements|ways to choose)\s+(?:of\s+)?(\d+)\s+(?:from|out of|of|choose)\s+(\d+)$").captures(l) {
        let k: u64 = m[2].parse().ok()?;
        let n: u64 = m[3].parse().ok()?;
        if k > n || n > 10_000 {
            return None;
        }
        let c = crate::numtheory::binomial(&num_bigint::BigInt::from(n), k);
        let perm = c.clone() * crate::numtheory::factorial(k);
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("choose {k} from {n}"))),
            primary(pod("Result", if m[1].starts_with("perm") || m[1].starts_with("arr") { "Permutations" } else { "Combinations" }, text(if m[1].starts_with("perm") || m[1].starts_with("arr") { perm.to_string() } else { c.to_string() }))),
            pod("Both", "Counts", table(None, vec![vec![text(format!("C({n}, {k}) unordered")), text(c.to_string())], vec![text(format!("P({n}, {k}) ordered")), text(perm.to_string())]])),
        ];
        return Some(Answer::new("combinatorics", pods));
    }
    None
}

pub fn handle(q: &Query) -> Option<Answer> {
    dates(q).or_else(|| finance(q)).or_else(|| probability(q)).or_else(|| combinatorics(q))
}
