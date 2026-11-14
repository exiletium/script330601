//! Everyday calculations: ages and calendar facts, percentages and simple
//! finance, body-mass index, words and letters, and arithmetic with
//! quantities that carry units.

use super::misc::{date_long, money, num, parse_date, today};
use super::units;
use crate::engine::*;
use crate::print::fmt_float;
use chrono::{Datelike, NaiveDate, TimeZone};
use regex::Regex;
use std::sync::OnceLock;

macro_rules! re {
    ($s:expr) => {{
        static R: OnceLock<Regex> = OnceLock::new();
        R.get_or_init(|| Regex::new($s).unwrap())
    }};
}

fn n(v: f64) -> String {
    fmt_float(v, 6)
}

fn group(v: i64) -> String {
    let s = v.abs().to_string();
    let mut out = String::new();
    for (k, ch) in s.chars().enumerate() {
        if k > 0 && (s.len() - k) % 3 == 0 {
            out.push(',');
        }
        out.push(ch);
    }
    if v < 0 { format!("-{out}") } else { out }
}

fn plural(v: i64, word: &str) -> String {
    format!("{v} {word}{}", if v == 1 { "" } else { "s" })
}

// ---------------------------------------------------------------------------
// calendar
// ---------------------------------------------------------------------------

fn is_leap(y: i32) -> bool {
    (y % 4 == 0 && y % 100 != 0) || y % 400 == 0
}

const MONTHS: [&str; 12] = ["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"];

fn month_number(s: &str) -> Option<u32> {
    MONTHS.iter().position(|m| *m == s || (s.len() >= 3 && m.starts_with(s))).map(|k| k as u32 + 1)
}

fn days_in_month(y: i32, m: u32) -> u32 {
    match m {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        _ => if is_leap(y) { 29 } else { 28 },
    }
}

/// Whole years, months and days from `a` to `b` (`a <= b`).
fn ymd_between(a: NaiveDate, b: NaiveDate) -> (i64, i64, i64) {
    let mut years = b.year() as i64 - a.year() as i64;
    let mut months = b.month() as i64 - a.month() as i64;
    let mut days = b.day() as i64 - a.day() as i64;
    if days < 0 {
        months -= 1;
        let (py, pm) = if b.month() == 1 { (b.year() - 1, 12) } else { (b.year(), b.month() - 1) };
        days += days_in_month(py, pm) as i64;
    }
    if months < 0 {
        years -= 1;
        months += 12;
    }
    (years, months, days)
}

fn age(l: &str) -> Option<Answer> {
    let m = re!(r"^(?:how old (?:is|am|are|would|was|will)\s+(?:someone|somebody|a person|anyone|i|you|he|she|they)(?:\s+be)?(?:\s+(?:if|who|that)\s+(?:i was|i am|they were|he was|she was|is|was|were))?\s+born\s+(?:on\s+|in\s+)?(.+?)|age\s+(?:of\s+(?:someone|a person|somebody)\s+)?(?:if\s+)?born\s+(?:on\s+|in\s+)?(.+?)|(?:i was\s+)?born\s+(?:on\s+|in\s+)?(.+?)(?:\s+how old am i)?|age\s+(?:from|since)\s+(.+?))(?:\s+(?:today|now))?$").captures(l)?;
    let ds = m.get(1).or(m.get(2)).or(m.get(3)).or(m.get(4))?.as_str().trim();
    let now = today();
    if let Some(y) = re!(r"^(\d{4})$").captures(ds) {
        let y: i32 = y[1].parse().ok()?;
        let a = now.year() - y;
        if !(0..=150).contains(&a) {
            return None;
        }
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("age of someone born in {y}, as of {}", date_long(now)))),
            primary(pod("Result", "Result", text(format!("{} or {} years old, depending on whether this year's birthday has passed", a - 1, a)))),
        ];
        return Some(Answer::new(format!("age of someone born in {y}"), pods));
    }
    let born = parse_date(ds)?;
    if born > now {
        return None;
    }
    let (y, mo, d) = ymd_between(born, now);
    let total = (now - born).num_days();
    let mut next = NaiveDate::from_ymd_opt(now.year(), born.month(), born.day()).or_else(|| NaiveDate::from_ymd_opt(now.year(), 3, 1))?;
    if next < now {
        next = NaiveDate::from_ymd_opt(now.year() + 1, born.month(), born.day()).or_else(|| NaiveDate::from_ymd_opt(now.year() + 1, 3, 1))?;
    }
    let until = (next - now).num_days();
    let pods = vec![
        pod("Input", "Input interpretation", text(format!("age of someone born on {}, as of {}", date_long(born), date_long(now)))),
        primary(pod("Result", "Age", text(format!("{}, {}, {}", plural(y, "year"), plural(mo, "month"), plural(d, "day"))))),
        pod(
            "Units",
            "In other units",
            table(None, vec![vec![text("months"), text(group(y * 12 + mo))], vec![text("weeks"), text(group(total / 7))], vec![text("days"), text(group(total))], vec![text("hours"), text(group(total * 24))]]),
        ),
        pod("Birthday", "Next birthday", text(if until == 0 { format!("today — turning {y}") } else { format!("{} ({} from now), turning {}", date_long(next), plural(until, "day"), y + 1) })),
        pod("Weekday", "Day of the week of birth", text(born.format("%A").to_string())),
    ];
    Some(Answer::new("age", pods))
}

fn calendar(l: &str) -> Option<Answer> {
    let now = today();
    // leap years
    if let Some(m) = re!(r"^(?:is|was|will)\s+(?:the year\s+)?(\d{1,4}|this year|next year|last year)\s+(?:be\s+)?a leap year$|^leap year\s+(\d{1,4})$").captures(l) {
        let y: i32 = match m.get(1).or(m.get(2))?.as_str() {
            "this year" => now.year(),
            "next year" => now.year() + 1,
            "last year" => now.year() - 1,
            s => s.parse().ok()?,
        };
        let leap = is_leap(y);
        let why = if y % 400 == 0 {
            format!("{y} is divisible by 400")
        } else if y % 100 == 0 {
            format!("{y} is divisible by 100 but not by 400")
        } else if y % 4 == 0 {
            format!("{y} is divisible by 4 and not by 100")
        } else {
            format!("{y} is not divisible by 4")
        };
        let next = (y + 1..).find(|k| is_leap(*k))?;
        let prev = (1..y).rev().find(|k| is_leap(*k)).unwrap_or(0);
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("is {y} a leap year?"))),
            primary(pod("Result", "Result", text(format!("{} — {y} {} a leap year ({} days)", if leap { "Yes" } else { "No" }, if leap { "is" } else { "is not" }, if leap { 366 } else { 365 })))),
            pod("Reason", "Reason", text(format!("{why}. (Gregorian rule: every 4th year, except century years not divisible by 400.)"))),
            pod("Nearby", "Nearest leap years", text(format!("previous: {prev},  next: {next}"))),
        ];
        return Some(Answer::new(format!("is {y} a leap year"), pods));
    }
    if let Some(m) = re!(r"^leap years\s+(?:between|from)\s+(\d{1,4})\s+(?:and|to)\s+(\d{1,4})$").captures(l) {
        let (a, b): (i32, i32) = (m[1].parse().ok()?, m[2].parse().ok()?);
        if b < a || b - a > 2000 {
            return None;
        }
        let ys: Vec<String> = (a..=b).filter(|y| is_leap(*y)).map(|y| y.to_string()).collect();
        let pods = vec![pod("Input", "Input interpretation", text(format!("leap years from {a} to {b}"))), primary(pod("Result", "Result", text(if ys.is_empty() { "none".to_string() } else { ys.join(", ") }))), pod("Count", "Count", text(ys.len().to_string()))];
        return Some(Answer::new("leap years", pods));
    }
    // days in a month or a year
    if let Some(m) = re!(r"^(?:number of\s+)?days\s+(?:are\s+)?(?:there\s+)?in\s+(?:the\s+month\s+of\s+)?([a-z]{3,9})(?:\s+(?:of\s+)?(\d{4}))?$").captures(l) {
        if let Some(mo) = month_number(&m[1]) {
            let year = m.get(2).and_then(|y| y.as_str().parse::<i32>().ok());
            let name = &MONTHS[mo as usize - 1];
            let name = format!("{}{}", name[..1].to_uppercase(), &name[1..]);
            let (label, value) = match year {
                Some(y) => (format!("{name} {y}"), format!("{} days", days_in_month(y, mo))),
                None if mo == 2 => (name.clone(), "28 days (29 in a leap year)".to_string()),
                None => (name.clone(), format!("{} days", days_in_month(2001, mo))),
            };
            let mut pods = vec![pod("Input", "Input interpretation", text(format!("number of days in {label}"))), primary(pod("Result", "Result", text(value)))];
            if let Some(y) = year {
                let first = NaiveDate::from_ymd_opt(y, mo, 1)?;
                let last = NaiveDate::from_ymd_opt(y, mo, days_in_month(y, mo))?;
                pods.push(pod("Span", "First and last day", text(format!("{} to {}", date_long(first), date_long(last)))));
                if mo == 2 {
                    pods.push(pod("Leap", "Leap year", text(format!("{y} {} a leap year", if is_leap(y) { "is" } else { "is not" }))));
                }
            }
            return Some(Answer::new(format!("days in {label}"), pods));
        }
    }
    if let Some(m) = re!(r"^(?:number of\s+)?(days|weeks|hours|minutes|seconds)\s+(?:are\s+)?(?:there\s+)?in\s+(?:the year\s+)?(\d{4})$").captures(l) {
        let y: i32 = m[2].parse().ok()?;
        let days = if is_leap(y) { 366i64 } else { 365 };
        let value = match &m[1] {
            "days" => format!("{days} days"),
            "weeks" => format!("{} weeks and {} day{}", days / 7, days % 7, if days % 7 == 1 { "" } else { "s" }),
            "hours" => format!("{} hours", group(days * 24)),
            "minutes" => format!("{} minutes", group(days * 1440)),
            _ => format!("{} seconds", group(days * 86400)),
        };
        let pods = vec![pod("Input", "Input interpretation", text(format!("number of {} in {y}", &m[1]))), primary(pod("Result", "Result", text(value))), pod("Leap", "Leap year", text(format!("{y} {} a leap year", if is_leap(y) { "is" } else { "is not" })))];
        return Some(Answer::new(format!("{} in {y}", &m[1]), pods));
    }
    // week number, day of the year
    if let Some(m) = re!(r"^(?:the\s+)?(?:current\s+)?(?:iso\s+)?(week number|week of the year|week|day of the year|day of year|day number)(?:\s+(?:of|for|on|is)\s+|\s+)?(.*)$").captures(l) {
        let rest = m[2].trim();
        let d = if matches!(rest, "" | "today" | "now" | "it" | "this week" | "is it" | "the year") { now } else { parse_date(rest)? };
        let week = d.iso_week();
        let total = if is_leap(d.year()) { 366 } else { 365 };
        let week_pod = pod("Week", "ISO week number", text(format!("week {} of {}", week.week(), week.year())));
        let day_pod = pod("DayOfYear", "Day of the year", text(format!("day {} of {total}  ({} remaining)", d.ordinal(), total - d.ordinal())));
        let mut pods = vec![pod("Input", "Input interpretation", text(format!("{} | {}", &m[1], date_long(d))))];
        if m[1].starts_with("week") {
            pods.push(primary(week_pod));
            pods.push(day_pod);
            let monday = d - chrono::Duration::days(d.weekday().num_days_from_monday() as i64);
            pods.push(pod("Span", "That week", text(format!("{} to {}", date_long(monday), date_long(monday + chrono::Duration::days(6))))));
        } else {
            pods.push(primary(day_pod));
            pods.push(week_pod);
        }
        return Some(Answer::new(m[1].to_string(), pods));
    }
    // unix time
    if re!(r"^(?:the\s+)?(?:current\s+)?(?:unix|epoch|posix)\s*(?:time|timestamp|epoch)?(?:\s+(?:now|today|right now))?$").is_match(l) {
        let t = chrono::Utc::now();
        let pods = vec![
            pod("Input", "Input interpretation", text("current Unix time")),
            primary(pod("Result", "Result", text(format!("{} seconds since 1970-01-01 00:00:00 UTC", t.timestamp())))),
            pod("UTC", "Corresponding time", text(t.format("%A, %B %-d, %Y %H:%M:%S UTC").to_string())),
            pod("Forms", "Other forms", table(None, vec![vec![text("milliseconds"), text(t.timestamp_millis().to_string())], vec![text("hexadecimal"), text(format!("0x{:x}", t.timestamp()))], vec![text("days since epoch"), text((t.timestamp() / 86400).to_string())]])),
        ];
        return Some(Answer::new("Unix time", pods));
    }
    if let Some(m) = re!(r"^(?:(?:unix|epoch|posix)\s*(?:time|timestamp)?\s+(?:of\s+|for\s+)?(-?\d{1,13})(?:\s+(?:to|as|in)\s+(?:a\s+)?date)?|(-?\d{9,13})\s+(?:unix|epoch)\s*(?:time|timestamp)?(?:\s+(?:to|as|in)\s+(?:a\s+)?date)?|convert\s+(?:unix\s+)?timestamp\s+(-?\d{1,13})(?:\s+to\s+(?:a\s+)?date)?)$").captures(l) {
        let raw: i64 = m.get(1).or(m.get(2)).or(m.get(3))?.as_str().parse().ok()?;
        let secs = if raw.abs() >= 100_000_000_000 { raw / 1000 } else { raw };
        let t = chrono::Utc.timestamp_opt(secs, 0).single()?;
        let delta = (chrono::Utc::now() - t).num_days();
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("Unix time {raw}{}", if secs != raw { " (milliseconds)" } else { "" }))),
            primary(pod("Result", "Result", text(t.format("%A, %B %-d, %Y %H:%M:%S UTC").to_string()))),
            pod("ISO", "ISO 8601", text(t.format("%Y-%m-%dT%H:%M:%SZ").to_string())),
            pod("Relative", "Relative to now", text(if delta >= 0 { format!("{} ago", plural(delta, "day")) } else { format!("{} from now", plural(-delta, "day")) })),
        ];
        return Some(Answer::new("Unix time to date", pods));
    }
    if let Some(m) = re!(r"^(?:unix|epoch|posix)\s*(?:time|timestamp)\s+(?:of|for|on|at)\s+(.+)$").captures(l) {
        let d = parse_date(&m[1])?;
        let t = chrono::Utc.from_utc_datetime(&d.and_hms_opt(0, 0, 0)?);
        let pods = vec![pod("Input", "Input interpretation", text(format!("Unix time at the start of {} (UTC)", date_long(d)))), primary(pod("Result", "Result", text(t.timestamp().to_string())))];
        return Some(Answer::new("Unix time", pods));
    }
    None
}

// ---------------------------------------------------------------------------
// percentages and simple finance
// ---------------------------------------------------------------------------

fn pct(v: f64) -> String {
    format!("{}%", n(v))
}

fn percentages(l: &str) -> Option<Answer> {
    const N: &str = r"\$?(-?[\d,]*\.?\d+k?)";
    let rx = |s: &str| Regex::new(&s.replace("{N}", N)).ok();
    // discounts
    let disc = rx(r"^(?:(\d+(?:\.\d+)?)\s*(?:%|percent)\s+(?:off|discount on|discount off|discount of)\s+(?:of\s+)?{N}|{N}\s+(?:with|at|less|minus|after)\s+(?:a\s+)?(\d+(?:\.\d+)?)\s*(?:%|percent)\s+(?:off|discount)|discount\s+of\s+(\d+(?:\.\d+)?)\s*(?:%|percent)\s+on\s+{N})$")?;
    if let Some(m) = disc.captures(l) {
        let (p, price) = match (m.get(1), m.get(2), m.get(3), m.get(4), m.get(5), m.get(6)) {
            (Some(p), Some(v), ..) => (num(p.as_str())?, num(v.as_str())?),
            (_, _, Some(v), Some(p), ..) => (num(p.as_str())?, num(v.as_str())?),
            (.., Some(p), Some(v)) => (num(p.as_str())?, num(v.as_str())?),
            _ => return None,
        };
        let save = price * p / 100.0;
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("{} off {}", pct(p), money(price)))),
            primary(pod("Result", "Sale price", text(money(price - save)))),
            pod("Saving", "You save", text(money(save))),
            pod("Method", "Calculation", text(format!("{} × (1 − {}/100) = {}", n(price), n(p), n(price - save)))),
        ];
        return Some(Answer::new("discount", pods));
    }
    // percent change
    let change = rx(r"^(?:the\s+)?(?:percent(?:age)?|%)\s+(change|increase|decrease|difference|growth)\s+(?:from|between|of)\s+{N}\s+(?:to|and)\s+{N}$")?;
    if let Some(m) = change.captures(l) {
        let (a, b) = (num(&m[2])?, num(&m[3])?);
        if &m[1] == "difference" {
            let mean = (a + b) / 2.0;
            if mean == 0.0 {
                return None;
            }
            let d = (a - b).abs() / mean.abs() * 100.0;
            let pods = vec![
                pod("Input", "Input interpretation", text(format!("percent difference between {} and {}", n(a), n(b)))),
                primary(pod("Result", "Result", text(pct(d)))),
                pod("Method", "Calculation", text(format!("|{} − {}| ÷ (({} + {})/2) × 100", n(a), n(b), n(a), n(b)))),
                pod("Change", "As a change from the first value", text(if a != 0.0 { pct((b - a) / a.abs() * 100.0) } else { "undefined".into() })),
            ];
            return Some(Answer::new("percent difference", pods));
        }
        if a == 0.0 {
            return None;
        }
        let c = (b - a) / a.abs() * 100.0;
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("percent change from {} to {}", n(a), n(b)))),
            primary(pod("Result", "Result", text(format!("{}{}  ({})", if c > 0.0 { "+" } else { "" }, pct(c), if c > 0.0 { "increase" } else if c < 0.0 { "decrease" } else { "no change" })))),
            pod("Method", "Calculation", text(format!("({} − {}) ÷ {} × 100", n(b), n(a), n(a)))),
            pod("Absolute", "Absolute change", text(n(b - a))),
            pod("Factor", "Ratio", text(format!("{} is {} times {}", n(b), n(b / a), n(a)))),
        ];
        return Some(Answer::new("percent change", pods));
    }
    // "what percent of A is B", "B is what percent of A", "B as a percentage of A", "B out of A as a percent"
    let what = rx(r"^(?:what\s+(?:percent(?:age)?|%)\s+of\s+{N}\s+is\s+{N}|{N}\s+is\s+what\s+(?:percent(?:age)?|%)\s+of\s+{N}|{N}\s+as\s+a\s+(?:percent(?:age)?|%)\s+of\s+{N}|{N}\s+out\s+of\s+{N}\s+(?:as\s+a\s+|in\s+)?(?:percent(?:age)?|%))$")?;
    if let Some(m) = what.captures(l) {
        let (part, whole) = match (m.get(1), m.get(2), m.get(3), m.get(4), m.get(5), m.get(6), m.get(7), m.get(8)) {
            (Some(w), Some(p), ..) => (num(p.as_str())?, num(w.as_str())?),
            (_, _, Some(p), Some(w), ..) | (_, _, _, _, Some(p), Some(w), ..) | (.., Some(p), Some(w)) => (num(p.as_str())?, num(w.as_str())?),
            _ => return None,
        };
        if whole == 0.0 {
            return None;
        }
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("{} as a percentage of {}", n(part), n(whole)))),
            primary(pod("Result", "Result", text(pct(part / whole * 100.0)))),
            pod("Method", "Calculation", text(format!("{} ÷ {} × 100", n(part), n(whole)))),
        ];
        let mut pods = pods;
        if part.fract() == 0.0 && whole.fract() == 0.0 && part.abs() < 1e15 && whole.abs() < 1e15 {
            let (mut a, mut b) = (part.abs() as u64, whole.abs() as u64);
            while b != 0 {
                (a, b) = (b, a % b);
            }
            let g = a.max(1) as f64;
            let neg = (part < 0.0) != (whole < 0.0);
            pods.push(pod("Fraction", "As a fraction", text(format!("{}{}/{}", if neg { "−" } else { "" }, n(part.abs() / g), n(whole.abs() / g)))));
        }
        return Some(Answer::new("percentage", pods));
    }
    // "A is P% of what"
    let of_what = rx(r"^{N}\s+is\s+(\d+(?:\.\d+)?)\s*(?:%|percent)\s+of\s+what(?:\s+number)?$")?;
    if let Some(m) = of_what.captures(l) {
        let (a, p) = (num(&m[1])?, num(&m[2])?);
        if p == 0.0 {
            return None;
        }
        let pods = vec![pod("Input", "Input interpretation", text(format!("{} is {} of what number?", n(a), pct(p)))), primary(pod("Result", "Result", text(n(a / (p / 100.0))))), pod("Method", "Calculation", text(format!("{} ÷ ({}/100)", n(a), n(p))))];
        return Some(Answer::new("percentage base", pods));
    }
    // increase / decrease by a percentage
    let adj = rx(r"^(?:(increase|decrease|raise|reduce|lower|grow|shrink)\s+{N}\s+by\s+(\d+(?:\.\d+)?)\s*(?:%|percent)|{N}\s*(\+|-|plus|minus|increased by|decreased by|reduced by)\s*(\d+(?:\.\d+)?)\s*(?:%|percent))$")?;
    if let Some(m) = adj.captures(l) {
        let (verb, v, p) = match (m.get(1), m.get(2), m.get(3), m.get(4), m.get(5), m.get(6)) {
            (Some(verb), Some(v), Some(p), ..) => (verb.as_str(), num(v.as_str())?, num(p.as_str())?),
            (_, _, _, Some(v), Some(verb), Some(p)) => (verb.as_str(), num(v.as_str())?, num(p.as_str())?),
            _ => return None,
        };
        let up = matches!(verb, "increase" | "raise" | "grow" | "+" | "plus" | "increased by");
        let r = v * (1.0 + if up { p } else { -p } / 100.0);
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("{} {} by {}", n(v), if up { "increased" } else { "decreased" }, pct(p)))),
            primary(pod("Result", "Result", text(n(r)))),
            pod("Method", "Calculation", text(format!("{} × (1 {} {}/100)", n(v), if up { "+" } else { "−" }, n(p)))),
            pod("Change", "Amount of change", text(n((r - v).abs()))),
        ];
        return Some(Answer::new("percentage change", pods));
    }
    // tips, also split between people
    let tip = rx(r"^(?:tip\s+(?:of\s+)?(\d+(?:\.\d+)?)\s*(?:%|percent)\s+(?:on|for|of)\s+{N}|(\d+(?:\.\d+)?)\s*(?:%|percent)\s+tip\s+(?:on|for)\s+{N})(?:\s+(?:bill\s+)?split\s+(?:between\s+|among\s+|by\s+)?(\d+)(?:\s+(?:ways|people|persons))?)?$")?;
    if let Some(m) = tip.captures(l) {
        let (p, bill) = match (m.get(1), m.get(2), m.get(3), m.get(4)) {
            (Some(p), Some(b), ..) | (_, _, Some(p), Some(b)) => (num(p.as_str())?, num(b.as_str())?),
            _ => return None,
        };
        let people: f64 = m.get(5).and_then(|g| g.as_str().parse().ok()).unwrap_or(1.0);
        let (t, total) = (bill * p / 100.0, bill * (1.0 + p / 100.0));
        let mut pods = vec![pod("Input", "Input interpretation", text(format!("{} tip on {}{}", pct(p), money(bill), if people > 1.0 { format!(", split {people} ways") } else { String::new() }))), primary(pod("Result", "Tip", text(format!("{}   (total {})", money(t), money(total)))))];
        if people > 1.0 {
            pods.push(pod("Split", "Per person", text(format!("{} each  ({} tip each)", money(total / people), money(t / people)))));
        }
        let rows: Vec<Vec<Content>> = [10.0, 15.0, 18.0, 20.0, 25.0].iter().map(|r| vec![text(format!("{r}%")), text(money(bill * r / 100.0)), text(money(bill * (1.0 + r / 100.0)))]).collect();
        pods.push(pod("Table", "Common tip amounts", table(Some(vec!["rate", "tip", "total"]), rows)));
        return Some(Answer::new("tip", pods));
    }
    // sales tax
    let tax = rx(r"^(?:(?:sales\s+)?(?:tax|vat)\s+(?:of\s+|at\s+)?(\d+(?:\.\d+)?)\s*(?:%|percent)\s+on\s+{N}|{N}\s+(?:plus|with|\+|and|including)\s+(\d+(?:\.\d+)?)\s*(?:%|percent)\s+(?:sales\s+)?(?:tax|vat)|(\d+(?:\.\d+)?)\s*(?:%|percent)\s+(?:sales\s+)?(?:tax|vat)\s+on\s+{N})$")?;
    if let Some(m) = tax.captures(l) {
        let (p, price) = match (m.get(1), m.get(2), m.get(3), m.get(4), m.get(5), m.get(6)) {
            (Some(p), Some(v), ..) | (.., Some(p), Some(v)) => (num(p.as_str())?, num(v.as_str())?),
            (_, _, Some(v), Some(p), ..) => (num(p.as_str())?, num(v.as_str())?),
            _ => return None,
        };
        let t = price * p / 100.0;
        let pods = vec![pod("Input", "Input interpretation", text(format!("{} tax on {}", pct(p), money(price)))), primary(pod("Result", "Total with tax", text(money(price + t)))), pod("Tax", "Tax amount", text(money(t)))];
        return Some(Answer::new("sales tax", pods));
    }
    // simple interest
    let simple = rx(r"^simple interest\s*(?:on|of|for)?\s*{N}\s*(?:at|with|@)\s*(\d+(?:\.\d+)?)\s*(?:%|percent)(?:\s*(?:per year|annually|apr|interest|a year|per annum))?\s*(?:for|over)\s*(\d+(?:\.\d+)?)\s*(years?|months?)$")?;
    if let Some(m) = simple.captures(l) {
        let (p, r, t) = (num(&m[1])?, num(&m[2])? / 100.0, num(&m[3])?);
        let years = if m[4].starts_with("month") { t / 12.0 } else { t };
        let interest = p * r * years;
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("simple interest on {} at {} per year for {} {}", money(p), pct(r * 100.0), n(t), &m[4]))),
            primary(pod("Result", "Interest", text(money(interest)))),
            pod("Total", "Final amount", text(money(p + interest))),
            pod("Formula", "Formula", text(format!("I = P·r·t = {} × {} × {}", n(p), n(r), n(years)))),
            pod("Compare", "With annual compounding instead", text(format!("{} interest", money(p * (1.0 + r).powf(years) - p)))),
        ];
        return Some(Answer::new("simple interest", pods));
    }
    // present value
    let pv = rx(r"^(?:the\s+)?present value\s*(?:of)?\s*{N}\s*(?:due\s+|received\s+|paid\s+)?(?:(?:in|after)\s*(\d+(?:\.\d+)?)\s*years?\s*(?:at|with|@|discounted at)\s*(?:a\s+)?(?:discount rate of\s+|rate of\s+)?(\d+(?:\.\d+)?)\s*(?:%|percent)|(?:at|with|@|discounted at)\s*(\d+(?:\.\d+)?)\s*(?:%|percent)\s*(?:in|after|for|over)\s*(\d+(?:\.\d+)?)\s*years?)(?:\s+(?:interest|discount rate|per year|annually))?$")?;
    if let Some(m) = pv.captures(l) {
        let f = num(&m[1])?;
        let (t, r) = match (m.get(2), m.get(3), m.get(4), m.get(5)) {
            (Some(t), Some(r), ..) => (num(t.as_str())?, num(r.as_str())? / 100.0),
            (_, _, Some(r), Some(t)) => (num(t.as_str())?, num(r.as_str())? / 100.0),
            _ => return None,
        };
        let v = f / (1.0 + r).powf(t);
        let rows: Vec<Vec<Content>> = (0..=(t.ceil() as usize).min(40)).map(|y| vec![text(y.to_string()), text(money(v * (1.0 + r).powi(y as i32)))]).collect();
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("present value of {} received in {} years, discounted at {} per year", money(f), n(t), pct(r * 100.0)))),
            primary(pod("Result", "Present value", text(money(v)))),
            pod("Formula", "Formula", text(format!("PV = FV ÷ (1 + r)^t = {} ÷ {}^{}", n(f), n(1.0 + r), n(t)))),
            pod("Discount", "Discount factor", text(n(1.0 / (1.0 + r).powf(t)))),
            pod("Growth", "Value by year", table(Some(vec!["year", "value"]), rows)),
        ];
        return Some(Answer::new("present value", pods));
    }
    // doubling time
    if let Some(m) = re!(r"^(?:doubling time|time to double|rule of 72|how long to double(?: money| an investment)?)\s*(?:at|for|with|@)?\s*(\d+(?:\.\d+)?)\s*(?:%|percent)(?:\s+(?:interest|growth|per year|annually))?$").captures(l) {
        let r = num(&m[1])? / 100.0;
        if r <= 0.0 {
            return None;
        }
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("doubling time at {} growth per year", pct(r * 100.0)))),
            primary(pod("Result", "Result", text(format!("{} years", n(2f64.ln() / (1.0 + r).ln()))))),
            pod("Rule", "Rule of 72 estimate", text(format!("72 ÷ {} ≈ {} years", n(r * 100.0), n(72.0 / (r * 100.0))))),
            pod("Formula", "Formula", text("t = ln 2 ÷ ln(1 + r)")),
        ];
        return Some(Answer::new("doubling time", pods));
    }
    None
}

// ---------------------------------------------------------------------------
// body-mass index
// ---------------------------------------------------------------------------

fn bmi(l: &str) -> Option<Answer> {
    let rest = re!(r"^(?:my\s+)?(?:bmi|body mass index)\b(.*)$").captures(l)?.get(1)?.as_str().to_string();
    let w = re!(r"(\d+(?:\.\d+)?)\s*(kg|kilograms?|kilos?|lbs?|pounds?|stone|st)\b").captures(&rest)?;
    let kg = {
        let v: f64 = w[1].parse().ok()?;
        match &w[2] {
            u if u.starts_with("kg") || u.starts_with("kilo") => v,
            u if u.starts_with("st") => v * 6.350_293_18,
            _ => v * 0.453_592_37,
        }
    };
    let rest2 = rest.replace(&w[0], " ");
    let metres = if let Some(h) = re!(r#"(\d+)\s*(?:ft|feet|foot|')\s*(?:(\d+(?:\.\d+)?)\s*(?:in|inches|inch|")?)?"#).captures(&rest2) {
        h[1].parse::<f64>().ok()? * 0.3048 + h.get(2).and_then(|g| g.as_str().parse::<f64>().ok()).unwrap_or(0.0) * 0.0254
    } else if let Some(h) = re!(r#"(\d+(?:\.\d+)?)\s*(cm|centimeters?|centimetres?|m|meters?|metres?|in|inches|")"#).captures(&rest2) {
        let v: f64 = h[1].parse().ok()?;
        match &h[2] {
            u if u.starts_with('c') => v / 100.0,
            u if u.starts_with('m') => v,
            _ => v * 0.0254,
        }
    } else {
        return None;
    };
    if metres <= 0.0 || kg <= 0.0 {
        return None;
    }
    let b = kg / (metres * metres);
    let class = match b {
        x if x < 16.0 => "severely underweight",
        x if x < 18.5 => "underweight",
        x if x < 25.0 => "normal weight",
        x if x < 30.0 => "overweight",
        x if x < 35.0 => "obese (class I)",
        x if x < 40.0 => "obese (class II)",
        _ => "obese (class III)",
    };
    let (lo, hi) = (18.5 * metres * metres, 25.0 * metres * metres);
    let pods = vec![
        pod("Input", "Input interpretation", text(format!("body mass index | mass {} kg | height {} m", fmt_float(kg, 4), fmt_float(metres, 4)))),
        primary(pod("Result", "Body mass index", text(format!("{} kg/m²  ({class})", fmt_float(b, 3))))),
        pod("Formula", "Formula", text(format!("BMI = mass ÷ height² = {} ÷ {}²", fmt_float(kg, 4), fmt_float(metres, 4)))),
        pod("Range", "Normal-weight range for this height", text(format!("{} kg to {} kg  ({} lb to {} lb)", fmt_float(lo, 3), fmt_float(hi, 3), fmt_float(lo / 0.453_592_37, 3), fmt_float(hi / 0.453_592_37, 3)))),
        pod(
            "Classes",
            "WHO classification (adults)",
            table(Some(vec!["BMI", "category"]), vec![vec![text("below 18.5"), text("underweight")], vec![text("18.5 – 24.9"), text("normal weight")], vec![text("25 – 29.9"), text("overweight")], vec![text("30 and above"), text("obese")]]),
        ),
    ];
    Some(Answer::new("body mass index", pods))
}

// ---------------------------------------------------------------------------
// words
// ---------------------------------------------------------------------------

const MORSE: &[(char, &str)] = &[
    ('a', ".-"), ('b', "-..."), ('c', "-.-."), ('d', "-.."), ('e', "."), ('f', "..-."), ('g', "--."), ('h', "...."), ('i', ".."), ('j', ".---"), ('k', "-.-"), ('l', ".-.."), ('m', "--"),
    ('n', "-."), ('o', "---"), ('p', ".--."), ('q', "--.-"), ('r', ".-."), ('s', "..."), ('t', "-"), ('u', "..-"), ('v', "...-"), ('w', ".--"), ('x', "-..-"), ('y', "-.--"), ('z', "--.."),
    ('0', "-----"), ('1', ".----"), ('2', "..---"), ('3', "...--"), ('4', "....-"), ('5', "....."), ('6', "-...."), ('7', "--..."), ('8', "---.."), ('9', "----."),
    ('.', ".-.-.-"), (',', "--..--"), ('?', "..--.."), ('!', "-.-.--"), ('/', "-..-."), ('-', "-....-"), ('@', ".--.-."),
];

const NATO: [&str; 26] = ["Alfa", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot", "Golf", "Hotel", "India", "Juliett", "Kilo", "Lima", "Mike", "November", "Oscar", "Papa", "Quebec", "Romeo", "Sierra", "Tango", "Uniform", "Victor", "Whiskey", "X-ray", "Yankee", "Zulu"];

fn scrabble_value(c: char) -> u32 {
    match c.to_ascii_lowercase() {
        'a' | 'e' | 'i' | 'o' | 'u' | 'l' | 'n' | 's' | 't' | 'r' => 1,
        'd' | 'g' => 2,
        'b' | 'c' | 'm' | 'p' => 3,
        'f' | 'h' | 'v' | 'w' | 'y' => 4,
        'k' => 5,
        'j' | 'x' => 8,
        'q' | 'z' => 10,
        _ => 0,
    }
}

fn unquote(s: &str) -> &str {
    s.trim().trim_matches(|c| c == '"' || c == '\'' || c == '“' || c == '”')
}

/// The tail of the original-case query that corresponds to a capture taken
/// from the lowercased query.
fn original<'a>(q: &'a Query, lowered: &str) -> &'a str {
    let c = q.clean.trim();
    match q.lower.trim().rfind(lowered) {
        Some(at) if c.is_char_boundary(at) && at + lowered.len() <= c.len() && c.is_char_boundary(at + lowered.len()) => &c[at..at + lowered.len()],
        _ => c,
    }
}

fn words(q: &Query) -> Option<Answer> {
    let l = q.lower.trim();
    // letter counts
    if let Some(m) = re!(r"^(?:number of\s+|count\s+(?:the\s+)?)?(letters|characters|vowels|consonants)\s+(?:are\s+)?(?:there\s+)?in\s+(?:the\s+(?:word|name|phrase)\s+)?(.+)$").captures(l) {
        let w = unquote(original(q, &m[2]));
        if w.is_empty() || w.contains('{') || w.contains('=') {
            return None;
        }
        let letters: Vec<char> = w.chars().filter(|c| c.is_alphabetic()).collect();
        let vowels = letters.iter().filter(|c| "aeiouAEIOU".contains(**c)).count();
        let count = match &m[1] {
            "letters" => letters.len(),
            "characters" => w.chars().count(),
            "vowels" => vowels,
            _ => letters.len() - vowels,
        };
        let mut freq: Vec<(char, usize)> = Vec::new();
        for c in letters.iter().map(|c| c.to_ascii_lowercase()) {
            match freq.iter_mut().find(|(k, _)| *k == c) {
                Some(e) => e.1 += 1,
                None => freq.push((c, 1)),
            }
        }
        freq.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("number of {} in \"{w}\"", &m[1]))),
            primary(pod("Result", "Result", text(count.to_string()))),
            pod("Breakdown", "Breakdown", table(None, vec![vec![text("letters"), text(letters.len().to_string())], vec![text("vowels"), text(vowels.to_string())], vec![text("consonants"), text((letters.len() - vowels).to_string())], vec![text("distinct letters"), text(freq.len().to_string())]])),
            pod("Frequency", "Letter frequency", table(Some(vec!["letter", "count"]), freq.iter().map(|(c, k)| vec![text(c.to_string()), text(k.to_string())]).collect())),
        ];
        return Some(Answer::new(format!("{} in \"{w}\"", &m[1]), pods));
    }
    // scrabble
    if let Some(m) = re!(r"^(?:the\s+)?scrabble\s*(?:score|value|points|word score)?\s*(?:of|for)?\s+(?:the\s+word\s+)?([a-z]+)$|^([a-z]+)\s+scrabble\s*(?:score|value|points)$").captures(l) {
        let w = m.get(1).or(m.get(2))?.as_str();
        if matches!(w, "score" | "value" | "points") {
            return None;
        }
        let total: u32 = w.chars().map(scrabble_value).sum();
        let rows: Vec<Vec<Content>> = w.chars().map(|c| vec![text(c.to_ascii_uppercase().to_string()), text(scrabble_value(c).to_string())]).collect();
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("Scrabble score of \"{w}\" (English tile values)"))),
            primary(pod("Result", "Result", text(format!("{total} points")))),
            pod("Tiles", "Tile values", table(Some(vec!["letter", "points"]), rows)),
            pod("Note", "Note", text(if w.len() == 7 { "Board bonus squares are not included; playing all seven tiles adds a 50-point bonus." } else { "Board bonus squares (double/triple letter and word) are not included." })),
        ];
        return Some(Answer::new(format!("Scrabble score of {w}"), pods));
    }
    // palindromes (numbers are handled by the number analyzer)
    if let Some(m) = re!(r"^is\s+(.+?)\s+a\s+palindrome$").captures(l) {
        let w = unquote(original(q, &m[1]));
        let norm: Vec<char> = w.chars().filter(|c| c.is_alphanumeric()).flat_map(|c| c.to_lowercase()).collect();
        if norm.is_empty() {
            return None;
        }
        let rev: Vec<char> = norm.iter().rev().cloned().collect();
        let yes = norm == rev;
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("is \"{w}\" a palindrome?"))),
            primary(pod("Result", "Result", text(format!("{} — \"{w}\" {} the same forwards and backwards", if yes { "Yes" } else { "No" }, if yes { "reads" } else { "does not read" })))),
            pod("Reversed", "Reversed", text(w.chars().rev().collect::<String>())),
        ];
        return Some(Answer::new("palindrome test", pods));
    }
    // reversal and case
    if let Some(m) = re!(r"^(reverse|uppercase|upper case|capitalize|lowercase|lower case|title case)\s+(?:of\s+)?(?:the\s+(?:word|string|text|letters of|phrase)\s+)?(.+)$").captures(l) {
        let w = unquote(original(q, &m[2]));
        if w.is_empty() || w.contains('{') || w.contains('[') || w.starts_with("the list") || w.starts_with("the digits") {
            return None;
        }
        let out = match &m[1] {
            "reverse" => w.chars().rev().collect::<String>(),
            "uppercase" | "upper case" => w.to_uppercase(),
            "lowercase" | "lower case" => w.to_lowercase(),
            _ => w.split(' ').map(|p| p.chars().next().map_or(String::new(), |f| format!("{}{}", f.to_uppercase(), &p[f.len_utf8()..]))).collect::<Vec<_>>().join(" "),
        };
        let pods = vec![pod("Input", "Input interpretation", text(format!("{} \"{w}\"", &m[1]))), primary(pod("Result", "Result", text(out))), pod("Length", "Length", text(format!("{} characters", w.chars().count())))];
        return Some(Answer::new(m[1].to_string(), pods));
    }
    // Morse code
    if let Some(m) = re!(r"^(?:(.+?)\s+(?:in|to|into|as)\s+morse(?:\s+code)?|morse(?:\s+code)?\s+(?:for|of)\s+(.+))$").captures(l) {
        let w = unquote(m.get(1).or(m.get(2))?.as_str()).trim_start_matches("translate ").trim_start_matches("convert ").to_string();
        let code: Option<Vec<String>> = w.split_whitespace().map(|word| word.chars().map(|c| MORSE.iter().find(|(k, _)| *k == c).map(|(_, v)| *v)).collect::<Option<Vec<_>>>().map(|v| v.join(" "))).collect();
        let code = code?.join("  /  ");
        let rows: Vec<Vec<Content>> = w.chars().filter(|c| !c.is_whitespace()).filter_map(|c| MORSE.iter().find(|(k, _)| *k == c).map(|(_, v)| vec![text(c.to_ascii_uppercase().to_string()), text(*v)])).collect();
        let pods = vec![pod("Input", "Input interpretation", text(format!("\"{w}\" in Morse code"))), primary(pod("Result", "Result", text(code))), pod("Letters", "Letter by letter", table(Some(vec!["character", "code"]), rows))];
        return Some(Answer::new("Morse code", pods));
    }
    // NATO phonetic spelling
    if let Some(m) = re!(r"^(?:(.+?)\s+(?:in|to|into|as)\s+(?:the\s+)?(?:nato|phonetic)(?:\s+phonetic)?(?:\s+alphabet)?|(?:nato|phonetic)(?:\s+phonetic)?(?:\s+alphabet)?\s+(?:for|of)\s+(.+)|spell\s+(.+?)\s+phonetically)$").captures(l) {
        let w = unquote(m.get(1).or(m.get(2)).or(m.get(3))?.as_str()).to_string();
        let parts: Vec<String> = w.chars().filter(|c| c.is_ascii_alphabetic()).map(|c| NATO[(c.to_ascii_lowercase() as u8 - b'a') as usize].to_string()).collect();
        if parts.is_empty() {
            return None;
        }
        let pods = vec![pod("Input", "Input interpretation", text(format!("\"{w}\" in the NATO phonetic alphabet"))), primary(pod("Result", "Result", text(parts.join(" "))))];
        return Some(Answer::new("NATO phonetic alphabet", pods));
    }
    // character codes
    if let Some(m) = re!(r"^(?:ascii|unicode)(?:\s+(?:code|value|number|code point))?\s+(?:of|for)\s+(?:the\s+(?:letter|character)\s+)?(.+)$").captures(l) {
        let w = unquote(original(q, &m[1]));
        let chars: Vec<char> = w.chars().collect();
        if chars.is_empty() || chars.len() > 24 {
            return None;
        }
        let rows: Vec<Vec<Content>> = chars.iter().map(|c| vec![text(c.to_string()), text((*c as u32).to_string()), text(format!("U+{:04X}", *c as u32)), text(format!("{:08b}", *c as u32))]).collect();
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("character code of \"{w}\""))),
            primary(pod("Result", "Result", text(chars.iter().map(|c| (*c as u32).to_string()).collect::<Vec<_>>().join(" ")))),
            pod("Codes", "Codes", table(Some(vec!["character", "decimal", "code point", "binary"]), rows)),
        ];
        return Some(Answer::new("character code", pods));
    }
    if let Some(m) = re!(r"^(?:character|char|chr|ascii character|ascii)\s+(?:code\s+)?(?:for\s+|with code\s+)?(\d{1,7})$").captures(l) {
        let c = char::from_u32(m[1].parse().ok()?)?;
        if c.is_control() {
            return None;
        }
        let pods = vec![pod("Input", "Input interpretation", text(format!("character with code {}", &m[1]))), primary(pod("Result", "Result", text(c.to_string()))), pod("Codes", "Code point", text(format!("U+{:04X}", c as u32)))];
        return Some(Answer::new("character from code", pods));
    }
    // word and character counts of quoted text
    if let Some(m) = re!(r#"^(?:word count|number of words|count (?:the )?words|length|number of characters)\s+(?:of|in)\s+["“'](.+)["”']$"#).captures(l) {
        let w = &m[1];
        let pods = vec![
            pod("Input", "Input interpretation", text(format!("counts for \"{w}\""))),
            primary(pod("Result", "Result", table(None, vec![vec![text("words"), text(w.split_whitespace().count().to_string())], vec![text("characters"), text(w.chars().count().to_string())], vec![text("characters without spaces"), text(w.chars().filter(|c| !c.is_whitespace()).count().to_string())]]))),
        ];
        return Some(Answer::new("text counts", pods));
    }
    None
}

// ---------------------------------------------------------------------------
// arithmetic with quantities
// ---------------------------------------------------------------------------

fn quantities(q: &Query) -> Option<Answer> {
    let c = q.clean.trim();
    let c = c.strip_prefix("convert ").or_else(|| c.strip_prefix("Convert ")).unwrap_or(c);
    // optional target unit after the last " in " / " to "
    let mut body = c;
    let mut target: Option<units::Unit> = None;
    for sep in [" in ", " to ", " into ", " as "] {
        if let Some(k) = c.rfind(sep) {
            if let Some(u) = units::parse_unit(c[k + sep.len()..].trim()) {
                if !c[k + sep.len()..].chars().any(|ch| ch.is_ascii_digit()) {
                    body = &c[..k];
                    target = Some(u);
                    break;
                }
            }
        }
    }
    // split into signed "number unit" terms
    let mut terms: Vec<(f64, f64, String)> = Vec::new();
    let mut sign = 1.0;
    for tok in body.split_whitespace() {
        let lower = tok.to_lowercase();
        match lower.as_str() {
            "+" | "plus" | "and" => {
                sign = 1.0;
                continue;
            }
            "-" | "−" | "minus" => {
                sign = -1.0;
                continue;
            }
            _ => {}
        }
        let tok = tok.trim_end_matches(',');
        if tok.chars().next()?.is_ascii_digit() || tok.starts_with('.') {
            let split = tok.find(|ch: char| !(ch.is_ascii_digit() || ch == '.' || ch == ',')).unwrap_or(tok.len());
            let v: f64 = tok[..split].replace(',', "").parse().ok()?;
            terms.push((sign, v, tok[split..].to_string()));
            sign = 1.0;
        } else {
            let last = terms.last_mut()?;
            if !last.2.is_empty() {
                last.2.push(' ');
            }
            last.2.push_str(tok);
        }
    }
    if terms.len() < 2 || !terms.iter().any(|t| t.2.len() >= 2) {
        return None;
    }
    let mut parsed: Vec<(f64, f64, units::Unit)> = Vec::new();
    for (s, v, u) in &terms {
        let unit = units::parse_unit(u)?;
        if unit.offset != 0.0 {
            return None;
        }
        parsed.push((*s, *v, unit));
    }
    let dim = parsed[0].2.dim;
    if parsed.iter().any(|t| t.2.dim != dim) || target.as_ref().is_some_and(|t| t.dim != dim || t.offset != 0.0) {
        return None;
    }
    let si: f64 = parsed.iter().map(|(s, v, u)| s * units::to_si(*v, u)).sum();
    let shown = parsed
        .iter()
        .enumerate()
        .map(|(k, (s, v, u))| format!("{}{} {}", if k == 0 { if *s < 0.0 { "−" } else { "" } } else if *s < 0.0 { "− " } else { "+ " }, n(*v), if *v == 1.0 { units::singular(&u.name) } else { u.name.clone() }))
        .collect::<Vec<_>>()
        .join(" ");
    // answer in the requested unit, otherwise in the largest unit used
    let out = target.clone().unwrap_or_else(|| parsed.iter().map(|t| t.2.clone()).max_by(|a, b| a.factor.partial_cmp(&b.factor).unwrap_or(std::cmp::Ordering::Equal)).unwrap());
    let value = units::from_si(si, &out);
    let label = match &target {
        Some(t) => format!("{shown} in {}", t.name),
        None => shown.clone(),
    };
    let mut pods = vec![pod("Input", "Input interpretation", text(label.clone()))];
    let plain_text = format!("{} {}", fmt_float(value, 10), if value == 1.0 { units::singular(&out.name) } else { out.name.clone() });
    let plain = pod("Result", "Result", text(plain_text.clone()));
    let is_time = units::parse_unit("s").is_some_and(|s| s.dim == dim);
    if is_time && si >= 60.0 && (si - si.round()).abs() < 1e-6 {
        let total = si.round() as i64;
        let (d, h, m, s) = (total / 86400, total % 86400 / 3600, total % 3600 / 60, total % 60);
        let parts: Vec<String> = [(d, "day"), (h, "hour"), (m, "minute"), (s, "second")].iter().filter(|(v, _)| *v > 0).map(|(v, w)| plural(*v, w)).collect();
        let duration = pod("Duration", "As a duration", text(parts.join(" ")));
        if target.is_none() {
            pods.push(primary(duration));
            pods.push(pod("Decimal", "In one unit", text(plain_text)));
        } else {
            pods.push(primary(plain));
            pods.push(duration);
        }
    } else {
        pods.push(primary(plain));
    }
    if let Some(t) = units::conversion_table(si, &dim, &out.name) {
        pods.push(t);
    }
    Some(Answer::new(label, pods))
}

pub fn handle(q: &Query) -> Option<Answer> {
    let l = q.lower.trim();
    age(l).or_else(|| calendar(l)).or_else(|| percentages(l)).or_else(|| bmi(l)).or_else(|| words(q)).or_else(|| quantities(q))
}
