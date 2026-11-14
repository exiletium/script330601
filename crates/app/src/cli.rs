//! Command-line interface.

use noesis::engine::{content_text, Content, Options, QueryResult};
use std::io::{BufRead, Write};

pub const HELP: &str = "Noesis — open computational knowledge engine

USAGE:
    noesis                      open the desktop app
    noesis [OPTIONS] <QUERY>    answer a query in the terminal
    noesis --repl               interactive prompt

OPTIONS:
    --json             print the full result as JSON
    --short            print only the primary answer
    --device <D>       compute device: auto (default), cpu or gpu
    --digits <N>       significant digits for decimals (default 6)
    --timeout <S>      time limit in seconds (default 20)
    --no-steps         skip step-by-step solutions
    --offline          never use the network (no live weather, rates, encyclopedia)
    --svg <DIR>        write plots as SVG files into DIR
    --benchmark        measure CPU/GPU evaluation throughput
    --devices          show available compute devices
    -h, --help         show this help

EXAMPLES:
    noesis \"integrate x^2 sin(x) dx\"
    noesis --short \"solve x^3 - 6x^2 + 11x - 6 = 0\"
    noesis \"10 miles in km\"
    noesis --json \"molar mass of caffeine\"";

pub fn run(args: Vec<String>) -> i32 {
    let mut opts = Options::default();
    let mut json = false;
    let mut short = false;
    let mut svg_dir: Option<String> = None;
    let mut words: Vec<String> = Vec::new();
    let mut it = args.into_iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "-h" | "--help" => {
                println!("{HELP}");
                return 0;
            }
            "--json" => json = true,
            "--short" => short = true,
            "--no-steps" => opts.steps = false,
            "--offline" => noesis::domains::live::set_online(false),
            "--device" => noesis::compute::set_preference(&it.next().unwrap_or_default()),
            "--digits" => opts.digits = it.next().and_then(|v| v.parse().ok()).unwrap_or(6),
            "--timeout" => opts.timeout = it.next().and_then(|v| v.parse().ok()).unwrap_or(20.0),
            "--svg" => svg_dir = it.next(),
            "--benchmark" => {
                benchmark();
                return 0;
            }
            "--devices" => {
                let i = noesis::compute::info();
                println!("CPU: {} threads", i.cpu_threads);
                match (i.gpu, i.gpu_backend) {
                    (Some(g), Some(b)) => println!("GPU: {g} ({b})"),
                    _ => println!("GPU: none available (CPU fallback in use)"),
                }
                println!("active: {}", noesis::compute::device_label());
                return 0;
            }
            "--repl" => return repl(&opts),
            _ => words.push(a),
        }
    }
    if words.is_empty() {
        println!("{HELP}");
        return 2;
    }
    let q = words.join(" ");
    let r = noesis::query_with(&q, &opts);
    if let Some(dir) = &svg_dir {
        write_svgs(&r, dir);
    }
    if json {
        println!("{}", serde_json::to_string_pretty(&r).unwrap_or_default());
    } else if short {
        match r.short_answer() {
            Some(a) if r.success => println!("{a}"),
            _ => {
                eprintln!("{}", r.error.clone().unwrap_or_else(|| "no answer".into()));
                return 1;
            }
        }
    } else {
        print_result(&r);
    }
    if r.success {
        0
    } else {
        1
    }
}

fn bold(s: &str) -> String {
    if std::env::var("NO_COLOR").is_ok() {
        s.to_string()
    } else {
        format!("\x1b[1m{s}\x1b[0m")
    }
}

fn dim(s: &str) -> String {
    if std::env::var("NO_COLOR").is_ok() {
        s.to_string()
    } else {
        format!("\x1b[2m{s}\x1b[0m")
    }
}

pub fn print_result(r: &QueryResult) {
    if !r.success {
        println!("{}", r.error.clone().unwrap_or_default());
        if !r.suggestions.is_empty() {
            println!("Try: {}", r.suggestions.join("  ·  "));
        }
        return;
    }
    for p in &r.pods {
        let title = if p.primary { format!("{} ★", p.title) } else { p.title.clone() };
        println!("{}", bold(&title));
        for sp in &p.subpods {
            if !sp.title.is_empty() {
                println!("  {}", dim(&sp.title));
            }
            let body = match &sp.content {
                Content::Table { header, rows } => {
                    let mut lines = Vec::new();
                    let ncol = rows.iter().map(|r| r.len()).max().unwrap_or(0).max(header.as_ref().map_or(0, |h| h.len()));
                    let mut widths = vec![0usize; ncol];
                    let cells: Vec<Vec<String>> = rows.iter().map(|r| r.iter().map(content_text).collect()).collect();
                    for row in cells.iter().chain(header.iter()) {
                        for (k, c) in row.iter().enumerate() {
                            widths[k] = widths[k].max(c.chars().count());
                        }
                    }
                    let fmt_row = |row: &Vec<String>| row.iter().enumerate().map(|(k, c)| format!("{c:<w$}", w = widths[k])).collect::<Vec<_>>().join("  │  ");
                    if let Some(h) = header {
                        lines.push(dim(&fmt_row(h)));
                    }
                    for row in &cells {
                        lines.push(fmt_row(row));
                    }
                    lines.join("\n")
                }
                c => content_text(c),
            };
            for line in body.lines() {
                println!("  {line}");
            }
        }
        println!();
    }
    println!("{}", dim(&format!("{} pods · {:.0} ms · {}", r.pods.len(), r.timing_ms, r.device)));
}

fn write_svgs(r: &QueryResult, dir: &str) {
    let _ = std::fs::create_dir_all(dir);
    let mut k = 0;
    for p in &r.pods {
        for sp in &p.subpods {
            if let Content::Plot { plot } = &sp.content {
                if plot.grid.is_some() {
                    continue;
                }
                k += 1;
                let path = std::path::Path::new(dir).join(format!("{}-{k}.svg", p.id));
                if std::fs::write(&path, noesis::plot::to_svg(plot, false, 640.0, 400.0)).is_ok() {
                    eprintln!("wrote {}", path.display());
                }
            }
        }
    }
}

fn repl(opts: &Options) -> i32 {
    let stdin = std::io::stdin();
    loop {
        print!("noesis> ");
        let _ = std::io::stdout().flush();
        let mut line = String::new();
        if stdin.lock().read_line(&mut line).unwrap_or(0) == 0 {
            return 0;
        }
        let q = line.trim();
        if q.is_empty() {
            continue;
        }
        if matches!(q, "exit" | "quit" | ":q") {
            return 0;
        }
        print_result(&noesis::query_with(q, opts));
    }
}

fn benchmark() {
    let i = noesis::compute::info();
    println!("Devices: CPU {} threads; GPU {}", i.cpu_threads, i.gpu.clone().unwrap_or_else(|| "none".into()));
    println!("Evaluating sin(x)·cos(y) + exp(-(x²+y²)/4)·sqrt(|xy|+1) on a 1024×1024 grid:");
    for (dev, rate) in noesis::compute::benchmark() {
        println!("  {dev:<40} {:>10.1} M evaluations/s", rate / 1e6);
    }
}
