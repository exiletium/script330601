//! Run a file of queries (one per line, `#` comments) and report which are understood.
//!   cargo run --release --example corpus -- crates/engine/tests/data/corpus.txt
fn main() {
    let path = std::env::args().nth(1).expect("usage: corpus <file> [--fails]");
    let only_fails = std::env::args().any(|a| a == "--fails");
    noesis::domains::live::set_online(std::env::var("NOESIS_TEST_NET").is_ok());
    let text = std::fs::read_to_string(&path).expect("read corpus");
    let (mut ok, mut total) = (0, 0);
    for line in text.lines().map(str::trim).filter(|l| !l.is_empty() && !l.starts_with('#')) {
        total += 1;
        let r = noesis::query(line);
        let ans: String = r.short_answer().unwrap_or_default().replace('\n', " ⏎ ").chars().take(110).collect();
        if r.success {
            ok += 1;
            if !only_fails {
                println!("ok   {line:<52} {ans}");
            }
        } else {
            println!("FAIL {line}");
        }
    }
    println!("{ok}/{total} understood");
}
