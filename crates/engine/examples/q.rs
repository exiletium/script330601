fn main() {
    let json = std::env::args().any(|a| a == "--json");
    for a in std::env::args().skip(1).filter(|a| a != "--json") {
        let r = noesis::query(&a);
        if json {
            println!("{}", serde_json::to_string(&r).unwrap().chars().take(3000).collect::<String>());
            continue;
        }
        println!("=== {a}   [{:.0} ms, {}]  interp: {:?}", r.timing_ms, r.device, r.interpretation);
        if let Some(e) = &r.error { println!("  ERROR {e}  suggestions {:?}", r.suggestions); }
        for p in &r.pods {
            let star = if p.primary { "*" } else { " " };
            for (k, sp) in p.subpods.iter().enumerate() {
                let t = noesis::engine::content_text(&sp.content).replace('\n', " ⏎ ");
                let t: String = t.chars().take(160).collect();
                if k == 0 { println!(" {star}{:<28} {}", p.title, t); } else { println!("  {:<28} {}", "", t); }
            }
        }
    }
}
