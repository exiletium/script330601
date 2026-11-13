use noesis::expr::sym;
use noesis::parse;
use noesis::print;
use noesis::simplify as s;
fn main() {
    for a in std::env::args().skip(1) {
        let (cmd, body) = match a.split_once(':') {
            Some((c, b)) if c.len() == 1 => (c, b),
            _ => ("p", a.as_str()),
        };
        let t0 = std::time::Instant::now();
        match parse::parse(body) {
            Ok(p) => {
                let r = noesis::budget::attempt(10.0, || match cmd {
                    "s" => s::simplify(&p.expr),
                    "f" => s::factor(&p.expr),
                    "e" => s::expand(&p.expr),
                    "c" => s::cancel(&p.expr),
                    "a" => s::apart(&p.expr, &sym("x")).unwrap_or(noesis::expr::undef()),
                    "t" => s::trig_reduce(&p.expr),
                    "x" => s::trig_expand(&p.expr),
                    _ => p.expr.clone(),
                });
                match r {
                    Some(r) => println!("{a}\n   => {}   [{:?}]", print::text(&r), t0.elapsed()),
                    None => println!("{a}\n   TIMEOUT/PANIC"),
                }
            }
            Err(e) => println!("{a}\n   ERROR: {e}"),
        }
    }
}
