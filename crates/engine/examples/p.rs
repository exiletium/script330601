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
                    "d" => noesis::calculus::doit(&p.expr),
                    "z" => {
                        let r = noesis::solve::solve(&p.expr, &sym("x"));
                        let mut v = r.roots.clone();
                        if r.periodic { v.push(sym("PERIODIC")); }
                        if r.numeric { v.push(sym("NUMERIC")); }
                        if r.identity { v.push(sym("IDENTITY")); }
                        noesis::expr::Expr::List(v)
                    }
                    "q" => match &p.expr {
                        noesis::expr::Expr::Rel(op, a, b) => noesis::solve::solve_inequality(*op, a, b, &sym("x")).unwrap_or(noesis::expr::undef()),
                        _ => noesis::expr::undef(),
                    },
                    "y" => {
                        let eqs = p.expr.as_list().cloned().unwrap_or_default();
                        let mut vars: Vec<noesis::expr::Expr> = Vec::new();
                        for e in &eqs { for s0 in e.free_symbols() { let v = sym(&s0); if !vars.contains(&v) { vars.push(v); } } }
                        vars.sort();
                        let r = noesis::solve::solve_system(&eqs, &vars);
                        noesis::expr::Expr::List(r.sols.into_iter().map(|s0| noesis::expr::Expr::List(s0.into_iter().map(|(a, b)| noesis::expr::eq(a, b)).collect())).collect())
                    }
                    "k" => s::factor_terms(&s::cancel(&p.expr)),
                    "D" => { println!("{:?}", p.expr); p.expr.clone() }
                    "i" => noesis::calculus::integrate::integrate(&p.expr, &sym("x")).unwrap_or(noesis::expr::undef()),
                    "S" => match noesis::calculus::series(&p.expr, &sym("x"), &noesis::expr::zero(), 6) {
                        Some(ex) => noesis::expr::add(vec![ex.poly, ex.big_o.map_or(noesis::expr::zero(), |o| noesis::expr::raw_fn("O", vec![o]))]),
                        None => noesis::expr::undef(),
                    },
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
