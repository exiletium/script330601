//! Computer-algebra tests: parsing, simplification, factoring, calculus and
//! equation solving.

use noesis::calculus::{self, Dir};
use noesis::expr::{inf, sym, zero};
use noesis::parse::parse_expr;
use noesis::print::text;
use noesis::simplify as s;
use noesis::solve;

fn p(src: &str) -> noesis::expr::Expr {
    parse_expr(src).unwrap_or_else(|e| panic!("parse {src}: {e}"))
}

fn x() -> noesis::expr::Expr {
    sym("x")
}

#[test]
fn parsing_and_printing() {
    assert_eq!(text(&p("2x^2 + 3x - 5")), "2 x^2 + 3 x - 5");
    assert_eq!(text(&p("sin^2 x")), "sin^2(x)");
    assert_eq!(text(&p("3 1/2")), "7/2");
    assert_eq!(text(&p("20% of 150")), "30");
    assert_eq!(text(&p("e^(i pi)")), "-1");
    assert_eq!(text(&p("sqrt(8)")), "2 sqrt(2)");
    assert_eq!(text(&p("5 choose 2")), "10");
}

#[test]
fn special_values() {
    assert_eq!(text(&p("sin(pi/12)")), "(sqrt(6) - sqrt(2))/4");
    assert_eq!(text(&p("zeta(2)")), "π^2/6");
    assert_eq!(text(&p("cos(pi/3)")), "1/2");
    assert_eq!(text(&p("ln(e^3)")), "3");
}

#[test]
fn simplification() {
    assert_eq!(text(&s::simplify(&p("sin(2x) - 2 sin(x) cos(x)"))), "0");
    assert_eq!(text(&s::simplify(&p("sin(x)^2 + cos(x)^2"))), "1");
    assert_eq!(text(&s::simplify(&p("(x^2 - 1)/(x - 1)"))), "x + 1");
    assert_eq!(text(&s::simplify(&p("sqrt(3 + 2 sqrt(2))"))), "1 + sqrt(2)");
    assert_eq!(text(&s::simplify(&p("1/(1 + i)"))), "(1 - i)/2");
    assert_eq!(text(&s::simplify(&p("e^x (-x/2 - 1/2)"))), "-(e^x (x + 1))/2");
}

#[test]
fn factoring_and_partial_fractions() {
    assert_eq!(text(&s::factor(&p("x^4 + 4"))), "(x^2 - 2 x + 2) (x^2 + 2 x + 2)");
    assert_eq!(text(&s::factor(&p("x^3 - x^2 y - x y^2 + y^3"))), "(x - y)^2 (x + y)");
    assert_eq!(text(&s::factor(&p("x^4 - 1"))), "(x - 1) (x + 1) (x^2 + 1)");
    assert_eq!(text(&s::apart(&p("1/(x^2 - 1)"), &x()).unwrap()), "1/(2 (x - 1)) - 1/(2 (x + 1))");
}

#[test]
fn derivatives() {
    let d = |e: &str| text(&s::simplify(&calculus::diff(&p(e), &x())));
    assert_eq!(d("x^3 sin(x)"), "x^2 (x cos(x) + 3 sin(x))");
    assert_eq!(d("x^x"), "x^x (log(x) + 1)");
    assert_eq!(d("tan(x)"), "sec^2(x)");
}

#[test]
fn integrals() {
    let i = |e: &str| text(&calculus::integrate::integrate(&p(e), &x()).unwrap_or_else(|| panic!("no antiderivative for {e}")));
    assert_eq!(i("x^2 sin(x)"), "-x^2 cos(x) + 2 x sin(x) + 2 cos(x)");
    assert_eq!(i("1/(x^2 + 1)"), "tan^(-1)(x)");
    assert_eq!(i("e^(-x^2)"), "(sqrt(π) erf(x))/2");
    assert_eq!(i("sin(x)/x"), "Si(x)");
    assert_eq!(i("ln(x)"), "x (log(x) - 1)");
    assert_eq!(i("x/(x^4 + 1)"), "tan^(-1)(x^2)/2");
    for e in ["sqrt(1 - x^2)", "1/(x^3 + 1)", "sec(x)^3", "x atan(x)", "1/(1 + sin(x))", "x^3 e^(-x^2)"] {
        let f = p(e);
        let big = calculus::integrate::integrate(&f, &x()).unwrap_or_else(|| panic!("no antiderivative for {e}"));
        assert!(calculus::integrate::verify(&f, &big, &x()), "{e}");
    }
    assert!(calculus::integrate::integrate(&p("x^x"), &x()).is_none());
}

#[test]
fn definite_integrals() {
    use calculus::integrate::{definite, Definite};
    let exact = |e: &str, a: noesis::expr::Expr, b: noesis::expr::Expr| match definite(&p(e), &x(), &a, &b) {
        Definite::Exact(v) => text(&v),
        other => panic!("{e}: {other:?}"),
    };
    assert_eq!(exact("e^(-x^2)", noesis::expr::neg(inf()), inf()), "sqrt(π)");
    assert_eq!(exact("sin(x)/x", zero(), inf()), "π/2");
    assert_eq!(exact("1/x^2", p("1"), inf()), "1");
    assert_eq!(exact("ln(x)", zero(), p("1")), "-1");
    assert!(matches!(definite(&p("1/x"), &x(), &zero(), &p("1")), Definite::Divergent));
    assert!(matches!(definite(&p("tan(x)"), &x(), &zero(), &p("pi")), Definite::Divergent | Definite::Unknown));
    match definite(&p("x^x"), &x(), &zero(), &p("1")) {
        Definite::Numeric(v, _) => assert!((v - 0.7834305107121344).abs() < 1e-8),
        other => panic!("{other:?}"),
    }
}

#[test]
fn limits() {
    let l = |e: &str, a: noesis::expr::Expr, d: Dir| calculus::limit(&p(e), &x(), &a, d).map(|v| text(&v));
    assert_eq!(l("sin(x)/x", zero(), Dir::Both).as_deref(), Some("1"));
    assert_eq!(l("(1 + 1/x)^x", inf(), Dir::Both).as_deref(), Some("e"));
    assert_eq!(l("(sin(x) - x)/x^3", zero(), Dir::Both).as_deref(), Some("-1/6"));
    assert_eq!(l("sqrt(x^2 + x) - x", inf(), Dir::Both).as_deref(), Some("1/2"));
    assert_eq!(l("x^x", zero(), Dir::Right).as_deref(), Some("1"));
    assert_eq!(l("1/x", zero(), Dir::Right).as_deref(), Some("∞"));
    assert_eq!(l("1/x", zero(), Dir::Both), None);
}

#[test]
fn series_and_sums() {
    let ser = calculus::series(&p("sin(x)"), &x(), &zero(), 5).unwrap();
    assert_eq!(text(&ser.poly), "x^5/120 - x^3/6 + x");
    let sum = |e: &str, k: &str, a: noesis::expr::Expr, b: noesis::expr::Expr| match calculus::sum::sum(&p(e), &sym(k), &a, &b) {
        calculus::sum::SumResult::Exact(v) => text(&v),
        other => panic!("{e}: {other:?}"),
    };
    assert_eq!(sum("1/k^2", "k", p("1"), inf()), "π^2/6");
    assert_eq!(sum("k^2", "k", p("1"), sym("n")), "n^3/3 + n^2/2 + n/6");
    assert_eq!(sum("1/(k(k+1))", "k", p("1"), sym("n")), "n/(n + 1)");
    assert_eq!(sum("x^k/k!", "k", zero(), inf()), "e^x");
    assert_eq!(sum("(-1)^(k+1)/k", "k", p("1"), inf()), "log(2)");
    assert!(matches!(calculus::sum::sum(&p("1/k"), &sym("k"), &p("1"), &inf()), calculus::sum::SumResult::Divergent(_)));
}

fn roots(e: &str) -> Vec<String> {
    solve::solve(&p(e), &x()).roots.iter().map(text).collect()
}

#[test]
fn equations() {
    assert_eq!(roots("x^2 - 5x + 6 = 0"), vec!["2", "3"]);
    assert_eq!(roots("x^2 + 1 = 0"), vec!["-i", "i"]);
    assert_eq!(roots("x^4 - 5x^2 + 4"), vec!["-2", "-1", "1", "2"]);
    assert_eq!(roots("e^(2x) - 3e^x + 2 = 0"), vec!["0", "log(2)"]);
    assert_eq!(roots("sqrt(x + 3) = x - 3"), vec!["6"]);
    assert_eq!(roots("sqrt(x) + sqrt(x - 5) = 5"), vec!["9"]);
    assert_eq!(roots("abs(x - 2) = 3"), vec!["-1", "5"]);
    assert_eq!(roots("ln(x) = 2"), vec!["e^2"]);
    assert_eq!(roots("x^3 - 3x + 1 = 0").len(), 3);
    let r = solve::solve(&p("sin(x) = 1/2"), &x());
    assert!(r.periodic);
    assert_eq!(r.roots.len(), 2);
    let r = solve::solve(&p("x e^x = 1"), &x());
    assert!(r.numeric);
    assert!((r.roots[0].to_f64_const().unwrap() - 0.567143290409784).abs() < 1e-6);
}

#[test]
fn systems_and_inequalities() {
    let eqs = match p("{x^2 + y^2 = 25, x + y = 7}") {
        noesis::expr::Expr::List(v) => v,
        _ => unreachable!(),
    };
    let sol = solve::solve_system(&eqs, &[sym("x"), sym("y")]);
    assert_eq!(sol.sols.len(), 2);
    let ineq = |e: &str| match p(e) {
        noesis::expr::Expr::Rel(op, a, b) => text(&solve::solve_inequality(op, &a, &b, &x()).unwrap()),
        _ => unreachable!(),
    };
    assert_eq!(ineq("x^2 - 1 > 0"), "x < -1 or x > 1");
    assert_eq!(ineq("x^2 <= 4"), "-2 ≤ x ≤ 2");
    assert_eq!(ineq("(x - 1)/(x + 2) >= 0"), "x < -2 or x ≥ 1");
    assert_eq!(ineq("x^2 + 1 > 0"), "True");
}

#[test]
fn number_theory() {
    use noesis::numtheory as nt;
    use num_bigint::BigInt;
    assert!(nt::is_prime(&BigInt::from(2305843009213693951u64)));
    assert_eq!(nt::nth_prime(100), Some(541));
    assert_eq!(nt::to_roman(1994).as_deref(), Some("MCMXCIV"));
    assert_eq!(nt::totient(&BigInt::from(100)), BigInt::from(40));
    assert_eq!(nt::partitions(100).to_string(), "190569292");
}

#[test]
fn high_precision_decimals() {
    assert_eq!(noesis::bigfloat::decimal(&p("pi"), 30).as_deref(), Some("3.14159265358979323846264338327"));
    assert_eq!(noesis::bigfloat::decimal(&p("sqrt(2)"), 20).as_deref(), Some("1.4142135623730950488"));
}
