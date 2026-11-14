//! End-to-end tests of the query engine (natural language in, pods out).

use noesis::engine::{content_text, QueryResult};

fn q(input: &str) -> QueryResult {
    let r = noesis::query(input);
    assert!(r.success, "{input}: {:?}", r.error);
    r
}

fn short(input: &str) -> String {
    q(input).short_answer().unwrap_or_default()
}

fn pod_text(r: &QueryResult, id: &str) -> String {
    r.pods.iter().find(|p| p.id == id).map(|p| p.subpods.iter().map(|s| content_text(&s.content)).collect::<Vec<_>>().join(" | ")).unwrap_or_else(|| panic!("no pod {id} in {:?}", r.pods.iter().map(|p| &p.id).collect::<Vec<_>>()))
}

#[test]
fn arithmetic_and_numbers() {
    assert_eq!(short("2+2"), "4");
    assert_eq!(short("what is 2^10?"), "1024");
    let r = q("pi");
    assert!(pod_text(&r, "DecimalApproximation").starts_with("3.14159265358979323846"));
    let r = q("100");
    assert_eq!(pod_text(&r, "NumberName"), "one hundred");
    assert_eq!(pod_text(&r, "RomanNumerals"), "C");
    assert!(pod_text(&q("100!"), "ScientificNotation").contains("10^157"));
    assert_eq!(short("(1+2i)^3"), "-11 - 2 i");
}

#[test]
fn calculus_queries() {
    assert_eq!(short("derivative of x^3 ln(x)"), "d/dx(x^3 log(x)) = x^2 (3 log(x) + 1)");
    assert_eq!(short("integrate e^(-x^2) from -inf to inf"), "∫_(-∞)^∞ e^(-x^2) dx = sqrt(π)");
    assert_eq!(short("limit sin(x)/x as x->0"), "lim_(x→0) sin(x)/x = 1");
    assert_eq!(short("sum 1/k^2, k=1 to inf"), "∑_(k=1)^∞ 1/k^2 = π^2/6");
    assert!(short("sum 1/k, k=1 to inf").contains("diverges"));
    assert!(short("integrate 1/x from 0 to 1").contains("does not converge"));
}

#[test]
fn algebra_queries() {
    assert_eq!(short("solve x^3 - 6x^2 + 11x - 6 = 0"), "x = 1 | x = 2 | x = 3".replace(" | ", "\n"));
    assert_eq!(short("factor x^4 - 1"), "(x - 1) (x + 1) (x^2 + 1)");
    assert_eq!(short("x^2 < 4"), "-2 < x < 2");
    assert_eq!(short("{x+y=3, x-y=1}"), "x = 2, y = 1");
    let r = q("x^2+y^2=25");
    assert!(pod_text(&r, "Geometry").contains("circle"));
}

#[test]
fn function_analysis() {
    let r = q("x^2 - 5x + 6");
    assert!(pod_text(&r, "RealRoots").contains("x = 2"));
    assert!(pod_text(&r, "GlobalMinimum").contains("-1/4"));
    assert!(r.pods.iter().any(|p| p.id == "Plot"));
    let r = q("1/x");
    assert!(pod_text(&r, "Properties").contains("x ≠ 0"));
}

#[test]
fn matrices_and_statistics() {
    let r = q("{{1,2},{3,4}}");
    assert_eq!(pod_text(&r, "Determinant"), "-2");
    assert_eq!(pod_text(&r, "Inverse"), "{{-2, 1}, {3/2, -1/2}}");
    let r = q("{1,2,3,4,10}");
    assert!(pod_text(&r, "Statistics").contains("mean | 4"));
}

#[test]
fn units_and_constants() {
    assert_eq!(short("10 miles in km"), "16.09344 kilometers");
    assert_eq!(short("32 F to C"), "0 degrees Celsius");
    assert_eq!(short("5 ft 3 in to cm"), "160.02 centimeters");
    assert_eq!(short("how many feet in a mile"), "5280 feet");
    assert!(short("speed of light").starts_with("299792458 m/s"));
}

#[test]
fn knowledge_domains() {
    assert_eq!(short("molar mass of caffeine"), "194.194 g/mol");
    assert_eq!(short("capital of australia"), "Canberra");
    assert!(short("distance from paris to london").starts_with("343."));
    assert!(pod_text(&q("iron"), "BasicProperties").contains("26"));
    assert_eq!(short("what day was July 4, 1776"), "Thursday, July 4, 1776");
    assert_eq!(short("days between 2024-01-01 and 2024-12-25"), "359 days  |  51 weeks 2 days");
    assert_eq!(short("mortgage $300000 at 6.5% for 30 years"), "$1,896.20");
    assert!(short("probability of 3 heads in 5 coin flips").contains("0.3125"));
}

#[test]
fn multivariable_limits() {
    assert_eq!(short("lim (x,y)->(0,0) y/x"), "The limit does not exist.");
    assert_eq!(short("limit of x y/(x^2+y^2) as (x,y)->(0,0)"), "The limit does not exist.");
    assert_eq!(short("limit x y^2/(x^2+y^4) as (x,y)->(0,0)"), "The limit does not exist.");
    assert_eq!(short("limit x^2 y/(x^2+y^2) as (x,y) -> (0,0)"), "0");
    assert_eq!(short("limit sin(x^2+y^2)/(x^2+y^2) as (x,y)->(0,0)"), "1");
    assert_eq!(short("limit (x^2+y^2) as (x,y)->(1,2)"), "5");
}

#[test]
fn laplace_transforms() {
    assert_eq!(short("laplace transform of t^2 e^t"), "ℒ(e^t t^2) = 2/(s - 1)^3");
    assert_eq!(short("laplace transform of sin(3t)"), "ℒ(sin(3 t)) = 3/(s^2 + 9)");
    assert_eq!(short("inverse laplace transform of 1/(s(s+1))"), "ℒ^(-1)(1/(s (s + 1))) = 1 - e^(-t)");
    assert_eq!(short("inverse laplace of (s+1)/(s^2+2s+5)"), "ℒ^(-1)((s + 1)/(s^2 + 2 s + 5)) = e^(-t) cos(2 t)");
    assert_eq!(short("inverse laplace of 2/(s-3)^3"), "ℒ^(-1)(2/(s - 3)^3) = e^(3 t) t^2");
}

#[test]
fn step_by_step() {
    let r = q("integrate x^2 sin(x) dx");
    let steps = pod_text(&r, "Steps");
    assert!(steps.contains("Integrate by parts"), "{steps}");
    let r = q("derivative of x^3 ln(x)");
    assert!(pod_text(&r, "Steps").contains("product rule"));
    let r = q("x^2 - 5x + 6 = 0");
    assert!(pod_text(&r, "SolutionSteps").contains("quadratic formula"));
}

#[test]
fn differential_equations() {
    assert_eq!(short("y'' + y = 0"), "y(x) = c_1 cos(x) + c_2 sin(x)");
    assert_eq!(short("y' = y, y(0) = 1"), "y(x) = e^x");
    assert_eq!(short("y'' + 4y = sin(x), y(0)=0, y'(0)=1"), "y(x) = (sin(x) + sin(2 x))/3");
    assert_eq!(short("y' + 2y = x"), "y(x) = e^(-2 x) c_1 + x/2 - 1/4");
    assert_eq!(short("y'' + 2y' + y = 0"), "y(x) = e^(-x) (c_2 x + c_1)");
    let r = q("y' = sin(x y), y(0) = 1");
    assert!(r.pods.iter().any(|p| p.id == "NumericSolution"));
}

#[test]
fn unknown_input_fails_gracefully() {
    let r = noesis::query("qwzx blorp flim");
    assert!(!r.success);
    assert!(!r.suggestions.is_empty());
}
