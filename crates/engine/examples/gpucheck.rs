use noesis::compute;
use noesis::eval::Program;
fn main() {
    println!("info: {:?}", compute::info());
    println!("gpu available: {}", compute::gpu_available());
    for src in ["sin(x)*cos(y) + exp(-(x^2+y^2)/4)", "x^2 - y^3 + sqrt(abs(x))", "gamma(x/3+2) + erf(y)", "floor(x) + mod(y, 2) + atan2(y, x)", "(x^2+y^2)^(1/3)"] {
        let e = noesis::parse::parse_expr(src).unwrap();
        let p = Program::compile(&e, &["x", "y"]).unwrap();
        println!("{src}\n  wgsl: {:?}", p.to_wgsl().map(|s| s.chars().take(90).collect::<String>()));
        compute::set_preference("cpu");
        let (a, _) = compute::eval_grid(&p, -3.0, 3.0, 256, -3.0, 3.0, 256);
        compute::set_preference("gpu");
        let (b, dev) = compute::eval_grid(&p, -3.0, 3.0, 256, -3.0, 3.0, 256);
        let err = a.iter().zip(&b).filter(|(x, y)| x.is_finite() && y.is_finite()).map(|(x, y)| (x - y).abs() / (1.0 + x.abs())).fold(0.0, f64::max);
        println!("  device {:?}, max rel err {:.2e}", dev, err);
    }
    let (f, dev) = compute::fractal(-0.5, 0.0, 3.0 / 400.0, 400, 300, 200, None);
    println!("fractal {:?} inside={}", dev, f.iter().filter(|v| **v < 0.0).count());
    println!("bench {:?}", compute::benchmark());
}
