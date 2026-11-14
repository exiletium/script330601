//! Linear algebra over exact (possibly symbolic) entries.

use crate::engine::*;
use crate::expr::Expr::*;
use crate::expr::*;
use crate::parse::Parsed;
use crate::simplify as s;
use crate::solve;

pub type Mat = Vec<Vec<Expr>>;

pub fn from_expr(e: &Expr) -> Option<Mat> {
    let (r, c) = e.matrix_shape()?;
    let rows = e.as_list()?;
    let m: Mat = rows.iter().map(|row| row.as_list().cloned().unwrap_or_default()).collect();
    if m.len() == r && m.iter().all(|row| row.len() == c) {
        Some(m)
    } else {
        None
    }
}

pub fn to_expr(m: &Mat) -> Expr {
    List(m.iter().map(|r| List(r.clone())).collect())
}

pub fn identity(n: usize) -> Mat {
    (0..n).map(|i| (0..n).map(|j| if i == j { one() } else { zero() }).collect()).collect()
}

pub fn transpose(m: &Mat) -> Mat {
    if m.is_empty() {
        return vec![];
    }
    (0..m[0].len()).map(|j| m.iter().map(|r| r[j].clone()).collect()).collect()
}

pub fn matmul(a: &Mat, b: &Mat) -> Option<Mat> {
    if a.is_empty() || b.is_empty() || a[0].len() != b.len() {
        return None;
    }
    let n = b[0].len();
    Some(a.iter().map(|r| (0..n).map(|j| s::simplify(&add(r.iter().zip(b.iter()).map(|(x, row)| mul(vec![x.clone(), row[j].clone()])).collect()))).collect()).collect())
}

pub fn matpow(a: &Mat, k: i64) -> Option<Mat> {
    let n = a.len();
    if k < 0 {
        return matpow(&inverse(a)?, -k);
    }
    let mut r = identity(n);
    let mut b = a.clone();
    let mut k = k;
    while k > 0 {
        if k & 1 == 1 {
            r = matmul(&r, &b)?;
        }
        k >>= 1;
        if k > 0 {
            b = matmul(&b, &b)?;
        }
    }
    Some(r)
}

pub fn det(m: &Mat) -> Expr {
    s::simplify(&solve::det(m))
}

pub fn trace(m: &Mat) -> Expr {
    s::simplify(&add((0..m.len().min(m[0].len())).map(|i| m[i][i].clone()).collect()))
}

pub fn rref(m: &Mat) -> (Mat, Vec<usize>) {
    let cols = m.first().map_or(0, |r| r.len());
    let (r, p) = solve::rref(m.clone(), cols);
    (r.into_iter().map(|row| row.into_iter().map(|e| s::simplify(&e)).collect()).collect(), p)
}

pub fn rank(m: &Mat) -> usize {
    rref(m).1.len()
}

pub fn inverse(m: &Mat) -> Option<Mat> {
    let n = m.len();
    if n == 0 || m[0].len() != n {
        return None;
    }
    let aug: Mat = m.iter().enumerate().map(|(i, r)| r.iter().cloned().chain((0..n).map(|j| if i == j { one() } else { zero() })).collect()).collect();
    let (r, piv) = solve::rref(aug, n);
    if piv.len() < n || piv.iter().enumerate().any(|(i, &p)| p != i) {
        return None;
    }
    Some(r.into_iter().map(|row| row[n..].iter().map(s::simplify).collect()).collect())
}

/// Characteristic polynomial det(λ I − A) in `lam`.
pub fn charpoly(m: &Mat, lam: &Expr) -> Expr {
    let n = m.len();
    let a: Mat = (0..n).map(|i| (0..n).map(|j| if i == j { sub(lam.clone(), m[i][j].clone()) } else { neg(m[i][j].clone()) }).collect()).collect();
    s::expand(&solve::det(&a))
}

/// Eigenvalues with algebraic multiplicities.
pub fn eigenvalues(m: &Mat) -> Vec<(Expr, usize)> {
    let lam = sym("λ");
    let p = charpoly(m, &lam);
    let rs = solve::solve(&p, &lam);
    let mut out: Vec<(Expr, usize)> = Vec::new();
    for r in rs.roots {
        // multiplicity by repeated division
        let mut k = 0;
        let mut q = p.clone();
        while k < m.len() {
            let v = q.subs(&lam, &r);
            if !crate::eval::const_c64(&s::simplify(&v)).map_or(s::simplify(&v).is_zero(), |z| z.norm() < 1e-8) {
                break;
            }
            k += 1;
            q = s::simplify(&crate::calculus::diff(&q, &lam));
        }
        out.push((r, k.max(1)));
    }
    out
}

/// Basis of the null space.
pub fn nullspace(m: &Mat) -> Vec<Vec<Expr>> {
    let cols = m.first().map_or(0, |r| r.len());
    let (r, piv) = solve::rref(m.clone(), cols);
    let free: Vec<usize> = (0..cols).filter(|c| !piv.contains(c)).collect();
    let mut out = Vec::new();
    for &f in &free {
        let mut v = vec![zero(); cols];
        v[f] = one();
        for (row, &p) in piv.iter().enumerate() {
            v[p] = s::simplify(&neg(r[row][f].clone()));
        }
        // clear denominators for readability
        out.push(v);
    }
    out
}

pub fn eigenvectors(m: &Mat) -> Vec<(Expr, Vec<Expr>)> {
    let n = m.len();
    let mut out = Vec::new();
    for (lam, _) in eigenvalues(m) {
        if lam.has_float() {
            continue;
        }
        let a: Mat = (0..n).map(|i| (0..n).map(|j| if i == j { sub(m[i][j].clone(), lam.clone()) } else { m[i][j].clone() }).collect()).collect();
        for v in nullspace(&a) {
            out.push((lam.clone(), v));
        }
    }
    out
}

fn is_numeric_matrix(m: &Mat) -> bool {
    m.iter().flatten().all(|e| e.free_symbols().is_empty())
}

/// Pods for a matrix input.
pub fn matrix_pods(p: &Parsed) -> Option<Vec<Pod>> {
    let m = from_expr(&p.expr)?;
    let (r, c) = (m.len(), m[0].len());
    let mut pods = vec![crate::engine::math::input_pod(&p.d)];
    pods.push(pod("Dimensions", "Dimensions", text(format!("{r} (rows) × {c} (columns)"))));
    let mut jobs: Vec<PodJob> = Vec::new();
    if r == c {
        let m1 = m.clone();
        jobs.push(job1(move || Some(pod("Determinant", "Determinant", math(&det(&m1))))));
        let m2 = m.clone();
        jobs.push(job1(move || Some(pod("Trace", "Trace", math(&trace(&m2))))));
        let m3 = m.clone();
        jobs.push(job1(move || {
            let inv = inverse(&m3)?;
            Some(pod("Inverse", "Inverse", math(&to_expr(&inv))))
        }));
        if r <= 6 {
            let m4 = m.clone();
            jobs.push(job(move || {
                let lam = sym("λ");
                let cp = charpoly(&m4, &lam);
                let ev = eigenvalues(&m4);
                let mut out = vec![];
                if !ev.is_empty() {
                    let rows: Vec<Vec<Content>> = ev
                        .iter()
                        .enumerate()
                        .map(|(k, (v, mult))| {
                            let lhs = sym(&format!("λ_{}", k + 1));
                            let mut row = vec![math(&eq(lhs, v.clone()))];
                            if *mult > 1 {
                                row.push(text(format!("(multiplicity {mult})")));
                            }
                            if v.size() > 6 && v.free_symbols().is_empty() {
                                row.push(math(&raw_fn("Approx", vec![sym("λ"), s::evalf(v)])));
                            }
                            row
                        })
                        .collect();
                    out.push(pod("Eigenvalues", "Eigenvalues", table(None, rows)));
                }
                let evs = eigenvectors(&m4);
                if !evs.is_empty() && evs.len() <= 8 {
                    let rows: Vec<Vec<Content>> = evs.iter().enumerate().map(|(k, (_, v))| vec![math(&eq(sym(&format!("v_{}", k + 1)), raw_fn("Tuple", vec![List(v.clone())])))]).collect();
                    out.push(pod("Eigenvectors", "Eigenvectors", table(None, rows)));
                }
                out.push(pod("CharacteristicPolynomial", "Characteristic polynomial", math(&cp)));
                if ev.iter().all(|(_, m)| *m == 1) && ev.len() == m4.len() {
                    out.push(pod("Diagonalization", "Diagonalizable", text("yes (distinct eigenvalues)")));
                }
                Some(out)
            }));
        }
    }
    let m5 = m.clone();
    jobs.push(job(move || {
        let (rr, piv) = rref(&m5);
        let mut out = vec![pod("Rank", "Rank", text(piv.len().to_string()))];
        out.push(pod("RowReducedForm", "Row-reduced form", math(&to_expr(&rr))));
        let ns = nullspace(&m5);
        if !ns.is_empty() && ns.len() <= 6 {
            let rows: Vec<Vec<Content>> = ns.iter().map(|v| vec![math(&raw_fn("Tuple", vec![List(v.clone())]))]).collect();
            out.push(pod("NullSpace", "Null space basis", table(None, rows)));
        }
        Some(out)
    }));
    let m6 = m.clone();
    jobs.push(job1(move || Some(pod("Transpose", "Transpose", math(&to_expr(&transpose(&m6)))))));
    if is_numeric_matrix(&m) && r == c {
        let m7 = m.clone();
        jobs.push(job1(move || {
            // condition number in the 2-norm via singular values of A^T A (numeric)
            let n = m7.len();
            let a: Vec<Vec<f64>> = m7.iter().map(|r| r.iter().map(|e| e.to_f64_const().unwrap_or(f64::NAN)).collect()).collect();
            let ata: Vec<Vec<f64>> = (0..n).map(|i| (0..n).map(|j| (0..n).map(|k| a[k][i] * a[k][j]).sum()).collect()).collect();
            let ev = sym_eigen(&ata)?;
            let (mx, mn) = (ev.iter().cloned().fold(0.0, f64::max), ev.iter().cloned().fold(f64::INFINITY, f64::min));
            let sv: Vec<String> = {
                let mut v: Vec<f64> = ev.iter().map(|x| x.max(0.0).sqrt()).collect();
                v.sort_by(|p, q| q.partial_cmp(p).unwrap());
                v.iter().map(|x| crate::print::fmt_float(*x, 6)).collect()
            };
            let cond = if mn > 1e-300 { crate::print::fmt_float((mx / mn).sqrt(), 6) } else { "∞ (singular)".into() };
            Some(pod("SingularValues", "Singular values and condition number", table(None, vec![vec![text("singular values"), text(sv.join(", "))], vec![text("condition number"), text(cond)]])))
        }));
    }
    pods.extend(run_jobs(jobs));
    Some(pods)
}

/// Eigenvalues of a symmetric real matrix (Jacobi rotations).
pub fn sym_eigen(a: &[Vec<f64>]) -> Option<Vec<f64>> {
    let n = a.len();
    let mut m: Vec<Vec<f64>> = a.to_vec();
    if m.iter().flatten().any(|v| !v.is_finite()) {
        return None;
    }
    for _ in 0..100 {
        let mut off = 0.0;
        for i in 0..n {
            for j in 0..n {
                if i != j {
                    off += m[i][j] * m[i][j];
                }
            }
        }
        if off < 1e-22 {
            break;
        }
        for p in 0..n {
            for q in p + 1..n {
                if m[p][q].abs() < 1e-300 {
                    continue;
                }
                let theta = (m[q][q] - m[p][p]) / (2.0 * m[p][q]);
                let t = theta.signum() / (theta.abs() + (theta * theta + 1.0).sqrt());
                let t = if theta == 0.0 { 1.0 } else { t };
                let c = 1.0 / (t * t + 1.0).sqrt();
                let s = t * c;
                for k in 0..n {
                    let (mkp, mkq) = (m[k][p], m[k][q]);
                    m[k][p] = c * mkp - s * mkq;
                    m[k][q] = s * mkp + c * mkq;
                }
                for k in 0..n {
                    let (mpk, mqk) = (m[p][k], m[q][k]);
                    m[p][k] = c * mpk - s * mqk;
                    m[q][k] = s * mpk + c * mqk;
                }
            }
        }
    }
    Some((0..n).map(|i| m[i][i]).collect())
}
