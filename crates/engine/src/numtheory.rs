//! Number theory on arbitrary-precision integers.

use crate::budget;
use num_bigint::BigInt;
use num_integer::{Integer, Roots};
use num_traits::{One, Signed, ToPrimitive, Zero};

pub fn bi(n: i64) -> BigInt {
    BigInt::from(n)
}

fn mulmod(a: u64, b: u64, m: u64) -> u64 {
    ((a as u128 * b as u128) % m as u128) as u64
}

fn powmod(mut b: u64, mut e: u64, m: u64) -> u64 {
    let mut r = 1u64 % m;
    b %= m;
    while e > 0 {
        if e & 1 == 1 {
            r = mulmod(r, b, m);
        }
        b = mulmod(b, b, m);
        e >>= 1;
    }
    r
}

const SMALL_PRIMES: [u64; 12] = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37];

pub fn is_prime_u64(n: u64) -> bool {
    if n < 2 {
        return false;
    }
    for p in SMALL_PRIMES {
        if n % p == 0 {
            return n == p;
        }
    }
    let mut d = n - 1;
    let mut s = 0;
    while d % 2 == 0 {
        d /= 2;
        s += 1;
    }
    'outer: for a in SMALL_PRIMES {
        let mut x = powmod(a, d, n);
        if x == 1 || x == n - 1 {
            continue;
        }
        for _ in 1..s {
            x = mulmod(x, x, n);
            if x == n - 1 {
                continue 'outer;
            }
        }
        return false;
    }
    true
}

pub fn is_prime(n: &BigInt) -> bool {
    if let Some(v) = n.to_u64() {
        return is_prime_u64(v);
    }
    if n.is_negative() {
        return false;
    }
    for p in [2u32, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97] {
        if (n % p).is_zero() {
            return false;
        }
    }
    let one = BigInt::one();
    let nm1 = n - &one;
    let s = nm1.trailing_zeros().unwrap_or(0);
    let d = &nm1 >> s;
    let bases: [u32; 24] = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89];
    'outer: for a in bases {
        budget::check();
        let mut x = BigInt::from(a).modpow(&d, n);
        if x == one || x == nm1 {
            continue;
        }
        for _ in 1..s {
            x = (&x * &x) % n;
            if x == nm1 {
                continue 'outer;
            }
        }
        return false;
    }
    true
}

fn rho_u64(n: u64) -> u64 {
    if n % 2 == 0 {
        return 2;
    }
    let mut c = 1u64;
    loop {
        let f = |x: u64| (mulmod(x, x, n) + c) % n;
        let (mut x, mut y, mut d) = (2u64, 2u64, 1u64);
        let mut prod = 1u64;
        let mut count = 0u32;
        while d == 1 {
            x = f(x);
            y = f(f(y));
            let diff = x.abs_diff(y);
            if diff == 0 {
                d = n;
                break;
            }
            prod = mulmod(prod, diff, n);
            count += 1;
            if count % 64 == 0 {
                d = prod.gcd(&n);
                prod = 1;
                if count % 65536 == 0 {
                    budget::check();
                }
            }
        }
        if d == 1 {
            d = prod.gcd(&n);
        }
        if d != n && d != 1 {
            return d;
        }
        if d == n {
            // retry slowly with this constant, then move on
            let (mut x, mut y) = (2u64, 2u64);
            loop {
                x = f(x);
                y = f(f(y));
                let g = x.abs_diff(y).gcd(&n);
                if g == n {
                    break;
                }
                if g != 1 {
                    return g;
                }
            }
        }
        c += 1;
    }
}

fn factor_u64(mut n: u64, out: &mut Vec<u64>) {
    if n < 2 {
        return;
    }
    for p in [2u64, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47] {
        while n % p == 0 {
            out.push(p);
            n /= p;
        }
    }
    if n == 1 {
        return;
    }
    if is_prime_u64(n) {
        out.push(n);
        return;
    }
    let r = n.sqrt();
    if r * r == n {
        factor_u64(r, out);
        factor_u64(r, out);
        return;
    }
    let d = rho_u64(n);
    factor_u64(d, out);
    factor_u64(n / d, out);
}

fn rho_big(n: &BigInt) -> Option<BigInt> {
    let one = BigInt::one();
    for c in 1u32..20 {
        let c = BigInt::from(c);
        let (mut x, mut y) = (BigInt::from(2), BigInt::from(2));
        let mut prod = one.clone();
        let mut count = 0u64;
        loop {
            x = (&x * &x + &c) % n;
            y = (&y * &y + &c) % n;
            y = (&y * &y + &c) % n;
            let diff = (&x - &y).abs();
            if diff.is_zero() {
                break;
            }
            prod = (prod * diff) % n;
            count += 1;
            if count % 128 == 0 {
                budget::check();
                let g = prod.gcd(n);
                if g != one {
                    if &g != n {
                        return Some(g);
                    }
                    break;
                }
                prod = one.clone();
            }
            if count > 40_000_000 {
                return None;
            }
        }
    }
    None
}

/// Prime factorisation.  The boolean is `false` when a composite cofactor
/// could not be split within the time budget (it is then listed as-is).
pub fn factor_full(n: &BigInt) -> (Vec<(BigInt, u32)>, bool) {
    let mut primes: Vec<BigInt> = Vec::new();
    let mut complete = true;
    let mut n = n.abs();
    if n < BigInt::from(2) {
        return (vec![], true);
    }
    // small primes by trial division
    let mut p = 2u32;
    while p < 5000 && n.bits() > 64 {
        while (&n % p).is_zero() {
            primes.push(BigInt::from(p));
            n /= p;
        }
        p += if p == 2 { 1 } else { 2 };
    }
    let mut stack = vec![n];
    while let Some(m) = stack.pop() {
        if m.is_one() {
            continue;
        }
        if let Some(v) = m.to_u64() {
            let mut fs = Vec::new();
            factor_u64(v, &mut fs);
            primes.extend(fs.into_iter().map(BigInt::from));
            continue;
        }
        if is_prime(&m) {
            primes.push(m);
            continue;
        }
        // perfect powers
        let mut split = false;
        for k in [2u32, 3, 5, 7] {
            let r = m.nth_root(k);
            if r.pow(k) == m {
                for _ in 0..k {
                    stack.push(r.clone());
                }
                split = true;
                break;
            }
        }
        if split {
            continue;
        }
        let found = budget::attempt_opt((budget::remaining() * 0.8).min(8.0), || rho_big(&m));
        match found {
            Some(d) => {
                stack.push(&m / &d);
                stack.push(d);
            }
            None => {
                complete = false;
                primes.push(m);
            }
        }
    }
    primes.sort();
    let mut out: Vec<(BigInt, u32)> = Vec::new();
    for p in primes {
        match out.last_mut() {
            Some((q, k)) if *q == p => *k += 1,
            _ => out.push((p, 1)),
        }
    }
    (out, complete)
}

pub fn factor(n: &BigInt) -> Vec<(BigInt, u32)> {
    factor_full(n).0
}

pub fn divisors(n: &BigInt) -> Vec<BigInt> {
    let fs = factor(n);
    let mut ds = vec![BigInt::one()];
    for (p, k) in fs {
        let prev = ds.clone();
        let mut pk = BigInt::one();
        for _ in 0..k {
            pk *= &p;
            ds.extend(prev.iter().map(|d| d * &pk));
            if ds.len() > 200_000 {
                break;
            }
        }
    }
    ds.sort();
    ds
}

pub fn divisor_count(n: &BigInt) -> BigInt {
    factor(n).iter().map(|(_, k)| BigInt::from(k + 1)).product()
}

pub fn sigma(k: u32, n: &BigInt) -> BigInt {
    factor(n)
        .iter()
        .map(|(p, e)| {
            let mut s = BigInt::zero();
            let pk = p.pow(k);
            let mut t = BigInt::one();
            for _ in 0..=*e {
                s += &t;
                t *= &pk;
            }
            s
        })
        .product()
}

pub fn totient(n: &BigInt) -> BigInt {
    let mut r = n.abs();
    for (p, _) in factor(n) {
        r = &r / &p * (&p - 1);
    }
    r
}

pub fn mobius(n: &BigInt) -> i32 {
    let fs = factor(n);
    if fs.iter().any(|(_, k)| *k > 1) {
        0
    } else if fs.len() % 2 == 0 {
        1
    } else {
        -1
    }
}

pub fn next_prime(n: &BigInt) -> BigInt {
    let mut c: BigInt = n + 1;
    if c <= BigInt::from(2) {
        return BigInt::from(2);
    }
    if c.is_even() {
        c += 1;
    }
    while !is_prime(&c) {
        c += 2;
    }
    c
}

pub fn prev_prime(n: &BigInt) -> Option<BigInt> {
    let mut c: BigInt = n - 1;
    if c < BigInt::from(2) {
        return None;
    }
    if c == BigInt::from(2) {
        return Some(c);
    }
    if c.is_even() {
        c -= 1;
    }
    while !is_prime(&c) {
        c -= 2;
    }
    Some(c)
}

/// Sieve of Eratosthenes up to `limit` (inclusive).
pub fn sieve(limit: usize) -> Vec<u64> {
    if limit < 2 {
        return vec![];
    }
    let mut comp = vec![false; limit / 2 + 1];
    let mut out = vec![2u64];
    let mut i = 3usize;
    while i <= limit {
        if !comp[i / 2] {
            out.push(i as u64);
            let mut j = i * i;
            while j <= limit {
                comp[j / 2] = true;
                j += 2 * i;
            }
        }
        i += 2;
    }
    out
}

/// The n-th prime (1-indexed), for n up to about 5·10^7.
pub fn nth_prime(n: u64) -> Option<u64> {
    if n == 0 || n > 50_000_000 {
        return None;
    }
    let nf = n as f64;
    let limit = if n < 6 { 15.0 } else { nf * (nf.ln() + nf.ln().ln()) + 10.0 };
    let ps = sieve(limit as usize);
    ps.get(n as usize - 1).copied()
}

/// Number of primes `<= x` (sieve; x up to about 10^9).
pub fn prime_pi(x: u64) -> Option<u64> {
    if x > 1_000_000_000 {
        return None;
    }
    Some(sieve(x as usize).len() as u64)
}

pub fn fib(n: u64) -> BigInt {
    // fast doubling
    fn fd(n: u64) -> (BigInt, BigInt) {
        if n == 0 {
            return (BigInt::zero(), BigInt::one());
        }
        let (a, b) = fd(n / 2);
        let c = &a * (&b * 2 - &a);
        let d = &a * &a + &b * &b;
        if n % 2 == 0 {
            (c, d)
        } else {
            let e = &c + &d;
            (d, e)
        }
    }
    fd(n).0
}

pub fn lucas(n: u64) -> BigInt {
    if n == 0 {
        return BigInt::from(2);
    }
    fib(n - 1) + fib(n + 1)
}

pub fn factorial(n: u64) -> BigInt {
    fn prod(lo: u64, hi: u64) -> BigInt {
        if hi - lo < 8 {
            let mut r = BigInt::one();
            for k in lo..=hi {
                r *= k;
            }
            return r;
        }
        let mid = (lo + hi) / 2;
        prod(lo, mid) * prod(mid + 1, hi)
    }
    if n < 2 {
        BigInt::one()
    } else {
        prod(2, n)
    }
}

pub fn double_factorial(n: u64) -> BigInt {
    let mut r = BigInt::one();
    let mut k = n;
    while k > 1 {
        r *= k;
        k -= 2;
    }
    r
}

pub fn binomial(n: &BigInt, k: u64) -> BigInt {
    let mut r = BigInt::one();
    for i in 0..k {
        r = r * (n - i) / (i + 1);
    }
    r
}

pub fn mod_inv(a: &BigInt, m: &BigInt) -> Option<BigInt> {
    let e = a.extended_gcd(m);
    if e.gcd.is_one() {
        Some(e.x.mod_floor(m))
    } else {
        None
    }
}

pub fn jacobi(a: &BigInt, n: &BigInt) -> i32 {
    let mut a = a.mod_floor(n);
    let mut n = n.clone();
    let mut result = 1;
    while !a.is_zero() {
        while a.is_even() {
            a >>= 1;
            let r = (&n % 8u32).to_u32().unwrap();
            if r == 3 || r == 5 {
                result = -result;
            }
        }
        std::mem::swap(&mut a, &mut n);
        if (&a % 4u32).to_u32() == Some(3) && (&n % 4u32).to_u32() == Some(3) {
            result = -result;
        }
        a = a.mod_floor(&n);
    }
    if n.is_one() {
        result
    } else {
        0
    }
}

/// Number of integer partitions p(n) (Euler's pentagonal recurrence).
pub fn partitions(n: usize) -> BigInt {
    let mut p = vec![BigInt::zero(); n + 1];
    p[0] = BigInt::one();
    for m in 1..=n {
        let mut k = 1i64;
        let mut acc = BigInt::zero();
        loop {
            let g1 = (k * (3 * k - 1) / 2) as usize;
            if g1 > m {
                break;
            }
            let sign = if k % 2 == 0 { -1 } else { 1 };
            acc += &p[m - g1] * sign;
            let g2 = (k * (3 * k + 1) / 2) as usize;
            if g2 <= m {
                acc += &p[m - g2] * sign;
            }
            k += 1;
        }
        p[m] = acc;
        if m % 512 == 0 {
            budget::check();
        }
    }
    p[n].clone()
}

pub fn primitive_root(p: &BigInt) -> Option<BigInt> {
    if !is_prime(p) {
        return None;
    }
    if *p == BigInt::from(2) {
        return Some(BigInt::one());
    }
    let phi = p - 1;
    let fs = factor(&phi);
    let mut g = BigInt::from(2);
    while &g < p {
        if fs.iter().all(|(q, _)| !g.modpow(&(&phi / q), p).is_one()) {
            return Some(g);
        }
        g += 1;
    }
    None
}

/// Multiplicative order of `a` modulo `n`.
pub fn mult_order(a: &BigInt, n: &BigInt) -> Option<BigInt> {
    if !a.gcd(n).is_one() {
        return None;
    }
    let phi = totient(n);
    let mut ord = phi.clone();
    for (q, k) in factor(&phi) {
        for _ in 0..k {
            let cand = &ord / &q;
            if a.modpow(&cand, n).is_one() {
                ord = cand;
            } else {
                break;
            }
        }
    }
    Some(ord)
}

/// Chinese remainder theorem for pairwise compatible congruences.
pub fn crt(residues: &[(BigInt, BigInt)]) -> Option<(BigInt, BigInt)> {
    let mut x = BigInt::zero();
    let mut m = BigInt::one();
    for (r, n) in residues {
        let e = m.extended_gcd(n);
        let diff = r - &x;
        if !(&diff % &e.gcd).is_zero() {
            return None;
        }
        let lcm = &m / &e.gcd * n;
        x = (&x + &m * ((&diff / &e.gcd * &e.x).mod_floor(&(n / &e.gcd)))).mod_floor(&lcm);
        m = lcm;
    }
    Some((x, m))
}

/// Continued fraction expansion of p/q.
pub fn continued_fraction(p: &BigInt, q: &BigInt) -> Vec<BigInt> {
    let mut out = Vec::new();
    let (mut a, mut b) = (p.clone(), q.clone());
    while !b.is_zero() && out.len() < 400 {
        let (d, r) = a.div_mod_floor(&b);
        out.push(d);
        a = b;
        b = r;
    }
    out
}

/// Continued fraction of a float (limited by double precision).
pub fn continued_fraction_f64(mut x: f64, max_terms: usize) -> Vec<i64> {
    let mut out = Vec::new();
    for _ in 0..max_terms {
        let a = x.floor();
        if a.abs() > 1e12 {
            break;
        }
        out.push(a as i64);
        let f = x - a;
        if f.abs() < 1e-9 {
            break;
        }
        x = 1.0 / f;
    }
    out
}

/// Convergents `(numerator, denominator)` of a continued fraction.
pub fn convergents(cf: &[BigInt]) -> Vec<(BigInt, BigInt)> {
    let mut out = Vec::new();
    let (mut h0, mut h1) = (BigInt::zero(), BigInt::one());
    let (mut k0, mut k1) = (BigInt::one(), BigInt::zero());
    for a in cf {
        let h = a * &h1 + &h0;
        let k = a * &k1 + &k0;
        out.push((h.clone(), k.clone()));
        h0 = h1;
        h1 = h;
        k0 = k1;
        k1 = k;
    }
    out
}

/// Digits of `n` in the given base (2..=36), lowercase.
pub fn to_base(n: &BigInt, base: u32) -> String {
    n.to_str_radix(base)
}

pub fn from_base(s: &str, base: u32) -> Option<BigInt> {
    BigInt::parse_bytes(s.to_lowercase().as_bytes(), base)
}

pub fn to_roman(mut n: u64) -> Option<String> {
    if n == 0 || n >= 4000 {
        return None;
    }
    let table = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")];
    let mut s = String::new();
    for (v, r) in table {
        while n >= v {
            s.push_str(r);
            n -= v;
        }
    }
    Some(s)
}

pub fn from_roman(s: &str) -> Option<u64> {
    let val = |c: char| match c {
        'I' => Some(1),
        'V' => Some(5),
        'X' => Some(10),
        'L' => Some(50),
        'C' => Some(100),
        'D' => Some(500),
        'M' => Some(1000),
        _ => None,
    };
    let cs: Vec<u64> = s.chars().map(val).collect::<Option<Vec<_>>>()?;
    if cs.is_empty() {
        return None;
    }
    let mut total = 0;
    for i in 0..cs.len() {
        if i + 1 < cs.len() && cs[i] < cs[i + 1] {
            total -= cs[i] as i64;
        } else {
            total += cs[i] as i64;
        }
    }
    let total = total as u64;
    // only accept canonical numerals
    if to_roman(total).as_deref() == Some(s) {
        Some(total)
    } else {
        None
    }
}

/// English name of an integer ("one hundred twenty-three").
pub fn number_name(n: &BigInt) -> Option<String> {
    const ONES: [&str; 20] = [
        "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen",
    ];
    const TENS: [&str; 10] = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"];
    const SCALES: [&str; 22] = [
        "", "thousand", "million", "billion", "trillion", "quadrillion", "quintillion", "sextillion", "septillion", "octillion", "nonillion", "decillion", "undecillion", "duodecillion", "tredecillion", "quattuordecillion", "quindecillion", "sexdecillion", "septendecillion", "octodecillion", "novemdecillion", "vigintillion",
    ];
    if n.is_zero() {
        return Some("zero".into());
    }
    let digits = n.abs().to_string();
    if digits.len() > 66 {
        return None;
    }
    let mut groups: Vec<u32> = Vec::new();
    let bytes = digits.as_bytes();
    let mut end = bytes.len();
    while end > 0 {
        let start = end.saturating_sub(3);
        groups.push(digits[start..end].parse().unwrap());
        end = start;
    }
    let mut parts: Vec<String> = Vec::new();
    for (idx, g) in groups.iter().enumerate().rev() {
        if *g == 0 {
            continue;
        }
        let mut s = String::new();
        let (h, r) = (g / 100, g % 100);
        if h > 0 {
            s.push_str(ONES[h as usize]);
            s.push_str(" hundred");
            if r > 0 {
                s.push(' ');
            }
        }
        if r > 0 {
            if r < 20 {
                s.push_str(ONES[r as usize]);
            } else {
                s.push_str(TENS[(r / 10) as usize]);
                if r % 10 > 0 {
                    s.push('-');
                    s.push_str(ONES[(r % 10) as usize]);
                }
            }
        }
        if idx > 0 {
            s.push(' ');
            s.push_str(SCALES[idx]);
        }
        parts.push(s);
    }
    let mut out = parts.join(" ");
    if n.is_negative() {
        out = format!("negative {out}");
    }
    Some(out)
}

pub fn is_square(n: &BigInt) -> bool {
    if n.is_negative() {
        return false;
    }
    let r = n.sqrt();
    &r * &r == *n
}

pub fn digit_sum(n: &BigInt, base: u32) -> BigInt {
    let (_, digits) = n.abs().to_radix_be(base);
    digits.iter().map(|d| BigInt::from(*d)).sum()
}

/// Egyptian fraction by the greedy algorithm (denominators).
pub fn egyptian(p: &BigInt, q: &BigInt) -> Vec<BigInt> {
    let mut out = Vec::new();
    let (mut p, mut q) = (p.clone(), q.clone());
    while !p.is_zero() && out.len() < 12 {
        let d = q.div_ceil(&p);
        p = &p * &d - &q;
        q *= &d;
        let g = p.gcd(&q);
        if !g.is_zero() {
            p /= &g;
            q /= &g;
        }
        out.push(d);
    }
    out
}

/// Decimal expansion of p/q as (integer part, non-repeating digits, repeating digits).
pub fn repeating_decimal(p: &BigInt, q: &BigInt, max_digits: usize) -> Option<(BigInt, String, String)> {
    let neg = p.is_negative() != q.is_negative();
    let (p, q) = (p.abs(), q.abs());
    let (ip, mut rem) = p.div_rem(&q);
    let mut seen: std::collections::HashMap<BigInt, usize> = std::collections::HashMap::new();
    let mut digits = String::new();
    while !rem.is_zero() {
        if let Some(&pos) = seen.get(&rem) {
            let rep = digits.split_off(pos);
            return Some((if neg { -ip } else { ip }, digits, rep));
        }
        if digits.len() >= max_digits {
            return None;
        }
        seen.insert(rem.clone(), digits.len());
        rem *= 10;
        let (d, r) = rem.div_rem(&q);
        digits.push_str(&d.to_string());
        rem = r;
    }
    Some((if neg { -ip } else { ip }, digits, String::new()))
}

/// Represent n as a sum of two squares, if possible.
pub fn two_squares(n: &BigInt) -> Option<(BigInt, BigInt)> {
    let limit = n.sqrt().to_u64()?;
    if limit > 5_000_000 {
        return None;
    }
    for a in 0..=limit {
        let a2 = BigInt::from(a) * a;
        if &a2 * 2 > *n {
            break;
        }
        let rest = n - &a2;
        if is_square(&rest) {
            return Some((BigInt::from(a), rest.sqrt()));
        }
    }
    None
}

/// Collatz trajectory length (number of steps to reach 1).
pub fn collatz_steps(n: &BigInt) -> Option<u64> {
    let mut x = n.clone();
    if !x.is_positive() {
        return None;
    }
    let mut steps = 0;
    while !x.is_one() {
        if x.is_even() {
            x >>= 1;
        } else {
            x = x * 3 + 1;
        }
        steps += 1;
        if steps > 1_000_000 {
            return None;
        }
    }
    Some(steps)
}

/// Bernoulli number B_n as (numerator, denominator), B_1 = -1/2.
pub fn bernoulli(n: usize) -> num_rational::BigRational {
    use num_rational::BigRational as R;
    let mut b: Vec<R> = Vec::with_capacity(n + 1);
    for m in 0..=n {
        if m == 0 {
            b.push(R::one());
            continue;
        }
        if m > 1 && m % 2 == 1 {
            b.push(R::zero());
            continue;
        }
        // B_m = -1/(m+1) Σ_{k<m} C(m+1, k) B_k
        let mut s = R::zero();
        let mut c = BigInt::one();
        for (k, bk) in b.iter().enumerate() {
            s += R::from_integer(c.clone()) * bk;
            c = c * (m + 1 - k) / (k + 1);
        }
        b.push(-s / R::from_integer(BigInt::from(m + 1)));
        budget::tick();
    }
    b[n].clone()
}
