//! Parser for mathematical input as people actually type it:
//! `2x^2 + 3x`, `sin^2 x`, `sin 2x cos x`, `|x - 1|`, `5 choose 2`,
//! `{{1,2},{3,4}}`, `x squared`, `3 1/2`, `20% of 150`, ...
//!
//! The parser produces an [`Ast`], from which both the evaluated expression
//! and an unevaluated presentation (echoing the input as typed) are derived.

use crate::expr::*;
use crate::print::{self, D};
use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};
use regex::Regex;
use std::sync::LazyLock;

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Num(Rat, String),
    Name(String),
    /// `+ - * / ^` and `j` for juxtaposition (implicit multiplication).
    Bin(char, Box<Ast>, Box<Ast>),
    Neg(Box<Ast>),
    Call(String, Vec<Ast>),
    Rel(Vec<Ast>, Vec<RelOp>),
    List(Vec<Ast>),
    Percent(Box<Ast>),
    Degree(Box<Ast>),
    Prime(Box<Ast>, u32),
}

#[derive(Clone, Debug, Default)]
pub struct Ctx {
    /// Names that are functions when followed by `(`, e.g. `f`, `y`.
    pub functions: Vec<String>,
    /// Multi-letter names to keep as single symbols.
    pub symbols: Vec<String>,
    /// Variable for primes: `y'` means dy/d(prime_var).
    pub prime_var: Option<String>,
    /// Treat `i` as an ordinary symbol.
    pub no_imag: bool,
    /// Treat `e` as an ordinary symbol.
    pub no_euler: bool,
}

#[derive(Clone, Debug)]
pub struct Parsed {
    pub ast: Ast,
    pub expr: Expr,
    pub d: D,
    pub has_decimal: bool,
    pub notes: Vec<String>,
}

// ---------------------------------------------------------------------------
// vocabulary
// ---------------------------------------------------------------------------

/// alias -> canonical function name
const FUNCS: &[(&str, &str)] = &[
    ("sin", "sin"),
    ("cos", "cos"),
    ("tan", "tan"),
    ("cot", "cot"),
    ("sec", "sec"),
    ("csc", "csc"),
    ("cosec", "csc"),
    ("sine", "sin"),
    ("cosine", "cos"),
    ("tangent", "tan"),
    ("cotangent", "cot"),
    ("secant", "sec"),
    ("cosecant", "csc"),
    ("tg", "tan"),
    ("ctg", "cot"),
    ("asin", "asin"),
    ("acos", "acos"),
    ("atan", "atan"),
    ("acot", "acot"),
    ("asec", "asec"),
    ("acsc", "acsc"),
    ("arcsin", "asin"),
    ("arccos", "acos"),
    ("arctan", "atan"),
    ("arccot", "acot"),
    ("arcsec", "asec"),
    ("arccsc", "acsc"),
    ("arctg", "atan"),
    ("sinh", "sinh"),
    ("cosh", "cosh"),
    ("tanh", "tanh"),
    ("coth", "coth"),
    ("sech", "sech"),
    ("csch", "csch"),
    ("asinh", "asinh"),
    ("acosh", "acosh"),
    ("atanh", "atanh"),
    ("acoth", "acoth"),
    ("asech", "asech"),
    ("acsch", "acsch"),
    ("arcsinh", "asinh"),
    ("arccosh", "acosh"),
    ("arctanh", "atanh"),
    ("arsinh", "asinh"),
    ("arcosh", "acosh"),
    ("artanh", "atanh"),
    ("ln", "ln"),
    ("log", "log"),
    ("lg", "log10"),
    ("lb", "log2"),
    ("log10", "log10"),
    ("log2", "log2"),
    ("exp", "exp"),
    ("sqrt", "sqrt"),
    ("cbrt", "cbrt"),
    ("root", "root"),
    ("surd", "surd"),
    ("abs", "abs"),
    ("sgn", "sign"),
    ("sign", "sign"),
    ("signum", "sign"),
    ("floor", "floor"),
    ("ceil", "ceil"),
    ("ceiling", "ceil"),
    ("round", "round"),
    ("frac", "frac"),
    ("fractionalpart", "frac"),
    ("gamma", "gamma"),
    ("lgamma", "lgamma"),
    ("loggamma", "lgamma"),
    ("beta", "beta"),
    ("zeta", "zeta"),
    ("erf", "erf"),
    ("erfc", "erfc"),
    ("erfi", "erfi"),
    ("erfinv", "erfinv"),
    ("Si", "Si"),
    ("Ci", "Ci"),
    ("Ei", "Ei"),
    ("li", "li"),
    ("W", "W"),
    ("lambertw", "W"),
    ("productlog", "W"),
    ("besselj", "besselj"),
    ("bessely", "bessely"),
    ("besseli", "besseli"),
    ("besselk", "besselk"),
    ("binomial", "binomial"),
    ("binom", "binomial"),
    ("ncr", "binomial"),
    ("npr", "permutations"),
    ("permutations", "permutations"),
    ("factorial", "factorial"),
    ("factorial2", "factorial2"),
    ("gcd", "gcd"),
    ("gcf", "gcd"),
    ("hcf", "gcd"),
    ("lcm", "lcm"),
    ("mod", "mod"),
    ("quotient", "quotient"),
    ("powmod", "powmod"),
    ("modinv", "modinv"),
    ("min", "min"),
    ("max", "max"),
    ("re", "re"),
    ("real", "re"),
    ("im", "im"),
    ("imag", "im"),
    ("conj", "conj"),
    ("conjugate", "conj"),
    ("arg", "arg"),
    ("atan2", "atan2"),
    ("fibonacci", "fibonacci"),
    ("fib", "fibonacci"),
    ("lucas", "lucas"),
    ("prime", "prime"),
    ("primepi", "primepi"),
    ("isprime", "isprime"),
    ("nextprime", "nextprime"),
    ("prevprime", "prevprime"),
    ("totient", "totient"),
    ("eulerphi", "totient"),
    ("phi", "totient"),
    ("sigma", "sigma"),
    ("divisorsigma", "sigma"),
    ("divisors", "divisors"),
    ("mobius", "mobius"),
    ("moebiusmu", "mobius"),
    ("jacobi", "jacobi"),
    ("bernoulli", "bernoulli"),
    ("harmonic", "harmonic"),
    ("harmonicnumber", "harmonic"),
    ("partitions", "partitions"),
    ("catalan", "catalan"),
    ("heaviside", "heaviside"),
    ("unitstep", "heaviside"),
    ("theta", "heaviside"),
    ("H", "heaviside"),
    ("dirac", "dirac"),
    ("diracdelta", "dirac"),
    ("delta", "dirac"),
    ("sinc", "sinc"),
    ("digamma", "digamma"),
    ("psi", "digamma"),
    ("polylog", "polylog"),
    ("ellipticK", "ellipticK"),
    ("elliptick", "ellipticK"),
    ("ellipticE", "ellipticE"),
    ("elliptice", "ellipticE"),
    ("airyai", "airyai"),
    ("airybi", "airybi"),
    ("fresnels", "fresnels"),
    ("fresnelc", "fresnelc"),
    ("det", "det"),
    ("determinant", "det"),
    ("inverse", "inverse"),
    ("inv", "inverse"),
    ("transpose", "transpose"),
    ("trace", "trace"),
    ("tr", "trace"),
    ("rank", "rank"),
    ("rref", "rref"),
    ("nullspace", "nullspace"),
    ("eigenvalues", "eigenvalues"),
    ("eigenvectors", "eigenvectors"),
    ("cross", "cross"),
    ("dot", "dot"),
    ("norm", "norm"),
    ("integrate", "integrate"),
    ("integral", "integrate"),
    ("int", "integrate"),
    ("diff", "diff"),
    ("derivative", "diff"),
    ("D", "diff"),
    ("limit", "limit"),
    ("lim", "limit"),
    ("sum", "sum"),
    ("product", "product"),
    ("prod", "product"),
    ("solve", "solve"),
    ("factor", "factor"),
    ("expand", "expand"),
    ("simplify", "simplify"),
    ("series", "series"),
    ("taylor", "series"),
    ("mean", "mean"),
    ("median", "median"),
    ("variance", "variance"),
    ("stdev", "stdev"),
    ("std", "stdev"),
    ("N", "N"),
    ("C", "binomial"),
    ("P", "permutations"),
    ("and", "and"),
    ("or", "or"),
    ("not", "not"),
    ("xor", "xor"),
    ("nand", "nand"),
    ("nor", "nor"),
    ("implies", "implies"),
];

/// Names that are only functions when directly followed by `(`.
const PAREN_ONLY: &[&str] = &[
    "gamma", "beta", "zeta", "delta", "theta", "sigma", "psi", "phi", "C", "P", "W", "H", "D", "N", "li", "Si", "Ci", "Ei", "re", "im", "arg", "det", "prime", "sum", "product", "prod", "int", "diff", "min", "max", "mod", "factor", "expand", "simplify", "solve", "series", "limit", "lim", "integral", "integrate", "derivative", "inverse", "inv",
    "transpose", "trace", "tr", "rank", "norm", "cross", "dot", "mean", "median", "variance", "std", "stdev", "root", "real", "imag", "sign", "round", "catalan", "harmonic", "partitions", "divisors", "lucas", "fib", "and", "or", "not", "xor", "nand", "nor", "implies", "binomial", "binom", "frac", "lb", "lg", "tg", "ctg", "quotient", "sinc",
    "conj", "conjugate", "totient", "mobius", "jacobi", "bernoulli", "eigenvalues", "eigenvectors", "nullspace", "rref", "taylor", "surd", "factorial",
];

/// Function names that may be glued to neighbouring letters: `xsinx`, `2cosx`.
const GLUE: &[&str] = &[
    "arcsinh", "arccosh", "arctanh", "arcsin", "arccos", "arctan", "arccot", "arcsec", "arccsc", "asinh", "acosh", "atanh", "sinh", "cosh", "tanh", "coth", "sech", "csch", "asin", "acos", "atan", "sqrt", "cbrt", "sin", "cos", "tan", "cot", "sec", "csc", "exp", "log", "ln", "abs", "pi",
];

/// Functions that take a single factor when written without parentheses.
const TIGHT: &[&str] = &["sqrt", "cbrt", "abs", "floor", "ceil", "round", "factorial", "sign", "gamma"];

const GREEK_NAMES: &[&str] = &[
    "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda", "mu", "nu", "xi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega", "Gamma", "Delta", "Theta", "Lambda", "Xi", "Sigma", "Phi", "Psi", "Omega",
];

const CONST_NAMES: &[(&str, &str)] = &[
    ("pi", "pi"),
    ("Pi", "pi"),
    ("PI", "pi"),
    ("infinity", "infinity"),
    ("Infinity", "infinity"),
    ("inf", "infinity"),
    ("oo", "infinity"),
    ("true", "true"),
    ("True", "true"),
    ("false", "false"),
    ("False", "false"),
    ("eulergamma", "eulergamma"),
    ("EulerGamma", "eulergamma"),
    ("goldenratio", "goldenratio"),
    ("GoldenRatio", "goldenratio"),
    ("undefined", "undefined"),
];

const STOPWORDS: &[&str] = &[
    "is", "of", "to", "in", "it", "at", "on", "an", "as", "be", "do", "go", "he", "if", "me", "my", "no", "so", "up", "us", "we", "am", "the", "for", "are", "but", "you", "all", "can", "had", "her", "was", "one", "our", "out", "day", "get", "has", "him", "his", "how", "man", "new", "now", "old", "see", "way", "who", "did", "its", "let", "put",
    "say", "she", "too", "use", "why", "what", "with", "from", "this", "that", "when", "where", "which", "and", "not", "any", "many", "much", "does", "between", "than", "then", "there", "their", "about", "would", "make", "like", "time", "just", "know", "take", "into", "year", "your", "good", "some", "could", "them", "other",
    "only", "come", "over", "think", "also", "back", "after", "work", "first", "well", "even", "want", "because", "these", "give", "most",
];

fn func_lookup(name: &str) -> Option<&'static str> {
    if let Some((_, c)) = FUNCS.iter().find(|(a, _)| *a == name) {
        return Some(c);
    }
    if name.len() >= 2 {
        let lower = name.to_lowercase();
        if let Some((_, c)) = FUNCS.iter().find(|(a, _)| *a == lower) {
            return Some(c);
        }
    }
    None
}

fn const_lookup(name: &str) -> Option<&'static str> {
    CONST_NAMES.iter().find(|(a, _)| *a == name).map(|(_, c)| *c)
}

// ---------------------------------------------------------------------------
// normalisation
// ---------------------------------------------------------------------------

static RE_TENS_HYPHEN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(?i)\b(twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)-(one|two|three|four|five|six|seven|eight|nine)\b").unwrap());
static RE_X_TIMES: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\d)\s+x\s+(\d)").unwrap());
static RE_WORD: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[A-Za-z]+").unwrap());

fn small_number_word(w: &str) -> Option<u64> {
    Some(match w {
        "zero" => 0,
        "one" => 1,
        "two" => 2,
        "three" => 3,
        "four" => 4,
        "five" => 5,
        "six" => 6,
        "seven" => 7,
        "eight" => 8,
        "nine" => 9,
        "ten" => 10,
        "eleven" => 11,
        "twelve" => 12,
        "thirteen" => 13,
        "fourteen" => 14,
        "fifteen" => 15,
        "sixteen" => 16,
        "seventeen" => 17,
        "eighteen" => 18,
        "nineteen" => 19,
        "twenty" => 20,
        "thirty" => 30,
        "forty" => 40,
        "fifty" => 50,
        "sixty" => 60,
        "seventy" => 70,
        "eighty" => 80,
        "ninety" => 90,
        _ => return None,
    })
}

fn scale_word(w: &str) -> Option<u128> {
    Some(match w {
        "hundred" => 100,
        "thousand" => 1_000,
        "million" => 1_000_000,
        "billion" => 1_000_000_000,
        "trillion" => 1_000_000_000_000,
        "quadrillion" => 1_000_000_000_000_000,
        _ => return None,
    })
}

/// Replace spelled-out numbers ("twenty one", "three hundred") by digits.
pub fn words_to_numbers(s: &str) -> String {
    let s = RE_TENS_HYPHEN.replace_all(s, "$1 $2").to_string();
    let mut out = String::new();
    let mut last = 0;
    let mut pending: Option<(u128, u128, usize)> = None; // (total, current, end index)
    let flush = |pending: &mut Option<(u128, u128, usize)>, out: &mut String| {
        if let Some((total, cur, _)) = pending.take() {
            out.push_str(&(total + cur).to_string());
        }
    };
    for m in RE_WORD.find_iter(&s) {
        let w = m.as_str().to_lowercase();
        let between = &s[last..m.start()];
        let is_small = small_number_word(&w);
        let is_scale = scale_word(&w);
        if let Some((total, cur, end)) = pending {
            // continue a number only across plain whitespace
            let gap_ok = s[end..m.start()].chars().all(|c| c == ' ');
            if gap_ok {
                if let Some(v) = is_small {
                    pending = Some((total, cur + v as u128, m.end()));
                    last = m.end();
                    continue;
                }
                if let Some(sc) = is_scale {
                    if sc == 100 {
                        pending = Some((total, cur.max(1) * 100, m.end()));
                    } else {
                        pending = Some((total + cur.max(1) * sc, 0, m.end()));
                    }
                    last = m.end();
                    continue;
                }
            }
            flush(&mut pending, &mut out);
        }
        out.push_str(between);
        if let Some(v) = is_small {
            pending = Some((0, v as u128, m.end()));
        } else if let (Some(sc), true) = (is_scale, out.trim_end().ends_with(|c: char| c.is_ascii_digit())) {
            // "3 million"
            out.push_str(&format!("*{sc}"));
        } else {
            out.push_str(m.as_str());
        }
        last = m.end();
    }
    flush(&mut pending, &mut out);
    out.push_str(&s[last..]);
    out
}

fn normalize(input: &str) -> String {
    let mut s = String::with_capacity(input.len() + 8);
    let sup = |c: char| -> Option<char> {
        Some(match c {
            '⁰' => '0',
            '¹' => '1',
            '²' => '2',
            '³' => '3',
            '⁴' => '4',
            '⁵' => '5',
            '⁶' => '6',
            '⁷' => '7',
            '⁸' => '8',
            '⁹' => '9',
            '⁻' => '-',
            '⁺' => '+',
            'ⁿ' => 'n',
            _ => return None,
        })
    };
    let chars: Vec<char> = input.chars().collect();
    let mut k = 0;
    while k < chars.len() {
        let c = chars[k];
        if sup(c).is_some() {
            s.push_str("^(");
            while k < chars.len() {
                match sup(chars[k]) {
                    Some(d) => s.push(d),
                    None => break,
                }
                k += 1;
            }
            s.push(')');
            continue;
        }
        match c {
            '×' | '·' | '∙' | '⋅' | '∗' => s.push('*'),
            '÷' | '∕' | '⁄' => s.push('/'),
            '−' | '–' | '—' | '‐' => s.push('-'),
            '√' => s.push_str(" sqrt "),
            '∛' => s.push_str(" cbrt "),
            'π' => s.push_str(" pi "),
            '∞' => s.push_str(" infinity "),
            '≤' | '⩽' => s.push_str("<="),
            '≥' | '⩾' => s.push_str(">="),
            '≠' => s.push_str("!="),
            '½' => s.push_str("(1/2)"),
            '⅓' => s.push_str("(1/3)"),
            '¼' => s.push_str("(1/4)"),
            '¾' => s.push_str("(3/4)"),
            '⅔' => s.push_str("(2/3)"),
            '∧' => s.push_str(" and "),
            '∨' => s.push_str(" or "),
            '¬' => s.push_str(" not "),
            '⊻' | '⊕' => s.push_str(" xor "),
            '⇒' | '→' => s.push_str(" implies "),
            '⇔' | '↔' => s.push_str(" iff "),
            '₀'..='₉' => s.push(char::from_digit(c as u32 - '₀' as u32, 10).unwrap()),
            '“' | '”' | '"' => {}
            '’' | '′' => s.push('\''),
            '\u{a0}' | '\u{2009}' | '\t' | '\n' | '\r' => s.push(' '),
            'α' => s.push_str(" alpha "),
            'β' => s.push_str(" beta "),
            'γ' => s.push_str(" gamma "),
            'δ' => s.push_str(" delta "),
            'ε' | 'ϵ' => s.push_str(" epsilon "),
            'ζ' => s.push_str(" zeta "),
            'η' => s.push_str(" eta "),
            'θ' => s.push_str(" theta "),
            'κ' => s.push_str(" kappa "),
            'λ' => s.push_str(" lambda "),
            'μ' | 'µ' => s.push_str(" mu "),
            'ν' => s.push_str(" nu "),
            'ξ' => s.push_str(" xi "),
            'ρ' => s.push_str(" rho "),
            'σ' => s.push_str(" sigma "),
            'τ' => s.push_str(" tau "),
            'φ' | 'ϕ' => s.push_str(" phi "),
            'χ' => s.push_str(" chi "),
            'ψ' => s.push_str(" psi "),
            'ω' => s.push_str(" omega "),
            'Γ' => s.push_str(" Gamma "),
            'Δ' => s.push_str(" Delta "),
            'Θ' => s.push_str(" Theta "),
            'Λ' => s.push_str(" Lambda "),
            'Σ' => s.push_str(" Sigma "),
            'Φ' => s.push_str(" Phi "),
            'Ψ' => s.push_str(" Psi "),
            'Ω' => s.push_str(" Omega "),
            _ => s.push(c),
        }
        k += 1;
    }
    let mut s = s.replace("**", "^");
    static PHRASES: LazyLock<Vec<(Regex, &'static str)>> = LazyLock::new(|| {
        [
            (r"(?i)\bdivided\s+by\b", "/"),
            (r"(?i)\bmultiplied\s+by\b", "*"),
            (r"(?i)\b(raised\s+)?to\s+the\s+power(\s+of)?\b", "^"),
            (r"(?i)\braised\s+to(\s+the)?\b", "^"),
            (r"(?i)\b(the\s+)?square\s+root(\s+of)?\b", " sqrt "),
            (r"(?i)\b(the\s+)?cube\s+root(\s+of)?\b", " cbrt "),
            (r"(?i)\bsqrt\s+of\b", " sqrt "),
            (r"(?i)\b(natural\s+)?log(arithm)?\s+of\b", " ln "),
            (r"(?i)\babsolute\s+value\s+of\b", " abs "),
            (r"(?i)\bfactorial\s+of\b", " factorial "),
            (r"(?i)(%|\bpercent)\s*of\b", "% * "),
            (r"(?i)\bpercent\b", "%"),
            (r"(?i)\bhalf\s+of\b", "(1/2)*"),
            (r"(?i)\ba\s+half\b", "(1/2)"),
            (r"(?i)\btimes\b", "*"),
            (r"(?i)\bplus\b", "+"),
            (r"(?i)\bminus\b", "-"),
            (r"(?i)\bover\b", "/"),
            (r"(?i)\bsquared\b", "^2"),
            (r"(?i)\bcubed\b", "^3"),
            (r"(?i)\bnegative\s+", "-"),
            (r"(?i)\bdegrees?\b", "°"),
            (r"(?i)\bfactorial\b\s*$", "!"),
            (r"(?i)\bequals\b|\bis\s+equal\s+to\b", "="),
            (r"(?i)\bgolden\s+ratio\b", "goldenratio"),
            (r"(?i)\beuler(-mascheroni|\s+mascheroni)?\s+(constant|gamma)\b", "eulergamma"),
        ]
        .iter()
        .map(|(p, r)| (Regex::new(p).unwrap(), *r))
        .collect()
    });
    for (re, rep) in PHRASES.iter() {
        if re.is_match(&s) {
            s = re.replace_all(&s, *rep).to_string();
        }
    }
    s = words_to_numbers(&s);
    s = RE_X_TIMES.replace_all(&s, "$1*$2").to_string();
    s
}

// ---------------------------------------------------------------------------
// tokens
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
enum T {
    Num(Rat, String, bool),
    Func(String),
    Sym(String),
    Op(&'static str),
}

#[derive(Clone, Debug)]
struct Tok {
    t: T,
    space: bool,
}

const OPS: &[&str] = &[
    "<->", "<=>", "...", "<=", ">=", "!=", "==", "<>", "=>", "&&", "||", "!!", "+", "-", "*", "/", "^", "!", "%", "°", "'", "(", ")", "[", "]", "{", "}", ",", "=", "<", ">", "|", "_", "~", ";", ":",
];

fn decimal_to_rat(text: &str) -> Option<Rat> {
    let (mant, exp) = match text.find(|c| c == 'e' || c == 'E') {
        Some(k) => (&text[..k], text[k + 1..].parse::<i32>().ok()?),
        None => (text, 0),
    };
    let (ip, fp) = match mant.split_once('.') {
        Some((a, b)) => (a, b),
        None => (mant, ""),
    };
    let digits = format!("{ip}{fp}");
    let n = BigInt::parse_bytes(if digits.is_empty() { b"0" } else { digits.as_bytes() }, 10)?;
    let scale = exp - fp.len() as i32;
    if scale.abs() > 5000 {
        return None;
    }
    let ten = BigInt::from(10);
    Some(if scale >= 0 { Rat::from_integer(n * ten.pow(scale as u32)) } else { Rat::new(n, ten.pow((-scale) as u32)) })
}

struct Lexer<'a> {
    #[allow(dead_code)]
    ctx: &'a Ctx,
    out: Vec<Tok>,
}

impl<'a> Lexer<'a> {
    fn push(&mut self, t: T, space: bool) {
        self.out.push(Tok { t, space });
    }

    /// Split a run of letters into known names and single-letter symbols.
    fn word(&mut self, w: &str, next_is_paren: bool, space: bool) -> Result<(), String> {
        let ctx = self.ctx;
        // whole-word matches first
        if ctx.symbols.iter().any(|s| s == w) {
            self.push(T::Sym(w.to_string()), space);
            return Ok(());
        }
        if ctx.functions.iter().any(|s| s == w) && next_is_paren {
            self.push(T::Func(format!("user:{w}")), space);
            return Ok(());
        }
        if let Some(c) = const_lookup(w) {
            self.push(T::Sym(c.to_string()), space);
            return Ok(());
        }
        let lower = w.to_lowercase();
        let word_op: Option<&'static str> = match lower.as_str() {
            "and" => Some("and"),
            "or" => Some("or"),
            "not" => Some("not"),
            "xor" => Some("xor"),
            "nand" => Some("nand"),
            "nor" => Some("nor"),
            "implies" => Some("implies"),
            "iff" => Some("iff"),
            "mod" => Some("mod"),
            "choose" => Some("choose"),
            _ => None,
        };
        if let Some(o) = word_op {
            if !next_is_paren || matches!(o, "and" | "or" | "not" | "xor" | "implies" | "iff") {
                self.push(T::Op(o), space);
                return Ok(());
            }
        }
        if let Some(canon) = func_lookup(w) {
            let paren_only = PAREN_ONLY.contains(&w) || PAREN_ONLY.contains(&lower.as_str());
            if !paren_only || next_is_paren {
                self.push(T::Func(canon.to_string()), space);
                return Ok(());
            }
        }
        if GREEK_NAMES.contains(&w) {
            self.push(T::Sym(w.to_string()), space);
            return Ok(());
        }
        if w.chars().count() == 1 {
            self.push(T::Sym(w.to_string()), space);
            return Ok(());
        }
        if STOPWORDS.contains(&lower.as_str()) {
            return Err(format!("unexpected word “{w}”"));
        }
        // segmentation: glued function names, Greek letters, single letters
        let mut pieces: Vec<T> = Vec::new();
        let mut singles = 0;
        let mut rest = w;
        while !rest.is_empty() {
            let mut matched = false;
            for g in GLUE {
                if rest.len() >= g.len() && rest[..g.len()].eq_ignore_ascii_case(g) && rest.is_char_boundary(g.len()) {
                    if *g == "pi" {
                        pieces.push(T::Sym("pi".into()));
                    } else {
                        pieces.push(T::Func(func_lookup(g).unwrap().to_string()));
                    }
                    rest = &rest[g.len()..];
                    matched = true;
                    break;
                }
            }
            if matched {
                continue;
            }
            for g in GREEK_NAMES {
                if g.len() >= 3 && rest.starts_with(g) {
                    pieces.push(T::Sym(g.to_string()));
                    rest = &rest[g.len()..];
                    matched = true;
                    break;
                }
            }
            if matched {
                continue;
            }
            let ch = rest.chars().next().unwrap();
            if !ch.is_alphabetic() {
                return Err(format!("unexpected character “{ch}”"));
            }
            pieces.push(T::Sym(ch.to_string()));
            singles += 1;
            rest = &rest[ch.len_utf8()..];
        }
        let total = w.chars().count();
        let has_named = pieces.iter().any(|p| matches!(p, T::Func(_)) || matches!(p, T::Sym(s) if s.len() > 1));
        if singles > 3 || (!has_named && total > 3) {
            return Err(format!("unknown word “{w}”"));
        }
        for (k, p) in pieces.into_iter().enumerate() {
            self.push(p, space && k == 0);
        }
        Ok(())
    }
}

fn tokenize(src: &str, ctx: &Ctx) -> Result<Vec<Tok>, String> {
    let chars: Vec<char> = src.chars().collect();
    let mut lx = Lexer { ctx, out: Vec::new() };
    let mut k = 0;
    let mut depth = 0i32;
    let mut space = false;
    while k < chars.len() {
        let c = chars[k];
        if c.is_whitespace() {
            space = true;
            k += 1;
            continue;
        }
        if c.is_ascii_digit() || (c == '.' && k + 1 < chars.len() && chars[k + 1].is_ascii_digit()) {
            let start = k;
            while k < chars.len() && chars[k].is_ascii_digit() {
                k += 1;
            }
            // thousands separators at top level: 1,234,567
            if depth == 0 && k - start <= 3 {
                let mut j = k;
                let mut groups = 0;
                while j + 3 < chars.len() + 0 && chars.get(j) == Some(&',') && (1..=3).all(|d| chars.get(j + d).map_or(false, |x| x.is_ascii_digit())) && !chars.get(j + 4).map_or(false, |x| x.is_ascii_digit()) {
                    j += 4;
                    groups += 1;
                }
                // also accept the final group at end of input
                if groups > 0 {
                    k = j;
                }
            }
            let mut is_decimal = false;
            if k < chars.len() && chars[k] == '.' && chars.get(k + 1).map_or(false, |x| x.is_ascii_digit()) {
                is_decimal = true;
                k += 1;
                while k < chars.len() && chars[k].is_ascii_digit() {
                    k += 1;
                }
            } else if k < chars.len() && chars[k] == '.' && !chars.get(k + 1).map_or(false, |x| *x == '.') && k > start {
                // trailing dot: "2." is a decimal number, unless it ends the sentence
                if k + 1 < chars.len() {
                    is_decimal = true;
                    k += 1;
                }
            }
            // scientific notation
            if k < chars.len() && (chars[k] == 'e' || chars[k] == 'E') {
                let mut j = k + 1;
                if j < chars.len() && (chars[j] == '+' || chars[j] == '-') {
                    j += 1;
                }
                if j < chars.len() && chars[j].is_ascii_digit() {
                    let digits_start = j;
                    while j < chars.len() && chars[j].is_ascii_digit() {
                        j += 1;
                    }
                    // not followed by a letter (e.g. "2e3x" is fine, "2ex" is not sci)
                    if j - digits_start <= 4 {
                        k = j;
                        is_decimal = true;
                    }
                }
            }
            let text: String = chars[start..k].iter().filter(|c| **c != ',').collect();
            let text = text.trim_end_matches('.').to_string();
            let r = decimal_to_rat(&text).ok_or_else(|| format!("bad number “{text}”"))?;
            let is_decimal = is_decimal && (text.contains('.') || text.contains('e') || text.contains('E'));
            lx.push(T::Num(r, text, is_decimal), space);
            space = false;
            continue;
        }
        if c.is_alphabetic() {
            let start = k;
            while k < chars.len() && chars[k].is_alphabetic() {
                k += 1;
            }
            let mut word: String = chars[start..k].iter().collect();
            // attached digits / subscripts
            let mut suffix = String::new();
            let mut j = k;
            if j < chars.len() && chars[j] == '_' && word != "log" && word != "Log" {
                let mut q = j + 1;
                let braced = chars.get(q) == Some(&'{');
                if braced {
                    q += 1;
                }
                let sub_start = q;
                while q < chars.len() && (chars[q].is_alphanumeric()) {
                    q += 1;
                }
                if q > sub_start && (!braced || chars.get(q) == Some(&'}')) {
                    suffix = format!("_{}", chars[sub_start..q].iter().collect::<String>());
                    j = if braced { q + 1 } else { q };
                }
            } else {
                while j < chars.len() && chars[j].is_ascii_digit() {
                    j += 1;
                }
                if j > k {
                    suffix = chars[k..j].iter().collect();
                }
            }
            let next_paren = |from: usize| chars[from..].iter().find(|x| !x.is_whitespace()) == Some(&'(');
            if !suffix.is_empty() {
                let joined = format!("{word}{suffix}");
                if ctx.symbols.iter().any(|s| *s == joined) {
                    lx.push(T::Sym(joined), space);
                    k = j;
                    space = false;
                    continue;
                }
                if !suffix.starts_with('_') && func_lookup(&joined).is_some() && joined.len() > 2 {
                    lx.push(T::Func(func_lookup(&joined).unwrap().to_string()), space);
                    k = j;
                    space = false;
                    continue;
                }
                let is_func_word = func_lookup(&word).is_some() && !PAREN_ONLY.contains(&word.as_str()) && word.chars().count() > 1;
                if is_func_word && !suffix.starts_with('_') {
                    // "sqrt2", "log 2": leave the digits for the number lexer
                    lx.word(&word, false, space)?;
                    space = false;
                    continue;
                }
                // attach to the last single-letter symbol
                let before = lx.out.len();
                lx.word(&word, false, space)?;
                let grew = lx.out.len() > before;
                match lx.out.last_mut() {
                    Some(Tok { t: T::Sym(s), .. }) if grew && const_lookup(s).is_none() => {
                        s.push_str(&suffix);
                        k = j;
                    }
                    _ => {
                        if suffix.starts_with('_') {
                            return Err("unexpected subscript".into());
                        }
                    }
                }
                space = false;
                continue;
            }
            if word == "Log" {
                word = "log".into();
            }
            lx.word(&word, next_paren(k), space)?;
            space = false;
            continue;
        }
        let rest: String = chars[k..chars.len().min(k + 3)].iter().collect();
        let Some(op) = OPS.iter().find(|o| rest.starts_with(**o)) else {
            return Err(format!("unexpected character “{c}”"));
        };
        match *op {
            "(" | "[" | "{" => depth += 1,
            ")" | "]" | "}" => depth -= 1,
            _ => {}
        }
        if *op != "..." {
            lx.push(T::Op(op), space);
        }
        space = false;
        k += op.chars().count();
    }
    Ok(lx.out)
}

// ---------------------------------------------------------------------------
// parser
// ---------------------------------------------------------------------------

#[allow(dead_code)]
struct Parser<'a> {
    toks: Vec<Tok>,
    pos: usize,
    ctx: &'a Ctx,
    abs_depth: usize,
    notes: Vec<String>,
    has_decimal: bool,
    depth: usize,
}

type PResult = Result<Ast, String>;

fn bin(op: char, a: Ast, b: Ast) -> Ast {
    Ast::Bin(op, Box::new(a), Box::new(b))
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Option<&T> {
        self.toks.get(self.pos).map(|t| &t.t)
    }
    fn peek_at(&self, k: usize) -> Option<&T> {
        self.toks.get(self.pos + k).map(|t| &t.t)
    }
    fn space_before(&self, k: usize) -> bool {
        self.toks.get(self.pos + k).map_or(false, |t| t.space)
    }
    fn is_op(&self, s: &str) -> bool {
        matches!(self.peek(), Some(T::Op(o)) if *o == s)
    }
    fn eat(&mut self, s: &str) -> bool {
        if self.is_op(s) {
            self.pos += 1;
            true
        } else {
            false
        }
    }
    fn expect(&mut self, s: &str) -> Result<(), String> {
        if self.eat(s) {
            Ok(())
        } else {
            Err(match self.peek() {
                Some(t) => format!("expected “{s}” before {}", describe(t)),
                None => format!("expected “{s}”"),
            })
        }
    }
    fn note(&mut self, s: &str) {
        if !self.notes.iter().any(|n| n == s) {
            self.notes.push(s.to_string());
        }
    }

    fn expr(&mut self) -> PResult {
        self.depth += 1;
        if self.depth > 200 {
            return Err("expression is nested too deeply".into());
        }
        let r = self.iff();
        self.depth -= 1;
        r
    }

    fn iff(&mut self) -> PResult {
        let mut a = self.implies()?;
        while self.is_op("iff") || self.is_op("<->") || self.is_op("<=>") {
            self.pos += 1;
            let b = self.implies()?;
            a = Ast::Call("iff".into(), vec![a, b]);
        }
        Ok(a)
    }

    fn implies(&mut self) -> PResult {
        let a = self.or()?;
        if self.is_op("implies") || self.is_op("=>") {
            self.pos += 1;
            let b = self.implies()?;
            return Ok(Ast::Call("implies".into(), vec![a, b]));
        }
        Ok(a)
    }

    fn or(&mut self) -> PResult {
        let mut a = self.xor()?;
        loop {
            if self.is_op("or") || self.is_op("||") {
                self.pos += 1;
                let b = self.xor()?;
                a = Ast::Call("or".into(), vec![a, b]);
            } else if self.is_op("nor") {
                self.pos += 1;
                let b = self.xor()?;
                a = Ast::Call("nor".into(), vec![a, b]);
            } else {
                return Ok(a);
            }
        }
    }

    fn xor(&mut self) -> PResult {
        let mut a = self.and()?;
        while self.is_op("xor") {
            self.pos += 1;
            let b = self.and()?;
            a = Ast::Call("xor".into(), vec![a, b]);
        }
        Ok(a)
    }

    fn and(&mut self) -> PResult {
        let mut a = self.not()?;
        loop {
            if self.is_op("and") || self.is_op("&&") {
                self.pos += 1;
                let b = self.not()?;
                a = Ast::Call("and".into(), vec![a, b]);
            } else if self.is_op("nand") {
                self.pos += 1;
                let b = self.not()?;
                a = Ast::Call("nand".into(), vec![a, b]);
            } else {
                return Ok(a);
            }
        }
    }

    fn not(&mut self) -> PResult {
        if self.is_op("not") || self.is_op("~") || (self.is_op("!") && self.peek_at(1).is_some()) {
            self.pos += 1;
            let a = self.not()?;
            return Ok(Ast::Call("not".into(), vec![a]));
        }
        self.relation()
    }

    fn relation(&mut self) -> PResult {
        let first = self.additive()?;
        let mut parts = vec![first];
        let mut ops = Vec::new();
        loop {
            let op = match self.peek() {
                Some(T::Op("=")) | Some(T::Op("==")) => RelOp::Eq,
                Some(T::Op("!=")) | Some(T::Op("<>")) => RelOp::Ne,
                Some(T::Op("<")) => RelOp::Lt,
                Some(T::Op("<=")) => RelOp::Le,
                Some(T::Op(">")) => RelOp::Gt,
                Some(T::Op(">=")) => RelOp::Ge,
                _ => break,
            };
            self.pos += 1;
            ops.push(op);
            parts.push(self.additive()?);
        }
        if ops.is_empty() {
            Ok(parts.pop().unwrap())
        } else {
            Ok(Ast::Rel(parts, ops))
        }
    }

    fn additive(&mut self) -> PResult {
        let mut a = self.modlevel()?;
        loop {
            if self.eat("+") {
                let b = self.modlevel()?;
                a = bin('+', a, b);
            } else if self.eat("-") {
                let b = self.modlevel()?;
                a = bin('-', a, b);
            } else {
                return Ok(a);
            }
        }
    }

    fn modlevel(&mut self) -> PResult {
        let mut a = self.product()?;
        loop {
            if self.eat("mod") {
                let b = self.product()?;
                a = Ast::Call("mod".into(), vec![a, b]);
            } else if self.eat("choose") {
                let b = self.product()?;
                a = Ast::Call("binomial".into(), vec![a, b]);
            } else {
                return Ok(a);
            }
        }
    }

    fn starts_operand(&self, k: usize) -> bool {
        match self.peek_at(k) {
            Some(T::Num(..)) | Some(T::Func(_)) | Some(T::Sym(_)) => true,
            Some(T::Op("(")) => true,
            Some(T::Op("|")) => self.abs_depth == 0,
            _ => false,
        }
    }

    fn product(&mut self) -> PResult {
        let mut a = self.unary()?;
        loop {
            if self.eat("*") {
                let b = self.unary()?;
                a = bin('*', a, b);
            } else if self.eat("/") {
                let b = self.unary()?;
                a = bin('/', a, b);
            } else if self.is_op("%") && self.starts_operand(1) && !matches!(self.peek_at(1), Some(T::Op(_))) && self.space_before(0) {
                // "17 % 5": modulo
                self.pos += 1;
                let b = self.unary()?;
                a = Ast::Call("mod".into(), vec![a, b]);
            } else if self.starts_operand(0) {
                let b = self.power()?;
                a = bin('j', a, b);
            } else {
                return Ok(a);
            }
        }
    }

    fn unary(&mut self) -> PResult {
        if self.eat("-") {
            let a = self.unary()?;
            return Ok(match a {
                Ast::Num(r, s) if !r.is_zero() && !s.starts_with('-') => Ast::Neg(Box::new(Ast::Num(r, s))),
                other => Ast::Neg(Box::new(other)),
            });
        }
        if self.eat("+") {
            return self.unary();
        }
        self.power()
    }

    fn power(&mut self) -> PResult {
        let base = self.postfix()?;
        if self.eat("^") {
            let ex = self.exponent()?;
            return Ok(bin('^', base, ex));
        }
        Ok(base)
    }

    /// Exponent: a signed power; `e^2x` reads as `e^(2x)` when written tight.
    fn exponent(&mut self) -> PResult {
        if self.eat("-") {
            let a = self.exponent()?;
            return Ok(Ast::Neg(Box::new(a)));
        }
        if self.eat("+") {
            return self.exponent();
        }
        let first_is_num = matches!(self.peek(), Some(T::Num(..)));
        let mut a = self.power()?;
        if first_is_num && matches!(a, Ast::Num(..)) {
            if let Some(T::Sym(_)) = self.peek() {
                if !self.space_before(0) {
                    let b = self.postfix()?;
                    a = bin('j', a, b);
                }
            }
        }
        Ok(a)
    }

    fn postfix(&mut self) -> PResult {
        let mut a = self.primary()?;
        loop {
            if self.eat("!!") {
                a = Ast::Call("factorial2".into(), vec![a]);
            } else if self.is_op("!") && !matches!(self.peek_at(1), Some(T::Op("="))) {
                self.pos += 1;
                a = Ast::Call("factorial".into(), vec![a]);
            } else if self.is_op("%") && !(self.starts_operand(1) && self.space_before(0) && !matches!(self.peek_at(1), Some(T::Op(_)))) {
                self.pos += 1;
                a = Ast::Percent(Box::new(a));
            } else if self.eat("°") {
                a = Ast::Degree(Box::new(a));
            } else if self.is_op("'") {
                let mut n = 0;
                while self.eat("'") {
                    n += 1;
                }
                a = Ast::Prime(Box::new(a), n);
            } else {
                return Ok(a);
            }
        }
    }

    fn arglist(&mut self, close: &str) -> Result<Vec<Ast>, String> {
        let mut args = Vec::new();
        if self.eat(close) {
            return Ok(args);
        }
        let saved = self.abs_depth;
        self.abs_depth = 0;
        loop {
            args.push(self.expr()?);
            if self.eat(",") || self.eat(";") {
                continue;
            }
            self.expect(close)?;
            break;
        }
        self.abs_depth = saved;
        Ok(args)
    }

    fn primary(&mut self) -> PResult {
        let Some(tok) = self.peek().cloned() else {
            return Err("unexpected end of input".into());
        };
        match tok {
            T::Num(r, text, dec) => {
                self.pos += 1;
                self.has_decimal |= dec;
                // mixed fraction: 3 1/2
                if r.is_integer() && !dec {
                    if let (Some(T::Num(n2, t2, false)), Some(T::Op("/")), Some(T::Num(d2, t3, false))) = (self.peek(), self.peek_at(1), self.peek_at(2)) {
                        if self.space_before(0) && !self.space_before(1) && !self.space_before(2) && n2.is_integer() && d2.is_integer() && n2 < d2 && !matches!(self.peek_at(3), Some(T::Op("^"))) {
                            let frac = bin('/', Ast::Num(n2.clone(), t2.clone()), Ast::Num(d2.clone(), t3.clone()));
                            self.pos += 3;
                            return Ok(bin('+', Ast::Num(r, text), frac));
                        }
                    }
                }
                Ok(Ast::Num(r, text))
            }
            T::Op("(") => {
                self.pos += 1;
                let args = self.arglist(")")?;
                match args.len() {
                    0 => Err("empty parentheses".into()),
                    1 => Ok(args.into_iter().next().unwrap()),
                    _ => Ok(Ast::List(args)),
                }
            }
            T::Op("[") => {
                self.pos += 1;
                Ok(Ast::List(self.arglist("]")?))
            }
            T::Op("{") => {
                self.pos += 1;
                Ok(Ast::List(self.arglist("}")?))
            }
            T::Op("|") => {
                self.pos += 1;
                self.abs_depth += 1;
                let inner = self.expr()?;
                self.abs_depth -= 1;
                self.expect("|")?;
                Ok(Ast::Call("abs".into(), vec![inner]))
            }
            T::Func(name) => {
                self.pos += 1;
                self.application(&name)
            }
            T::Sym(name) => {
                self.pos += 1;
                Ok(Ast::Name(name))
            }
            T::Op(o) => Err(format!("unexpected “{o}”")),
        }
    }

    /// Function application in its many written forms.
    fn application(&mut self, name: &str) -> PResult {
        let user = name.strip_prefix("user:");
        if let Some(u) = user {
            self.expect("(")?;
            let args = self.arglist(")")?;
            return Ok(Ast::Call(format!("user:{u}"), args));
        }
        // sin^2 x, sin^-1 x
        let mut power: Option<Ast> = None;
        if self.is_op("^") {
            self.pos += 1;
            let neg = self.eat("-");
            let p = match self.peek().cloned() {
                Some(T::Num(r, t, _)) => {
                    self.pos += 1;
                    Ast::Num(r, t)
                }
                Some(T::Sym(s)) => {
                    self.pos += 1;
                    Ast::Name(s)
                }
                Some(T::Op("(")) => {
                    self.pos += 1;
                    let e = self.expr()?;
                    self.expect(")")?;
                    e
                }
                _ => return Err(format!("bad exponent after “{name}”")),
            };
            power = Some(if neg { Ast::Neg(Box::new(p)) } else { p });
        }
        let mut base: Option<Ast> = None;
        if name == "log" && self.is_op("_") {
            self.pos += 1;
            base = Some(match self.peek().cloned() {
                Some(T::Num(r, t, _)) => {
                    self.pos += 1;
                    Ast::Num(r, t)
                }
                Some(T::Sym(s)) => {
                    self.pos += 1;
                    Ast::Name(s)
                }
                Some(T::Op("(")) | Some(T::Op("{")) => {
                    let close = if self.is_op("(") { ")" } else { "}" };
                    self.pos += 1;
                    let e = self.expr()?;
                    self.expect(close)?;
                    e
                }
                _ => return Err("bad logarithm base".into()),
            });
        }
        let mut args: Vec<Ast> = if self.is_op("(") {
            self.pos += 1;
            self.arglist(")")?
        } else if self.is_op("[") && matches!(name, "integrate" | "diff" | "limit" | "sum" | "product" | "N" | "solve" | "factor" | "expand" | "simplify" | "series") {
            self.pos += 1;
            self.arglist("]")?
        } else {
            vec![self.bare_argument(name)?]
        };
        if args.is_empty() {
            return Err(format!("“{name}” needs an argument"));
        }
        if let Some(b) = base {
            args.insert(0, b);
        }
        if name == "log" && args.len() == 1 {
            self.note("log(x) is the natural logarithm");
        }
        let mut call_name = name.to_string();
        if let Some(p) = &power {
            let is_minus_one = matches!(p, Ast::Neg(inner) if matches!(&**inner, Ast::Num(r, _) if r.is_one()));
            if is_minus_one {
                let inv = match name {
                    "sin" | "cos" | "tan" | "cot" | "sec" | "csc" | "sinh" | "cosh" | "tanh" | "coth" | "sech" | "csch" => Some(format!("a{name}")),
                    _ => None,
                };
                if let Some(inv) = inv {
                    call_name = inv;
                    power = None;
                    self.note("f^(-1)(x) is the inverse function");
                }
            }
        }
        let call = Ast::Call(call_name, args);
        Ok(match power {
            Some(p) => bin('^', call, p),
            None => call,
        })
    }

    /// Argument written without parentheses: `sin 2x`, `ln x`, `sqrt 2`.
    fn bare_argument(&mut self, name: &str) -> PResult {
        let neg = self.eat("-");
        let tight = TIGHT.contains(&name);
        let arg = match self.peek() {
            Some(T::Func(f)) => {
                let f = f.clone();
                self.pos += 1;
                self.application(&f)?
            }
            Some(T::Num(..)) | Some(T::Sym(_)) | Some(T::Op("|")) => {
                let mut a = self.power()?;
                if !tight {
                    loop {
                        match self.peek() {
                            Some(T::Num(..)) | Some(T::Sym(_)) => {
                                let b = self.power()?;
                                a = bin('j', a, b);
                            }
                            _ => break,
                        }
                    }
                }
                a
            }
            Some(t) => return Err(format!("“{name}” needs an argument, found {}", describe(t))),
            None => return Err(format!("“{name}” needs an argument")),
        };
        Ok(if neg { Ast::Neg(Box::new(arg)) } else { arg })
    }
}

fn describe(t: &T) -> String {
    match t {
        T::Num(_, s, _) => format!("“{s}”"),
        T::Func(s) | T::Sym(s) => format!("“{s}”"),
        T::Op(o) => format!("“{o}”"),
    }
}

// ---------------------------------------------------------------------------
// AST -> expression
// ---------------------------------------------------------------------------

fn name_expr(name: &str, ctx: &Ctx) -> Expr {
    match name {
        "pi" => pi(),
        "e" if !ctx.no_euler => e(),
        "i" if !ctx.no_imag => i(),
        "infinity" => inf(),
        "true" => boolean(true),
        "false" => boolean(false),
        "eulergamma" => Expr::Const(K::EulerGamma),
        "goldenratio" => Expr::Const(K::Golden),
        "undefined" => undef(),
        other => sym(other),
    }
}

fn range_args(args: &[Expr]) -> Option<(Expr, Expr, Expr)> {
    match args {
        [Expr::List(v)] if v.len() == 3 => Some((v[0].clone(), v[1].clone(), v[2].clone())),
        [x, a, b] => Some((x.clone(), a.clone(), b.clone())),
        _ => None,
    }
}

fn default_var(e: &Expr) -> Expr {
    let syms = e.free_symbols();
    for pref in ["x", "t", "y", "z", "n", "u", "s", "r", "theta"] {
        if syms.iter().any(|s| &**s == pref) {
            return sym(pref);
        }
    }
    syms.iter().next().map(|s| Expr::Sym(s.clone())).unwrap_or_else(|| sym("x"))
}

pub fn ast_expr(a: &Ast, ctx: &Ctx) -> Result<Expr, String> {
    crate::budget::tick();
    Ok(match a {
        Ast::Num(r, _) => Expr::Num(r.clone()),
        Ast::Name(n) => name_expr(n, ctx),
        Ast::Bin(op, l, r) => {
            let (x, y) = (ast_expr(l, ctx)?, ast_expr(r, ctx)?);
            match op {
                '+' => x + y,
                '-' => x - y,
                '*' | 'j' => x * y,
                '/' => x / y,
                _ => pow(x, y),
            }
        }
        Ast::Neg(x) => neg(ast_expr(x, ctx)?),
        Ast::Percent(x) => ast_expr(x, ctx)? / int(100),
        Ast::Degree(x) => ast_expr(x, ctx)? * pi() / int(180),
        Ast::List(v) => Expr::List(v.iter().map(|x| ast_expr(x, ctx)).collect::<Result<_, _>>()?),
        Ast::Rel(parts, ops) => {
            let ps: Vec<Expr> = parts.iter().map(|x| ast_expr(x, ctx)).collect::<Result<_, _>>()?;
            let mut rels: Vec<Expr> = Vec::new();
            for (k, op) in ops.iter().enumerate() {
                rels.push(crate::funcs::rel(*op, ps[k].clone(), ps[k + 1].clone()));
            }
            if rels.len() == 1 {
                rels.pop().unwrap()
            } else {
                func("and", rels)
            }
        }
        Ast::Prime(x, n) => {
            let var = ctx.prime_var.clone().unwrap_or_else(|| "x".to_string());
            let inner = ast_expr(x, ctx)?;
            func("Derivative", vec![inner, sym(&var), int(*n as i64)])
        }
        Ast::Call(name, args) => {
            let xs: Vec<Expr> = args.iter().map(|x| ast_expr(x, ctx)).collect::<Result<_, _>>()?;
            if let Some(u) = name.strip_prefix("user:") {
                // y(x) with x the independent variable is just y
                if let (Some(pv), [Expr::Sym(s)]) = (&ctx.prime_var, xs.as_slice()) {
                    if **s == **pv {
                        return Ok(sym(u));
                    }
                }
                return Ok(raw_fn(u, xs));
            }
            match (name.as_str(), xs.len()) {
                ("integrate", 1) => {
                    let v = default_var(&xs[0]);
                    func("Integral", vec![xs[0].clone(), v])
                }
                ("integrate", 2) => match &xs[1] {
                    Expr::List(r) if r.len() == 3 => func("Integral", vec![xs[0].clone(), r[0].clone(), r[1].clone(), r[2].clone()]),
                    _ => func("Integral", xs),
                },
                ("integrate", 4) => func("Integral", xs),
                ("diff", 1) => {
                    let v = default_var(&xs[0]);
                    func("Derivative", vec![xs[0].clone(), v, one()])
                }
                ("diff", 2) => match &xs[1] {
                    Expr::List(r) if r.len() == 2 => func("Derivative", vec![xs[0].clone(), r[0].clone(), r[1].clone()]),
                    _ => func("Derivative", vec![xs[0].clone(), xs[1].clone(), one()]),
                },
                ("diff", 3) => func("Derivative", xs),
                ("limit", 3) => func("Limit", xs),
                ("limit", 4) => func("Limit", xs),
                ("sum" | "product", n) if n == 2 || n == 4 => {
                    let holder = if name == "sum" { "Sum" } else { "Product" };
                    match range_args(&xs[1..]) {
                        Some((v, lo, hi)) => func(holder, vec![xs[0].clone(), v, lo, hi]),
                        None => return Err(format!("{name} needs a range such as (f, n, 1, 10)")),
                    }
                }
                ("sum", 1) => match &xs[0] {
                    Expr::List(v) => add(v.clone()),
                    _ => raw_fn("sum", xs),
                },
                ("product", 1) => match &xs[0] {
                    Expr::List(v) => mul(v.clone()),
                    _ => raw_fn("product", xs),
                },
                _ => func(name, xs),
            }
        }
    })
}

// ---------------------------------------------------------------------------
// AST -> presentation (input echo)
// ---------------------------------------------------------------------------

pub fn ast_d(a: &Ast) -> D {
    match a {
        Ast::Num(_, text) => D::Num(text.clone()),
        Ast::Name(n) => D::Ident(
            match n.as_str() {
                "pi" => "π",
                "infinity" => "∞",
                "eulergamma" => "γ",
                "goldenratio" => "ϕ",
                other => other,
            }
            .to_string(),
        ),
        Ast::Bin('+', ..) | Ast::Bin('-', ..) => {
            let mut terms = Vec::new();
            collect_sum(a, false, &mut terms);
            D::Sum(terms)
        }
        Ast::Bin('*', ..) | Ast::Bin('j', ..) => {
            let mut fs = Vec::new();
            collect_prod(a, &mut fs);
            D::Prod(fs)
        }
        Ast::Bin('/', l, r) => D::Frac(Box::new(ast_d(l)), Box::new(ast_d(r))),
        Ast::Bin(_, l, r) => D::Pow(Box::new(ast_d(l)), Box::new(ast_d(r))),
        Ast::Neg(x) => D::Neg(Box::new(ast_d(x))),
        Ast::Percent(x) => D::Postfix(Box::new(ast_d(x)), "%".into()),
        Ast::Degree(x) => D::Postfix(Box::new(ast_d(x)), "°".into()),
        Ast::Prime(x, n) => D::Postfix(Box::new(ast_d(x)), "′".repeat(*n as usize)),
        Ast::List(v) => {
            let rows: Vec<&Vec<Ast>> = v
                .iter()
                .filter_map(|r| match r {
                    Ast::List(c) if !c.is_empty() && c.iter().all(|x| !matches!(x, Ast::List(_))) => Some(c),
                    _ => None,
                })
                .collect();
            if !v.is_empty() && rows.len() == v.len() && rows.iter().all(|r| r.len() == rows[0].len()) {
                D::Matrix(rows.iter().map(|r| r.iter().map(ast_d).collect()).collect())
            } else {
                D::List(v.iter().map(ast_d).collect())
            }
        }
        Ast::Rel(parts, ops) => D::Rel(parts.iter().map(ast_d).collect(), ops.iter().map(|o| o.symbol().to_string()).collect()),
        Ast::Call(name, args) => {
            let ds: Vec<D> = args.iter().map(ast_d).collect();
            let name = name.strip_prefix("user:").unwrap_or(name);
            match (name, ds.len()) {
                ("integrate", 1) => D::Call("∫".into(), ds),
                ("integrate", 2) => print::call_d("Integral", ds),
                ("integrate", 4) => print::call_d("Integral", ds),
                ("diff", 2) => print::call_d("Derivative", ds),
                ("diff", 3) => print::call_d("Derivative", ds),
                ("limit", 3) => print::call_d("Limit", ds),
                ("sum", 4) => print::call_d("Sum", ds),
                ("product", 4) => print::call_d("Product", ds),
                ("sec" | "csc" | "cot" | "sech" | "csch" | "coth", _) => D::Call(name.to_string(), ds),
                ("acot" | "asec" | "acsc", 1) => D::CallPow(name[1..].to_string(), Box::new(D::Neg(Box::new(D::Num("1".into())))), ds),
                _ => print::call_d(name, ds),
            }
        }
    }
}

fn collect_sum(a: &Ast, negate: bool, out: &mut Vec<D>) {
    match a {
        Ast::Bin('+', l, r) => {
            collect_sum(l, negate, out);
            collect_sum(r, negate, out);
        }
        Ast::Bin('-', l, r) => {
            collect_sum(l, negate, out);
            collect_sum(r, !negate, out);
        }
        other => {
            let d = ast_d(other);
            out.push(match (negate, d) {
                (true, D::Neg(x)) => *x,
                (true, x) => D::Neg(Box::new(x)),
                (false, x) => x,
            });
        }
    }
}

fn collect_prod(a: &Ast, out: &mut Vec<D>) {
    match a {
        Ast::Bin('*', l, r) | Ast::Bin('j', l, r) => {
            collect_prod(l, out);
            collect_prod(r, out);
        }
        other => out.push(ast_d(other)),
    }
}

// ---------------------------------------------------------------------------
// entry points
// ---------------------------------------------------------------------------

pub fn parse_ast(input: &str, ctx: &Ctx) -> Result<(Ast, Vec<String>, bool), String> {
    let src = normalize(input);
    let toks = tokenize(&src, ctx)?;
    if toks.is_empty() {
        return Err("empty input".into());
    }
    let mut p = Parser { toks, pos: 0, ctx, abs_depth: 0, notes: Vec::new(), has_decimal: false, depth: 0 };
    let ast = p.expr()?;
    if p.pos < p.toks.len() {
        return Err(format!("unexpected {}", describe(&p.toks[p.pos].t)));
    }
    Ok((ast, p.notes, p.has_decimal))
}

pub fn parse_with(input: &str, ctx: &Ctx) -> Result<Parsed, String> {
    let (ast, notes, has_decimal) = parse_ast(input, ctx)?;
    let expr = ast_expr(&ast, ctx)?;
    let d = ast_d(&ast);
    Ok(Parsed { ast, expr, d, has_decimal, notes })
}

pub fn parse(input: &str) -> Result<Parsed, String> {
    parse_with(input, &Ctx::default())
}

/// Parse straight to an expression.
pub fn parse_expr(input: &str) -> Result<Expr, String> {
    parse(input).map(|p| p.expr)
}

/// Parse a trusted literal (used for built-in formulas); panics on error.
pub fn p(input: &str) -> Expr {
    match parse_expr(input) {
        Ok(e) => e,
        Err(err) => panic!("internal formula “{input}” failed to parse: {err}"),
    }
}

/// Integer value of a small exact integer expression.
pub fn as_usize(e: &Expr) -> Option<usize> {
    e.as_int().and_then(|n| n.to_usize())
}
