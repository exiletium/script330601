//! Noesis computational engine.
pub mod bigfloat;
pub mod budget;
pub mod calculus;
pub mod compute;
pub mod domains;
pub mod engine;
pub mod eval;
pub mod expr;
pub mod funcs;
#[cfg(feature = "gpu")]
pub mod gpu;
pub mod linalg;
pub mod numeric;
pub mod numtheory;
pub mod parse;
pub mod plot;
pub mod poly;
pub mod print;
pub mod simplify;
pub mod solve;
pub mod special;
pub mod stats;
pub mod steps;

pub use engine::{query, query_with, Options, QueryResult};
