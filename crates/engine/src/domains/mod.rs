//! Knowledge domains beyond pure mathematics: units, physical constants,
//! chemistry, geography, dates and times, finance, probability.

use crate::engine::Handler;

/// Domain handlers in priority order (they run before the general math
/// analyzer, and only claim inputs that use their vocabulary).
pub fn handlers() -> Vec<(&'static str, Handler)> {
    vec![]
}
