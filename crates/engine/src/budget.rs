//! Cooperative time budgets.
//!
//! Symbolic algorithms can explode combinatorially.  Every expression
//! constructor calls [`tick`], which cheaply checks a thread-local deadline
//! and unwinds with a [`Timeout`] payload once it has passed.  [`attempt`]
//! runs a closure under a (nested) deadline and turns the unwind into `None`.

use std::cell::Cell;
use std::panic::{self, AssertUnwindSafe};
use std::sync::Once;
use std::time::{Duration, Instant};

thread_local! {
    static DEADLINE: Cell<Option<Instant>> = const { Cell::new(None) };
    static COUNTER: Cell<u32> = const { Cell::new(0) };
}

/// Panic payload used to unwind out of a computation that ran out of time.
pub struct Timeout;

static HOOK: Once = Once::new();

fn install_hook() {
    HOOK.call_once(|| {
        let default = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            if info.payload().is::<Timeout>() {
                return;
            }
            if std::env::var_os("NOESIS_DEBUG").is_some() {
                default(info);
            }
        }));
    });
}

#[inline]
pub fn tick() {
    COUNTER.with(|c| {
        let n = c.get().wrapping_add(1);
        c.set(n);
        if n & 0xff == 0 {
            check();
        }
    });
}

/// Immediate deadline check (use inside long numeric loops).
pub fn check() {
    DEADLINE.with(|d| {
        if let Some(t) = d.get() {
            if Instant::now() > t {
                panic::panic_any(Timeout);
            }
        }
    });
}

/// Seconds left in the innermost budget (large when unlimited).
pub fn remaining() -> f64 {
    DEADLINE.with(|d| match d.get() {
        Some(t) => t.saturating_duration_since(Instant::now()).as_secs_f64(),
        None => 1e9,
    })
}

/// Run `f` with at most `seconds` of time.  Returns `None` when the budget is
/// exhausted or the computation fails internally.
pub fn attempt<T>(seconds: f64, f: impl FnOnce() -> T) -> Option<T> {
    install_hook();
    let outer = DEADLINE.with(|d| d.get());
    let mut mine = Instant::now() + Duration::from_secs_f64(seconds.max(0.0));
    if let Some(o) = outer {
        if o < mine {
            mine = o;
        }
    }
    DEADLINE.with(|d| d.set(Some(mine)));
    let res = panic::catch_unwind(AssertUnwindSafe(f));
    DEADLINE.with(|d| d.set(outer));
    match res {
        Ok(v) => Some(v),
        Err(payload) => {
            if payload.is::<Timeout>() {
                // if the enclosing budget is exhausted as well, keep unwinding
                if let Some(o) = outer {
                    if Instant::now() > o {
                        panic::resume_unwind(payload);
                    }
                }
            }
            None
        }
    }
}

/// Like [`attempt`] but flattens an inner `Option`.
pub fn attempt_opt<T>(seconds: f64, f: impl FnOnce() -> Option<T>) -> Option<T> {
    attempt(seconds, f).flatten()
}

/// Run a closure on a thread with a large stack (deep expression trees
/// recurse deeply) and an overall time budget.
pub fn run_isolated<T: Send + 'static>(seconds: f64, f: impl FnOnce() -> T + Send + 'static) -> Option<T> {
    install_hook();
    let handle = std::thread::Builder::new()
        .name("noesis-query".into())
        .stack_size(256 * 1024 * 1024)
        .spawn(move || attempt(seconds, f))
        .ok()?;
    handle.join().ok().flatten()
}
