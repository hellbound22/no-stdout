#![cfg_attr(not(feature = "std"), no_std)]

use core::{
    fmt::{self},
    hint,
    sync::atomic::{AtomicU8, Ordering},
};

static STATE: AtomicU8 = AtomicU8::new(UNINITIALIZED);
static mut STDOUT: &dyn StdOut = &NopOut;

/// A trait describes common operations with the stdout.
pub trait StdOut: Send + 'static {
    /// Writes a bytes slice into the stdout, returning whether write succeeded.
    fn write_bytes(&self, bytes: &[u8]) -> fmt::Result;
    /// Writes a string slice.
    fn write_str(&self, s: &str) -> fmt::Result;
    /// Writes a formatted string.
    fn write_fmt(&self, args: fmt::Arguments) -> fmt::Result;
    /// Ensures that none of the previously written bytes are still buffered.
    fn flush(&self) -> fmt::Result;
}

// Three different states can occur during the program lifecycle:
//
// The stdout is uninitialized yet.
const UNINITIALIZED: u8 = 0;
// The stdout is initializing right now.
const INITIALIZING: u8 = 1;
// The stdout has been initialized and currently is active.
const INITIALIZED: u8 = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SetStdoutError(());

impl SetStdoutError {
    fn new() -> Self {
        Self(())
    }
}

struct NopOut;

impl StdOut for NopOut {
    fn write_str(&self, _: &str) -> fmt::Result {
        Ok(())
    }

    fn write_bytes(&self, _bytes: &[u8]) -> fmt::Result {
        Ok(())
    }

    fn write_fmt(&self, _args: fmt::Arguments) -> fmt::Result {
        Ok(())
    }

    fn flush(&self) -> fmt::Result {
        Ok(())
    }
}

pub fn atomic_cas_emulated(value: &AtomicU8, current: u8, new: u8) -> u8 {
    avr_device::interrupt::free(|_| {
        let old_value = value.load(Ordering::SeqCst);
        if old_value == current {
            value.store(new, Ordering::SeqCst);
        }
        old_value
    })
}

fn set_stdout_inner<F>(stdout: F) -> Result<(), SetStdoutError>
where
    F: FnOnce() -> &'static dyn StdOut,
{
    let old_state = atomic_cas_emulated(&STATE, UNINITIALIZED, INITIALIZING);

    match old_state {
        // The state was UNINITIALIZED and then changed to INITIALIZING.
        UNINITIALIZED => {
            unsafe {
                STDOUT = stdout();
            }
            STATE.store(INITIALIZED, Ordering::SeqCst);

            Ok(())
        }

        // The state is already INITIALIZING.
        INITIALIZING => {
            // Make sure the state became INITIALIZING finally.
            while STATE.load(Ordering::SeqCst) == INITIALIZING {
                hint::spin_loop();
            }

            Err(SetStdoutError::new())
        }

        _ => Err(SetStdoutError::new()),
    }
}

/// Initialized the global stdout with the a specified `&'static dyn StdOut`.
///
/// This function may only be called once during the program lifecycle.
pub fn init(stdout: &'static dyn StdOut) -> Result<(), SetStdoutError> {
    set_stdout_inner(move || stdout)
}

/// Returns a reference to the stdout.
///
/// If a stdout has not been set, returns a no-op implementation.
pub fn stdout() -> &'static dyn StdOut {
    if STATE.load(Ordering::SeqCst) != INITIALIZED {
        static NOP: NopOut = NopOut;
        &NOP
    } else {
        unsafe { STDOUT }
    }
}

/// Macro for printing to the configured stdout, without a newline.
#[macro_export]
macro_rules! uprint {
    ($s:expr) => {{
        $crate::stdout()
            .write_str($s)
            .ok();
    }};
    ($s:expr, $($tt:tt)*) => {{
        $crate::stdout()
            .write_fmt(format_args!($s, $($tt)*))
            .ok();
    }};
}

/// Macro for printing to the configured stdout, with a newline.
#[macro_export]
macro_rules! uprintln {
    () => {{
        $crate::stdout()
            .write_str(uprintln!(@newline))
            .ok();
    }};
    ($s:expr) => {{
        $crate::stdout()
            .write_str(concat!($s, uprintln!(@newline)))
            .ok();
    }};
    ($s:expr, $($tt:tt)*) => {{
        $crate::stdout()
            .write_fmt(format_args!(concat!($s, uprintln!(@newline)), $($tt)*))
            .ok();
    }};

    (@newline) => { "\r\n" };
}

/// Macro for printing to the configured stdout, without a newline.
///
/// This method prints only if the `dprint` feature enabled, which is useful
/// for debugging purposes.
#[cfg(any(feature = "dprint", doc))]
#[macro_export]
macro_rules! dprint {
    ($s:expr) => {{
        $crate::stdout()
            .write_str($s)
            .ok();
    }};
    ($s:expr, $($tt:tt)*) => {{
        $crate::stdout()
            .write_fmt(format_args!($s, $($tt)*))
            .ok();
    }};
}
#[cfg(not(any(feature = "dprint", doc)))]
#[macro_export]
macro_rules! dprint {
    ($s:expr) => {};
    ($s:expr, $($tt:tt)*) => {};
}

/// Macro for printing to the configured stdout, with a newline.
///
/// This method prints only if the `dprint` feature enabled, which is useful
/// for debugging purposes.
#[macro_export]
#[cfg(any(feature = "dprint", doc))]
macro_rules! dprintln {
    () => {{
        $crate::stdout()
            .write_str(dprintln!(@newline))
            .ok();
    }};
    ($s:expr) => {{
        $crate::stdout()
            .write_str(concat!($s, dprintln!(@newline)))
            .ok();
    }};
    ($s:expr, $($tt:tt)*) => {{
        $crate::stdout()
            .write_fmt(format_args!(concat!($s, dprintln!(@newline)), $($tt)*))
            .ok();
    }};

    (@newline) => { "\r\n" };
}
#[cfg(not(any(feature = "dprint", doc)))]
#[macro_export]
macro_rules! dprintln {
    () => {};
    ($s:expr) => {};
    ($s:expr, $($tt:tt)*) => {};
}
