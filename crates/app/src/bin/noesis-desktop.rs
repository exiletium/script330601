//! `noesis-desktop`: the desktop app without a console window on Windows.
#![cfg_attr(windows, windows_subsystem = "windows")]

fn main() {
    if let Err(e) = noesis_app::gui::run() {
        eprintln!("Could not start the desktop app: {e}");
        std::process::exit(1);
    }
}
