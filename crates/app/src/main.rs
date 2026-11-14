//! `noesis`: answers a query in the terminal when given arguments, and
//! opens the desktop app otherwise.

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() || args.iter().any(|a| a == "--gui") {
        #[cfg(feature = "gui")]
        {
            if let Err(e) = noesis_app::gui::run() {
                eprintln!("Could not start the desktop app: {e}");
                std::process::exit(1);
            }
            return;
        }
        #[cfg(not(feature = "gui"))]
        {
            println!("{}", noesis_app::cli::HELP);
            return;
        }
    }
    std::process::exit(noesis_app::cli::run(args));
}
