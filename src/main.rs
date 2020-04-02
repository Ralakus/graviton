#![warn(clippy::nursery)]
use clap::{App, Arg};

mod driver;

const PROGRAM_NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");
const DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");

fn main() {
    let args = App::new(PROGRAM_NAME)
        .version(VERSION)
        .about(DESCRIPTION)
        .author(AUTHOR)
        .arg(
            Arg::with_name("Input")
                .help("The input file to process")
                .required(true)
                .index(1),
        )
        .get_matches();

    let driver = driver::Driver::create(args);

    let tir = futures::executor::block_on(driver);

    // println!("\n{}", tir);
}
