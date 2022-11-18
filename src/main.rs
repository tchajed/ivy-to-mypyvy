extern crate pest;
mod ivy_l2s;

use clap::Parser;
use std::fs;

use crate::ivy_l2s::parse;

#[derive(clap::Parser, Debug)]
#[command(about, long_about=None)]
struct Args {
    /// Filename for a file with l2s_debug=true output
    file: String,
}

fn main() {
    let args = Args::parse();
    let unparsed_file = fs::read_to_string(args.file).expect("could not read input file");

    let file = parse(&unparsed_file)
        .expect("unsuccessful parse of input file");
    println!("{:?}", file);
}
