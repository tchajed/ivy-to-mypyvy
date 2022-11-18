extern crate pest;
mod ivy_l2s;

// import only for trait
use clap::Parser as _;
use ivy_l2s::{IvyParser, Rule};
use pest::Parser as _;
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

    let file = IvyParser::parse(Rule::file, &unparsed_file)
        .expect("unsuccessful parse of input file")
        .next()
        .unwrap();
    let transitions = parse(file);

    println!("{:?}", transitions);
}
