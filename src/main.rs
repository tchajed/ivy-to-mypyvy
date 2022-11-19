extern crate pest;

use clap::Parser;
use ivy_to_mypyvy::{ivy_l2s, mypyvy, pretty::print_transitions};
use std::{fs, io};

#[derive(clap::Parser, Debug)]
#[command(about, long_about=None)]
struct Args {
    #[arg(long)]
    ivy: bool,

    /// Filename for a file with l2s_debug=true output
    file: String,
}

fn main() {
    let args = Args::parse();

    let unparsed_file = fs::read_to_string(args.file).expect("could not read input file");
    let file = ivy_l2s::parse(&unparsed_file).expect("unsuccessful parse of input file");

    if args.ivy {
        print_transitions(&mut io::stdout(), &file);
    } else {
        mypyvy::emit_transitions(&mut io::stdout(), &file).expect("could not write output");
    }
}
