extern crate pest;

use clap::Parser;
use ivy_to_mypyvy::{ivy_l2s, mypyvy, pretty};
use std::{fs, process};

#[derive(clap::Parser, Debug)]
#[command(about, long_about=None)]
struct Args {
    /// (for debugging parser) print back an Ivy file
    #[arg(long)]
    ivy: bool,

    /// Filename for a file with l2s_debug=true output
    file: String,
}

fn main() {
    let args = Args::parse();

    let unparsed_file = fs::read_to_string(args.file).expect("could not read input file");
    let (subs, sys) = match ivy_l2s::parse(&unparsed_file) {
        Ok(v) => v,
        Err(err) => {
            eprintln!("could not parse input:");
            eprintln!("{err}");
            process::exit(1);
        }
    };

    if args.ivy {
        print!("{}", pretty::fmt_system(&sys));
    } else {
        print!("{}", mypyvy::fmt_system(&subs, &sys));
    }
}
