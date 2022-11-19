extern crate pest;

use clap::Parser;
use ivy_to_mypyvy::{ivy_l2s, mypyvy, pretty::fmt_system};
use std::{fs, io, process};

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
    let sys = match ivy_l2s::parse(&unparsed_file) {
        Ok(sys) => sys,
        Err(err) => {
            eprintln!("could not parse input:");
            eprintln!("{err}");
            process::exit(1);
        }
    };

    if args.ivy {
        print!("{}", fmt_system(&sys));
    } else {
        mypyvy::emit_transitions(&mut io::stdout(), &sys).expect("could not write output");
    }
}
