extern crate pest;

// import only for trait
use clap::Parser as _;
use pest::{
    pratt_parser::{Assoc, Op, PrattParser},
    Parser as _,
};
use std::fs;

#[derive(pest_derive::Parser)]
#[grammar = "ivy.pest"]
pub struct IvyParser;

#[derive(clap::Parser, Debug)]
#[command(about, long_about=None)]
struct Args {
    /// Filename for a file with l2s_debug=true output
    file: String,
}

fn main() {
    let args = Args::parse();
    let unparsed_file = fs::read_to_string(args.file).expect("could not read input file");

    IvyParser::parse(Rule::ident, "ext:mutex_protocol.step_atomic_store")
        .expect("unsuccessful ident parse");
    IvyParser::parse(Rule::ident, "fml:t:mutex_protocol.thread").expect("unsuccessful ident parse");
    IvyParser::parse(Rule::expr, "forall V0.  l2s_a(V0)").expect("unsuccessful expr parse");
    IvyParser::parse(Rule::expr, "l2s_g_1 -> ~(forall T. mutex_protocol.d(T))")
        .expect("unsuccessful expr parse");
    IvyParser::parse(Rule::assume_rule, "assume l2s_g_1 -> ~(forall T. mutex_protocol.d(T))")
        .expect("unsuccessful assume parse");
    IvyParser::parse(Rule::expr, "forall V0. l2s_g_4(V0) -> ~l2s_g_3(V0)")
        .expect("unsuccessful expr parse");

    let file = IvyParser::parse(Rule::file, &unparsed_file)
        .expect("unsuccessful parse of input file")
        .next()
        .unwrap();

    println!("{:?}", file);

    let _pratt = PrattParser::new()
        .op(Op::infix(Rule::and, Assoc::Left))
        .op(Op::infix(Rule::implies, Assoc::Left))
        .op(Op::prefix(Rule::not));
}
