use std::fs;

use ivy_to_mypyvy::{
    ivy_l2s::{self, System},
    mypyvy, pretty,
};

fn read_input() -> String {
    fs::read_to_string("tests/l2s.out").expect("could not read input file")
}

fn parse_input() -> System {
    let input = read_input();
    ivy_l2s::parse(&input).expect("unsuccessful parse of input file")
}

#[test]
fn mutex_l2s_output_parses() {
    let sys = parse_input();
    assert_eq!(
        sys.transitions.len(),
        7,
        "incorrect number of transitions parsed"
    );
    assert_eq!(
        sys.transitions[0].name, "ext:mutex_protocol.step_atomic_store",
        "incorrect name for first transition"
    );
}

#[test]
fn mutex_l2s_file() {
    let sys = parse_input();
    let output = mypyvy::fmt_system(&sys);
    insta::assert_display_snapshot!(output);
}

#[test]
fn mutex_l2s_ivy_pretty_print() {
    let sys = parse_input();
    let output = pretty::fmt_system(&sys);
    insta::assert_display_snapshot!(output);
}
