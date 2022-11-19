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

fn mypyvy_output(file: &System) -> String {
    let mut buf = vec![];
    mypyvy::emit_transitions(&mut buf, file).expect("writing output");
    String::from_utf8(buf).expect("output is invalid utf-8")
}

#[test]
fn mutex_l2s_file() {
    let sys = parse_input();
    let output = mypyvy_output(&sys);
    insta::assert_display_snapshot!(output);
}

fn ivy_output(sys: &System) -> String {
    let mut buf = vec![];
    pretty::print_system(&mut buf, sys);
    String::from_utf8(buf).expect("output is invalid utf-8")
}

#[test]
fn mutex_l2s_ivy_pretty_print() {
    let sys = parse_input();
    let output = ivy_output(&sys);
    insta::assert_display_snapshot!(output);
}
