use std::fs;

use ivy_to_mypyvy::{
    ivy_l2s::{self, System},
    mypyvy, pretty,
};

fn read_input(name: &str) -> String {
    fs::read_to_string(format!("tests/{name}")).expect("could not read input file")
}

fn parse_input(name: &str) -> System {
    let input = read_input(name);
    ivy_l2s::parse(&input)
        .unwrap_or_else(|err| panic!("unsuccessful parse of input file {name}: {err}"))
}

#[test]
fn mutex_l2s_output_parses() {
    let sys = parse_input("mutex.l2s.out");
    assert_eq!(
        sys.transitions.len(),
        8,
        "incorrect number of transitions parsed"
    );
    assert_eq!(
        sys.transitions[0].name, "ext:mutex_protocol.step_atomic_store",
        "incorrect name for first transition"
    );
}

#[test]
fn mutex_l2s_file() {
    let sys = parse_input("mutex.l2s.out");
    let output = mypyvy::fmt_system(&sys);
    insta::assert_display_snapshot!(output);
}

#[test]
fn better_mutex_l2s_file() {
    let sys = parse_input("better_mutex.l2s.out");
    let output = mypyvy::fmt_system(&sys);
    insta::assert_display_snapshot!(output);
}

#[test]
fn mutex_l2s_ivy_pretty_print() {
    let sys = parse_input("mutex.l2s.out");
    let output = pretty::fmt_system(&sys);
    insta::assert_display_snapshot!(output);
}
