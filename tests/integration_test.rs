use std::fs;

use ivy_to_mypyvy::{
    ivy_l2s::{self, System},
    mypyvy,
};

fn read_input() -> String {
    fs::read_to_string("tests/l2s.out").expect("could not read input file")
}

#[test]
fn mutex_l2s_output_parses() {
    let input = read_input();
    let sys = ivy_l2s::parse(&input).expect("unsuccessful parse of input file");
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
    let input = read_input();
    let file = ivy_l2s::parse(&input).expect("unsuccessful parse of input file");
    let output = mypyvy_output(&file);
    insta::assert_display_snapshot!(output);
}
