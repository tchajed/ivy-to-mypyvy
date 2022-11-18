use std::fs;

use ivy_to_mypyvy::ivy_l2s;

#[test]
fn mutex_l2s_output_parses() {
    let unparsed_file = fs::read_to_string("tests/l2s.out").expect("could not read input file");
    let file = ivy_l2s::parse(&unparsed_file).expect("unsuccessful parse of input file");
    assert_eq!(file.len(), 7, "incorrect number of transitions parsed");
    assert_eq!(
        file[0].name, "ext:mutex_protocol.step_atomic_store",
        "incorrect name for first transition"
    );
}
