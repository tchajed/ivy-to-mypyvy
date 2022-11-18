
use crate::ivy_l2s::{Transition, Transitions};

fn transition(t: &Transition) {
    let args = match &t.bound {
        // TODO: arg actually has type here
        Some(arg) => format!("({}: ???)", arg),
        None => "".to_string(),
    };
    println!("transition {}{}", t.name, args);

    // TODO: process steps
    for step in &t.steps {
        println!("  {:?}", step);
    }
    println!();
}

pub fn transitions(ts: &Transitions) {
    for t in ts.iter() {
        transition(t)
    }
}
