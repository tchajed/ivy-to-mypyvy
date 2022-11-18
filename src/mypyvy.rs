use std::io;

use crate::ivy_l2s::{Transition, Transitions};

fn transition(w: &mut impl io::Write, t: &Transition) -> io::Result<()> {
    let args = match &t.bound {
        // TODO: arg actually has type here
        Some(arg) => format!("({}: ???)", arg),
        None => "".to_string(),
    };
    writeln!(w, "transition {}{}", t.name, args)?;

    // TODO: process steps
    for step in &t.steps {
        writeln!(w, "  {:?}", step)?;
    }
    writeln!(w)?;
    Ok(())
}

pub fn transitions(w: &mut impl io::Write, ts: &Transitions) -> io::Result<()> {
    for t in ts.iter() {
        transition(w, t)?
    }
    Ok(())
}
