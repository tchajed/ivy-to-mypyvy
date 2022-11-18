use std::{fmt, io};

use crate::ivy_l2s::{Transition, Transitions};

struct Indentation<'a, W>
where
    W: io::Write,
{
    _indent: usize,
    writer: &'a mut W,
}

impl<'a, W> Indentation<'a, W>
where
    W: io::Write,
{
    fn new(w: &'a mut W) -> Self {
        Indentation {
            _indent: 0,
            writer: w,
        }
    }

    fn indent(&mut self, amount: usize) {
        self._indent += amount;
    }

    fn unindent(&mut self, amount: usize) {
        assert!(
            self._indent >= amount,
            "unindent exceeds current indentation"
        );
        self._indent -= amount;
    }

    fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) -> io::Result<()> {
        write!(self.writer, "{:width$}", "", width = self._indent)?;
        self.writer.write_fmt(fmt)?;
        Ok(())
    }
}

fn transition(w: &mut impl io::Write, t: &Transition) -> io::Result<()> {
    let mut w = Indentation::new(w);
    let args = match &t.bound {
        // TODO: arg actually has type here
        Some(arg) => format!("({}: ???)", arg),
        None => "".to_string(),
    };
    writeln!(w, "transition {}{}", t.name, args)?;

    w.indent(2);
    // TODO: process steps
    for step in &t.steps {
        writeln!(w, "{:?}", step)?;
    }
    w.unindent(2);
    writeln!(w)?;
    Ok(())
}

pub fn transitions(w: &mut impl io::Write, ts: &Transitions) -> io::Result<()> {
    for t in ts.iter() {
        transition(w, t)?
    }
    Ok(())
}
