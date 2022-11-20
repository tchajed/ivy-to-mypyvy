//! Pretty-printer to print back parsed Ivy output.

use std::fmt::Write;

use crate::{
    ivy_l2s::{BinOp, Expr, PrefixOp, Quantifier, Relation, Step, System, Transition},
    printing::{self, indented, parens},
};

fn relation(r: &Relation) -> String {
    if r.args.is_empty() {
        r.name.clone()
    } else {
        format!("{}({})", r.name, r.args.join(", "))
    }
}

fn bin_op(op: &BinOp) -> &'static str {
    match op {
        BinOp::And => "&",
        BinOp::Or => "|",
        BinOp::Implies => "->",
        BinOp::Equal => "=",
        BinOp::NotEqual => "!=",
        BinOp::Iff => "<->",
    }
}

fn prefix_op(op: &PrefixOp) -> &'static str {
    match op {
        PrefixOp::Not => "~",
    }
}

fn quantifier(q: &Quantifier) -> &'static str {
    match q {
        Quantifier::Forall => "forall",
        Quantifier::Some => "some",
        Quantifier::Exists => "exists",
    }
}

fn expr(e: &Expr) -> String {
    match e {
        Expr::Relation(r) => relation(r),
        Expr::Infix { lhs, op, rhs } => {
            format!(
                "{} {} {}",
                parens(&expr(lhs)),
                bin_op(op),
                parens(&expr(rhs))
            )
        }
        Expr::Quantified {
            quantifier: q,
            bound,
            body,
        } => {
            format!("({} {bound}. {})", quantifier(q), expr(body),)
        }
        Expr::Prefix { op, e } => format!("{}{}", prefix_op(op), parens(&expr(e))),
        Expr::Havoc => "*".to_string(),
    }
}

fn steps(steps: &[Step]) -> String {
    steps.iter().map(step).collect::<Vec<_>>().join(";\n")
}

fn step(s: &Step) -> String {
    match s {
        Step::Assume(e) => format!("assume {}", expr(e)),
        Step::Assert(e) => format!("assert {}", expr(e)),
        Step::Assign(r, e) => format!("{} := {}", relation(r), expr(e)),
        Step::If { cond, then, else_ } => printing::with_buf(|w| {
            writeln!(w, "if {} {{", expr(cond))?;
            writeln!(indented(w), "{}", steps(then))?;
            if else_.is_empty() {
                write!(w, "}}")?;
            } else {
                writeln!(w, "}} else {{")?;
                writeln!(indented(w), "{}", steps(else_))?;
                write!(w, "}}")?;
            }
            Ok(())
        }),
    }
}

fn transition(t: &Transition) -> String {
    let arg = match &t.bound {
        Some(name) => format!("({name})"),
        None => "".to_string(),
    };
    printing::with_buf(|w| {
        writeln!(w, "{} = action{}{{", t.name, arg)?;
        writeln!(indented(w), "{}", steps(&t.steps))?;
        write!(w, "}}")?;
        Ok(())
    })
}

pub fn fmt_system(sys: &System) -> String {
    printing::with_buf(|w| {
        writeln!(w, "let")?;
        for t in sys.transitions.iter() {
            writeln!(w, "{}", transition(t))?;
            writeln!(w)?;
        }
        writeln!(w, "in {{")?;
        writeln!(indented(w), "{}", steps(&sys.init))?;
        writeln!(w, "}}")?;
        Ok(())
    })
}
