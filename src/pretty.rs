//! Pretty-printer to print back parsed Ivy output.

use std::fmt::Write;

use crate::{
    ivy_l2s::{BinOp, Expr, IfCond, PrefixOp, Quantifier, Relation, Step, System, Transition},
    names,
    printing::{self, indented, parens},
};

fn relation(r: &Relation) -> String {
    r.to_string()
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
        Expr::IfElse { cond, then, else_ } => format!(
            "{} if {} else {}",
            parens(&expr(then)),
            expr(cond),
            parens(&expr(else_))
        ),
    }
}

fn if_cond(c: &IfCond) -> String {
    match c {
        IfCond::Expr(e) => expr(e),
        IfCond::Some { name, e } => format!("some {name}. {}", expr(e)),
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
            writeln!(w, "if {} {{", if_cond(cond))?;
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
    let arg = if t.bound.is_empty() {
        "".to_string()
    } else {
        format!("({})", t.bound.join(", "))
    };
    printing::with_buf(|w| {
        writeln!(w, "{} = action{}{{", t.name, arg)?;
        writeln!(indented(w), "{}", steps(&t.steps))?;
        write!(w, "}}")?;
        Ok(())
    })
}

pub fn fmt_system(sys: &System) -> String {
    let sys = names::clean_namespaces(sys);
    printing::with_buf(|w| {
        writeln!(w, "let")?;
        for t in sys.transitions.iter() {
            writeln!(w, "{}", transition(t))?;
            writeln!(w)?;
        }
        writeln!(w, "in {{")?;
        writeln!(indented(w), "{}", steps(&sys.init))?;
        writeln!(w, "}}")?;
        if !sys.invariants.is_empty() {
            writeln!(w, "while *")?;
            for (name, e) in sys.invariants.iter() {
                writeln!(indented(w), "invariant [{name}] {}", expr(e))?;
            }
        }
        Ok(())
    })
}
