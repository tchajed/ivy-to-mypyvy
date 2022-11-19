use std::io;

use crate::{
    ivy_l2s::{BinOp, Expr, PrefixOp, Relation, Step, Transition},
    printing::parens,
};

/// Pretty-printer to print back parsed Ivy output.

fn relation(r: &Relation) -> String {
    match r {
        Relation::Ident(f) => f.to_string(),
        Relation::Call(f, arg) => format!("{f}({arg})"),
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
        Expr::Forall { bound, body } => {
            format!("(forall {bound}. {})", expr(body),)
        }
        Expr::Some { bound, body } => {
            format!("(some {bound}. {})", expr(body),)
        }
        Expr::Prefix { op, e } => format!("{}{}", prefix_op(op), parens(&expr(e))),
        Expr::Havoc => "*".to_string(),
    }
}

fn steps(steps: &[Step]) -> String {
    format!(
        "{{{}}}",
        steps.iter().map(step).collect::<Vec<_>>().join(";\n")
    )
}

fn step(s: &Step) -> String {
    match s {
        Step::Assume(e) => format!("assume {}", expr(e)),
        Step::Assert(e) => format!("assert {}", expr(e)),
        Step::Assign(r, e) => format!("{} := {}", relation(r), expr(e)),
        Step::If { cond, then, else_ } => {
            if else_.is_empty() {
                format!("if {cond}\n{then}", cond = expr(cond), then = steps(then))
            } else {
                format!(
                    "if {cond}\n{then}\nelse {else_}",
                    cond = expr(cond),
                    then = steps(then),
                    else_ = steps(else_),
                )
            }
        }
    }
}

fn transition(t: &Transition) -> String {
    let arg = match &t.bound {
        Some(name) => format!("({name})"),
        None => "".to_string(),
    };
    format!("{} = action{}{}", t.name, arg, steps(&t.steps))
}

pub fn print_transitions(w: &mut impl io::Write, ts: &[Transition]) {
    for t in ts.iter() {
        writeln!(w, "{}", transition(t)).expect("could not write");
    }
}
