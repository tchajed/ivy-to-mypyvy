use std::fmt::{self, Write};

use indenter::CodeFormatter;

use crate::{
    ivy_l2s::{BinOp, Expr, PrefixOp, Quantifier, Relation, Step, System, Transition},
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

fn system(sys: &System) -> Result<String, fmt::Error> {
    let mut buf = String::new();
    let mut w = CodeFormatter::new(&mut buf, " ");
    writeln!(w, "let")?;
    for t in sys.transitions.iter() {
        writeln!(w, "{}", transition(t))?;
    }
    writeln!(w, "in")?;
    writeln!(w, "{}", steps(&sys.init))?;
    Ok(buf)
}

pub fn fmt_system(sys: &System) -> String {
    system(sys).expect("formatting error")
}
