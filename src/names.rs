use crate::ivy_l2s::{Expr, Relation, Step, System, Transition};

/// Clean up names from Ivy output.

fn ident(name: &str) -> String {
    let name = name.strip_prefix("fml:").unwrap_or(name);
    let name = name.strip_prefix("loc:").unwrap_or(name);
    let name = name.strip_prefix("ext:").unwrap_or(name);

    let name = name
        .split_once(':')
        .map(|(name, _typ)| name)
        .unwrap_or(name);
    let name = name
        .split_once('.')
        .map(|(_namespace, name)| name)
        .unwrap_or(name);
    name.to_string()

    // TODO: the below code preserves types; these interfere with substitution
    // but should otherwise be used for some type inference of relations

    // // remove any foo. prefix from each : component (sometimes we have
    // // x:mutex_protocol.thread, for example, and we want to keep that type
    // // annotation)
    // name.split(':')
    //     .map(|part| {
    //         if let Some((_namespace, name)) = part.split_once('.') {
    //             name.to_string()
    //         } else {
    //             part.to_string()
    //         }
    //     })
    //     .collect::<Vec<_>>()
    //     .join(":")
}

fn relation(r: &Relation) -> Relation {
    Relation {
        name: ident(&r.name),
        args: r.args.iter().map(|arg| ident(arg)).collect(),
    }
}

fn expr(e: &Expr) -> Expr {
    match e {
        Expr::Relation(r) => Expr::Relation(relation(r)),
        Expr::Infix { lhs, op, rhs } => Expr::Infix {
            lhs: Box::new(expr(lhs)),
            op: *op,
            rhs: Box::new(expr(rhs)),
        },
        Expr::Quantified {
            quantifier: q,
            bound,
            body,
        } => Expr::Quantified {
            quantifier: *q,
            bound: ident(bound),
            body: Box::new(expr(body)),
        },
        Expr::Prefix { op, e } => Expr::Prefix {
            op: *op,
            e: Box::new(expr(e)),
        },
        Expr::Havoc => Expr::Havoc,
    }
}

fn steps(s: &[Step]) -> Vec<Step> {
    s.iter().map(step).collect()
}

fn step(s: &Step) -> Step {
    match s {
        Step::Assume(e) => Step::Assume(expr(e)),
        Step::Assert(e) => Step::Assert(expr(e)),
        Step::Assign(r, e) => Step::Assign(relation(r), expr(e)),
        Step::If { cond, then, else_ } => Step::If {
            cond: expr(cond),
            then: steps(then),
            else_: steps(else_),
        },
    }
}

fn transition(t: &Transition) -> Transition {
    Transition {
        name: ident(&t.name),
        bound: t.bound.as_ref().map(|name| ident(name)),
        steps: steps(&t.steps),
    }
}

pub fn clean_system(sys: &System) -> System {
    System {
        transitions: sys.transitions.iter().map(transition).collect(),
        init: steps(&sys.init),
    }
}
