use crate::ivy_l2s::{Expr, IfCond, Relation, Step, System, Transition};

/// Clean up names from Ivy output.

struct IdentFn<F: Fn(&str) -> String>(F);

fn ident_clean_prefixes(name: &str) -> &str {
    let name = name.strip_prefix("fml:").unwrap_or(name);
    let name = name.strip_prefix("loc:").unwrap_or(name);
    let name = name.strip_prefix("ext:").unwrap_or(name);
    name
}

fn ident_remove_namespaces(name: &str) -> String {
    let name = ident_clean_prefixes(name);
    // remove any foo. prefix from each : component (sometimes we have
    // x:mutex_protocol.thread, for example, and we want to keep that type
    // annotation)
    name.split(':')
        .map(|part| {
            if let Some((_namespace, name)) = part.split_once('.') {
                name.to_string()
            } else {
                part.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join(":")
}

fn ident_remove_types(name: &str) -> String {
    let name = ident_clean_prefixes(name);
    let name = name
        .split_once(':')
        .map(|(name, _typ)| name)
        .unwrap_or(name);
    name.to_string()
}

impl<F: Fn(&str) -> String> IdentFn<F> {
    fn ident(&self, name: &str) -> String {
        (self.0)(name)
    }

    fn relation(&self, r: &Relation) -> Relation {
        Relation {
            name: self.ident(&r.name),
            args: r.args.iter().map(|arg| self.ident(arg)).collect(),
        }
    }

    fn expr(&self, e: &Expr) -> Expr {
        match e {
            Expr::Relation(r) => Expr::Relation(self.relation(r)),
            Expr::Infix { lhs, op, rhs } => Expr::Infix {
                lhs: Box::new(self.expr(lhs)),
                op: *op,
                rhs: Box::new(self.expr(rhs)),
            },
            Expr::Quantified {
                quantifier: q,
                bound,
                body,
            } => Expr::Quantified {
                quantifier: *q,
                bound: self.ident(bound),
                body: Box::new(self.expr(body)),
            },
            Expr::Prefix { op, e } => Expr::Prefix {
                op: *op,
                e: Box::new(self.expr(e)),
            },
            Expr::Havoc => Expr::Havoc,
        }
    }

    fn if_cond(&self, c: &IfCond) -> IfCond {
        match c {
            IfCond::Expr(e) => IfCond::Expr(self.expr(e)),
            IfCond::Some { name, e } => IfCond::Some {
                name: self.ident(name),
                e: self.expr(e),
            },
        }
    }

    fn steps(&self, s: &[Step]) -> Vec<Step> {
        s.iter().map(|s| self.step(s)).collect()
    }

    fn step(&self, s: &Step) -> Step {
        match s {
            Step::Assume(e) => Step::Assume(self.expr(e)),
            Step::Assert(e) => Step::Assert(self.expr(e)),
            Step::Assign(r, e) => Step::Assign(self.relation(r), self.expr(e)),
            Step::If { cond, then, else_ } => Step::If {
                cond: self.if_cond(cond),
                then: self.steps(then),
                else_: self.steps(else_),
            },
        }
    }

    fn transition(&self, t: &Transition) -> Transition {
        Transition {
            name: self.ident(&t.name),
            bound: t.bound.iter().map(|name| self.ident(name)).collect(),
            steps: self.steps(&t.steps),
        }
    }

    fn system(&self, sys: &System) -> System {
        System {
            transitions: sys.transitions.iter().map(|t| self.transition(t)).collect(),
            init: self.steps(&sys.init),
        }
    }
}

pub fn clean_namespaces(sys: &System) -> System {
    IdentFn(ident_remove_namespaces).system(sys)
}

pub fn clean_types(sys: &System) -> System {
    IdentFn(ident_remove_types).system(sys)
}
