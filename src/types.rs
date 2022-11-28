use std::collections::{HashMap, HashSet};

use crate::ivy_l2s::{Expr, IfCond, Step, System, Transition};

pub type Type = Vec<String>;

#[derive(Debug, Clone)]
pub struct Types {
    typs: HashMap<String, Type>,
}

fn ident_typ(name: &str) -> Option<(String, Type)> {
    name.split_once(':')
        .map(|(name, typ)| (name.to_string(), vec![typ.to_string()]))
}

impl Types {
    pub fn new() -> Self {
        Self {
            typs: HashMap::new(),
        }
    }

    pub fn find(&self, name: &str) -> Option<&Type> {
        self.typs.get(name)
    }

    pub fn insert(&mut self, name: String, typ: Type) {
        self.typs.insert(name, typ);
    }

    pub fn all_sorts(&self) -> Vec<&String> {
        let mut typs = HashSet::new();
        for ts in self.typs.values() {
            typs.extend(ts.iter());
        }
        let mut sorts = typs.into_iter().collect::<Vec<_>>();
        sorts.sort();
        sorts
    }

    fn with_bound<R, F: FnOnce(&mut Types) -> R>(&mut self, name: &str, typ: Type, f: F) -> R {
        self.insert(name.to_string(), typ);
        let x = f(self);
        self.typs.remove(name);
        return x;
    }

    pub fn infer(&mut self, sys: &System) {
        self.infer_steps(&sys.init);
        for t in &sys.transitions {
            self.infer_transition(t)
        }
    }

    fn infer_transition(&mut self, t: &Transition) {
        if let Some(ident) = &t.bound {
            if let Some((name, typ)) = ident_typ(ident) {
                self.with_bound(&name, typ, |typs| typs.infer_steps(&t.steps));
                return;
            }
        }
        self.infer_steps(&t.steps);
    }

    fn infer_steps(&mut self, steps: &[Step]) {
        for s in steps.iter() {
            self.infer_step(s)
        }
    }

    fn infer_step(&mut self, s: &Step) {
        match s {
            Step::Assume(e) => self.infer_expr(e),
            Step::Assert(e) => self.infer_expr(e),
            Step::Assign(r, e) => {
                if r.args.is_empty() {
                    self.insert(r.name.to_string(), vec![]);
                }
                // TODO: this is really a bit of a hack due to the lack of type
                // variables
                if r.args.len() == 1 {
                    if let Some(typ) = self.find(&r.args[0]) {
                        self.insert(r.name.to_string(), typ.clone());
                    } else {
                        // TODO: another hack, useful for recording that a relation is unary and not nullary
                        self.insert(r.name.to_string(), vec!["?".to_string()]);
                    }
                }
                self.infer_expr(e)
            }
            Step::If { cond, then, else_ } => {
                self.infer_if_cond(cond);
                self.infer_steps(then);
                self.infer_steps(else_);
            }
        }
    }

    fn infer_expr(&mut self, _e: &Expr) {}

    fn infer_if_cond(&mut self, e: &IfCond) {
        match e {
            IfCond::Expr(e) => self.infer_expr(e),
            IfCond::Some { name: _, e } => self.infer_expr(e),
        }
    }
}
