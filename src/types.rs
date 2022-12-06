use std::collections::{HashMap, HashSet};

use crate::ivy_l2s::{Expr, IfCond, Step, Sub, Subs, System, Transition};

pub type Type = Vec<String>;

#[derive(Debug, Clone)]
pub struct Types {
    typs: HashMap<String, Type>,
}

fn ident_typ(name: &str) -> Option<(String, Type)> {
    name.split_once(':')
        .map(|(name, typ)| (name.to_string(), vec![typ.to_string()]))
}

fn idents_typ(names: &[String]) -> Option<Type> {
    let mut typs = vec![];
    for i in names {
        if let Some((_, typ)) = ident_typ(i) {
            typs.extend(typ)
        } else {
            return None;
        }
    }
    Some(typs)
}

impl Types {
    pub fn new() -> Self {
        Self {
            typs: HashMap::new(),
        }
    }

    pub fn find(&self, name: &str) -> Option<Type> {
        if let Some((_, typ)) = ident_typ(name) {
            return Some(typ);
        }
        self.typs.get(name).cloned()
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

    pub fn infer(&mut self, subs: &Subs, sys: &System) {
        for sub in subs {
            self.infer_sub(sub);
        }
        self.infer_steps(&sys.init);
        for t in &sys.transitions {
            self.infer_transition(t)
        }
    }

    fn infer_sub(&mut self, sub: &Sub) {
        if let Some(typ) = idents_typ(&sub.binders) {
            self.insert(sub.name.clone(), typ);
        }
    }

    fn infer_transition(&mut self, t: &Transition) {
        let bounds: Vec<_> = t.bound.iter().flat_map(|ident| ident_typ(ident)).collect();
        for (name, typ) in bounds.iter() {
            self.insert(name.clone(), typ.clone())
        }
        self.infer_steps(&t.steps);
        for (name, _) in bounds.iter() {
            self.typs.remove(name);
        }
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
                    self.infer_expr(e);
                    return;
                }
                // TODO: this is really a bit of a hack due to the lack of type
                // variables
                if r.args.len() == 1 {
                    if let Some(typ) = self.find(&r.args[0]) {
                        self.insert(r.name.to_string(), typ);
                        self.infer_expr(e);
                        return;
                    }
                }
                // TODO: hack, useful for recording that a relation's arity
                let args: Vec<String> = (0..r.args.len()).map(|_| "?".to_string()).collect();
                let name = r.name.to_string();
                if !self.typs.contains_key(&name) {
                    self.insert(r.name.to_string(), args);
                }
                self.infer_expr(e);
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
