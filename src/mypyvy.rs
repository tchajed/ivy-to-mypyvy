use std::collections::HashSet;
use std::{collections::HashMap, fmt::Write};

use crate::ivy_l2s::{BinOp, Expr, PrefixOp, Quantifier, Relation, Step, System, Transition};
use crate::names;
use crate::printing::{self, indented, parens};

#[derive(Debug)]
struct SysState {
    havoc_num: usize,
    havoc_relations: Vec<Relation>,
    assigned_relations: HashSet<Relation>,
}

impl SysState {
    fn new() -> Self {
        Self {
            havoc_num: 0,
            havoc_relations: vec![],
            assigned_relations: HashSet::new(),
        }
    }

    /// Compute the right-hand side of a havoc assignment to r.
    ///
    /// This mutates self to record that a havoc relation was used for this
    /// instance of havoc.
    fn havoc_rel(&mut self, r: &Relation) -> Expr {
        let name = format!("havoc_{}_{}", r.name, self.havoc_num);
        self.havoc_num += 1;
        let havoc_rel = Relation {
            name,
            args: r.args.clone(),
        };
        self.havoc_relations.push(havoc_rel.clone());
        Expr::Relation(havoc_rel)
    }

    fn all_relations(&self) -> Vec<&Relation> {
        let mut rs: Vec<&Relation> = self.assigned_relations.iter().collect();
        rs.sort();
        let mut havoc_rs: Vec<&Relation> = self.havoc_relations.iter().collect();
        havoc_rs.sort();
        rs.append(&mut havoc_rs);
        rs
    }
}

struct Relations {
    values: HashMap<Relation, Expr>,
    assumes: Vec<Expr>,
}

impl Relations {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
            assumes: vec![],
        }
    }

    // TODO: this is wrong; we need to store the base relation name without
    // arguments and then look that up. Capitalizing is only useful for
    // identifying if something is already expressed as a forall.
    fn to_universal(r: &Relation) -> Relation {
        Relation {
            name: r.name.clone(),
            args: r
                .args
                .iter()
                .map(|arg| {
                    if let Some((base, typ)) = arg.split_once(':') {
                        format!("{}:{typ}", base.to_uppercase())
                    } else {
                        arg.to_uppercase()
                    }
                })
                .collect(),
        }
    }

    fn arg(r: &Relation) -> String {
        if r.args.len() == 1 {
            return r.args[0].clone();
        }
        panic!("attempt to get arg of non-unary relation");
    }

    fn get(&self, r: &Relation) -> Expr {
        let universal = Relations::to_universal(r);
        match self.values.get(r) {
            None => Expr::Relation(r.clone()),
            Some(e) => {
                if r != &universal {
                    unimplemented!("substitution into universal expressions is not yet supported");
                }
                e.clone()
            }
        }
    }

    fn eval(&self, e: &Expr) -> Expr {
        match e {
            Expr::Relation(r) => self.get(r),
            Expr::Infix { lhs, op, rhs } => Expr::Infix {
                lhs: Box::new(self.eval(lhs)),
                op: *op,
                rhs: Box::new(self.eval(rhs)),
            },
            Expr::Quantified {
                quantifier,
                bound,
                body,
            } => Expr::Quantified {
                quantifier: *quantifier,
                bound: bound.clone(),
                body: Box::new(self.eval(body)),
            },
            Expr::Prefix { op, e } => Expr::Prefix {
                op: *op,
                e: Box::new(self.eval(e)),
            },
            Expr::Havoc => Expr::Havoc,
        }
    }

    /// Record an assignment to a relation.
    ///
    /// Assumes e is already evaluated.
    #[allow(non_snake_case)]
    fn insert(&mut self, r: &Relation, e: &Expr) {
        let r_V = Relations::to_universal(r);

        if r == &r_V {
            self.values.insert(r_V, e.clone());
        } else {
            // r has at least one lower-case (specialized) argument
            //
            // TODO: need to generalize to more than one argument, where some
            // might be universal and others might be specialized
            // rename r to r_v reduce confusion
            let r_v = r.clone();
            let v = Relation::ident(Relations::arg(&r_v));
            let V = Relation::ident(Relations::arg(&r_V));
            // r is a relation at a particular value; need to manually construct an
            // expression for a mutation at a particular value
            let V_not_eq = Expr::not_equal(Expr::Relation(V.clone()), Expr::Relation(v.clone()));
            let V_eq = Expr::equal(Expr::Relation(V), Expr::Relation(v));
            let r_V_eval = self.eval(&Expr::Relation(r_V.clone()));
            self.values.insert(
                r_V,
                // (eval(R(V) & V != v) | (eval(e) & V = v))
                Expr::or(Expr::and(r_V_eval, V_not_eq), Expr::and(e.clone(), V_eq)),
            );
        }
    }

    /// Get a list of modified relation names
    fn modified(&self) -> Vec<String> {
        let mut relations = self
            .values
            .keys()
            .map(|r| r.name.clone())
            .collect::<Vec<_>>();
        relations.sort();
        relations
    }
}

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
        PrefixOp::Not => "!",
    }
}

fn quantifier(q: &Quantifier) -> &'static str {
    match q {
        Quantifier::Forall => "forall",
        Quantifier::Some => "exists",
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
        } => format!("({} {bound}. {})", quantifier(q), expr(body)),
        Expr::Prefix { op, e } => format!("{}{}", prefix_op(op), parens(&expr(e))),
        Expr::Havoc => "*".to_string(),
    }
}

impl SysState {
    fn add_step_path(&mut self, rs: &mut Relations, path_cond: Option<&Expr>, s: &Step) {
        match s {
            Step::Assume(e) => {
                let e = match path_cond {
                    Some(cond) => Expr::implies(cond.clone(), rs.eval(e)),
                    None => rs.eval(e),
                };
                rs.assumes.push(e)
            }
            Step::Assert(e) => eprintln!("  # unhandled assert {}", expr(&rs.eval(e))),
            Step::Assign(r, e) => {
                self.assigned_relations.insert(r.clone());
                let e = if e == &Expr::Havoc {
                    self.havoc_rel(r)
                } else {
                    rs.eval(e)
                };

                let path_e = match path_cond {
                    Some(cond) => Expr::or(
                        Expr::and(cond.clone(), e),
                        Expr::and(Expr::negate(cond.clone()), Expr::Relation(r.clone())),
                    ),
                    None => e,
                };
                rs.insert(r, &path_e);
            }
            Step::If { cond, then, else_ } => {
                let cond = if cond == &Expr::Havoc {
                    self.havoc_rel(&Relation::ident("path".to_string()))
                } else {
                    rs.eval(cond)
                };
                let (then_cond, else_cond) = (cond.clone(), Expr::negate(cond));
                let (then_cond, else_cond) = match path_cond {
                    Some(c) => (
                        Expr::and(c.clone(), then_cond),
                        Expr::and(c.clone(), else_cond),
                    ),
                    None => (then_cond, else_cond),
                };
                for s in then.iter() {
                    self.add_step_path(rs, Some(&then_cond), s);
                }
                for s in else_.iter() {
                    self.add_step_path(rs, Some(&else_cond), s);
                }
            }
        }
    }

    fn add_step(&mut self, rs: &mut Relations, s: &Step) {
        self.add_step_path(rs, None, s);
    }

    fn transition(&mut self, t: &Transition) -> String {
        printing::with_buf(|w| {
            let args = match &t.bound {
                Some(arg) => format!("({})", arg),
                None => "".to_string(),
            };
            writeln!(w, "transition {}{}", t.name, args)?;

            let w = &mut indented(w);
            let mut rs = Relations::new();
            for s in &t.steps {
                self.add_step(&mut rs, s);
            }

            writeln!(w, "modifies {}", rs.modified().join(", "))?;
            writeln!(w, "# assumes:")?;
            for e in rs.assumes.into_iter() {
                writeln!(w, "& {}", parens(&expr(&e)))?;
            }
            writeln!(w, "# transitions:")?;
            // print these in sorted order so output is stable
            let mut new_relations = rs.values.into_iter().collect::<Vec<_>>();
            new_relations.sort_by_key(|(k, _)| k.clone());
            for (r, e) in new_relations.into_iter() {
                let conjunct = format!("new({}) <-> {}", relation(&r), expr(&e));
                writeln!(w, "& ({conjunct})")?;
            }
            Ok(())
        })
    }
}

fn init_step(step: &Step) -> String {
    match step {
        Step::Assume(e) => format!("init {}", parens(&expr(e))),
        Step::Assert(_) => panic!("unexpected `assert` in init"),
        Step::Assign(r, e) => {
            format!("init {} <-> {}", relation(r), parens(&expr(e)))
        }
        Step::If { .. } => unimplemented!("unhandled `if` in init"),
    }
}

pub fn fmt_system(sys: &System) -> String {
    let sys = names::clean_system(sys);
    let mut state = SysState::new();

    let transitions: Vec<_> = sys
        .transitions
        .iter()
        .map(|t| state.transition(t))
        .collect();

    printing::with_buf(|w| {
        for r in state.all_relations() {
            // TODO: not quite right, needs to have arguments that are the
            // correct types
            writeln!(w, "mutable relation {}", r)?;
        }
        writeln!(w)?;

        for s in sys.init.iter() {
            writeln!(w, "{}", init_step(s))?;
        }
        writeln!(w)?;

        for t in transitions.iter() {
            writeln!(w, "{}", t)?;
        }
        Ok(())
    })
}
