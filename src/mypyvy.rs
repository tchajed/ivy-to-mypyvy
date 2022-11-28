use std::collections::HashSet;
use std::{collections::HashMap, fmt::Write};

use crate::ivy_l2s::{BinOp, Expr, PrefixOp, Quantifier, Relation, Step, System, Transition};
use crate::names;
use crate::printing::{self, indented, parens};

#[derive(Debug)]
struct SysState {
    // TODO: eventually will want to associate type information here
    havoc_relations: HashSet<String>,
    assigned_relations: HashSet<String>,
}

impl SysState {
    fn new() -> Self {
        Self {
            havoc_relations: HashSet::new(),
            assigned_relations: HashSet::new(),
        }
    }

    fn fresh_name(&self, r: &str) -> String {
        if !self.havoc_relations.contains(r) {
            return r.to_string();
        }
        let mut n = 1;
        loop {
            let new_r = format!("{}__{}", r, n);
            if !self.havoc_relations.contains(&new_r) {
                return new_r;
            }
            n += 1;
        }
    }

    /// Compute the right-hand side of a havoc assignment to r.
    ///
    /// This mutates self to record that a havoc relation was used for this
    /// instance of havoc.
    fn havoc_rel(&mut self, r: &Relation) -> Expr {
        let havoc_name = self.fresh_name(&format!("havoc_{}", r.name));
        let havoc_rel = Relation {
            name: havoc_name.clone(),
            args: r.args.clone(),
        };
        self.havoc_relations.insert(havoc_name);
        Expr::Relation(havoc_rel)
    }

    fn all_relations(&self) -> Vec<&String> {
        let mut rs: Vec<&String> = self.assigned_relations.iter().collect();
        rs.sort();
        let mut havoc_rs: Vec<&String> = self.havoc_relations.iter().collect();
        havoc_rs.sort();
        rs.append(&mut havoc_rs);
        rs
    }
}

struct Relations {
    /// Mapping from relation name to current value, stored as a vector of bound
    /// variables and an expression with those variables (potentially) free.
    values: HashMap<String, (Vec<String>, Expr)>,
    assumes: Vec<Expr>,
}

/// Parallel substitute all args for all vals, for a single identifier.
/// [`subst`] is the usual substitution into an expression.
fn subst_one_ident<S1: AsRef<str>, S2: AsRef<str>>(e: &str, args: &[S1], vals: &[S2]) -> String {
    let maybe_val =
        args.iter()
            .zip(vals.iter())
            .find_map(|(arg, val)| if arg.as_ref() == e { Some(val) } else { None });
    match maybe_val {
        Some(val) => val.as_ref().to_string(),
        None => e.to_string(),
    }
}

/// Substitute a bound variable for a different bound variable.
///
/// This limited form of substitution conforms to the restricted grammar of
/// expressions, where relations are only applied to variables rather than
/// arbitrary expressions.
///
/// TODO: write some tests
fn subst(e: &Expr, args: &[String], vals: &[String]) -> Expr {
    match e {
        Expr::Relation(r) => {
            let name = subst_one_ident(&r.name, args, vals);
            let args = r
                .args
                .iter()
                .map(|a| subst_one_ident(a, args, vals))
                .collect();
            Expr::Relation(Relation { name, args })
        }
        Expr::Infix { lhs, op, rhs } => Expr::Infix {
            lhs: Box::new(subst(lhs, args, vals)),
            op: *op,
            rhs: Box::new(subst(rhs, args, vals)),
        },
        Expr::Quantified {
            quantifier,
            bound,
            body,
        } => {
            // remove bound from args, since it is shadowed
            let args: Vec<String> = args
                .iter()
                .filter(|a| a != &bound)
                .map(|s| s.to_string())
                .collect();
            let body = Box::new(subst(body, &args, vals));
            Expr::Quantified {
                quantifier: *quantifier,
                bound: bound.to_string(),
                body,
            }
        }
        Expr::Prefix { op, e } => Expr::Prefix {
            op: *op,
            e: Box::new(subst(e, args, vals)),
        },
        Expr::Havoc => Expr::Havoc,
    }
}

impl Relations {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
            assumes: vec![],
        }
    }

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
        match self.values.get(&r.name) {
            None => Expr::Relation(r.clone()),
            Some((bound, e)) => subst(e, bound, &r.args),
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
            self.values.insert(r_V.name, (r.args.clone(), e.clone()));
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
                r_V.name,
                // (eval(R(V) & V != v) | (eval(e) & V = v))
                (
                    r_V.args,
                    Expr::or(Expr::and(r_V_eval, V_not_eq), Expr::and(e.clone(), V_eq)),
                ),
            );
        }
    }

    /// Get a list of modified relation names
    fn modified(&self) -> Vec<String> {
        let mut relations = self.values.keys().cloned().collect::<Vec<_>>();
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
                self.assigned_relations.insert(r.name.clone());
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
            for (name, (args, e)) in new_relations.into_iter() {
                let r = Relation { name, args };
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
