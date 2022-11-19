use std::{
    collections::{HashMap, HashSet},
    io,
};

use crate::{
    ivy_l2s::{BinOp, Expr, PrefixOp, Relation, Step, Transition, Transitions},
    names,
};

struct Relations<'a> {
    values: HashMap<Relation, Expr>,
    assumes: Vec<Expr>,
    havoc_relations: HashSet<Relation>,
    havoc_num: &'a mut usize,
}

impl<'a> Relations<'a> {
    fn new(havoc_num: &'a mut usize) -> Self {
        Self {
            values: HashMap::new(),
            assumes: vec![],
            havoc_relations: HashSet::new(),
            havoc_num,
        }
    }

    fn to_universal(r: &Relation) -> Relation {
        if let Relation::Call(f, arg) = r {
            let upper_arg = if let Some((base, typ)) = arg.split_once(':') {
                format!("{}:{typ}", base.to_uppercase())
            } else {
                arg.to_uppercase()
            };
            Relation::Call(f.clone(), upper_arg)
        } else {
            r.clone()
        }
    }

    fn arg(r: &Relation) -> String {
        match r {
            Relation::Call(_, arg) => arg.clone(),
            _ => panic!("attempt to get arg of non-call relation"),
        }
    }

    fn base(r: &Relation) -> String {
        match r {
            Relation::Ident(name) => name.to_string(),
            Relation::Call(f, _) => f.to_string(),
        }
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
            Expr::Forall { bound, body } => Expr::Forall {
                bound: bound.clone(),
                body: Box::new(self.eval(body)),
            },
            Expr::Some { bound, body } => Expr::Some {
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

    /// Compute the right-hand side of a havoc assignment to r.
    ///
    /// This mutates self to record that a havoc relation was used for this
    /// instance of havoc.
    fn havoc_rel(&mut self, r: &Relation) -> Expr {
        let name = format!("havoc_{}_{}", Relations::base(r), self.havoc_num);
        *self.havoc_num += 1;
        let havoc_rel = match r {
            Relation::Ident(_) => Relation::Ident(name),
            Relation::Call(_, arg) => Relation::Call(name, arg.clone()),
        };
        self.havoc_relations.insert(havoc_rel.clone());
        Expr::Relation(havoc_rel)
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
            // rename r to r_v reduce confusion
            let r_v = r.clone();
            let v = Relation::Ident(Relations::arg(&r_v));
            let V = Relation::Ident(Relations::arg(&r_V));
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

    /// Get a list of modified relations (without arguments, for unary relations).
    fn modified(&self) -> Vec<String> {
        self.values.keys().map(Relations::base).collect()
    }
}

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
        PrefixOp::Not => "!",
    }
}

/// Determine if s is already unambiguously parenthesized.
///
/// Returns true if s has no spaces (a special case), or if it starts with a (,
/// ends with a ), and these two are matching parentheses.
fn parenthesized(s: &str) -> bool {
    if !s.contains(' ') {
        return true;
    }
    // make sure we start at depth 1 from the beginning
    if !s.starts_with('(') {
        return false;
    }
    let mut depth = 1;
    for c in s[1..].chars() {
        // some situation like (...) ...
        // so trailing stuff needs to be parenthesized
        if depth == 0 {
            return false;
        }
        if c == '(' {
            depth += 1;
        }
        if c == ')' {
            assert!(depth > 0, "produced imbalanced parens: {s}");
            depth -= 1;
        }
    }
    assert_eq!(depth, 0, "produced inbalanced parens: {s}");
    return true;
}

fn parens(s: &str) -> String {
    if parenthesized(s) {
        s.to_string()
    } else {
        format!("({s})")
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
        Expr::Forall { bound, body } => format!("(forall {bound}. {})", expr(body)),
        Expr::Some { bound, body } => format!("(exists {bound}. {})", expr(body)),
        Expr::Prefix { op, e } => format!("{}{}", prefix_op(op), parens(&expr(e))),
        Expr::Havoc => "*".to_string(),
    }
}

impl<'a> Relations<'a> {
    fn step(&mut self, path_cond: Option<&Expr>, s: &Step) {
        match s {
            Step::Assume(e) => {
                let e = match path_cond {
                    Some(cond) => Expr::implies(cond.clone(), self.eval(e)),
                    None => self.eval(e),
                };
                self.assumes.push(e)
            }
            Step::Assert(e) => eprintln!("  # unhandled assert {}", expr(&self.eval(e))),
            Step::Assign(r, e) => {
                let e = if e == &Expr::Havoc {
                    self.havoc_rel(r)
                } else {
                    self.eval(e)
                };

                let path_e = match path_cond {
                    Some(cond) => Expr::or(
                        Expr::and(cond.clone(), e),
                        Expr::and(Expr::negate(cond.clone()), Expr::Relation(r.clone())),
                    ),
                    None => e,
                };
                self.insert(r, &path_e);
            }
            Step::If { cond, then, else_ } => {
                let cond = if cond == &Expr::Havoc {
                    self.havoc_rel(&Relation::Ident("path".to_string()))
                } else {
                    self.eval(cond)
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
                    self.step(Some(&then_cond), s);
                }
                for s in else_.iter() {
                    self.step(Some(&else_cond), s);
                }
            }
        }
    }
}

fn transition(w: &mut impl io::Write, havoc_num: &mut usize, t: &Transition) -> io::Result<()> {
    let args = match &t.bound {
        Some(arg) => format!("({})", arg),
        None => "".to_string(),
    };
    writeln!(w, "transition {}{}", t.name, args)?;

    let mut rs = Relations::new(havoc_num);
    for s in &t.steps {
        rs.step(None, s);
    }
    writeln!(w, "  modifies {}", rs.modified().join(", "))?;
    writeln!(w, "  # assumes:")?;
    for e in rs.assumes.into_iter() {
        writeln!(w, "  {} &", parens(&expr(&e)))?;
    }
    writeln!(w, "  # transitions:")?;
    for (r, e) in rs.values.into_iter() {
        let conjunct = format!("new({}) <-> {}", relation(&r), expr(&e));
        writeln!(w, "  ({conjunct}) &")?;
    }
    // need to terminate the list of & conjuncts
    writeln!(w, "  true")?;
    Ok(())
}

pub fn emit_transitions(w: &mut impl io::Write, ts: &Transitions) -> io::Result<()> {
    let ts = names::clean_transitions(ts);
    let mut havoc_num = 0;
    for t in ts.into_iter() {
        transition(w, &mut havoc_num, &t)?;
        writeln!(w)?;
    }
    Ok(())
}
