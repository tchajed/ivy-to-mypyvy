use std::{
    collections::{HashMap, HashSet},
    io,
};

use crate::ivy_l2s::{BinOp, Expr, PrefixOp, Relation, Step, Transition, Transitions};

struct Relations<'a> {
    values: HashMap<Relation, Expr>,
    havoc_relations: HashSet<Relation>,
    havoc_num: &'a mut usize,
}

impl<'a> Relations<'a> {
    fn new(havoc_num: &'a mut usize) -> Self {
        Self {
            values: HashMap::new(),
            havoc_relations: HashSet::new(),
            havoc_num,
        }
    }

    fn to_universal(r: &Relation) -> Relation {
        if let Relation::Call(f, arg) = r {
            let upper_arg = match arg.rfind(&[':', '.']) {
                None => arg.to_uppercase(),
                Some(i) => {
                    let (prefix, base) = arg.split_at(i);
                    format!("{prefix}{}", base.to_uppercase())
                }
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

    #[allow(non_snake_case)]
    fn insert(&mut self, r: &Relation, e: &Expr) {
        let r_V = Relations::to_universal(r);

        let e = if e == &Expr::Havoc {
            let name = format!("havoc_{}", self.havoc_num);
            *self.havoc_num += 1;
            let havoc_rel = match r {
                Relation::Ident(_) => Relation::Ident(name),
                Relation::Call(_, arg) => Relation::Call(name, arg.clone()),
            };
            // NOTE: we track these in case we want to associate some metadata
            // later, or change the naming scheme; for now havoc_num tells us
            // exactly which relations were created
            self.havoc_relations.insert(havoc_rel.clone());
            Expr::Relation(havoc_rel)
        } else {
            self.eval(e)
        };

        if r == &r_V {
            self.values.insert(r_V, e);
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
                Expr::or(Expr::and(r_V_eval, V_not_eq), Expr::and(e, V_eq)),
            );
        }
    }
}

fn relation(r: &Relation) -> String {
    match r {
        Relation::Ident(arg) => arg.to_string(),
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
        Expr::Infix { lhs, op, rhs } => format!("({} {} {})", expr(lhs), bin_op(op), expr(rhs)),
        Expr::Forall { bound, body } => format!("(forall {bound}. {})", expr(body)),
        Expr::Some { bound, body } => format!("(exists {bound}. {})", expr(body)),
        Expr::Prefix { op, e } => format!("{}{}", prefix_op(op), parens(&expr(e))),
        Expr::Havoc => "*".to_string(),
    }
}

fn step(w: &mut impl io::Write, rs: &mut Relations, s: &Step) -> io::Result<()> {
    match s {
        Step::Assume(e) => {
            writeln!(w, "  {} &", expr(&rs.eval(e)))?;
        }
        Step::Assert(e) => writeln!(w, "  # unhandled assert {}", expr(e))?,
        Step::Assign(r, e) => rs.insert(r, e),
        Step::If { cond, then, else_ } => {
            writeln!(w, "  # unhandled if {}", expr(cond))?;
            // TODO: need to preserve the path condition of the if and modify
            // all the assume/assigns in these blocks
            for s in then.iter() {
                step(w, rs, s)?;
            }
            for s in else_.iter() {
                step(w, rs, s)?;
            }
        }
    }
    Ok(())
}

fn transition(w: &mut impl io::Write, havoc_num: &mut usize, t: &Transition) -> io::Result<()> {
    let args = match &t.bound {
        // TODO: arg actually has the type here
        Some(arg) => format!("({}: ???)", arg),
        None => "".to_string(),
    };
    writeln!(w, "transition {}{}", t.name, args)?;

    let mut rs = Relations::new(havoc_num);
    writeln!(w, "  # assumes:")?;
    for s in &t.steps {
        step(w, &mut rs, s)?;
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

pub fn transitions(w: &mut impl io::Write, ts: &Transitions) -> io::Result<()> {
    let mut havoc_num = 0;
    for t in ts.iter() {
        transition(w, &mut havoc_num, t)?;
        writeln!(w)?;
    }
    Ok(())
}
