use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op, PrattParser},
};

#[derive(pest_derive::Parser)]
#[grammar = "ivy.pest"]
pub struct IvyParser;

#[derive(PartialEq, Eq, Debug)]
pub struct Transition {
    pub name: String,
    /// parameter to the transition (first argument to "action")
    pub bound: Option<String>,
    pub steps: Vec<Step>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum BinOp {
    And,
    Implies,
    Equal,
    Iff,
}

#[derive(PartialEq, Eq, Debug)]
pub enum PrefixOp {
    Not,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expr {
    Relation(Relation),
    Infix {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    Forall {
        bound: String,
        body: Box<Expr>,
    },
    Some {
        bound: String,
        body: Box<Expr>,
    },
    Prefix {
        op: PrefixOp,
        e: Box<Expr>,
    },
    Havoc,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Relation {
    // a nullary relation
    Ident(String),
    // a unary relation
    Call(String, String),
}

type Steps = Vec<Step>;

// TODO: grammar should use step rather than rule for consistency
#[derive(PartialEq, Eq, Debug)]
pub enum Step {
    Assume(Expr),
    Assert(Expr),
    Assign(Relation, Expr),
    If {
        cond: Expr,
        then: Steps,
        else_: Steps,
    },
}

fn parse_ident(ident: Pair<Rule>) -> String {
    ident.as_str().to_string()
}

fn parse_relation(pair: Pair<Rule>) -> Relation {
    match pair.as_rule() {
        Rule::call_expr => {
            let mut pairs = pair.into_inner();
            let f = pairs.next().unwrap();
            let arg = pairs.next().unwrap();
            Relation::Call(parse_ident(f), parse_ident(arg))
        }
        Rule::ident => Relation::Ident(parse_ident(pair)),
        _ => unreachable!(),
    }
}

fn make_pratt() -> PrattParser<Rule> {
    PrattParser::new()
        .op(Op::infix(Rule::and, Assoc::Left))
        .op(Op::infix(Rule::implies, Assoc::Left))
        .op(Op::infix(Rule::iff, Assoc::Left))
        .op(Op::infix(Rule::equal, Assoc::Left))
        .op(Op::prefix(Rule::not))
}

fn parse_base_expr(expr: Pair<Rule>) -> Expr {
    match expr.as_rule() {
        Rule::ident | Rule::call_expr => Expr::Relation(parse_relation(expr)),
        Rule::forall_expr => {
            let mut pairs = expr.into_inner();
            let bound = pairs.next().unwrap();
            let e = pairs.next().unwrap();
            Expr::Forall {
                bound: parse_ident(bound),
                body: Box::new(parse_expr(e)),
            }
        }
        Rule::some_expr => {
            let mut pairs = expr.into_inner();
            let bound = pairs.next().unwrap();
            let e = pairs.next().unwrap();
            Expr::Some {
                bound: parse_ident(bound),
                body: Box::new(parse_expr(e)),
            }
        }
        Rule::havoc_expr => Expr::Havoc,
        Rule::expr => parse_expr(expr),
        _ => unreachable!(),
    }
}

fn parse_expr(expr: Pair<Rule>) -> Expr {
    assert_eq!(expr.as_rule(), Rule::expr);
    let pratt = make_pratt();
    pratt
        .map_primary(parse_base_expr)
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::and => BinOp::And,
                Rule::implies => BinOp::Implies,
                Rule::iff => BinOp::Iff,
                Rule::equal => BinOp::Equal,
                _ => unreachable!(),
            };
            Expr::Infix {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .map_prefix(|op, e| {
            let op = match op.as_rule() {
                Rule::not => PrefixOp::Not,
                _ => unreachable!(),
            };
            Expr::Prefix { op, e: Box::new(e) }
        })
        .parse(expr.into_inner())
}

fn parse_step(step: Pair<Rule>) -> Step {
    assert_eq!(step.as_rule(), Rule::rule_step);
    // step has exactly one inner Pair
    let step = step.into_inner().next().unwrap();
    match step.as_rule() {
        Rule::assign_rule => {
            let mut pairs = step.into_inner();
            let lexpr = pairs.next().unwrap();
            let e = pairs.next().unwrap();
            Step::Assign(parse_relation(lexpr), parse_expr(e))
        }
        Rule::assert_rule => {
            let e = step.into_inner().next().unwrap();
            Step::Assert(parse_expr(e))
        }
        Rule::assume_rule => {
            let e = step.into_inner().next().unwrap();
            Step::Assume(parse_expr(e))
        }
        Rule::if_rule => {
            let mut pairs = step.into_inner();
            let cond = pairs.next().unwrap();
            let then = pairs.next().unwrap();
            let else_ = pairs.next();
            Step::If {
                cond: parse_expr(cond),
                then: parse_steps(then),
                else_: else_.map(parse_steps).unwrap_or_default(),
            }
        }
        _ => unreachable!(),
    }
}

/// Flatten the structure of a sequence of steps. Produces a sequence
///
/// applies to rule_step, and rule_block
fn flatten_steps(pair: Pair<Rule>) -> Vec<Pair<Rule>> {
    match pair.as_rule() {
        Rule::rule_step => vec![pair],
        Rule::rule_block => pair.into_inner().flat_map(flatten_steps).collect(),
        _ => unreachable!(),
    }
}

fn parse_steps(pair: Pair<Rule>) -> Vec<Step> {
    flatten_steps(pair).into_iter().map(parse_step).collect()
}

fn parse_transition(step_def: Pair<Rule>) -> Transition {
    assert_eq!(step_def.as_rule(), Rule::step_def);
    let mut pairs = step_def.into_inner();
    let name = parse_ident(pairs.next().unwrap());
    let action = pairs.next().unwrap();
    let mut action_pairs = action.into_inner();
    let first = action_pairs.next().unwrap();
    let (bound, steps) = if first.as_rule() == Rule::ident {
        (Some(parse_ident(first)), action_pairs.next().unwrap())
    } else {
        (None, first)
    };
    Transition {
        name,
        bound,
        steps: parse_steps(steps),
    }
}

pub fn parse(file: Pair<Rule>) -> Vec<Transition> {
    file.into_inner()
        .flat_map(|pair| {
            if pair.as_rule() == Rule::EOI {
                None
            } else {
                Some(parse_transition(pair))
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{IvyParser, Rule};
    use pest::Parser as _;

    #[test]
    fn ident_tests() {
        IvyParser::parse(Rule::ident, "ext:mutex_protocol.step_atomic_store")
            .expect("unsuccessful ident parse");
        IvyParser::parse(Rule::ident, "fml:t:mutex_protocol.thread")
            .expect("unsuccessful ident parse");
    }

    #[test]
    fn expr_tests() {
        IvyParser::parse(Rule::expr, "forall V0.  l2s_a(V0)").expect("unsuccessful expr parse");
        IvyParser::parse(Rule::expr, "l2s_g_1 -> ~(forall T. mutex_protocol.d(T))")
            .expect("unsuccessful expr parse");
        IvyParser::parse(Rule::forall_expr, "forall T. mutex_protocol.d(T)")
            .expect("unsuccessful forall parse");
        IvyParser::parse(Rule::expr, "forall V0. l2s_g_4(V0) -> ~l2s_g_3(V0)")
            .expect("unsuccessful expr parse");
    }
}
