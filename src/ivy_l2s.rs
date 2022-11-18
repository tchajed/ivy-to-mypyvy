use pest::{
    iterators::{Pair, Pairs},
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
pub enum Expr {
    Ident(String),
    Infix(BinOp, Box<Expr>, Box<Expr>),
    Forall(String, Box<Expr>),
    Some(String, Box<Expr>),
    Havoc,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Relation {
    // a nullary relation
    Ident(String),
    // a unary relation
    Call(String, String),
}

// TODO: grammar should use step rather than rule for consistency
#[derive(PartialEq, Eq, Debug)]
pub enum Step {
    Assume(Expr),
    Assert(Expr),
    Assign(Relation, Expr),
    If(Expr, Box<Step>, Option<Box<Step>>),
}

/// Flatten the structure of a sequence of steps. Produces a sequence
///
/// applies to rule_step, rule_block_or_step, and rule_block
fn flatten_steps(pair: Pair<Rule>) -> Vec<Pair<Rule>> {
    match pair.as_rule() {
        Rule::rule_step => vec![pair],
        Rule::rule_block | Rule::rule_block_or_step => {
            pair.into_inner().flat_map(flatten_steps).collect()
        }
        _ => unreachable!(),
    }
}

fn parse_lexpr(lexpr: Pair<Rule>) -> Relation {
    assert_eq!(lexpr.as_rule(), Rule::lexpr);
    let pair = lexpr.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::call_expr => {
            let pairs = pair.into_inner().collect::<Vec<_>>();
            Relation::Call(pairs[0].to_string(), pairs[1].to_string())
        }
        Rule::ident => Relation::Ident(pair.to_string()),
        _ => unreachable!(),
    }
}

fn parse_expr(expr: Pair<Rule>) -> Expr {
    assert_eq!(expr.as_rule(), Rule::expr);
    todo!()
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
            Step::Assign(parse_lexpr(lexpr), parse_expr(e))
        }
        Rule::assert_rule => {
            let e = step.into_inner().next().unwrap();
            Step::Assert(parse_expr(e))
        }
        Rule::assume_rule => {
            let e = step.into_inner().next().unwrap();
            Step::Assume(parse_expr(e))
        }
        Rule::if_rule => todo!(),
        _ => unreachable!(),
    }
}

fn parse_transition(step_def: Pair<Rule>) -> Transition {
    assert_eq!(step_def.as_rule(), Rule::step_def);
    let mut pairs = step_def.into_inner();
    let name = pairs.next().unwrap().to_string();
    let action = pairs.next().unwrap();
    let mut action_pairs = action.into_inner();
    let first = action_pairs.next().unwrap();
    let (bound, steps) = if first.as_rule() == Rule::ident {
        (Some(first.to_string()), action_pairs.next().unwrap())
    } else {
        (None, first)
    };
    Transition {
        name,
        bound,
        steps: flatten_steps(steps).into_iter().map(parse_step).collect(),
    }
}

#[allow(dead_code)]
fn parse(pairs: Pairs<Rule>) -> Vec<Transition> {
    pairs.map(parse_transition).collect()
}

#[allow(dead_code)]
pub fn pratt() -> PrattParser<Rule> {
    PrattParser::new()
        .op(Op::infix(Rule::and, Assoc::Left))
        .op(Op::infix(Rule::implies, Assoc::Left))
        .op(Op::infix(Rule::iff, Assoc::Left))
        .op(Op::infix(Rule::equal, Assoc::Left))
        .op(Op::prefix(Rule::not))
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
