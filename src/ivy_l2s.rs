use std::fmt;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Transition {
    pub name: String,
    /// parameters to the transition (first argument to "action")
    pub bound: Vec<String>,
    pub steps: Vec<Step>,
}

pub struct System {
    pub transitions: Vec<Transition>,
    pub init: Vec<Step>,
    /// (name, e)
    pub invariants: Vec<(String, Expr)>,
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum BinOp {
    And,
    Or,
    Implies,
    Equal,
    NotEqual, // not in Ivy but available in mypyvy
    Iff,
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum PrefixOp {
    Not,
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum Quantifier {
    Forall,
    Exists,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Expr {
    Relation(Relation),
    Infix {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    Quantified {
        quantifier: Quantifier,
        bound: String,
        body: Box<Expr>,
    },
    Prefix {
        op: PrefixOp,
        e: Box<Expr>,
    },
    IfElse {
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    },
    Havoc,
}

impl Expr {
    fn infix(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
        Expr::Infix {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    pub fn and(lhs: Expr, rhs: Expr) -> Expr {
        Expr::infix(BinOp::And, lhs, rhs)
    }

    pub fn implies(lhs: Expr, rhs: Expr) -> Expr {
        Expr::infix(BinOp::Implies, lhs, rhs)
    }

    pub fn or(lhs: Expr, rhs: Expr) -> Expr {
        Expr::infix(BinOp::Or, lhs, rhs)
    }

    pub fn equal(lhs: Expr, rhs: Expr) -> Expr {
        Expr::infix(BinOp::Equal, lhs, rhs)
    }

    pub fn not_equal(lhs: Expr, rhs: Expr) -> Expr {
        Expr::infix(BinOp::NotEqual, lhs, rhs)
    }

    pub fn negate(e: Expr) -> Expr {
        Expr::Prefix {
            op: PrefixOp::Not,
            e: Box::new(e),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, PartialOrd, Ord)]
pub struct Relation {
    pub name: String,
    pub args: Vec<String>,
}

impl fmt::Display for Relation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.args.is_empty() {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}({})", self.name, self.args.join(", "))
        }
    }
}

impl Relation {
    /// Constructor to build a nullary relation.
    pub fn ident(name: String) -> Self {
        Self { name, args: vec![] }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum IfCond {
    Expr(Expr),
    Some { name: String, e: Expr },
}

type Steps = Vec<Step>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Step {
    Assume(Expr),
    Assert(Expr),
    Assign(Relation, Expr),
    If {
        cond: IfCond,
        then: Steps,
        else_: Steps,
    },
}

peg::parser! {
    grammar ivy_parser() for str {
        rule whitespace() = quiet!{[' ' | '\n' | '\t']+}

        rule __ = whitespace()
        rule _ = whitespace()?

        rule ident_start_char()
            = [ 'a'..='z' | 'A'..='Z' | '_' | ':' ]

        rule ident_part()
            = ident_start_char() (ident_start_char() / ['0'..='9'])*

        pub(super) rule ident() -> String
            = s:$(quiet!{ident_part() ++ "."} / expected!("identifier")) { s.to_string() }

        pub(super) rule args() -> Vec<String>
            = args:("(" _ binders:(ident() ** (_ "," _)) _ ")" { binders })?
                { args.unwrap_or_default() }

        rule relation() -> Relation
            = name:ident() _ args:args() { Relation{name, args} }

        rule quantified_expr() -> Expr
            = quantifier:("forall" {Quantifier::Forall} / "exists" {Quantifier::Exists})
                __ bound:ident() _ "." _ e:expr()
              { Expr::Quantified { quantifier, bound, body: Box::new(e) } }

        pub(super) rule expr() -> Expr = precedence!{
            x:(@) _ "->" _ y:@ { Expr::infix(BinOp::Implies, x, y) }
            x:(@) _ "<->" _ y:@ { Expr::infix(BinOp::Iff, x, y) }
            --
            x:(@) _ "|" _ y:@ { Expr::infix(BinOp::Or, x, y) }
            --
            x:(@) _ "&" _ y:@ { Expr::infix(BinOp::And, x, y) }
            --
            "~" _ x:@ { Expr::Prefix{op: PrefixOp::Not, e: Box::new(x)} }
            --
            x:(@) _ "=" _ y:@ { Expr::infix(BinOp::Equal, x, y) }
            --
            e:quantified_expr() { e }
            "*" { Expr::Havoc }
            r:relation() { Expr::Relation(r) }
            "(" _ e:expr() _ ")" { e }
        }

        rule assign() -> Step
            = r:relation() _ ":=" _ e:expr() { Step::Assign(r, e) }

        rule assert() -> Step
            = "assert" _ e:expr() { Step::Assert(e) }

        rule assume() -> Step
            = "assume" _ e:expr() { Step::Assume(e) }

        rule if_cond() -> IfCond
            = ("some" __ name:ident() _ "." _ e:expr() { IfCond::Some{name, e} }) /
              e:expr() { IfCond::Expr(e) }

        rule step_or_block() -> Vec<Step>
            = s:step() { vec![s] } / step_block()

        rule if_step() -> Step
            = "if" __ cond:if_cond() _ then:step_or_block() _
              else_:("else" __ ss:step_or_block() { ss })?
            { Step::If { cond, then, else_: else_.unwrap_or_default() }}

        rule step() -> Step
            = assign() / assert() / assume() / if_step()

        rule step_block() -> Vec<Step>
            = "{" _ ss:(steps() ** (_ ";" _)) _ "}" { ss.concat() }

        pub(super) rule steps() -> Vec<Step>
            = step_block() /
              step() ** (_ ";" _)

        pub(super) rule action_def() -> Transition
            = name:ident() _ "=" _
              "action" _ args:args() _ steps:step_block()
              { Transition { name, bound: args, steps } }

        rule invariant() -> (String, Expr)
            = "invariant" _ "[" _ name:ident() _ "]" _ e:expr()
              { (name, e) }

        rule invariants() -> Vec<(String, Expr)>
            = invs:("while" __ "*" _ invs:(inv:invariant() _ { inv })* { invs })?
              { invs.unwrap_or_default() }

        rule eof() = ![_]

        rule actions() -> Vec<Transition>
            = (a:action_def() _ { a })*

        rule system() -> System
            = "let" _ transitions:actions() _ "in" __
              init:step_block() _
              invariants:invariants()
              { System { transitions, init, invariants } }

        rule file0() -> System
            = _ s:system() _ eof() { s }

        pub rule file() -> System = traced(<file0()>)

        // wrap a rule with tracing support, gated under the trace feature
        rule traced<T>(e: rule<T>) -> T =
            &(input:$([_]*) {
                #[cfg(feature = "trace")]
                println!("[PEG_INPUT_START]\n{}\n[PEG_TRACE_START]", input);
            })
            e:e()? {?
                #[cfg(feature = "trace")]
                println!("[PEG_TRACE_STOP]");
                e.ok_or("")
            }
    }
}

#[cfg(test)]
mod peg_tests {
    use super::ivy_parser::{action_def, args, expr, ident, steps};

    #[test]
    fn test_ident() {
        assert!(ident("a1").is_ok());
        assert!(ident("a.b").is_ok());
        assert!(ident("foo:thread").is_ok());
        assert!(ident("fml:t:mutex_protocol.thread").is_ok());
        assert!(ident("hello there").is_err());
        assert!(ident(".b").is_err());
    }

    #[test]
    fn test_args() {
        assert!(args("(fml:t:mutex_protocol.thread)").is_ok());
        assert!(args("()").is_ok());
        assert!(args("").is_ok());
    }

    #[test]
    fn test_expr() {
        assert!(expr("p&q|r").is_ok());
        assert!(expr("p|r|bar").is_ok());

        assert!(expr("(p|r)&bar").is_ok());
        assert!(expr("(p|r) & bar").is_ok());
        assert!(expr("(p | r)&bar").is_ok());

        let e = expr("p|(r&bar)").unwrap();
        assert_eq!(e, expr("p|r&bar").unwrap());
        assert_eq!(e, expr("p | r & bar").unwrap());

        assert_eq!(expr("p&q&r").unwrap(), expr("(p&q)&r").unwrap());

        assert!(expr("forall x. p(x) & x = y").is_ok())
    }

    #[test]
    fn test_steps() {
        assert!(steps("if p(x) { r(Y) := true } else { g(Y) := x = Y }").is_ok());
        assert!(steps("assume foo; assert bar").is_ok());
    }

    #[test]
    fn test_action_def() {
        assert!(action_def(
"ext:mutex_protocol.step_atomic_store = action(fml:t:mutex_protocol.thread){{l2s_d(fml:t) := true;
    assume l2s_g_1 -> ~(forall T. mutex_protocol.d(T));
    assume forall V0. l2s_g_4(V0) -> ~l2s_g_3(V0);
    assume l2s_g_0 -> ~mutex_protocol.pc_finished(mutex_protocol.t0);
    assume forall V0. l2s_g_3(V0) -> ~mutex_protocol.scheduled(V0);
    assume forall V0. l2s_g_2(V0) -> ~mutex_protocol.pc_finished(V0);
    {{{{assume mutex_protocol.pc_atomic_store(fml:t)};
    {mutex_protocol.pc_atomic_store(fml:t) := false};
    {mutex_protocol.pc_futex_wake(fml:t) := true};
    {mutex_protocol.locked := false};
    {{_old_l2s_g_3(V0) := l2s_g_3(V0);
    l2s_g_3(V0) := *;
    _old_l2s_g_4(V0) := l2s_g_4(V0);
    l2s_g_4(V0) := *;
    assume forall V0. _old_l2s_g_3(V0) -> l2s_g_3(V0);
    assume forall V0. ~_old_l2s_g_3(V0) & ~mutex_protocol.scheduled(V0) -> ~l2s_g_3(V0);
    assume forall V0. _old_l2s_g_4(V0) -> l2s_g_4(V0);
    assume forall V0. ~_old_l2s_g_4(V0) & ~l2s_g_3(V0) -> ~l2s_g_4(V0);
    mutex_protocol.scheduled(T) := T:mutex_protocol.thread = fml:t};
    assume forall V0. l2s_g_3(V0) -> ~mutex_protocol.scheduled(V0);
    assume forall V0. l2s_g_4(V0) -> ~l2s_g_3(V0);
    l2s_w_1(V0) := l2s_w_1(V0) & ~mutex_protocol.scheduled(V0) & ~l2s_g_3(V0)};
    {{_old_l2s_g_3(V0) := l2s_g_3(V0);
    l2s_g_3(V0) := *;
    _old_l2s_g_4(V0) := l2s_g_4(V0);
    l2s_g_4(V0) := *;
    assume forall V0. _old_l2s_g_3(V0) -> l2s_g_3(V0);
    assume forall V0. ~_old_l2s_g_3(V0) & ~mutex_protocol.scheduled(V0) -> ~l2s_g_3(V0);
    assume forall V0. _old_l2s_g_4(V0) -> l2s_g_4(V0);
    assume forall V0. ~_old_l2s_g_4(V0) & ~l2s_g_3(V0) -> ~l2s_g_4(V0);
    mutex_protocol.scheduled(T) := false};
    assume forall V0. l2s_g_3(V0) -> ~mutex_protocol.scheduled(V0);
    assume forall V0. l2s_g_4(V0) -> ~l2s_g_3(V0);
    l2s_w_1(V0) := l2s_w_1(V0) & ~mutex_protocol.scheduled(V0) & ~l2s_g_3(V0)}}}};
    l2s_d(mutex_protocol.t0) := true}}").is_ok())
    }
}

pub fn parse(s: &str) -> Result<System, String> {
    ivy_parser::file(s).map_err(|err| format!("{err}"))
}
