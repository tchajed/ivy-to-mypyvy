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

pub fn parens(s: &str) -> String {
    if parenthesized(s) {
        s.to_string()
    } else {
        format!("({s})")
    }
}

#[cfg(test)]
mod tests {
    use super::parenthesized;

    #[test]
    fn test_parenthesized() {
        assert!(parenthesized("nospaces"));
        assert!(!parenthesized("has spaces"));
        assert!(parenthesized("(already wrapped)"));
        assert!(!parenthesized("(foo) (bar)"));
    }

    #[test]
    #[should_panic(expected = "imbalanced")]
    fn test_parenthesized_imbalanced() {
        parenthesized("(imbalanced ()paren");
    }
}
