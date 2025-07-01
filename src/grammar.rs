use crate::errors::*;
use crate::symbols;
use std::collections::{HashMap, HashSet};
use std::fmt;

type Result<T> = std::result::Result<T, GrammarError>;

// TODO:
// - [] Make each rule has only one RHS
// - [] Make one non-terminal to a vector of rules

/// Construct represneting a production rule: LHS -> RHS, where
/// LHS should be a non-terminal (usually a sigle uppercase symbol).
/// RHS should be a vector of strs, each of which represnets a possible
/// expansion of LHS. `""` means the LHS can derive an empty string.
#[derive(Debug)]
pub struct Rule<'p> {
    // if the production can derive empty string directly, i.e. A ->
    // it does NOT know indirectly nuallble, i.e. A -> B and B ->
    pub dnull: bool,
    // derive operator such as ->, ::=, :=, =>, etc
    pub dop: &'p str,
    // separator between symbols on RHS and defaults to ""
    pub sep: &'p str,
    // left-hand side
    pub lhs: &'p str,
    // all right-hand sides
    pub rhs: &'p str
}

impl<'p> fmt::Display for Rule<'p> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rhs = if self.rhs.is_empty() { "ε" } else { self.rhs };
        write!(f, "{} {} {}", self.lhs, self.dop, rhs)
    }
}

impl<'r> Rule<'r> {
    pub fn new(
        dnull: bool,
        dop: &'r str,
        sep: &'r str,
        lhs: &'r str,
        rhs: &'r str,
    ) -> Self {
        Self {
            dnull,
            dop,
            sep,
            lhs,
            rhs,
        }
    }

    pub fn is_left_recusrive(&self) -> bool {
        self.rhs.starts_with(self.lhs)
    }

    // Check whether a rule's RHS consists of all non-terminals
    pub fn all_non_terms(&self, nts: &HashSet<&'r str>) -> bool {
        let rhs = self.rhs;
        symbols!(rhs, self.sep).all(|s| nts.contains(s))
    }
}

/// Grammar = (T,N,P,S) where:
/// T: set of terminal symbols
/// N: set of non-terminal symbols
/// P: production rules
/// S: start symbol
/// `nullables` contains only direct nullable non-terminals
pub struct Grammar<'g> {
    pub start_symb: &'g str,
    pub symb_sep: &'g str,
    pub terms: HashSet<&'g str>,
    pub non_terms: HashSet<&'g str>,
    pub nullables: HashSet<&'g str>,
    pub rules: HashMap<&'g str, Vec<Rule<'g>>>,
}

impl<'a> Grammar<'a> {
    // construct a new grammar with the given infomation.
    // each production rule should be in the below format:
    // 1. A -> aBy
    // 2. A -> B
    // 3. A -> y
    // 4. B ->
    // where A, B are non-terminals, y is a terminal and a can be one (non-)terminal
    pub fn new_with_src(
        dop: &'a str,
        start_symb: &'a str,
        symbsep: Option<&'a str>,
        terms: HashSet<&'a str>,
        non_terms: HashSet<&'a str>,
        rules_raw: Vec<&'a str>,
    ) -> Self {
        let mut rules = HashMap::<&str, Vec<Rule>>::new();
        let mut nullables = HashSet::<&str>::new();
        let symb_sep = symbsep.unwrap_or("");
        for s_raw in rules_raw {
            let s = s_raw.trim();
            // abort on any invalid production rule
            assert!(s.len() > 1, "error: invalid production: {s}");
            assert!(
                s.contains(dop),
                "error: unknown derive operator in rule {s}"
            );

            let r: Vec<&str> = s
                .split(dop)
                .filter(|s| !s.is_empty())
                .map(|s| s.trim())
                .collect();
            assert!(
                r.len() >= 1,
                "error: rule must have at least one expansion but got: {}",
                r.len()
            );
            assert!(
                non_terms.contains(r[0]),
                "error: expect non-terminal symbol as LHS but got {}",
                r[0]
            );
            assert!(
                r.iter().all(|s| !s.contains(dop)),
                "error: found unexpected derivation operator: {dop}"
            );

            if r.len() == 1 {
                rules
                    .entry(r[0])
                    .and_modify(|rules| rules.push(Rule::new(true, dop, symb_sep, r[0], "")))
                    .or_insert(vec![Rule::new(true, dop, symb_sep, r[0], "")]);
                nullables.insert(r[0]);
            } else {
                let rest: Vec<&str> = r[1..].to_vec();
                rules
                    .entry(r[0])
                    .and_modify(|rules| rules.push(Rule::new(false, dop, symb_sep, r[0], rest[0])))
                    .or_insert(vec![Rule::new(false, dop, symb_sep, r[0], rest[0])]);
            }
        }
        Self {
            start_symb,
            symb_sep,
            terms,
            non_terms,
            nullables,
            rules,
        }
    }

    // Compute FIRST set for a given string derived from the grammar
    // FIRST(empty) = {empty}
    // FIRST(terminal) = {terminal}
    // For X -> Y1Y2...Yn
    // FIRST(X) = FIRST(Y1) if Y1 cannot derive the empty string
    // FIRST(X) = FIRST(Y1) U FIRST(Y2) U ... FIRST(Yn) if Y1..Yn-1 can
    pub fn first(&self, strs: &'a str) -> Result<HashSet<&'a str>> {
        if strs.is_empty() {
            Ok(HashSet::<&str>::new())
        } else if self.is_terminal(strs) {
            Ok(HashSet::from([strs]))
        } else if self.is_non_terminal(strs) {
            self.first_of_nt(strs)
        } else {
            let mut fset = HashSet::<&str>::new();
            for s in symbols!(strs, self.symb_sep) {
                if self.is_terminal(s) {
                    fset.insert(s);
                    break;
                }
                if self.is_non_terminal(s) {
                    let f = self.first_of_nt(s)?;
                    fset = fset.union(&f).copied().collect();
                    continue;
                }
            }
            Ok(fset)
        }
    }

    // Compute FIRST set for a given non-terminal. See `first` for more details
    fn first_of_nt(&self, nt: &'a str) -> Result<HashSet<&'a str>> {
        let Some(rules) = self.rules.get(nt) else {
            return Err(GrammarError::RuleNotFound(nt.to_string()));
        };
        let mut first_set = HashSet::<&str>::new();
        for r in rules.iter() {
            if r.rhs.is_empty() {
                first_set.insert("");
            } else if self.is_terminal(r.rhs) {
                first_set.insert(r.rhs);
            } else {
                let rhs = r.rhs;
                for s in symbols!(rhs, self.symb_sep) {
                    if self.is_terminal(s) {
                        first_set.insert(s);
                        break;
                    }
                    if self.is_non_terminal(s) {
                        let n = self.first_of_nt(s)?;
                        first_set = first_set.union(&n).copied().collect();
                        if self.nullable(s)? {
                            // FIRST(s) U FIRST(s') where s' is immediately after s
                            continue;
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        Ok(first_set)
    }

    // Compute FOLLOW set for a given non-terminal:
    // A -> XBZ where B is non-terminal and a, c may be terminals or non-terminals
    // FOLLOW(B) = FIRST(Z) if Z cannot derive the empty string or
    // FOLLOW(B) = FIRST(Z) U FOLLOW(A) if Z can derive the empty string
    // Note: FOLLOW sets apply to non-terminals only
    pub fn follow(&self, nt: &'a str) -> Result<HashSet<&'a str>> {
        assert!(
            self.is_non_terminal(nt),
            "error: unknown non-terminal: {nt}"
        );
        let mut follow_set = if nt.eq(self.start_symb) {
            HashSet::from(["$"])
        } else {
            HashSet::<&str>::new()
        };
        for rules in self.rules.values() {
            for rule in rules.iter() {
                if rule.rhs.contains(&nt) {
                    let rhs = rule.rhs;
                    let symbs: Vec<&str> = symbols!(rhs, self.symb_sep).collect();
                    let Some(pos) = symbs.iter().position(|s| s.eq(&nt)) else {
                        return Err(GrammarError::SymbolNotFound(nt.to_string()));
                    };
                    if pos < symbs.len() - 1 {
                        let fsym = symbs[pos + 1];
                        let f = self.first(fsym)?;
                        follow_set = follow_set.union(&f).copied().collect();
                        // if self.is_terminal(fsymb) {
                        //     follow_set.insert(fsymb);
                        // } else if self.is_non_terminal(fsymb) {
                        //     let f = self.first_of_nt(fsymb)?;
                        // }
                    } else if pos == symbs.len() - 1 {
                        if rule.lhs.eq(nt) {
                            break;
                        } else {
                            let f = self.follow(rule.lhs)?;
                            follow_set = follow_set.union(&f).copied().collect();
                        }
                    }
                }
            }
        }
        Ok(follow_set)
    }

    pub fn is_cfg(&self) -> bool {
        self.rules
            .keys()
            .all(|&k| self.non_terms.contains(k) && !self.terms.contains(k))
    }

    pub fn is_terminal(&self, t: &str) -> bool {
        self.terms.contains(t)
    }

    pub fn is_non_terminal(&self, nt: &str) -> bool {
        self.non_terms.contains(nt)
    }

    // A non-terminal can be nullable if it meets any of the below:
    // 1. it can derive the empty string directly: X ->
    // 2. it can derive the empty string indirectly: X -> YZ where YZ all nullable
    pub fn nullable(&self, nt: &str) -> Result<bool> {
        assert!(
            self.is_non_terminal(nt),
            "error: unknown non-terminal: {nt}"
        );
        let Some(rules) = self.rules.get(nt) else {
            return Err(GrammarError::RuleNotFound(nt.to_string()));
        };
        let dnull = rules.iter().any(|r| r.rhs.is_empty());
        let mut indull = false;
        let all_nt_rs: Vec<&Rule> = rules.iter().filter(|r| r.all_non_terms(&self.non_terms)).collect();
        if !all_nt_rs.is_empty() {
            indull = indull
                || all_nt_rs
                .iter()
                .all(|&r| {
                    let rhs = r.rhs;
                    symbols!(rhs, self.symb_sep).all(|s| self.nullables.contains(s))
                })
        }
        Ok(indull || dnull)
    }

    pub fn is_left_recusrive(&self) -> bool {
        self.rules
            .values()
            .any(|rule| rule.iter().any(|r| r.is_left_recusrive()))
    }

    // For a non-terminal with multiple productions:
    // A -> a | b
    // A is said to have NO common prefix if either of the below two conditions met:
    // 1. if nullable(a) but !nullable(b) and FOLLOW(A) U FIRST(b) = empty set
    // 2. if both a and b are not nullable and FIRST(a) U FIRST(b) = empty set
    pub fn has_common_prefix(&self, nt: &str) -> Result<bool> {
        let Some(rules) = self.rules.get(nt) else {
            return Err(GrammarError::RuleNotFound(nt.to_string()));
        };
        if rules.len() == 1 {
            return Ok(false);
        } else if self.nullable(nt)?  {
            let mut fset = self.follow(nt)?;
            for rule in rules.iter().filter(|r| !r.rhs.is_empty()) {
                let rhs = rule.rhs;
                let mut fiter = symbols!(rhs, self.symb_sep).take(1);
                if let Some(s) = fiter.next() {
                    if fset.contains(s) {
                        return Ok(true);
                    } else if self.is_terminal(s) {
                        fset.insert(s);
                    } else {
                        let f = self.first(s)?;
                        if fset.is_disjoint(&f) {
                            fset = fset.union(&f).copied().collect();
                        } else {
                            return Ok(true);
                        }
                    }
                }
            }
        } else {
            let mut fset = HashSet::<&str>::with_capacity(self.terms.len() + self.non_terms.len());
            for rule in rules.iter() {
                let rhs = rule.rhs;
                let mut fiter = symbols!(rhs, self.symb_sep).take(1);
                if let Some(s) = fiter.next() {
                    if fset.contains(s) {
                        return Ok(true);
                    } else if self.is_terminal(s) {
                        fset.insert(s);
                    } else {
                        let f = self.first(s)?;
                        if fset.is_disjoint(&f) {
                            fset = fset.union(&f).copied().collect();
                        } else {
                            return Ok(true);
                        }
                    }
                }
            }
        }
        Ok(false)
    }

    pub fn ambigous_with_common_prefix(&self) -> Result<bool> {
        for key in self.rules.keys() {
            match self.has_common_prefix(key) {
                Ok(true) => return Ok(true),
                Ok(false) => continue,
                Err(e) => return Err(e),
            }
        }
        Ok(false)
    }

    pub fn is_ll1(&self) -> Result<bool> {
        Ok(!self.is_left_recusrive() && !self.ambigous_with_common_prefix()? && true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_cfg1() {
        let s = "S";
        let op = "::=";
        let t = HashSet::from(["b"]);
        let nt = HashSet::from(["S", "A"]);
        let rules = vec!["S ::= A", "A ::= ", "A ::= bbA"];
        let g = Grammar::new_with_src(op, s, None, t, nt, rules);

        assert!(g.is_cfg());
        assert!(g.rules.get("A").is_some());
        assert_eq!(g.rules.get("A").unwrap().len(), 2);
        for rules in g.rules.values() {
            for rule in rules.iter() {
                println!("{}", rule);
            }
        }
    }

    #[test]
    #[should_panic]
    fn test_is_cfg2() {
        let s = "S";
        let op = "->";
        let t = HashSet::from(["b", "c"]);
        let nt = HashSet::from(["S", "A"]);
        let rules = vec!["S -> A", "bA -> c", "A -> bbA"];
        let _g = Grammar::new_with_src(op, s, None, t, nt, rules);
    }

    #[test]
    fn test_first_set() -> Result<()> {
        let s = "S";
        let op = "->";
        let t = HashSet::from(["b", "c", ""]);
        let nt = HashSet::from(["S", "A", "B"]);
        let rules = vec!["S -> A", "A -> ", "A -> Bc", "B -> ", "B -> bb"];
        let g = Grammar::new_with_src(op, s, None, t, nt, rules);
        assert_eq!(g.first("A").unwrap(), HashSet::from(["", "c", "b"]));
        Ok(())
    }

    #[test]
    fn test_follow_set() -> Result<()> {
        let s = "S";
        let op = "->";
        let t = HashSet::from(["b", "c", ""]);
        let nt = HashSet::from(["S", "A", "B"]);
        let rules = vec!["S -> A", "A -> ", "A -> Bc", "B -> ", "B -> bb"];
        let g = Grammar::new_with_src(op, s, None, t, nt, rules);

        // println!("{:?}", g.rules);
        assert_eq!(g.follow("A").unwrap(), HashSet::from(["$"]));
        assert_eq!(g.follow("B").unwrap(), HashSet::from(["c"]));
        assert_eq!(g.nullables, HashSet::from(["A", "B"]));
        Ok(())
    }

    #[test]
    fn test_nullable() -> Result<()> {
        let s = "S";
        let op = "->";
        let t = HashSet::from(["b", "c", "d", ""]);
        let nt = HashSet::from(["S", "A", "B", "C", "D"]);
        let rules = vec![
            "S -> A", "A -> ", "A -> Bc", "B -> CD", "B -> bb", "C -> ", "C -> c", "D -> ",
            "D -> d",
        ];
        let g = Grammar::new_with_src(op, s, None, t, nt, rules);
        assert_eq!(g.nullables, HashSet::from(["A", "C", "D"]));
        assert!(g.nullable("B").unwrap());
        Ok(())
    }

    #[test]
    fn test_has_common_prefix1() -> Result<()> {
        let s = "S";
        let op = "->";
        let t = HashSet::from(["b", "c", "d", ""]);
        let nt = HashSet::from(["S", "A", "B", "C", "D"]);
        let rules = vec![
            "S -> A", "A -> cB", "A -> cD", "B -> CD", "B -> bb", "C -> c", "D -> d",
        ];
        let g = Grammar::new_with_src(op, s, None, t, nt, rules);
        assert!(g.ambigous_with_common_prefix()?);
        Ok(())
    }

    #[test]
    fn test_has_common_prefix2() -> Result<()> {
        let s = "S";
        let op = "->";
        let t = HashSet::from(["b", "c", "d", ""]);
        let nt = HashSet::from(["S", "A", "B", "C", "D"]);
        let rules = vec![
            "S -> A", "A -> Bc", "A -> BD", "B -> ", "B -> bb", "C -> c", "D -> d",
        ];
        let g = Grammar::new_with_src(op, s, None, t, nt, rules);
        assert!(g.ambigous_with_common_prefix()?);
        Ok(())
    }

    #[test]
    fn test_has_common_prefix3() -> Result<()> {
        let s = "S";
        let op = "->";
        let t = HashSet::from(["b", "c", "d", ""]);
        let nt = HashSet::from(["S", "A", "B", "C", "D"]);
        let rules = vec![
            "S -> A", "A -> ", "A -> BD", "B -> ", "B -> bb", "C -> c", "D -> d",
        ];
        let g = Grammar::new_with_src(op, s, None, t, nt, rules);
        assert!(!g.ambigous_with_common_prefix()?);
        Ok(())
    }
}
