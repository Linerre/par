use crate::errors::*;
use crate::symbols;
use std::collections::{HashMap, HashSet};
use std::fmt;

type Result<T> = std::result::Result<T, GrammarError>;

/// Construct represneting a production rule: LHS -> RHS, where
/// LHS should be a non-terminal (usually a sigle uppercase symbol).
/// RHS should be a vector of strs, each of which represnets a possible
/// expansion of LHS. `""` means the LHS can derive an empty string.
#[derive(Debug)]
pub struct Production<'p> {
    // if the production can derive empty string directly, i.e. A ->
    // it does NOT know indirectly nuallble, i.e. A -> B and B ->
    pub dnull: bool,
    // derive operator such as ->, ::=, :=, =>, etc
    pub dop: &'p str,
    // left-hand side
    pub lhs: &'p str,
    // all right-hand sides
    pub rhs: Vec<&'p str>,
}

impl<'p> fmt::Display for Production<'p> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rhs = if self.dnull {
            let mut r: Vec<&str> = self
                .rhs
                .iter()
                .filter(|&r| !r.is_empty())
                .copied()
                .collect();
            r.push("ε");
            r
        } else {
            self.rhs.clone()
        };
        write!(f, "{} -> {}", self.lhs, rhs.join(" | "))
    }
}

impl<'p> Production<'p> {
    pub fn new(dnull: bool, dop: &'p str, lhs: &'p str, rhs: Vec<&'p str>) -> Self {
        Self {
            dnull,
            dop,
            lhs,
            rhs,
        }
    }

    pub fn is_left_recusrive(&self) -> bool {
        self.rhs.iter().find(|r| r.starts_with(self.lhs)).is_some()
    }

    pub fn rhs_with_nt(&self, nt: &'p str) -> Vec<&'p str> {
        self.rhs
            .iter()
            .filter(|r| r.contains(&nt))
            .copied()
            .collect()
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
    pub terms: HashSet<&'g str>,
    pub non_terms: HashSet<&'g str>,
    pub nullables: HashSet<&'g str>,
    pub prods: HashMap<&'g str, Production<'g>>,
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
        terms: HashSet<&'a str>,
        non_terms: HashSet<&'a str>,
        rules: Vec<&'a str>,
    ) -> Self {
        let mut prods = HashMap::<&str, Production>::new();
        let mut nullables = HashSet::<&str>::new();
        for s_raw in rules {
            let s = s_raw.trim();
            // abort on any invalid production rule
            assert!(s.len() > 1, "error: invalid production: {s}");
            assert!(
                s.contains(dop),
                "error: unknown derive operator in rule {s}"
            );

            let p: Vec<&str> = s.split(dop).filter(|s| !s.is_empty()).map(|s| s.trim()).collect();
            assert!(
                p.len() >= 1,
                "error: prod must have at least one expansion but got: {}",
                p.len()
            );
            assert!(
                non_terms.contains(p[0]),
                "error: expect non-terminal symbol as LHS but got {}",
                p[0]
            );
            assert!(
                p.iter().all(|s| !s.contains(dop)),
                "error: derivation operator not expected but found"
            );

            if p.len() == 1 {
                prods
                    .entry(p[0])
                    .and_modify(|r| r.rhs.push(""))
                    .or_insert(Production::new(true, dop, p[0], vec![""]));
                nullables.insert(p[0]);
            } else {
                let mut rest: Vec<&str> = p[1..].to_vec();
                prods
                    .entry(p[0])
                    .and_modify(|r| r.rhs.append(&mut rest))
                    .or_insert(Production::new(false, dop, p[0], p[1..].to_vec()));
            }
        }
        Self {
            start_symb,
            terms,
            non_terms,
            nullables,
            prods,
        }
    }

    pub fn first(&self, strs: &'a str) -> Result<HashSet<&'a str>> {
        if strs.is_empty() {
            Ok(HashSet::<&str>::new())
        } else if self.is_terminal(strs) {
            Ok(HashSet::from([strs]))
        } else if self.is_non_terminal(strs) {
            self.first_of_nt(strs)
        } else {
            let mut fset = HashSet::<&str>::new();
            for s in symbols!(strs) {
                if self.is_terminal(s) {
                    fset.insert(s);
                    break;
                }
                if self.is_non_terminal(s) {
                    let f = self.first(s)?;
                    fset = fset.union(&f).copied().collect();
                    continue;
                }
            }
            Ok(fset)
        }
    }
    // Compute FIRST set for a given non-terminal
    // FIRST(empty) = {empty}
    // FIRST(terminal) = {terminal}
    // For X -> Y1Y2...Yn
    // FIRST(X) = FIRST(Y1) if Y1 cannot derive the empty string
    // FIRST(X) = FIRST(Y1) U FIRST(Y2) U ... FIRST(Yn) if Y1..Yn-1 can
    fn first_of_nt(&self, nt: &'a str) -> Result<HashSet<&'a str>> {
        let Some(prod) = self.prods.get(nt) else {
            return Err(GrammarError::ProdNotFound(nt.to_string()));
        };
        let mut first_set = HashSet::<&str>::new();
        for &rhs in prod.rhs.iter() {
            if rhs.is_empty() {
                first_set.insert("");
            } else if self.is_terminal(rhs) {
                first_set.insert(rhs);
            } else {
                for s in symbols!(rhs) {
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

        let ps: Vec<&Production> = self
            .prods
            .values()
            .filter(|p| p.rhs.iter().find(|&r| r.contains(&nt)).is_some())
            .collect();
        for p in ps {
            let rs: Vec<&str> = p.rhs_with_nt(nt);
            for r in rs {
                let symbs: Vec<&str> = symbols!(r).collect();
                let Some(pos) = symbs.iter().position(|s| s.eq(&nt)) else {
                    return Err(GrammarError::SymbolNotFound(nt.to_string()));
                };
                if pos < symbs.len() - 1 {
                    let fsymb = symbs[pos + 1];
                    if self.is_terminal(fsymb) {
                        follow_set.insert(fsymb);
                    } else if self.is_non_terminal(fsymb) {
                        let f = self.first(fsymb)?;
                        follow_set = follow_set.union(&f).copied().collect();
                    }
                } else if pos == symbs.len() - 1 {
                    if p.lhs.eq(nt) {
                        break;
                    } else {
                        let f = self.follow(p.lhs)?;
                        follow_set = follow_set.union(&f).copied().collect();
                    }
                }
            }
        }
        Ok(follow_set)
    }

    pub fn is_cfg(&self) -> bool {
        self.prods
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
    // 1. it can derive the empty string: X ->
    // 2. its RHS can derive the empty string: X -> YZ where YZ all nullable
    pub fn nullable(&self, nt: &str) -> Result<bool> {
        assert!(
            self.is_non_terminal(nt),
            "error: unknown non-terminal: {nt}"
        );
        match self.prods.get(nt) {
            Some(p) => {
                let mut indull = false;
                if !p.dnull {
                    let all_nts: Vec<&str> = p
                        .rhs
                        .iter()
                        .filter(|&r| {
                            r.chars().all(|c| {
                                let mut buf = [0; 4];
                                let symb_str = c.encode_utf8(&mut buf);
                                self.is_non_terminal(symb_str)
                            })
                        })
                        .copied()
                        .collect();
                    if !all_nts.is_empty() {
                        indull = indull
                            || all_nts
                                .iter()
                                .all(|&r| symbols!(r).all(|s| self.nullables.contains(s)))
                    }
                }
                Ok(p.dnull || indull)
            }
            None => Err(GrammarError::ProdNotFound(nt.to_string())),
        }
    }

    pub fn is_left_recusrive(&self) -> bool {
        self.prods
            .values()
            .find(|p| p.is_left_recusrive())
            .is_some()
    }

    // For a non-terminal with multiple productions:
    // A -> a | b
    // A is said to have NO common prefix if either of the below two conditions met:
    // 1. if both a and b are not nullable and FIRST(a) U FIRST(b) = empty set
    // 2. if either nullable(a) but !nullable(b) and FOLLOW(A) U FIRST(b) = empty set
    pub fn has_common_prefix(&self, prod: &Production) -> Result<bool> {
        if prod.rhs.len() == 1 {
            return Ok(false);
        } else if self.nullable(prod.lhs)? {
            let mut fset = self.follow(prod.lhs)?;
            for &r in prod.rhs.iter().filter(|&r| !r.is_empty()) {
                let mut fiter = symbols!(r).take(1);
                if let Some(s) = fiter.next() {
                    if fset.contains(s) {
                        return Ok(true);
                    } else if self.is_terminal(s) {
                        fset.insert(s);
                    } else {
                        fset = self.first(s)?;
                    }
                }
            }
        } else {
            let mut first = HashSet::<&str>::with_capacity(self.terms.len() + self.non_terms.len());
            for &r in prod.rhs.iter() {
                let mut fiter = symbols!(r).take(1);
                if let Some(s) = fiter.next() {
                    if first.contains(s) {
                        return Ok(true);
                    } else if self.is_terminal(s) {
                        first.insert(s);
                    } else {
                        let nt_first = self.first(s)?;
                        if first.is_disjoint(&nt_first) {
                            first = first.union(&nt_first).copied().collect();
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
        for prod in self.prods.values() {
            match self.has_common_prefix(prod) {
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
        let g = Grammar::new_with_src(op, s, t, nt, rules);

        assert!(g.is_cfg());
        assert!(g.prods.get("A").is_some());
        assert!(g.prods.get("A").unwrap().dnull);
        assert_eq!(g.prods.get("A").unwrap().lhs, "A");
        assert_eq!(g.prods.get("A").unwrap().rhs, vec!["", "bbA"]);
        for p in g.prods.values() {
            println!("{}", p);
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
        let g = Grammar::new_with_src(op, s, t, nt, rules);
    }

    #[test]
    fn test_first_set() -> Result<()> {
        let s = "S";
        let op = "->";
        let t = HashSet::from(["b", "c", ""]);
        let nt = HashSet::from(["S", "A", "B"]);
        let rules = vec!["S -> A", "A -> ", "A -> Bc", "B -> ", "B -> bb"];
        let g = Grammar::new_with_src(op, s, t, nt, rules);
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
        let g = Grammar::new_with_src(op, s, t, nt, rules);

        // println!("{:?}", g.prods);
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
        let g = Grammar::new_with_src(op, s, t, nt, rules);
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
        let g = Grammar::new_with_src(op, s, t, nt, rules);
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
        let g = Grammar::new_with_src(op, s, t, nt, rules);
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
        let g = Grammar::new_with_src(op, s, t, nt, rules);
        assert!(!g.ambigous_with_common_prefix()?);
        Ok(())
    }
}
