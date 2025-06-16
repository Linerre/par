
use std::collections::{HashMap, HashSet};


/// Construct that represnets a production rule: LHS -> RHS, where
/// LHS should be a non-terminal (usually a sigle uppercase symbol),
/// RHS should be a vector of strs, each of which represnets a possible
/// expansion of LHS. Empty str means the LHS can derive empty string.
pub struct Production<'p> {
    pub lhs: &'p str,
    pub rhs: Vec<&'p str>
}

impl<'p> Production<'p> {
    pub fn new(lhs: &'p str, rhs: Vec<&'p str>) -> Self {
        Self { lhs, rhs }
    }

    // When a non-terminal can expand to several possibilities:
    // A ->
    // A -> aBy
    // A -> c
    // also written as A -> aBy | c |
    pub fn extend_rhs(&mut self, new_rhs: &'p str) {
        self.rhs.push(new_rhs);
    }

    pub fn is_left_recusrive(&self) -> bool {
        self.rhs.iter().find(|r| r.starts_with(self.lhs)).is_some()
    }
}


/// Grammar = (T,N,P,S) where:
/// T: set of terminal symbols
/// N: set of non-terminal symbols
/// P: production rules
/// S: start symbol
pub struct Grammar<'g> {
    pub terms: HashSet<&'g str>,
    pub non_terms: HashSet<&'g str>,
    pub prods: HashMap<&'g str, Production<'g>>, // e.g. S -> aB
    pub start_symb: &'g str,
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
        terms: HashSet<&'a str>,
        non_terms: HashSet<&'a str>,
        start_symb: &'a str,
        rules: Vec<&'a str>
    ) -> Self {
        let mut prods = HashMap::<&str, Production>::new();
        for s in rules {
            let p: Vec<&str> = s.split_ascii_whitespace()
                .filter(|e| !e.contains("->"))
                .collect();

            // FIXME: handle invalid productions rules such as a single non-terminal: "A"
            if p.len() <= 1 {
                prods.entry(p[0])
                    .and_modify(|r| r.rhs.push(""))
                    .or_insert(Production::new(p[0], vec![""]));
            } else {
                let mut rest: Vec<&str> = p[1..].to_vec();
                prods.entry(p[0])
                    .and_modify(|r| r.rhs.append(&mut rest))
                    .or_insert(Production::new(p[0], p[1..].to_vec()));
            }
        }
        Self {
            terms,
            non_terms,
            prods,
            start_symb,
        }
    }

    pub fn is_cfg(&self) -> bool {
        self.prods.keys().find(|k| k.len() > 1).is_none()
    }

    pub fn is_termianl(&self, t: &str) -> bool {
        self.terms.contains(t)
    }

    pub fn is_non_terminal(&self, nt: &str) -> bool {
        self.non_terms.contains(nt)
    }

    pub fn is_left_recusrive(&self) -> bool {
        self.prods.values().find(|p| p.is_left_recusrive()).is_some()
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_cfg() {
        let s = "S";
        let t = HashSet::from(["b"]);
        let nt = HashSet::from(["S", "A"]);
        let rules = vec![
            "S -> A",
            "A -> ",
            "A -> bbA"
        ];
        let g = Grammar::new_with_src(t, nt, s, rules);

        assert!(g.is_cfg());
        assert!(g.prods.get("A").is_some());
        // assert!(g.prods.get("A").unwrap().len() == 2);
        assert_eq!(g.prods.get("A").unwrap().lhs, "A");
        assert_eq!(g.prods.get("A").unwrap().rhs, vec!["", "bbA"]);
    }
}
