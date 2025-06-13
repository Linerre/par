
use std::collections::{HashMap, HashSet};


/// Grammar = (T,N,P,S) where:
/// T: set of terminal symbols
/// N: set of non-terminal symbols
/// P: production rules
/// S: start symbol
pub struct Grammar<'a> {
    pub terms: HashSet<&'a str>,
    pub non_terms: HashSet<&'a str>,
    pub prods: HashMap<&'a str, Vec<&'a str>>, // e.g. S -> aB
    pub start_symb: &'a str,
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
        let mut prods = HashMap::<&str, Vec<&str>>::new();
        for s in rules {
            let p: Vec<&str> = s.split_ascii_whitespace()
                .filter(|e| !e.contains("->"))
                .collect();

            // FIXME: handle invalid productions rules such as a single non-terminal: "A"
            if p.len() <= 1 {
                prods.entry(p[0])
                    .and_modify(|rhs| rhs.push(""))
                    .or_insert(vec![""]);
            } else {
                let mut rest: Vec<&str> = p[1..].to_vec();
                prods.entry(p[0])
                    .and_modify(|rhs| rhs.append(&mut rest))
                    .or_insert(p[1..].to_vec());
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
        self.prods.iter().find(|(k,v)| v.contains(k)).is_some()
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
        assert!(g.prods.get("A").unwrap().len() == 2);
        assert_eq!(g.prods.get("A"), Some(&["", "bbA"].to_vec()));
    }
}
