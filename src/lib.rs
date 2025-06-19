use std::collections::{HashMap, HashSet};

// TODO:
// - [x] check if a non-terminal can be nullable
// - [x] compute FIRST set for each non terminal
// - [x] compute FOLLOW set for each non terminal
// - [x] check if a non-terminal can be nullable
// - [] detect common prefixes
// - [] intro custom errors to work with IO
// - [] finish checking LL1 grammar
// - [] finish checking LR0 grammar
// - [] finish checking LR1 grammar
// - [] finish checking SLR1 grammar
// - [] implement custom display for Production
// - [] modularize the project when appropriate

#[macro_export]
macro_rules! symbols {
    ($rhs:ident) => {
        $rhs.split("").filter(|s| !s.is_empty()).collect()
    };
}

/// Construct represneting a production rule: LHS -> RHS, where
/// LHS should be a non-terminal (usually a sigle uppercase symbol).
/// RHS should be a vector of strs, each of which represnets a possible
/// expansion of LHS. `""` means the LHS can derive an empty string.
#[derive(Debug)]
pub struct Production<'p> {
    pub lhs: &'p str,
    pub rhs: Vec<&'p str>,
}

impl<'p> Production<'p> {
    pub fn new(lhs: &'p str, rhs: Vec<&'p str>) -> Self {
        Self { lhs, rhs }
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

    pub fn get_symbols(&self, i: usize) -> Vec<&'p str> {
        if let Some(&rhs) = self.rhs.get(i) {
            let symbs: Vec<&str> = symbols!(rhs);
            symbs
        } else {
            // TODO: use Result once custom error is done
            Vec::<&str>::new()
        }
    }

    // This fn only checks if a production can be directly nullable.
    // It does not check if it is indirectly nullable. For example:
    // 1. A -> (A can directly derive the empty string)
    // 2. A -> XYZ where XYZ are all non-terminals that can derive the empty string
    // Case 1 is directly nullable while case 2 is nullable indirectly.
    pub fn nullable(&self) -> bool {
        self.rhs.iter().any(|&r| r.is_empty())
    }
}

/// Grammar = (T,N,P,S) where:
/// T: set of terminal symbols
/// N: set of non-terminal symbols
/// P: production rules
/// S: start symbol
pub struct Grammar<'g> {
    pub start_symb: &'g str,
    pub terms: HashSet<&'g str>,
    pub non_terms: HashSet<&'g str>,
    pub nullables: HashSet<&'g str>, // only contain direct nullables
    pub prods: HashMap<&'g str, Production<'g>>, // e.g. S -> aB
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
        rules: Vec<&'a str>,
    ) -> Self {
        let mut prods = HashMap::<&str, Production>::new();
        let mut nullables = HashSet::<&str>::new();
        for s in rules {
            let p: Vec<&str> = s
                .split_ascii_whitespace()
                .filter(|e| !e.contains("->"))
                .collect();
            // FIXME: handle invalid productions rules such as a single non-terminal: "A" (when p.len() == 0)
            if p.len() <= 1 {
                prods
                    .entry(p[0])
                    .and_modify(|r| r.rhs.push(""))
                    .or_insert(Production::new(p[0], vec![""]));
                nullables.insert(p[0]);
            } else {
                let mut rest: Vec<&str> = p[1..].to_vec();
                prods
                    .entry(p[0])
                    .and_modify(|r| r.rhs.append(&mut rest))
                    .or_insert(Production::new(p[0], p[1..].to_vec()));
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

    // Compute FIRST set for a given non-terminal
    // FIRST(empty) = {empty}
    // FIRST(terminal) = {terminal}
    // For X -> Y1Y2...Yn
    // FIRST(X) = FIRST(Y1) if Y1 cannot derive the empty string
    // FIRST(X) = FIRST(Y1) U FIRST(Y2) U ... FIRST(Yn) if Y1 can
    pub fn first(&self, nt: &'a str) -> HashSet<&'a str> {
        let mut first_set = HashSet::<&str>::new();
        if let Some(prod) = self.prods.get(nt) {
            for (i, &rhs) in prod.rhs.iter().enumerate() {
                if rhs.is_empty() {
                    first_set.insert("");
                } else if self.is_termianl(rhs) {
                    first_set.insert(rhs);
                } else {
                    let symbs = prod.get_symbols(i);
                    for s in symbs {
                        if self.is_termianl(s) {
                            first_set.insert(s);
                            break;
                        } else if self.is_non_terminal(s) && self.nullable(s) {
                            let n = self.first(s);
                            first_set = first_set.union(&n).copied().collect();
                            // FIRST(s) U FIRST(s') where s' is immediately after s
                            continue;
                        } else {
                            // non-nullable non-terminal
                            let n = self.first(s);
                            first_set = first_set.union(&n).copied().collect();
                            break;
                        }
                    }
                }
            }
        }
        // TODO: when custom errors are introduced, use Result
        first_set
    }

    // Compute FOLLOW set for a given non-terminal:
    // A -> XBZ where B is non-terminal and a, c may be terminals or non-terminals
    // FOLLOW(B) = FIRST(Z) if Z cannot derive the empty string or
    // FOLLOW(B) = FIRST(Z) U FOLLOW(A) if Z can derive the empty string
    pub fn follow(&self, nt: &'a str) -> HashSet<&'a str> {
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
                let symbs: Vec<&str> = symbols!(r);
                match symbs.iter().position(|s| s.eq(&nt)) {
                    Some(pos) => {
                        if pos < symbs.len() - 1 {
                            let fsymb = symbs[pos + 1];
                            if self.is_termianl(fsymb) {
                                follow_set.insert(fsymb);
                            } else if self.is_non_terminal(fsymb) {
                                let f = self.first(fsymb);
                                follow_set = follow_set.union(&f).copied().collect();
                            }
                        } else if pos == symbs.len() - 1 {
                            if p.lhs.eq(nt) {
                                break;
                            } else {
                                let f = self.follow(p.lhs);
                                follow_set = follow_set.union(&f).copied().collect();
                            }
                        }
                    }
                    None => {
                        println!("{nt} not found on RHS");
                    }
                }
            }
        }
        follow_set
    }

    pub fn is_cfg(&self) -> bool {
        self.prods
            .keys()
            .find(|&k| self.terms.contains(k))
            .is_none()
    }

    pub fn is_termianl(&self, t: &str) -> bool {
        self.terms.contains(t)
    }

    pub fn is_non_terminal(&self, nt: &str) -> bool {
        self.non_terms.contains(nt)
    }

    // A non-terminal can be nullable if it meets any of the below:
    // 1. it can derive the empty string: X ->
    // 2. its RHS can derive the empty string: X -> YZ where YZ all nullable
    pub fn nullable(&self, nt: &str) -> bool {
        match self.prods.get(nt) {
            Some(p) => {
                let dnull = self.nullables.contains(nt);
                println!("NT {nt} is directly nullable: {dnull}");
                let mut indull = false;
                if !dnull {
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
                            || all_nts.iter().all(|&r| {
                                let symbs: Vec<&str> = symbols!(r);
                                symbs.iter().all(|&s| self.nullables.contains(s))
                            })
                    }
                }
                dnull || indull
            }
            None => {
                todo!();
            }
        }
    }

    pub fn is_left_recusrive(&self) -> bool {
        self.prods
            .values()
            .find(|p| p.is_left_recusrive())
            .is_some()
    }

    pub fn is_ll1(&self) -> bool {
        // TODO: check for common prefixes
        !self.is_left_recusrive() && true
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
        let rules = vec!["S -> A", "A -> ", "A -> bbA"];
        let g = Grammar::new_with_src(t, nt, s, rules);

        assert!(g.is_cfg());
        assert!(g.prods.get("A").is_some());
        assert_eq!(g.prods.get("A").unwrap().lhs, "A");
        assert_eq!(g.prods.get("A").unwrap().rhs, vec!["", "bbA"]);
    }

    #[test]
    fn test_first_set() {
        let s = "S";
        let t = HashSet::from(["b", "c", ""]);
        let nt = HashSet::from(["S", "A", "B"]);
        let rules = vec!["S -> A", "A -> ", "A -> Bc", "B -> ", "B -> bb"];
        let g = Grammar::new_with_src(t, nt, s, rules);

        assert_eq!(g.first("A"), HashSet::from(["", "c", "b"]));
    }

    #[test]
    fn test_follow_set() {
        let s = "S";
        let t = HashSet::from(["b", "c", ""]);
        let nt = HashSet::from(["S", "A", "B"]);
        let rules = vec!["S -> A", "A -> ", "A -> Bc", "B -> ", "B -> bb"];
        let g = Grammar::new_with_src(t, nt, s, rules);

        // println!("{:?}", g.prods);
        assert_eq!(g.follow("A"), HashSet::from(["$"]));
        assert_eq!(g.follow("B"), HashSet::from(["c"]));
        assert_eq!(g.nullables, HashSet::from(["A", "B"]));
    }

    #[test]
    fn test_nullable() {
        let s = "S";
        let t = HashSet::from(["b", "c", "d", ""]);
        let nt = HashSet::from(["S", "A", "B", "C", "D"]);
        let rules = vec![
            "S -> A", "A -> ", "A -> Bc", "B -> CD", "B -> bb", "C -> ", "C -> c", "D -> ",
            "D -> d",
        ];
        let g = Grammar::new_with_src(t, nt, s, rules);
        assert_eq!(g.nullables, HashSet::from(["A", "C", "D"]));
        assert!(g.nullable("B"));
    }
}
