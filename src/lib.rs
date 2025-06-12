
use std::collections::{HashMap, HashSet};


/// Grammar = (T,N,P,S) where:
/// T: set of terminal symbols
/// N: set of non-terminal symbols
/// P: production rules
/// S: start symbol
pub struct Grammar<'a> {
    pub T: HashSet<&'a str>,
    pub N: HashSet<&'a str>,
    pub P: HashMap<&'a str, Vec<&'a str>>, // e.g. S -> aB
    pub S: &'a str,
}

impl<'a> Grammar<'a> {
    // fn new_with_src(src: Vec<&str>) -> Self {
    //
    // }

    pub fn is_cfg(&self) -> bool {
        self.P.keys().find(|k| k.len() > 1).is_none()
    }

    pub fn is_termianl(&self, t: &str) -> bool {
        self.T.contains(t)
    }

    pub fn is_non_terminal(&self, nt: &str) -> bool {
        self.N.contains(nt)
    }

    pub fn is_left_recusrive(&self) -> bool {
        self.P.iter().find(|(k,v)| v.contains(k)).is_some()
    }


}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_cfg() {
        let g1 = Grammar {
            T: HashSet::from(["a", "c"]),
            N: HashSet::from(["S", "A", "B"]),
            P: HashMap::from([
                ("S", vec!["A"]),
                ("A", vec!["a", "B"]),
                ("B", vec!["c"])
            ]),
            S: "S"
        };

        let g2 = Grammar {
            T: HashSet::from(["a", "c", "d"]),
            N: HashSet::from(["S", "A", "B"]),
            P: HashMap::from([
                ("S", vec!["A"]),
                ("A", vec!["a", "B"]),
                ("B", vec!["c"]),
                ("dB", vec!["d"])
            ]),
            S: "S"
        };

        assert!(g1.is_cfg());
        assert!(!g2.is_cfg());
    }
}
