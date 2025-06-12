
use std::collections::{HashMap, HashSet};


/// Grammar = (T,N,P,S) where:
/// T: set of terminal symbols
/// N: set of non-terminal symbols
/// P: production rules
/// S: start symbol
pub struct Grammar<'a> {
    pub T: HashSet<char>,
    pub N: HashSet<char>,
    pub P: HashMap<&'a str, Vec<&'a str>>, // e.g. S -> aB
    pub S: char,
}

impl<'a> Grammar<'a> {
    // fn () -> Self {
    //
    // }

    pub fn is_cfg(&self) -> bool {

    }

    pub fn is_termianl(&self, t: &char) -> bool {
        self.T.contains(t)
    }

    pub fn is_non_terminal(&self, nt: &char) -> bool {
        self.N.contains(nt)
    }

    pub fn is_left_recusrive(&self) -> bool {
        self.P.iter().find(|(k,v)| v.contains(k)).is_some()
    }


}



#[cfg(test)]
mod tests {
    use super::*;

}
