use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum GrammarError {
    SymbolNotFound(usize),
    ProdNotFound(String),
}

impl Error for GrammarError {}

impl fmt::Display for GrammarError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GrammarError::SymbolNotFound(i) => {
                write!(f, "error: symbol at {i} of RHS not found")
            },
            GrammarError::ProdNotFound(nt) => {
                write!(f, "error: production rule not found for {nt}")
            }
        }
    }
}
