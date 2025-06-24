use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum GrammarError {
    SymbolNotFound(String),
    ProdNotFound(String),
}

impl Error for GrammarError {}

impl fmt::Display for GrammarError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GrammarError::SymbolNotFound(nt) => {
                write!(f, "error: non-terminal {nt} found on RHS")
            }
            GrammarError::ProdNotFound(nt) => {
                write!(
                    f,
                    "error: production rule not found for nont-terminal: {nt}"
                )
            }
        }
    }
}
