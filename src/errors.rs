use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum GrammarError {
    RHSMissing,
}

impl Error for GrammarError {}

impl fmt::Display for GrammarError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GrammarError::RHSMissing => {
                write!(f, "error: Production RHS is missing")
            }
            // _ => todo!()
        }
    }
}
