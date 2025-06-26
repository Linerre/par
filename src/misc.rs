//! Macros, utils, etc

// return all the symbols in a single, non-multiple-expansion production rule
#[macro_export]
macro_rules! symbols {
    ($rhs:ident,$sep:expr) => {
        // TODO: in the future, split by symbols instead of ""
        $rhs.split($sep).filter(|s| !s.is_empty())
    };
}
