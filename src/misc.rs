//! Macros, utils, etc

// return all the symbols in a single, non-multiple-expansion production rule
#[macro_export]
macro_rules! symbols {
    ($rhs:ident) => {
        $rhs.split("").filter(|s| !s.is_empty()).collect()
    };
}
