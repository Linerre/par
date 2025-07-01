//! Parsing tables of the grammar

use crate::grammar::Grammar;

pub struct TableEntry<> {}

pub struct LL1Table<'t> {
    rows: Vec<&'t str>,       // rows contain non-terminals
    cols: Vec<&'t str>,       // cols contain terminals as well as EOF
    entries: Vec<(&'t str, &'t str, &'t str)>
}

impl<'t> LL1Table<'t> {
    pub fn from_grammar(g: &'t Grammar) -> Self {
        let rows: Vec<&str> = g.non_terms.iter().copied().collect();
        let mut cols: Vec<&str> = g.terms.iter().copied().collect();
        cols.push("$");
        // TODO: compute entries for the table
        Self {
            rows,
            cols,
            entries: Vec::new()
        }
    }
}
