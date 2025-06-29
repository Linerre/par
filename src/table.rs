//! Parsing tables of the grammar


pub struct TableEntry<> {}

pub struct LL1Table<'t> {
    rows: Vec<&'t str>,       // rows contain non-terminals
    cols: Vec<&'t str>,       // cols contain terminals as well as EOF
    entries: Vec<(&'t str, &'t str, &'t str)>
}
