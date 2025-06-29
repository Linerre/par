mod errors;
pub mod grammar;
pub mod table;
pub mod misc;
// TODO:
// - [x] check if a non-terminal can be nullable
// - [x] compute FIRST set for each non terminal
// - [x] compute FOLLOW set for each non terminal
// - [x] check if a non-terminal can be nullable
// - [x] modularize the project when appropriate
// - [x] detect common prefixes
// - [x] support different derivation operators: ->, ::=, :=
// - [-] add error handling and decide if to make them public
//   - [x] basic custom errors to cover common possible erroneous scenarios
//   - [] intro custom errors to work with IO
// - [] finish checking LL1 grammar
// - [] finish checking LR0 grammar
// - [] finish checking LR1 grammar
// - [] finish checking SLR1 grammar
// - [-] implement custom display
//   - [x] for Production
//   - [] for Grammar (may need to switch to BTreeMap to retain the insersion order)
