# Parsing Algorithms in Rust
It aims to implement several parsing algorithms for common grammars such as LL(1), LR(0), SLR(1), etc.

## Production Rules
Each production rule consists of **three** parts (separated by space)
1. a non-terminal symbol on the LHS
2. a derivation operator such as `->`, `::=` or `:=` (only `->` is supported now though)
3. a sequence of symbols on the RHS and nothing on RHS means the production derives the empty string

For example, the following are all valid production rules

``` m
A -> XYZ
B ->
C -> bbH
```

For any non-terminal that can have multiple derivations, write each derivation on a separate line as `|` separator has not been supported yet.
``` m
A -> XYZ
A ->
```

Any production rules that do not meet the above specs are considered *invalid*:

``` m
A->
B ->C
A->B
A -> x | BC
```
