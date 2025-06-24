# Parsing Algorithms in Rust
It aims to implement several parsing algorithms for common grammars such as LL(1), LR(0), SLR(1), etc.

Note this is *not* a parser generator. My initial motivation is just that I want to:
- see these algorithms implemented in the real code
- automate the parsing process via a program other than doing it manually
- draw the parsing tables using a program other then doing it manually


## Production Rules
Each production rule consists of **three** parts (separated by space)
1. a non-terminal symbol on the LHS
2. a derivation operator such as `->`, `::=` or `:=` (only `->` is supported now)
3. a sequence of symbols on the RHS and if nothing on the RHS, the LHS can derive the empty string

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

Any production rules that do *not* meet the above specs are considered *invalid*:

``` m
A->
B ->C
A->B
A -> x | BC
```
