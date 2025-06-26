# Parsing Algorithms in Rust
It aims to implement several parsing algorithms for common grammars such as LL(1), LR(0), SLR(1), etc. **Note**: this project is *not* a parser generator.

## Initial motivation
- I want to see these algorithms implemented in real code
- I want to automate the parsing process other than doing it manually
- I want to have a program to draw parsing tables other than doing it manually

## Production Rules
Each production rule consists of **three** parts (separated by space)
1. a non-terminal symbol on the LHS
2. a derivation operator such as `->`, `::=` or `:=`
3. a sequence of symbols on the RHS
   - if LHS derives the empty string, RHS is empty
   - symbols on the RHS by defaults are separated by `""` (empty string)
   - the separator can also be other strings such as `*`, `!`, etc and must be *explicitly* passed as an argument to the grammar constructor.

For example, the following are all valid production rules

``` m
A -> XYZ
B ->
C -> bbH
```

For any non-terminal that has multiple derivations, write each derivation on a separate line as `|` separator has not been supported yet.
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

## Resources
1. [Modern Compiler Implementation in Java](https://www.cs.princeton.edu/~appel/modern/java/) by Appel
Textbook used by the compiler course I took in Semester 2, 2025
2. [COMP8712 Compiler Construction](https://programsandcourses.anu.edu.au/2025/course/comp8712)
3. [Youtube videos](https://www.youtube.com/@VirCodificat) by Vir Condifcat. I came across this Youtuber about after I finished the FIRST, FOLLOW sets and common prefix checking. I have yet to know whether and where the youtuber hosts the source code on GitHub
