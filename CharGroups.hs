module CharGroups where
import SemiLattice

data CharGroups = EOF
                | Char Char
                | White
                | Num
                | Alpha
                | All
                | None
                deriving (Eq, Show)

instance JSL CharGroups where
    bottom = All
    add x y | x == y = x
    add EOF All = EOF
    add EOF _ = None
    add (Char _) (Char _) = None
    add (Char c) White | c == ' ' || c == '\t' || c == '\n' = Char c
    add (Char c) Num | c >= '0' && c <= '9' = Char c
    add (Char c) Alpha | (c >= 'a' && c <= 'z')
                      || (c >= 'A' && c <= 'Z') = Char c
    add x y = add y x
