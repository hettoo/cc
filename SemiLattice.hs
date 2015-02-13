module SemiLattice where

class JSL s where
    add :: s -> s -> s

class JSL s => BJSL s where
    bottom :: s

data SimpleLattice s = Bottom -- underspecification
                     | Val s
                     | Top -- overspecification
                     deriving (Eq, Show)

instance Eq s => JSL (SimpleLattice s) where
    add x Bottom = x
    add Bottom x = x
    add x y = if x == y then x else Top

instance Eq s => BJSL (SimpleLattice s) where
    bottom = Bottom
