module SemiLattice where

class SemiLattice s where
    bottom :: s
    add :: s -> s -> s

data SimpleLattice s = Bottom -- underspecification
                     | Val s
                     | Top -- overspecification
                     deriving (Eq, Show)

instance SemiLattice (SimpleLattice s) where
    bottom = Bottom
    add x Bottom = x
    add Bottom x = x
    add _ _ = Top
