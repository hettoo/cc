module SemiLattice where

class SemiLattice s where
    top :: s
    meet :: s -> s -> s

data SimpleLattice s = Top -- underspecification
                     | Val s
                     | Bottom -- overspecification

instance SemiLattice (SimpleLattice s) where
    top = Top
    meet x Top = x
    meet Top x = x
    meet _ _ = Bottom
