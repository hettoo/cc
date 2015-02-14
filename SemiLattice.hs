module SemiLattice where

class JSL s where
    bottom :: s
    add :: s -> s -> s

data SimpleLattice s = Bottom -- underspecification
                     | Val s
                     | Top -- overspecification
                     deriving (Eq, Show)

instance Eq s => JSL (SimpleLattice s) where
    bottom = Bottom
    add x Bottom = x
    add Bottom x = x
    add x y = if x == y then x else Top

instance JSL Bool where
    bottom = False
    add = (||)

newtype FJSL s j = FJSL (s -> j)

instance JSL j => JSL (FJSL s j) where
    bottom = FJSL $ \_ -> bottom
    add (FJSL f) (FJSL g) = FJSL $ \s -> f s `add` g s
