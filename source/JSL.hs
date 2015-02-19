module JSL where

class JSL s where
    bottom :: s
    add :: s -> s -> s

bigAdd :: JSL s =>
    [s] -> s
bigAdd = foldr add bottom

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

instance JSL j => JSL (s -> j) where
    bottom = \_ -> bottom
    add f g = \s -> f s `add` g s

instance (JSL a, JSL b) => JSL (a, b) where
    bottom = (bottom, bottom)
    add (a1, b1) (a2, b2) = (a1 `add` a2, b1 `add` b2)
