module JSL where
import Utils

class JSL s where
    bot :: s
    (\/) :: s -> s -> s
    infixl 6 \/

-- Some applications may be sensitive to redundancy.
bigJoin :: JSL s =>
    [s] -> s
bigJoin = nfoldr (\/) bot

-- We also want to be able to create that.
opt :: JSL s =>
    s -> s
opt = (\/) bot

data SimpleLattice s = Bottom -- underspecification
                     | Val s
                     | Top -- overspecification
                     deriving (Eq, Show)

instance Eq s => JSL (SimpleLattice s) where
    bot = Bottom
    (\/) x Bottom = x
    (\/) Bottom x = x
    (\/) x y = if x == y then x else Top

instance JSL Bool where
    bot = False
    (\/) = (||)

instance JSL j => JSL (s -> j) where
    bot = \_ -> bot
    (\/) f g = \s -> f s \/ g s

instance (JSL a, JSL b) => JSL (a, b) where
    bot = (bot, bot)
    (\/) (a1, b1) (a2, b2) = (a1 \/ a2, b1 \/ b2)
