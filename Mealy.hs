module Mealy where
import JSL

type Mealy a b q = (q, q -> a -> (b, q))

trace :: Mealy a b q -> [a] -> [b]
trace (i, f) = trace' i
    where
    trace' q l = case l of
        [] -> []
        a : r -> case f q a of
            (b, p) -> b : trace' p r

mealyId :: Mealy a a ()
mealyId = ((), \_ a -> (a, ()))

mealyComp :: Mealy a b q -> Mealy b c s -> Mealy a c (q, s)
mealyComp (i, m) (j, n) = ((i, j), t)
    where
    t (q, s) a = (c, (q', s'))
        where
        (b, q') = m q a
        (c, s') = n s b

mealyProd :: Mealy a b q -> Mealy c d s -> Mealy (a, c) (b, d) (q, s)
mealyProd (i, f) (j, g) = ((i, j), t)
    where
    t (q, s) (a, c) = ((a', b'), (q', s'))
        where
        (a', q') = f q a
        (b', s') = g s c

mealyList :: (JSL b, JSL q) =>
    Mealy a b q -> Mealy [a] b q
mealyList (i, m) = (i, \q l -> bigAdd $ fmap (m q) l)
