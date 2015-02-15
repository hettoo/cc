module Mealy where
import JSL

type Mealy a b q = q -> a -> (b, q)

type FullMealy a b q = (q, Mealy a b q)

trace :: FullMealy a b q -> [a] -> [b]
trace (i, f) = trace' i
    where
    trace' q l = case l of
        [] -> []
        a : r -> case f q a of
            (b, p) -> b : trace' p r

mealyId :: FullMealy a a ()
mealyId = ((), \_ a -> (a, ()))

mealyComp :: FullMealy a b q -> FullMealy b c s -> FullMealy a c (q, s)
mealyComp (i, m) (j, n) = ((i, j), t)
    where
    t (q, s) a = (c, (q', s'))
        where
        (b, q') = m q a
        (c, s') = n s b

mealyProd :: FullMealy a b q -> FullMealy c d s ->
    FullMealy (a, c) (b, d) (q, s)
mealyProd (i, f) (j, g) = ((i, j), t)
    where
    t (q, s) (a, c) = ((a', b'), (q', s'))
        where
        (a', q') = f q a
        (b', s') = g s c

mealyList :: (JSL b, JSL q) =>
    FullMealy a b q -> FullMealy [a] b q
mealyList (i, m) = (i, \q l -> bigAdd $ fmap (m q) l)
