module Mealy where
import JSL

type Mealy a b q = (q, q -> a -> (b, q))

(-!-) :: Mealy a b q -> [a] -> [b]
(-!-) (i, f) = trace' i
    where
    trace' q l = case l of
        [] -> []
        a : r -> case f q a of
            (b, p) -> b : trace' p r
infixl 5 -!-

mealySingle :: (a -> b) -> Mealy a b ()
mealySingle f = ((), \_ a -> (f a, ()))

mealyId :: Mealy a a ()
mealyId = mealySingle id

(-.-) :: Mealy b c s -> Mealy a b q -> Mealy a c (q, s)
(-.-) (j, n) (i, m) = ((i, j), t)
    where
    t (q, s) a = (c, (q', s'))
        where
        (b, q') = m q a
        (c, s') = n s b
infixl 6 -.-

(-*-) :: Mealy a b q -> Mealy c d s -> Mealy (a, c) (b, d) (q, s)
(-*-) (i, f) (j, g) = ((i, j), t)
    where
    t (q, s) (a, c) = ((a', b'), (q', s'))
        where
        (a', q') = f q a
        (b', s') = g s c
infixl 7 -*-

mealyList :: (JSL b, JSL q) =>
    Mealy a b q -> Mealy [a] b q
mealyList (i, m) = (i, \q l -> bigAdd $ map (m q) l)
