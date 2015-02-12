module Mealy where
import SemiLattice

type Mealy a b q = q -> a -> (b, q)

data MealyFormula a b x = TT
                        | Var x
                        | Trans a (MealyFormula a b x)
                        | Out a b
                        | Comp (MealyFormula a b x) (MealyFormula a b x)
                        | Nu x (MealyFormula a b x)
                        deriving Eq

sub :: Eq x =>
    MealyFormula a b x -> x -> MealyFormula a b x -> MealyFormula a b x
sub f x g = case f of
    Var y -> if y == x then g else Var y
    Trans a h -> Trans a (sub h x g)
    Comp h i -> Comp (sub h x g) (sub i x g)
    Nu y h -> if y == x then Nu y h else Nu y (sub h y g)
    _ -> f

synthesize :: (Eq a, Eq x, SemiLattice b) =>
    Mealy a b (MealyFormula a b x)
synthesize f a = case f of
    TT -> (top, TT)
    Trans a' g -> (top, if a' == a then g else TT)
    Out a' b -> (if a' == a then b else top, TT)
    Comp f g -> (meet b1 b2, Comp t1 t2)
        where
        (b1, t1) = synthesize f a
        (b2, t2) = synthesize g a
    Nu x f -> synthesize (sub f x (Nu x f)) a

--norm :: (Eq a, Eq b, Eq x) => MealyFormula a b x -> MealyFormula a b x
--norm f = case f of
--    TT -> TT
--    Trans a g -> Trans a (norm g)
--    Comp g h -> conj . rem . flatten $ Comp (norm g) (norm h)
--        where
--        conj = foldl Comp TT
--        rem = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []
--        flatten TT = []
--        flatten (Comp i j) = flatten i ++ flatten j
--        flatten i = [i]
--    Nu x g -> Nu x (norm g)
--    f -> f
--
--synthesize :: (Eq a, Eq b, Eq x, SemiLattice b) => Mealy a b (MealyFormula a b x)
--synthesize f a = case f of
--    TT -> (top, TT)
--    Trans a' g -> (top, if a' == a then norm g else TT)
--    Out a' b -> (if a' == a then b else top, TT)
--    Comp f g -> (meet b1 b2, norm $ Comp t1 t2)
--        where
--        (b1, t1) = synthesize f a
--        (b2, t2) = synthesize g a
--    Nu x f -> (b, norm g)
--        where
--        (b, g) = synthesize (sub f x (Nu x f)) a
