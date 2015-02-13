module Mealy where
import SemiLattice

-- Based on http://homepages.cwi.nl/~janr/papers/files-of-papers/2008-fossacs-2008-coalgebraic-logic-and-synthesis-etc.pdf
-- Note that the link to their implementation did not exist anymore.

type Mealy a b q = q -> a -> (b, q)

data MealyFormula a b x = FF
                        | Var x
                        | Trans a (MealyFormula a b x)
                        | Out a b
                        | Add (MealyFormula a b x) (MealyFormula a b x)
                        | Nu x (MealyFormula a b x)
                        deriving (Show, Eq)

subst :: Eq x =>
    MealyFormula a b x -> x -> MealyFormula a b x -> MealyFormula a b x
subst f x g = case f of
    Var y -> if y == x then g else Var y
    Trans a h -> Trans a (subst h x g)
    Add h i -> Add (subst h x g) (subst i x g)
    Nu y h -> if y == x then Nu y h else Nu y (subst h y g)
    _ -> f

synthesize :: (Eq a, Eq x, SemiLattice b) =>
    Mealy a b (MealyFormula a b x)
synthesize f a = case f of
    FF -> (bottom, FF)
    Trans a' g -> (bottom, if a' == a then g else FF)
    Out a' b -> (if a' == a then b else bottom, FF)
    Add f g -> (add b1 b2, Add t1 t2)
        where
        (b1, t1) = synthesize f a
        (b2, t2) = synthesize g a
    Nu x f -> synthesize (subst f x (Nu x f)) a

--norm :: (Eq a, Eq b, Eq x) =>
--    MealyFormula a b x -> MealyFormula a b x
--norm f = case f of
--    FF -> FF
--    Trans a g -> Trans a (norm g)
--    Add g h -> conj . rem . flatten $ Add (norm g) (norm h)
--        where
--        conj = foldl Add FF
--        rem = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []
--        flatten FF = []
--        flatten (Add i j) = flatten i ++ flatten j
--        flatten i = [i]
--    Nu x g -> Nu x (norm g)
--    f -> f
--
--synthesize :: (Eq a, Eq b, Eq x, SemiLattice b) =>
--    Mealy a b (MealyFormula a b x)
--synthesize f a = case f of
--    FF -> (bottom, FF)
--    Trans a' g -> (bottom, if a' == a then norm g else FF)
--    Out a' b -> (if a' == a then b else bottom, FF)
--    Add f g -> (add b1 b2, norm $ Add t1 t2)
--        where
--        (b1, t1) = synthesize f a
--        (b2, t2) = synthesize g a
--    Nu x f -> (b, norm g)
--        where
--        (b, g) = synthesize (subst f x (Nu x f)) a
