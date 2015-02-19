module MealyFormula where
import Mealy
import JSL
import Utils

-- Based on http://homepages.cwi.nl/~janr/papers/files-of-papers/2008-fossacs-2008-coalgebraic-logic-and-synthesis-etc.pdf.
-- We switched from meet semilattices to join semilattices because this makes
-- more sense from a synthesis point of view.
-- Note that the link to their implementation did not exist anymore.

data MealyFormula a b x = FF
                        | Var x
                        | Trans a b (MealyFormula a b x)
                        | Add (MealyFormula a b x) (MealyFormula a b x)
                        | Nu x (MealyFormula a b x)
                        deriving (Show, Eq)

instance JSL (MealyFormula a b x) where
    bottom = FF
    add = Add

subst :: Eq x =>
    MealyFormula a b x -> x -> MealyFormula a b x -> MealyFormula a b x
subst f x g = case f of
    Var y -> if y == x then g else f
    Trans a b h -> Trans a b $ subst h x g
    Add h i -> Add (subst h x g) (subst i x g)
    Nu y h -> if y == x then f else Nu y $ subst h x g
    _ -> f

-- Normalization avoids accumulated redundancy and could be used to generate a
-- finite state machine.
norm :: (Eq a, Eq b, Eq x) =>
    MealyFormula a b x -> MealyFormula a b x
norm f = case f of
    Trans a b g -> Trans a b $ norm g
    Add g h -> conj . rem . flatten $ Add (norm g) (norm h)
        where
        conj = nfoldr Add FF
        rem = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []
        flatten FF = []
        flatten (Add i j) = flatten i ++ flatten j
        flatten i = [i]
    Nu x g -> Nu x (norm g)
    _ -> f

type MealySynth a b x = Mealy a b (MealyFormula a b x)

synthesize :: (Eq a, Eq b, Eq x, JSL b, Show a, Show b, Show x) =>
    MealyFormula a b x -> MealySynth a b x
synthesize s = (norm s, synthesize')
    where
    synthesize' f a = case f of
        FF -> bottom
        Trans a' b g -> if a' == a then (b, norm g) else (bottom, FF)
        Add f g -> synthesize' (norm f) a `add` synthesize' (norm g) a
        Nu x f -> synthesize' (norm (subst f x (Nu x f))) a
        Var x -> error $ "Not a closed formula: " ++ show s

freshest :: Ord x =>
    MealyFormula a b x -> Maybe x
freshest f = case f of
    Var x -> Just x
    Trans a b g -> freshest g
    Add g h -> max (freshest g) (freshest h)
    Nu x g -> max (Just x) (freshest g)
    _ -> Nothing

class Fresh x where
    fresh :: MealyFormula a b x -> x
