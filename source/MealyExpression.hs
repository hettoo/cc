module MealyExpression where
import MealyFormula
import JSL

(.*.) :: MealyFormula a b x -> MealyFormula a b x -> MealyFormula a b x
(.*.) f g = case f of
    Trans a b h -> Trans a b (h .*. g)
    Add h i -> (h .*. g) \/ (i .*. g)
    Nu x h -> Nu x (h .*. g)
    FF -> g
    _ -> f
infixl 7 .*.

(.!.) :: JSL b =>
    [a] -> b -> MealyFormula a b x
(.!.) l b = case l of
    [] -> FF
    [a] -> Trans a b FF
    a : r -> Trans a bot $ r .!. b
infixl 8 .!.

(.|.) :: Eq b =>
    [a] -> b -> MealyFormula a (SimpleLattice b) x
(.|.) a b = a .!. Val b
infixl 8 .|.

skip :: JSL b =>
    [a] -> MealyFormula a b x
skip a = a .!. bot

plus :: Fresh x =>
    MealyFormula a b x -> MealyFormula a b x
plus f = Nu v $ f .*. (Var v)
    where
    v = fresh f

star :: Fresh x =>
    MealyFormula a b x -> MealyFormula a b x
star f = Nu v $ opt $ f .*. (Var v)
    where
    v = fresh f
