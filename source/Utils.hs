module Utils where

isEmpty l = case l of
    [] -> True
    _ -> False

rm :: Eq a =>
    [a] -> [a]
rm = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []

double :: a -> (a, a)
double a = (a, a)

pair :: (a -> b, c -> d) -> (a, c) -> (b, d)
pair (f, g) (a, c) = (f a, g c)

left :: (a -> b) -> (a, c) -> (b, c)
left f = pair (f, id)

right :: (c -> b) -> (a, c) -> (a, b)
right f = pair (id, f)

nfoldr :: (a -> a -> a) -> a -> [a] -> a
nfoldr f b l = case l of
    [] -> b
    [a] -> a
    a : r -> a `f` nfoldr f b r

classify :: (Enum c, Bounded c) =>
    (c -> a -> Bool) -> a -> [c]
classify f a = filter (\c -> f c a) [minBound ..]

sub :: Eq a =>
    [a] -> [a] -> [a]
sub a b = filter (\c -> not $ c `elem` b) a
