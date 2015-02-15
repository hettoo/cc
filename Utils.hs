module Utils where

double :: a -> (a, a)
double a = (a, a)

nfoldr :: Eq a =>
    (a -> a -> a) -> a -> [a] -> a
nfoldr f b l = case l of
    [] -> b
    [a] -> a
    a : r -> a `f` nfoldr f b r

classify :: (Enum c, Bounded c) =>
    (c -> a -> Bool) -> a -> [c]
classify f a = filter (\c -> f c a) [minBound ..]

data TChar = EOF
           | Char Char
           deriving (Eq, Show)

tstring :: String -> [TChar]
tstring s = map Char s ++ [EOF]

sub :: Eq a =>
    [a] -> [a] -> [a]
sub a b = filter (\c -> not $ c `elem` b) a
