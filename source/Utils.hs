module Utils where
import Data.List

isMinimal :: Eq a =>
    [a] -> Bool
isMinimal l = length l == length (nub l)

sm :: (a -> b) -> ([a] -> b) -> [a] -> b
sm f g l = case l of
    [a] -> f a
    _ -> g l

double :: a -> (a, a)
double a = (a, a)

pair :: (a -> b, c -> d) -> (a, c) -> (b, d)
pair (f, g) (a, c) = (f a, g c)

left :: (a -> b) -> (a, c) -> (b, c)
left f = pair (f, id)

right :: (c -> b) -> (a, c) -> (a, b)
right f = pair (id, f)
