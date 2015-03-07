module Combine where

class Combine a where
    combine :: a -> String

instance Combine Char where
    combine a = [a]

instance Combine a => Combine [a] where
    combine l = case l of
        [] -> ""
        a : r -> combine a ++ combine r

instance Combine a => Combine (Maybe a) where
    combine m = case m of
        Nothing -> ""
        Just a -> combine a

instance (Combine a, Combine b) => Combine (a, b) where
    combine (a, b) = combine a ++ combine b
