{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
module Listify where

class Listify a b where
    listify :: a -> [b]

instance Listify [a] a where
    listify = id

instance Listify a a where
    listify a = [a]

instance Listify a b => Listify [a] b where
    listify = foldr (\a r -> listify a ++ r) []

instance Listify a b => Listify (Maybe a) b where
    listify m = case m of
        Nothing -> []
        Just a -> listify a

instance (Listify a b, Listify c b) => Listify (a, c) b where
    listify (a, b) = listify a ++ listify b
