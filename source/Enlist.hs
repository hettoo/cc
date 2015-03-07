{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
module Enlist where

class Enlist a b where
    enlist :: a -> [b]

instance Enlist a a where
    enlist a = [a]

instance Enlist a b => Enlist [a] b where
    enlist l = case l of
        [] -> []
        a : r -> enlist a ++ enlist r

instance Enlist a b => Enlist (Maybe a) b where
    enlist m = case m of
        Nothing -> []
        Just a -> enlist a

instance (Enlist a b, Enlist c b) => Enlist (a, c) b where
    enlist (a, b) = enlist a ++ enlist b
