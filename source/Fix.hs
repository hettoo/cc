{-# LANGUAGE UndecidableInstances #-}
module Fix where

newtype Fix f = Fix {unFix :: f (Fix f)}

instance Eq (f (Fix f)) => Eq (Fix f) where
    (==) f g = unFix f == unFix g

instance Show (f (Fix f)) => Show (Fix f) where
    show = show . unFix
