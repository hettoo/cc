module State where
import Utils
import Control.Monad
import Control.Applicative

newtype State a b = ST (a -> (b, a))

instance Functor (State a) where
    fmap f (ST g) = ST $ \a -> left f (g a)

instance Monad (State a) where
    return b = ST $ \a -> (b, a)
    (>>=) (ST f) g = ST $ \a -> let (b, a') = f a; ST h = g b in h a'

instance Applicative (State a) where
    pure = return
    (<*>) = ap

indiff :: State a b -> State a b
indiff (ST f) = ST $ \a -> right (const a) (f a)

stl :: State a c -> State (a, b) c
stl (ST f) = ST $ \(a, b) -> right (\a' -> (a', b)) (f a)

str :: State b c -> State (a, b) c
str (ST f) = ST $ \(a, b) -> right (\b' -> (a, b')) (f b)

st :: (a -> b) -> State a b
st f = ST $ \a -> (f a, a)
