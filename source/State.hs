module State where
import Utils
import Control.Monad
import Control.Applicative

newtype State a b = ST (a -> Either (b, a) String)

instance Functor (State a) where
    fmap f (ST g) = ST $ \a -> case g a of
        Left (b, a') -> Left (f b, a')
        Right e -> Right e

instance Monad (State a) where
    return b = ST $ \a -> Left (b, a)
    (>>=) (ST f) g = ST $ \a -> case f a of
        Left (b, a') -> let ST h = g b in h a'
        Right e -> Right e
    fail e = ST $ \a -> Right e

instance Applicative (State a) where
    pure = return
    (<*>) = ap

stWrap :: (b -> a) -> (a -> b -> b) -> State a c -> State b c
stWrap f g (ST h) = ST $ \b -> case h (f b) of
    Left (c, a') -> Left (c, g a' b)
    Right e -> Right e

res :: (a -> b) -> State a b
res f = ST $ \a -> Left (f a, a)

st :: (a -> a) -> State a ()
st f = ST $ \a -> Left ((), f a)

apply :: State a b -> a -> (b, a)
apply (ST f) a = case f a of
    Left (b, a') -> (b, a')
    Right e -> error e

(>!>) :: State a b -> a -> b
(>!>) s a = fst (apply s a)
infixl 1 >!>

(>@>) :: State a b -> a -> a
(>@>) s a = snd (apply s a)
infixl 1 >@>

getState :: State a a
getState = res id

ids :: State a ()
ids = st id

indiff :: State a b -> State a b
indiff s = do
    a <- getState
    b <- s
    st $ const a
    return b
