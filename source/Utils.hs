module Utils where

(>>-) :: Monad m =>
    m a -> (a -> b) -> m b
(>>-) m f = m >>= \a -> return (f a)
infixl 1 >>-

isEmpty l = case l of
    [] -> True
    _ -> False

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
