{-# LANGUAGE RankNTypes #-}
module Parser where
import Listify
import Utils

type Parser a v = [a] -> Maybe (v, [a])

parse :: (Eq a, Eq v, Show v) =>
    Parser a v -> [a] -> v
parse p l = case (fmap fst . p) l of
    Nothing -> error "parse failed"
    Just v -> v

-- Main parser building blocks

nop :: Parser a v
nop _ = Nothing

yield :: v -> Parser a v
yield v l = Just (v, l)

skip :: Parser a a
skip l = case l of
    [] -> Nothing
    a : r -> yield a r

inspect :: ([a] -> Parser a v) -> Parser a v
inspect f l = f l l

phantom :: Parser a v -> Parser a v
phantom p l = fmap (\t -> (fst t, l)) (p l)

(.<) :: Parser a v -> Parser a (v -> w) -> Parser a w
(.<) p q l = p l >>= \(v, l') -> fmap (\(f, l'') -> (f v, l'')) (q l')
infixl 7 .<

cond :: Parser a w -> Parser a w -> Parser a w
cond p q l = case r of
    Nothing -> q l
    _ -> r
    where
    r = p l

-- Some useful abbreviations

(.*.) :: Parser a v -> Parser a w -> Parser a (v, w)
(.*.) p q = cond (p .< (q >@ \w v -> (v, w))) nop
infixl 7 .*.

(.*-) :: Parser a v -> Parser a w -> Parser a v
(.*-) p q = p .*. q >@ fst
infixl 7 .*-

(-*.) :: Parser a v -> Parser a w -> Parser a w
(-*.) p q = p .*. q >@ snd
infixl 7 -*.

(>@) :: Parser a v -> (v -> w) -> Parser a w
(>@) p f = p .< yield f
infixl 6 >@

(>!) :: Parser a v -> w -> Parser a w
(>!) p w = p >@ const w
infixl 6 >!

(\/) :: Parser a v -> Parser a v -> Parser a v
(\/) p = cond (p .< yield id)
infixr 5 \/

eof :: Parser a ()
eof = inspect $ \l -> if isEmpty l then yield () else nop

sep :: Parser a v -> Parser a ()
sep p = phantom p >! () \/ eof

opt :: Parser a v -> Parser a (Maybe v)
opt p = p >@ Just \/ yield Nothing

plus :: Parser a v -> Parser a (v, [v])
plus p = p .*. plus p >@ right (uncurry (:)) \/ p >@ \v -> (v, [])

star :: Parser a v -> Parser a [v]
star p = opt (plus p) >@ listify

satisfy :: (a -> Bool) -> Parser a a
satisfy f = inspect (\l -> case l of
    a : r | f a -> skip
    _ -> nop)

anything :: Parser a a
anything = satisfy $ const True

sym :: Eq a =>
    a -> Parser a a
sym a = satisfy ((==) a)

nsym :: Eq a =>
    a -> Parser a a
nsym a = satisfy ((/=) a)

sseq :: Eq a =>
    [a] -> Parser a [a]
sseq l = case l of
    [] -> yield []
    a : r -> sym a .*. sseq r >@ uncurry (:)
