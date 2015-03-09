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

eof :: Parser a ()
eof l = if isEmpty l then yield () l else Nothing

phantom :: Parser a v -> Parser a v
phantom p l = fmap (\t -> (fst t, l)) (p l)

satisfy :: (a -> Bool) -> Parser a a
satisfy f l = case l of
    a : r | f a -> Just (a, r)
    _ -> Nothing

cond :: Parser a v -> Parser a (v -> w) -> Parser a w -> Parser a w
cond p q r l = case m of
    Nothing -> r l
    _ -> m
    where
    m = case p l of
        Nothing -> Nothing
        Just (v, l') -> case q l' of
            Nothing -> Nothing
            Just (f, l'') -> Just (f v, l'')

(>@) :: Parser a v -> (v -> w) -> Parser a w
(>@) p f l = fmap (left f) (p l)
infixl 6 >@

-- Some useful abbreviations

(.*.) :: Parser a v -> Parser a w -> Parser a (v, w)
(.*.) p q = cond p (q >@ \w v -> (v, w)) nop
infixl 7 .*.

(.*-) :: Parser a v -> Parser a w -> Parser a v
(.*-) p q = p .*. q >@ fst
infixl 7 .*-

(-*.) :: Parser a v -> Parser a w -> Parser a w
(-*.) p q = p .*. q >@ snd
infixl 7 -*.

(>!) :: Parser a v -> w -> Parser a w
(>!) p w = p >@ const w
infixl 6 >!

(\/) :: Parser a v -> Parser a v -> Parser a v
(\/) p = cond p (yield id)
infixr 5 \/

sep :: Parser a v -> Parser a ()
sep p = phantom p >! () \/ eof

opt :: Parser a v -> Parser a (Maybe v)
opt p = p >@ Just \/ yield Nothing

plus :: Parser a v -> Parser a (v, [v])
plus p = p .*. plus p >@ right (uncurry (:)) \/ p >@ \v -> (v, [])

star :: Parser a v -> Parser a [v]
star p = opt (plus p) >@ listify

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
