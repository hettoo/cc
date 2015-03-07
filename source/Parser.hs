{-# LANGUAGE RankNTypes #-}
module Parser where
import Enlist
import Utils

type Parser a v = [a] -> [(v, [a])]

parse :: (Eq a, Eq v) =>
    Parser a v -> [a] -> v
parse p l = case (map fst . rm . fullParses . p) l of
    [] -> error "parse failed"
    v : r -> case r of
        [] -> v
        _ -> error "ambiguous parse"
    where
    fullParses :: [(v, [a])] -> [(v, [a])]
    fullParses = filter (\(_, l) -> isEmpty l)

-- Main parser building blocks

nop :: Parser a v
nop _ = []

yield :: v -> Parser a v
yield v l = [(v, l)]

satisfy :: (a -> Bool) -> Parser a a
satisfy f l = case l of
    a : r | f a -> [(a, r)]
    _ -> []

cond :: Parser a v -> Parser a (v -> w) -> Parser a w -> Parser a w
cond p q r l = if isEmpty m then r l else m
    where
    m = [(f v, l'') | (v, l') <- p l, (f, l'') <- q l']

(>@) :: Parser a v -> (v -> w) -> Parser a w
(>@) p f l = map (\(v, c) -> (f v, c)) (p l)
infixl 6 >@

(\/) :: Parser a v -> Parser a v -> Parser a v
(\/) p q l = p l ++ q l
infixl 5 \/

-- Some useful abbreviations

(.*.) :: Parser a v -> Parser a w -> Parser a (v, w)
(.*.) p q = cond p (q >@ \w v -> (v, w)) nop
infixl 7 .*.

(.*-) :: Parser a v -> Parser a w -> Parser a v
(.*-) p q = p .*. q >@ fst
infixl 7 .*-

(-*.) :: Parser a v -> Parser a w -> Parser a w
(-*.) p q = q .*- p
infixl 7 -*.

(>!) :: Parser a v -> w -> Parser a w
(>!) p w = p >@ \_ -> w
infixl 6 >!

(\</) :: Parser a v -> Parser a v -> Parser a v
(\</) p = cond p (yield id)
infixl 5 \</

(\>/) :: Parser a v -> Parser a v -> Parser a v
(\>/) p q = q \</ p
infixl 5 \>/

_opt :: (forall v. Parser a v -> Parser a v -> Parser a v) ->
    Parser a v -> Parser a (Maybe v)
_opt c p = (p >@ Just) `c` (yield Nothing)

_plus :: (forall v. Parser a v -> Parser a v -> Parser a v) ->
    Parser a v -> Parser a (v, [v])
_plus c p = (p >@ (\v -> (v, []))) `c` (p .*. _plus c p >@ pair (id, uncurry (:)))

_star :: (forall v. Parser a v -> Parser a v -> Parser a v) ->
    Parser a v -> Parser a [v]
_star c p = _opt c (_plus c p) >@ enlist

opt :: Parser a v -> Parser a (Maybe v)
opt = _opt (\/)

plus :: Parser a v -> Parser a (v, [v])
plus = _plus (\/)

star :: Parser a v -> Parser a [v]
star = _star (\/)

gopt :: Parser a v -> Parser a (Maybe v)
gopt = _opt (\</)

gplus :: Parser a v -> Parser a (v, [v])
gplus = _plus (\</)

gstar :: Parser a v -> Parser a [v]
gstar = _star (\</)

uopt :: Parser a v -> Parser a (Maybe v)
uopt = _opt (\>/)

uplus :: Parser a v -> Parser a (v, [v])
uplus = _plus (\>/)

ustar :: Parser a v -> Parser a [v]
ustar = _star (\>/)

anything :: Parser a a
anything = satisfy $ \_ -> True

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
