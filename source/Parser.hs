{-# LANGUAGE RankNTypes #-}
module Parser where
import Listify
import Utils

type Parser a v = [a] -> [(v, [a])]

parse :: (Eq a, Eq v, Show v) =>
    Parser a v -> [a] -> v
-- minimize with rm after fullParses if performance is not an issue
parse p l = case (map fst . fullParses . p) l of
    [] -> error "parse failed"
    v : r -> case r of
        [] -> v
        _ -> error $ "ambiguous grammar yielding " ++ show (v : r)
    where
    fullParses :: [(v, [a])] -> [(v, [a])]
    fullParses = filter (\(_, l) -> isEmpty l)

-- Main parser building blocks

nop :: Parser a v
nop _ = []

yield :: v -> Parser a v
yield v l = [(v, l)]

eof :: Parser a ()
eof l = if isEmpty l then yield () l else []

phantom :: Parser a v -> Parser a v
phantom p l = map (\t -> (fst t, l)) (p l)

satisfy :: (a -> Bool) -> Parser a a
satisfy f l = case l of
    a : r | f a -> [(a, r)]
    _ -> []

cond :: Parser a v -> Parser a (v -> w) -> Parser a w -> Parser a w
cond p q r l = if isEmpty m then r l else m
    where
    m = [(f v, l'') | (v, l') <- p l, (f, l'') <- q l']

(>@) :: Parser a v -> (v -> w) -> Parser a w
(>@) p f l = map (left f) (p l)
infixl 6 >@

(\/) :: Parser a v -> Parser a v -> Parser a v
(\/) p q l = p l ++ q l
infixr 5 \/

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

(\</) :: Parser a v -> Parser a v -> Parser a v
(\</) p = cond p (yield id)
infixr 5 \</

(\>/) :: Parser a v -> Parser a v -> Parser a v
(\>/) p q = q \</ p
infixr 5 \>/

sep :: Parser a v -> Parser a ()
sep p = phantom p >! () \/ eof

_opt :: (forall v. Parser a v -> Parser a v -> Parser a v) ->
    Parser a v -> Parser a (Maybe v)
_opt c p = (p >@ Just) `c` (yield Nothing)

_plus :: (forall v. Parser a v -> Parser a v -> Parser a v) ->
    Parser a v -> Parser a (v, [v])
_plus c p = (p .*. _plus c p >@ right (uncurry (:))) `c` (p >@ (\v -> (v, [])))

_star :: (forall v. Parser a v -> Parser a v -> Parser a v) ->
    Parser a v -> Parser a [v]
_star c p = _opt c (_plus c p) >@ listify

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
