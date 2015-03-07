module Parser where
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

yield :: v -> Parser a v
yield v l = [(v, l)]

(.*.) :: Parser a v -> Parser a w -> Parser a (v, w)
(.*.) p q l = [((v, w), l'') | (v, l') <- p l, (w, l'') <- q l']
infixl 7 .*.

(>@) :: Parser a v -> (v -> w) -> Parser a w
(>@) p f l = map (\(v, c) -> (f v, c)) (p l)
infixl 6 >@

combine :: ([(v, [a])] -> [(w, [a])] -> [(x, [a])]) ->
    Parser a v -> Parser a w -> Parser a x
combine f p q l = f (p l) (q l)

-- Some useful abbreviations

(.*-) :: Parser a v -> Parser a w -> Parser a v
(.*-) p q = p .*. q >@ fst
infixl 7 .*-

(-*.) :: Parser a v -> Parser a w -> Parser a w
(-*.) p q = p .*. q >@ snd
infixl 7 -*.

(>!) :: Parser a v -> w -> Parser a w
(>!) p w = p >@ \_ -> w
infixl 6 >!

(\/) :: Parser a v -> Parser a v -> Parser a v
(\/) = combine (++)
infixl 5 \/

(\</) :: Parser a v -> Parser a v -> Parser a v
(\</) = combine (\a b -> if isEmpty a then b else a)
infixl 5 \</

(\>/) :: Parser a v -> Parser a v -> Parser a v
(\>/) = combine (\a b -> if isEmpty b then a else b)
infixl 5 \>/

opt :: Parser a v -> Parser a (Maybe v)
opt p = p >@ Just \/ yield Nothing

plus :: Parser a v -> Parser a (v, [v])
plus p = p >@ (\v -> (v, [])) \/ p .*. plus p >@ pair (id, uncurry (:))

star :: Parser a v -> Parser a [v]
star p = opt (plus p) >@ \m -> case m of
    Nothing -> []
    Just (v, r) -> v : r

satisfy :: (a -> Bool) -> Parser a a
satisfy f l = case l of
    a : r | f a -> [(a, r)]
    _ -> []

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
