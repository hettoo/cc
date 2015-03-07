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
        where
        isEmpty l = case l of
            [] -> True
            _ -> False

-- Main parser building blocks

yield :: v -> Parser a v
yield v l = [(v, l)]

(.*.) :: Parser a v -> Parser a w -> Parser a (v, w)
(.*.) p q l = [((v, w), l'') | (v, l') <- p l, (w, l'') <- q l']
infixl 7 .*.

(>@) :: Parser a v -> (v -> w) -> Parser a w
(>@) p f l = apply (p l)
    where
    apply l = case l of
        [] -> []
        (v, c) : r -> (f v, c) : apply r
infixl 6 >@

(\/) :: Parser a v -> Parser a v -> Parser a v
(\/) p q l = p l ++ q l
infixl 5 \/

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

opt :: Parser a v -> Parser a (Maybe v)
opt p = p >@ Just \/ yield Nothing

plus :: Parser a v -> Parser a (v, [v])
plus p = p >@ (\v -> (v, [])) \/ p .*. plus p >@ \(v, (w, r)) -> (v, w : r)

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

sseq :: Eq a =>
    [a] -> Parser a [a]
sseq l = case l of
    [] -> yield []
    a : r -> sym a .*. sseq r >@ \(v, vs) -> v : vs
