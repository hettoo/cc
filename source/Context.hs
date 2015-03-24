module Context where

type Context t = (Int, [(String, t, Int)])

cnew :: Context t
cnew = (0, [])

cdown :: Context t -> Context t
cdown (i, l) = (i + 1, l)

cadd :: Context t -> String -> t -> Context t
cadd (i, l) s t = (i, cadd' l)
    where
    cadd' l = case l of
        [] -> [(s, t, i)]
        f@(s', t', i') : r ->
            if s == s' then
                if i' < i then
                    cadd' r
                else
                    error ("redefined entity " ++ s)
            else
                f : cadd' r

crem :: Context t -> String -> Context t
crem (i, l) s = (i, crem' l)
    where
    crem' l = case l of
        [] -> []
        f@(s', t, i) : r ->
            if s == s' then
                crem' r
            else
                f : crem' r

clookup :: Context t -> String -> Maybe t
clookup (i, l) s = case l of
    [] -> Nothing
    (s', t, _) : _ | s == s' -> Just t
    _ : r -> clookup (i, r) s

clookupe :: Context t -> String -> t
clookupe c s = case clookup c s of
    Nothing -> error ("undeclared entity " ++ s)
    Just t -> t

cfind :: Eq t =>
    Context t -> t -> Bool
cfind (i, l) t = case l of
    [] -> False
    (_, t', _) : _ | t == t' -> True
    _ : r -> cfind (i, r) t

class DistinctSequence t where
    createN :: Int -> t

fresh :: (Eq t, DistinctSequence t) =>
    Context t -> t
fresh = fresh' 0
    where
    fresh' n c = case cfind c f of
        False -> f
        True -> fresh' (n + 1) c
        where
        f = createN n
