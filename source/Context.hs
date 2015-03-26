module Context where

type Context t = (Int, Int, [(String, t, Int)])

cnew :: Context t
cnew = (0, 0, [])

cdown :: Context t -> Context t
cdown (i, n, l) = (i + 1, n, l)

caddc :: Eq t =>
    Bool -> Context t -> String -> t -> Context t
caddc b (i, n, l) s t = (i, n, cadd' l)
    where
    cadd' l = case l of
        [] -> [(s, t, i)]
        f@(s', t', i') : r ->
            if s == s' then
                if i' >= i && (b || t /= t') then
                    error ("redefined entity " ++ s)
                else
                    cadd' r
            else
                f : cadd' r

cadd :: Eq t =>
    Context t -> String -> t -> Context t
cadd = caddc True

caddr :: Eq t =>
    Context t -> String -> t -> Context t
caddr = caddc False

crem :: Context t -> String -> Context t
crem (i, n, l) s = (i, n, crem' l)
    where
    crem' l = case l of
        [] -> []
        f@(s', t, i) : r ->
            if s == s' then
                crem' r
            else
                f : crem' r

clookup :: Context t -> String -> Maybe t
clookup (i, n, l) s = case l of
    [] -> Nothing
    (s', t, _) : _ | s == s' -> Just t
    _ : r -> clookup (i, n, r) s

clookupe :: Context t -> String -> t
clookupe c s = case clookup c s of
    Nothing -> error ("undeclared entity " ++ s)
    Just t -> t

cfindf :: Context t -> (t -> Bool) -> Bool
cfindf (i, n, l) f = case l of
    [] -> False
    (_, t', _) : _ | f t' -> True
    _ : r -> cfindf (i, n, r) f

cfind :: Eq t =>
    Context t -> t -> Bool
cfind c t = cfindf c ((==) t)

creplace :: Context t -> (t -> Maybe String) -> t -> t
creplace c f t = case f t of
    Nothing -> t
    Just s -> case clookup c s of
        Nothing -> t
        Just t' -> t'

class DistinctSequence t where
    createN :: Int -> t

fresh :: DistinctSequence t =>
    Context t -> (t, Context t)
fresh (i, n, l) = (createN n, (i, n + 1, l))
