module Context where
import State
import Utils

type Context t = (Int, Int, [(String, t, Int)])

cnew :: Context t
cnew = (0, 0, [])

cdown :: State (Context t) ()
cdown = st $ \(i, n, l) -> (i + 1, n, l)

caddc :: (t -> Bool) -> String -> t -> String -> State (Context t) ()
caddc f s t e = ST $ \(i, n, l) -> case cadd' l i of
    Just l' -> Left ((), (i, n, l'))
    Nothing -> Right e
    where
    cadd' l i = case l of
        [] -> Just [(s, t, i)]
        c@(s', t', i') : r ->
            if s == s' then
                if i' >= i && f t' then
                    Nothing
                else
                    rec
            else
                fmap ((:) c) rec
            where
            rec = cadd' r i

cadd :: String -> t -> String -> State (Context t) ()
cadd = caddc (const True)

caddr :: Eq t =>
    String -> t -> String -> State (Context t) ()
caddr i t = caddc ((/=) t) i t

crem :: String -> State (Context t) ()
crem s = ST $ \(i, n, l) -> Left ((), (i, n, crem' l))
    where
    crem' l = case l of
        [] -> []
        f@(s', _, _) : r ->
            if s == s' then
                crem' r
            else
                f : crem' r

clookup :: String -> State (Context t) (Maybe t)
clookup s = res $ \(i, n, l) -> case l of
    [] -> Nothing
    (s', t, _) : _ | s == s' -> Just t
    _ : r -> clookup s >!> (i, n, r)

clookupe :: String -> State (Context t) t
clookupe s = do
    m <- clookup s
    case m of
        Nothing -> fail $ "undeclared entity " ++ s
        Just t -> return t

cfindf :: (t -> Bool) -> State (Context t) Bool
cfindf f = res $ \(i, n, l) -> case l of
    [] -> False
    (_, t', _) : _ | f t' -> True
    _ : r -> cfindf f >!> (i, n, r)

cfind :: Eq t =>
    t -> State (Context t) Bool
cfind t = cfindf ((==) t)

creplace :: (t -> Maybe String) -> t -> State (Context t) t
creplace f t = case f t of
    Nothing -> return t
    Just s -> do
        m <- clookup s
        return $ case m of
            Nothing -> t
            Just t' -> t'

class DistinctSequence t where
    createN :: Int -> t

fresh :: DistinctSequence t =>
    State (Context t) t
fresh = ST $ \(i, n, l) -> Left (createN n, (i, n + 1, l))
