module Context where
import State
import Data.List
import Data.Function

type Context t = (Int, Int, [(String, t, Int)])

cnew :: Context t
cnew = (0, 0, [])

cdown :: State (Context t) ()
cdown = st $ \(i, n, l) -> (i + 1, n, l)

cget :: Eq t =>
    Context t -> [(String, t)]
cget (_, _, l) = map (\(s, t, _) -> (s, t)) $ foldr improve [] l
    where
    improve v@(s, t, _) r =
        let
            r' = filter (\(s', t', _) -> s == s' && t == t') r
            m = maximumBy (compare `on` measure) r'
        in if null r' then
            [v]
        else if ((>) `on` measure) v m then
            v : r'
        else
            r'
    measure (_, _, i) = i

data CAddPolicy =
    Both
    | Override
    | Reject
    deriving Eq

caddc :: (t -> CAddPolicy) -> String -> t -> String -> State (Context t) ()
caddc f s t e = ST $ \(i, n, l) -> case cadd' i l of
    Just l' -> Left ((), (i, n, l'))
    Nothing -> Right e
    where
    cadd' i = flip foldr (Just [(s, t, i)]) $ \c@(s', t', i') r ->
        if s == s' then
            if i' >= i && f t' == Reject then
                Nothing
            else
                if f t' == Both then fmap ((:) c) r else r
        else
            fmap ((:) c) r

cadd :: String -> t -> String -> State (Context t) ()
cadd = caddc (const Reject)

caddr :: Eq t =>
    String -> t -> String -> State (Context t) ()
caddr i t = caddc (\t' -> if t == t' then Reject else Override) i t

crem :: String -> State (Context t) ()
crem s = ST $ \(i, n, l) -> Left ((), (i, n, crem' l))
    where
    crem' = foldr (\f@(s', _, _) r -> if s == s' then r else f : r) []

clookupa :: String -> State (Context t) [t]
clookupa s = res clookupa'
    where
    clookupa' (i, n, l) = foldr (\(s', t, _) r ->
        if s == s' then t : r else r) [] l

clookupf :: (t -> Bool) -> State (Context t) (Maybe t)
clookupf f = res $ \(i, n, l) -> case l of
    [] -> Nothing
    (_, t, _) : _ | f t -> Just t
    _ : r -> clookupf f >!> (i, n, r)

clookupfe :: (t -> Bool) -> State (Context t) t
clookupfe f = do
    m <- clookupf f
    case m of
        Nothing -> fail $ "context lookup error"
        Just t -> return t

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
