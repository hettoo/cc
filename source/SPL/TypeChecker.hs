module SPL.TypeChecker where
import SPL.Algebra

data FullType =
    VarType Type
    | FunType Type Type

type Context = [(String, Type)]

contextLookup :: String -> Context -> Maybe Type
contextLookup s l = case l of
    [] -> Nothing
    (s', t) : _ | s == s' -> Just t
    _ : r -> contextLookup s r

combineCover :: (Bool, Context) -> (Context -> (Bool, Context)) ->
    (Bool, Context)
combineCover (b, c) f = let p = f c in (b && fst p, snd p)

saneContext :: (Bool, Context) -> Bool
saneContext (b, c) = b && saneContext' c []
    where
    saneContext' c a = case c of
        [] -> True
        (s, t) : r -> (case contextLookup s a of
            Just t' -> t == t'
            _ -> True) && saneContext' r ((s, t) : a)

typeCover :: FullType -> FullType -> Context -> Bool
typeCover a b c = saneContext $ case (a, b) of
    (VarType t, VarType u) -> typeCover' t u c
    (FunType t1 t2, FunType u1 u2) ->
        combineCover (typeCover' t1 u1 c) (typeCover' t2 u2)
    _ -> (False, [])
    where
    typeCover' :: Type -> Type -> Context -> (Bool, Context)
    typeCover' a b c = case (a, b) of
        (TTuple t1 t2, TTuple t1' t2') ->
            combineCover (typeCover' t1 t1' c) (typeCover' t2 t2')
        (TList t, TList t') -> typeCover' t t' c
        (TPoly a, t) -> (True, (a, t) : c)
        _ -> (a == b, c)
