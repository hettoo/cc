module SPL.TypeChecker where
import SPL.Algebra
import Context

type SPLC = (Context Type, Context (Type, Type))

fieldType :: Field -> (Type, Type)
fieldType f = case f of
    Head -> (TList (TPoly "?"), TPoly "?")
    Tail -> (TList (TPoly "?"), TList (TPoly "?"))
    First -> (TTuple (TPoly "?") (TPoly "?"), TPoly "?")
    Second -> (TTuple (TPoly "?") (TPoly "?"), TPoly "?")

expType :: SPLC -> Exp -> Type
expType c@(cv, cf) e = case e of
    EInt i -> TInt
    EBool b -> TBool
    EChar c -> TChar
    ENil -> TList (TPoly "?")
    ETuple e1 e2 -> TTuple (expType c e1) (expType c e2)
    EId i fs -> case fs of
        [] -> clookupe cv i
        f : r -> case (clookupe cv i, fieldType f) of
            (t, (t', t'')) ->
                if t == t' then
                    expType (cadd (crem cv i) i t'', cf) (EId i r)
                else
                    error ("invalid use of field " ++ show f)
    EFunCall i as -> let (t, t') = clookupe cf i in
        if t == argType as then
            t'
        else
            error "function argument mismatch"
        where
        argType l = case l of
            [] -> TVoid
            [a] -> expType c a
            a : r -> TTuple (expType c a) (argType r)

--data FullType =
--    VarType Type
--    | FunType Type Type
--
--type Context = [(String, Type)]
--
--contextLookup :: String -> Context -> Maybe Type
--contextLookup s l = case l of
--    [] -> Nothing
--    (s', t) : _ | s == s' -> Just t
--    _ : r -> contextLookup s r
--
--combineCover :: (Bool, Context) -> (Context -> (Bool, Context)) ->
--    (Bool, Context)
--combineCover (b, c) f = let p = f c in (b && fst p, snd p)
--
--saneContext :: (Bool, Context) -> Bool
--saneContext (b, c) = b && saneContext' c []
--    where
--    saneContext' c a = case c of
--        [] -> True
--        (s, t) : r -> (case contextLookup s a of
--            Just t' -> t == t'
--            _ -> True) && saneContext' r ((s, t) : a)
--
--typeCover :: FullType -> FullType -> Context -> Bool
--typeCover a b c = saneContext $ case (a, b) of
--    (VarType t, VarType u) -> typeCover' t u c
--    (FunType t1 t2, FunType u1 u2) ->
--        combineCover (typeCover' t1 u1 c) (typeCover' t2 u2)
--    _ -> (False, [])
--    where
--    typeCover' :: Type -> Type -> Context -> (Bool, Context)
--    typeCover' a b c = case (a, b) of
--        (TTuple t1 t2, TTuple t1' t2') ->
--            combineCover (typeCover' t1 t1' c) (typeCover' t2 t2')
--        (TList t, TList t') -> typeCover' t t' c
--        (TPoly a, t) -> (True, (a, t) : c)
--        _ -> (a == b, c)
