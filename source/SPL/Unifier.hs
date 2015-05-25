module SPL.Unifier where
import SPL.Algebra
import State
import Context
import Fix
import Utils

unify :: Bool -> Type -> Type -> Maybe (Context Type)
unify all t u = let (b, c) = apply (unify' t u) cnew in
    if b then Just c else Nothing
    where
    unify' :: Type -> Type -> State (Context Type) Bool
    unify' t u = do
        rt <- rewrite t
        ru <- rewrite u
        case (rt, ru) of
            (TCustom n ts, TCustom m ts') ->
                if n == m && length ts == length ts' then
                    unifyAll ts ts'
                else
                    return False
                where
                unifyAll ts ts' = case (ts, ts') of
                    ((t' : r), (t'' : r')) -> do
                        b <- unify' t' t''
                        if b then unifyAll r r' else return False
                    _ -> return True
            (TPoly i, TPoly j) ->
                if i == j then
                    return True
                else
                    if isFlexible i then do
                        caddr i u "?"
                        return True
                    else
                        if isFlexible j then do
                            caddr j t "?"
                            return True
                        else
                            return False
            (TPoly i, _) ->
                if isFlexible i then do
                    caddr i u "?"
                    return True
                else
                    return False
            (_, TPoly j) ->
                if isFlexible j then do
                    caddr j t "?"
                    return True
                else
                    return False
            (t', u') -> return (t' == u')
            where
            rewrite t = do
                m <- clookup (name t)
                return $ case m of
                    Nothing -> t
                    Just t' -> t'
            name t = case t of
                TPoly i -> i
                _ -> ""
            isFlexible s = case s of
                '?' : r -> True
                _ -> all

unifyf :: Type -> Type -> Maybe (Context Type)
unifyf = unify False

unifyAll :: Type -> Type -> Maybe (Context Type)
unifyAll = unify True

unifiable :: Bool -> Type -> Type -> Bool
unifiable b t u = case unify b t u of
    Nothing -> False
    Just _ -> True

unifiablef :: Type -> Type -> Bool
unifiablef = unifiable False

unifiableAll :: Type -> Type -> Bool
unifiableAll = unifiable True

applyUnificationS :: Context Type -> StmtT -> StmtT
applyUnificationS c s = case s of
    Stmts l -> Stmts (map aus l)
    VarDecl t i m -> VarDecl (aut t) i (fmap aue m)
    FunDecl t i as b -> FunDecl (aut t) i (map (left aut) as) (aus b)
    FunCall i as -> FunCall i (map aue as)
    Return m -> Return $ fmap aue m
    Assign i fs e -> Assign i fs (aue e)
    Case e bs -> Case (aue e) (map (right aus) bs)
    If c b m -> If (aue c) (aus b) (fmap aus m)
    While c b -> While (aue c) (aus b)
    where
    aus = applyUnificationS c
    aue = applyUnificationE c
    aut = applyUnificationT c

applyUnificationE :: Context Type -> ExpT -> ExpT
applyUnificationE c e = expt (fmap aue (expC e')) (aut (typeC e'))
    where
    e' = unFix e
    aue = applyUnificationE c
    aut = applyUnificationT c

applyUnificationT :: Context Type -> Type -> Type
applyUnificationT c t = case t of
    TPoly p -> foldr (\(i, t') r -> if p == i then t' else r) t (cget c)
    TCustom i ts -> TCustom i (map aut ts)
    _ -> t
    where
    aut = applyUnificationT c
