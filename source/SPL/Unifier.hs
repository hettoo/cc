module SPL.Unifier where
import SPL.Algebra
import State
import Context
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
            (TTuple t1 t2, TTuple t1' t2') -> do
                b <- unify' t1 t1'
                case b of
                    False -> return False
                    True -> unify' t2 t2'
            (TList t', TList t'') -> unify' t' t''
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
    StmtsT l -> StmtsT (map aus l)
    VarDeclT t i e -> VarDeclT (aut t) i (aue e)
    FunDeclT t i as b -> FunDeclT (aut t) i (map (left aut) as) (aus b)
    FunCallT i as -> FunCallT i (map aue as)
    ReturnT m -> ReturnT $ fmap aue m
    AssignT i fs e -> AssignT i fs (aue e)
    IfT c b m -> IfT (aue c) (aus b) (fmap aus m)
    WhileT c b -> WhileT (aue c) (aus b)
    where
    aus = applyUnificationS c
    aue = applyUnificationE c
    aut = applyUnificationT c

applyUnificationE :: Context Type -> ExpT -> ExpT
applyUnificationE c e = case e of
    ENilT t -> ENilT (aut t)
    ETupleT e1 e2 t -> ETupleT (aue e1) (aue e2) (aut t)
    EIdT i fs t -> EIdT i fs (aut t)
    EFunCallT i as t -> EFunCallT i (map aue as) (aut t)
    EOp1T o e' t -> EOp1T o (aue e') (aut t)
    EOp2T o e1 e2 t -> EOp2T o (aue e1) (aue e2) (aut t)
    _ -> e
    where
    aue = applyUnificationE c
    aut = applyUnificationT c

applyUnificationT :: Context Type -> Type -> Type
applyUnificationT c t = case t of
    TPoly p -> findApplication (cget c)
        where
        findApplication l = case l of
            [] -> t
            (i, t') : r -> if p == i then t' else findApplication r
    TTuple t1 t2 -> TTuple (aut t1) (aut t2)
    TList t' -> TList (aut t')
    _ -> t
    where
    aut = applyUnificationT c
