module SPL.Unifier where
import SPL.Algebra
import State
import Context

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

unifiable :: Type -> Type -> Bool
unifiable t u = case unifyf t u of
    Nothing -> False
    Just _ -> True

applyUnificationS :: Context Type -> StmtT -> StmtT
applyUnificationS c s = case s of
    StmtsT l -> s -- TODO
    VarDeclT t i e -> s -- TODO
    FunDeclT t i a b -> s -- TODO
    FunCallT i as -> s -- TODO
    ReturnT m -> s -- TODO
    AssignT i fs e -> s -- TODO
    IfT c b m -> s -- TODO
    WhileT c b -> s -- TODO

applyUnificationE :: Context Type -> ExpT -> ExpT
applyUnificationE c e = case e of
    EIntT n t -> e -- TODO
    EBoolT b t -> e -- TODO
    ECharT c t -> e -- TODO
    ENilT t -> e -- TODO
    ETupleT e1 e2 t -> e -- TODO
    EIdT i fs t -> e -- TODO
    EFunCallT i as t -> e -- TODO
    EOp1T o e' t -> e -- TODO
    EOp2T o e1 e2 t -> e -- TODO

applyUnificationT :: Context Type -> Type -> Type
applyUnificationT c t = case t of
    TPoly p -> findApplication (cget c)
        where
        findApplication l = case l of
            [] -> t
            (i, t') : r -> if p == i then t' else findApplication r
    TTuple t1 t2 -> TTuple (applyUnificationT c t1) (applyUnificationT c t2)
    TList t' -> TList (applyUnificationT c t')
    _ -> t
