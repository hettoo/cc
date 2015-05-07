module SPL.Typer where
import SPL.Algebra
import SPL.Unifier
import SPL.Printer
import Context
import State
import Fix
import Control.Monad

type Cv = Context Type
type Cf = Context (Type, Type)
type Cd = Context (String, [String], [(Type, String)])
data SPLC = SPLC {cv :: Cv, cf :: Cf, cd :: Cd}

splcv :: State Cv a -> State SPLC a
splcv = stWrap cv (\x c -> c {cv = x})

splcf :: State Cf a -> State SPLC a
splcf = stWrap cf (\x c -> c {cf = x})

splcd :: State Cd a -> State SPLC a
splcd = stWrap cd (\x c -> c {cd = x})

caddvar :: String -> Type -> State SPLC ()
caddvar i t = splcv $ cadd i t ("redefined variable " ++ i)

caddfun :: String -> [(Type, String)] -> Type -> State SPLC ()
caddfun i as t = do
    splcf $ caddc checkOverloaded i p ("redefined function " ++ i)
    where
    p = (combineTypes (map fst as), t)
    checkOverloaded p' = (do
            (t, _) <- freshen p
            (t', _) <- freshen p'
            return $
                if unifiablef t t' then
                    Reject
                else
                    Both
        ) >!> cnew

instance DistinctSequence Type where
    createN n = TPoly ("?" ++ show n)

fieldType :: Field -> State (Context Type) (Type, Type)
fieldType f = case f of
    Head -> do
        a <- fresh
        return (TList a, a)
    Tail -> do
        a <- fresh
        return (TList a, TList a)
    First -> do
        a <- fresh
        b <- fresh
        return (TTuple a b, a)
    Second -> do
        a <- fresh
        b <- fresh
        return (TTuple a b, b)

treplace :: Type -> State (Context Type) Type
treplace t = case t of
    TTuple t1 t2 -> do
        t1 <- treplace t1
        t2 <- treplace t2
        return $ TTuple t1 t2
    TList t -> do
        t <- treplace t
        return $ TList t
    _ -> creplace findPoly t
    where
    findPoly t = case t of
        TPoly a -> Just a
        _ -> Nothing

checkApp :: (Type, Type) -> Type -> State a Type
checkApp (t, t') a = case unifyf t a of
    Nothing -> fail $ "application mismatch: expected type `" ++
        simplePrint t ++ "' cannot be unified with given type `" ++
        simplePrint a ++ "'"
    Just c -> return $ treplace t' >!> c

op1Type :: Op1 -> State Cv (Type, Type)
op1Type o = do
    a <- fresh
    return (a, a)

op2Type :: Op2 -> State Cv (Type, Type)
op2Type o = case o of
    OCons -> do
        a <- fresh
        return (TTuple a (TList a), TList a)
    OAnd -> tBBB
    OOr -> tBBB
    OEq -> taaB
    ONeq -> taaB
    OLt -> taaB
    OGt -> taaB
    OLe -> taaB
    OGe -> taaB
    OPlus -> taaa
    OMinus -> taaa
    OTimes -> taaa
    ODiv -> taaa
    OMod -> taaa
    where
    tBBB = return (TTuple TBool TBool, TBool)
    taaa = do
        a <- fresh
        return (TTuple a a, a)
    taaB = do
        a <- fresh
        return (TTuple a a, TBool)

initContext :: [Stmt] -> State SPLC ()
initContext l = case l of
    [] -> do
        splcf $ do
            cadd "isEmpty" (TList (TPoly "t"), TBool) "?"
            cadd "print" (TPoly "t", TVoid) "?"
            cadd "read" (TVoid, TInt) "?"
    s : r -> do
        case s of
            VarDecl t i _ -> do
                caddvar i t
            FunDecl t i as _ -> do
                caddfun i as t
            DataDecl i as cs -> sequence_ (map addCons cs)
                where
                addCons (c, ts) = splcd $ cadd c (i, as, ts)
                    ("duplicate constructor " ++ c)
            _ -> return ()
        initContext r

guaranteeReturn :: Stmt -> Bool
guaranteeReturn s = case s of
    Stmts l -> any guaranteeReturn l
    If e s' m -> case m of
        Nothing -> False
        Just s'' -> guaranteeReturn s' && guaranteeReturn s''
    Return m -> True
    _ -> False

guaranteeReturns :: [Stmt] -> [Stmt]
guaranteeReturns = flip foldr [] $ \s r -> case s of
    FunDecl t i as b ->
        if t == TVoid || guaranteeReturn b then
            s : r
        else
            error $ "function " ++ i ++ " may not return a value"
    _ -> s : r

annotateProgram :: [Stmt] -> [StmtT]
annotateProgram l = let
    l' = guaranteeReturns l
    in (do
        initContext l'
        splcv cdown
        splcf cdown
        annotateMulti [] l'
    ) >!> SPLC {cv = cnew, cf = cnew, cd = cnew}

annotateMulti :: [(Type, String)] -> [Stmt] -> State SPLC [StmtT]
annotateMulti l = mapM (annotateS l)

checkPoly :: [Type] -> Type -> State SPLC Bool
checkPoly l t = checkPoly' (listPoly t) (concat (map listPoly l))
    where
    checkPoly' :: [Type] -> [Type] -> State SPLC Bool
    checkPoly' r l = case r of
        [] -> return True
        a : r' -> let rec = checkPoly' r' l in
            if a `elem` l then
                rec
            else do
                b <- splcv (cfindf (\t -> a `elem` listPoly t))
                if b then rec else return False
    listPoly :: Type -> [Type]
    listPoly t = case t of
        TPoly _ -> [t]
        TTuple t1 t2 -> listPoly t1 ++ listPoly t2
        TList t' -> listPoly t'
        _ -> []

checkMain :: String -> Type -> [(Type, String)] -> State SPLC Bool
checkMain i t as = case i of
    "main" -> return $ null as
    _ -> return True

applyFun :: String -> [Exp] -> State SPLC ([ExpT], Type)
applyFun i as = do
    ts <- splcf (clookupa i)
    ts <- mapM (\t -> splcv (freshen t)) ts
    as <- mapM annotateE as
    let a = combineTypes (map getType as) in do
        us <- mapM (\(t, t') ->
            return $ fmap (\c -> treplace t' >!> c) (unifyf t a)) ts
        us <- filterM (\m -> return $ case m of Just _ -> True; _ -> False) us
        us <- mapM (\m -> case m of Just t -> return t) us
        t <- case us of
            t : _ -> return t
            _ -> fail $ "no candidate for application of " ++ i ++ " found"
        return (as, t)

applyCons :: String -> [Exp] -> State SPLC ([ExpT], Type)
applyCons i es = do
    (j, as, ts) <- splcd (clookupe i)
    es <- mapM annotateE es
    ats <- return $ map getType es
    c <- return $ sequence (map addArg (zip (map fst ts) ats)) >@> cnew
    rts <- return $ map (applyUnificationT c) (map TPoly as)
    rts <- splcv $ ST $ \cv ->
        let (r, (cv', _)) = apply (mapM freshenT rts) (cv, cnew) in
        Left (r, cv')
    return (es, TCustom j rts)
    where
    addArg (t, at) = case t of
        TPoly p -> cadd p at "duplicate type argument"
        _ -> return ()

annotateS :: [(Type, String)] -> Stmt -> State SPLC StmtT
annotateS l s = case s of
    Stmts s -> do
        s <- annotateMulti l s
        return $ Stmts s
    DataDecl i as l -> return $ DataDecl i as l
    VarDecl et i e -> do
        b <- checkPoly (map fst l) et
        if b then do
            e <- annotateE e
            let t = getType e in
                if unifiablef t et then do
                    caddvar i et
                    return (VarDecl et i e)
                else
                    fail $ "assignment mismatch: expected type `" ++
                        simplePrint et ++
                        "' cannot be unified with given type `" ++
                        simplePrint t ++ "' (variable " ++ i ++ ")"
        else
            fail ("free polymorphic variable " ++ i)
    FunDecl t i as b -> do
        m <- checkMain i t as
        if m then do
            caddfun i as t
            cv <- splcv getState
            splcv cdown
            splcv (mapM_ addArg as)
            splcf cdown
            splcv cdown
            b <- annotateS ((t, i) : l) b
            splcv $ st (const cv)
            return $ FunDecl t i as b
        else
            fail "invalid main function"
                where
                addArg (t, j) =
                    cadd j t ("duplicate formal argument " ++ j ++
                        " for function " ++ i)
    FunCall i as -> do
        (es, _) <- applyFun i as
        return $ FunCall i es
    Return m -> case l of
        [] -> fail "return outside function"
        (a, i) : l' -> case m of
            Nothing -> return $ Return Nothing
            Just e -> do
                e <- annotateE e
                let t = getType e in
                    if unifiablef t a then
                        return $ Return (Just e)
                    else
                        fail $ "incompatible return type `" ++
                            simplePrint t ++ "' provided; expected `" ++
                            simplePrint a ++ "' in function " ++ i
    Assign i fs e -> do
        vt <- idType i fs
        e <- annotateE e
        let t = getType e in
            if unifiablef t vt then
                return $ Assign i fs e
            else
                fail $ "assignment mismatch: expected type `" ++
                    simplePrint vt ++
                    "' cannot be unified with given type `" ++
                    simplePrint t ++ "' (variable " ++ i ++ ")"
    If e s m -> do
        e <- annotateE e
        s <- indiff (annotateS l s)
        case m of
            Nothing -> return $ If e s Nothing
            Just m -> do
                m <- indiff (annotateS l m)
                return $ If e s (Just m)
    While e s -> do
        e <- annotateE e
        s <- indiff (annotateS l s)
        return $ While e s

freshen :: (Type, Type) -> State Cv (Type, Type)
freshen t = ST $ \cv -> let
    (r, (cv', _)) = apply (freshen' t) (cv, cnew)
    in Left (r, cv')
    where
    freshen' (t1, t2) = do
        t1 <- freshenT t1
        t2 <- freshenT t2
        return (t1, t2)

freshenT :: Type -> State (Context Type, Context Type) Type
freshenT t = case t of
    TTuple t1 t2 -> do
        t1 <- freshenT t1
        t2 <- freshenT t2
        return (TTuple t1 t2)
    TList t -> do
        t <- freshenT t
        return (TList t)
    TPoly i -> do
        m <- str (clookup i)
        case m of
            Nothing -> do
                t <- stl fresh
                str $ cadd i t "?"
                return t
            Just t -> return t
    _ -> return t
    where
    stl = stWrap fst (\a (_, b) -> (a, b))
    str = stWrap snd (\b (a, _) -> (a, b))

idType :: String -> [Field] -> State SPLC Type
idType i fs = splcv . indiff $ idType' i fs
    where
    idType' i fs = case fs of
        [] -> clookupe i
        f : r -> do
            t <- clookupe i
            crem i
            t' <- fieldType f
            a <- checkApp t' t
            cadd i a "?"
            idType' i r

annotateE :: Exp -> State SPLC ExpT
annotateE e = case unFix e of
    EInt i -> res (EInt i) TInt
    EBool b -> res (EBool b) TBool
    EChar c -> res (EChar c) TChar
    ENil -> splcv $ do
        a <- fresh
        res ENil (TList a)
    ETuple e1 e2 -> do
        e1 <- annotateE e1
        e2 <- annotateE e2
        res (ETuple e1 e2) (TTuple (getType e1) (getType e2))
    EId i fs -> do
        t <- idType i fs
        res (EId i fs) t
    ECons i as -> do
        (es, t) <- applyCons i as
        res (ECons i es) t
    EFunCall i as -> do
        (es, t) <- applyFun i as
        res (EFunCall i es) t
    EOp1 o e -> do
        e <- annotateE e
        ft <- splcv (op1Type o)
        t <- checkApp ft (getType e)
        res (EOp1 o e) t
    EOp2 o e1 e2 -> do
        e1 <- annotateE e1
        e2 <- annotateE e2
        ft <- splcv (op2Type o)
        t <- checkApp ft (TTuple (getType e1) (getType e2))
        res (EOp2 o e1 e2) t
    where
    res e t = return $ expt e t
