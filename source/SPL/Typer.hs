module SPL.Typer where
import SPL.Algebra
import SPL.Unifier
import SPL.Printer
import Context
import State
import Fix
import Utils
import Control.Monad
import Data.List
import Debug.Trace

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

fieldType :: String -> Type -> State SPLC (Type, Type)
fieldType i bt = case bt of
    TCustom j _ -> do
        m <- splcd (clookupf (\(j', as, ts) -> j == j' &&
            case find ((== i) . snd) ts of
                Just _ -> True
                _ -> False))
        let (j, as, ts) = unMaybe ("field `" ++ i ++ "' not found") m
        splcv $ freshen (TCustom j (map TPoly as),
            (fst . unMaybe "?" . find ((== i) . snd)) ts)
    _ -> fail $ "not a custom type: `" ++ show bt ++ "'"
    where
    unMaybe e m = case m of
        Just x -> x
        _ -> error e

findPoly :: Type -> Maybe String
findPoly t = case t of
    TPoly a -> Just a
    _ -> Nothing

findPolys :: Type -> [String]
findPolys t = case t of
    TCustom i ts -> concatMap findPolys ts
    TPoly a -> [a]
    _ -> []

treplace :: Type -> State (Context Type) Type
treplace t = case t of
    TCustom i ts -> do
        ts <- mapM treplace ts
        return $ TCustom i ts
    _ -> creplace findPoly t

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
        return (tTuple a (tList a), tList a)
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
    tBBB = return (tTuple TBool TBool, TBool)
    taaa = do
        a <- fresh
        return (tTuple a a, a)
    taaB = do
        a <- fresh
        return (tTuple a a, TBool)

initContext :: [Stmt] -> State SPLC ()
initContext l = case l of
    [] -> do
        splcf $ do
            cadd "print" (TPoly "t", TVoid) "?"
            cadd "read" (TVoid, TInt) "?"
    s : r -> do
        case s of
            VarDecl t i _ -> do
                caddvar i t
            FunDecl t i as _ -> do
                caddfun i as t
            DataDecl i as cs -> do
                let ts = concatMap (map fst . snd) cs
                m <- splcd $ clookupf (\(j, _, _) -> i == j)
                case m of
                    Nothing -> let fs = map snd (concat (map snd cs)) in
                        if isMinimal fs then
                            sequence_ (map addCons cs)
                        else
                            fail "duplicate field name"
                    _ -> fail $ "duplicate type " ++ i
                where
                addCons (c, ts) = if all (`elem` as)
                    (concatMap (findPolys . fst) ts)
                    then splcd $ cadd c (i, as, ts)
                        ("duplicate constructor " ++ c)
                    else fail $ "free polymorphic data type parameter"
            _ -> return ()
        initContext r

guaranteeReturn :: (String -> [String]) -> StmtT -> Bool
guaranteeReturn f s = case s of
    Stmts l -> any (guaranteeReturn f) l
    If _ s' m -> case m of
        Nothing -> False
        Just s'' -> guaranteeReturn f s' && guaranteeReturn f s''
    Case e bs -> let TCustom i _ = getType e in
        if length bs /= length (f i) then
            False
        else
            all (guaranteeReturn f) (map snd bs)
    Return m -> True
    _ -> False

guaranteeReturns :: (String -> [String]) -> [StmtT] -> [StmtT]
guaranteeReturns f = flip foldr [] $ \s r -> case s of
    FunDecl t i as b ->
        if t == TVoid || guaranteeReturn f b then
            s : r
        else
            error $ "function " ++ i ++ " may not return a value"
    _ -> s : r

annotateProgram :: [Stmt] -> [StmtT]
annotateProgram l =
    (do
        initContext l
        splcv cdown
        splcf cdown
        l <- annotateMulti [] l
        return $ guaranteeReturns (consList l) l
    ) >!> SPLC {cv = cnew, cf = cnew, cd = cnew}
    where
    consList l i = concatMap (consList' i) l
    consList' i s = case s of
        DataDecl j _ cs -> if i == j then map fst cs else []
        _ -> []

annotateMulti :: [(Type, String)] -> [Stmt] -> State SPLC [StmtT]
annotateMulti l = mapM (annotateS l)

checkPoly :: [Type] -> Type -> State SPLC Bool
checkPoly l t = checkPoly' (listPoly t) (concat $ map listPoly l)
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
        TCustom _ ts -> concat $ map listPoly ts
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
    let a = combineTypes (map getType as)
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
    let ats = map getType es
    ts <- return $ map fst ts
    let atss = combineTypes ats
    let tss = combineTypes ts
    case unifiablef atss tss of
        True -> do
            let c = sequence (map addArg (zip ts ats)) >@> cnew
            let rts = map (applyUnificationT c) (map TPoly as)
            rts <- splcv $ ST $ \cv ->
                let (r, (cv', _)) = apply (mapM freshenT rts) (cv, cnew) in
                Left (r, cv')
            return (es, TCustom j rts)
        False -> fail $ "constructor mismatch: expected type `" ++
            simplePrint tss ++ "' cannot be unified with given type `" ++
            simplePrint atss ++ "'"
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
    VarDecl et i m -> do
        b <- checkPoly (map fst l) et
        if b then case m of
            Just e -> do
                e <- annotateE e
                let t = getType e in
                    if unifiablef t et then do
                        caddvar i et
                        return $ VarDecl et i (Just e)
                    else
                        fail $ "assignment mismatch: expected type `" ++
                            simplePrint et ++
                            "' cannot be unified with given type `" ++
                            simplePrint t ++ "' (variable " ++ i ++ ")"
            Nothing -> do
                caddvar i et
                return $ VarDecl et i Nothing
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
        (vt, tfs) <- idType i fs
        e <- annotateE e
        let t = getType e in
            if unifiablef t vt then
                return $ Assign i tfs e
            else
                fail $ "assignment mismatch: expected type `" ++
                    simplePrint vt ++
                    "' cannot be unified with given type `" ++
                    simplePrint t ++ "' (variable " ++ i ++ ")"
    Case e bs -> do
        if isMinimal (map fst bs) then ids else
            fail $ "duplicate constructor in case"
        e <- annotateE e
        let t = getType e
        j <- case t of
            TCustom j _ -> return j
            _ -> fail $ "not a matchable type: " ++ simplePrint t
        bs <- sequence $ map (annotateCase j) bs
        return $ Case e bs
        where
        annotateCase j (i, s) = do
            (j', _, _) <- splcd (clookupe i)
            case j == j' of
                True -> do
                    s <- annotateS l s
                    return (i, s)
                False -> fail $ "constructor " ++ i ++
                    " belongs to type " ++ j' ++ " rather than " ++ j
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
    TCustom i ts -> do
        ts <- mapM freshenT ts
        return $ TCustom i ts
    TPoly i -> do
        m <- str $ clookup i
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

idType :: String -> [String] -> State SPLC (Type, [(String, Type)])
idType i fs = do
    cv <- splcv getState
    t <- idType' i fs
    splcv . st $ const cv
    return t
    where
    idType' i fs = case fs of
        [] -> do
            t <- splcv $ clookupe i
            return (t, [])
        f : r -> do
            t <- splcv $ clookupe i
            splcv $ crem i
            t' <- fieldType f t
            a <- checkApp t' t
            splcv $ cadd i a "?"
            (tres, tfs) <- idType' i r
            return (tres, (f, t) : tfs)

annotateE :: Exp -> State SPLC ExpT
annotateE e = case unFix e of
    EInt i -> res (EInt i) TInt
    EBool b -> res (EBool b) TBool
    EChar c -> res (EChar c) TChar
    ENil -> splcv $ do
        a <- fresh
        res ENil (tList a)
    ETuple e1 e2 -> do
        e1 <- annotateE e1
        e2 <- annotateE e2
        res (ETuple e1 e2) (tTuple (getType e1) (getType e2))
    EId i fs -> do
        (t, tfs) <- idType i fs
        res (EId i tfs) t
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
        t <- checkApp ft (tTuple (getType e1) (getType e2))
        res (EOp2 o e1 e2) t
    where
    res e t = return $ expt e t
