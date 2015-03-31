module SPL.Typer where
import SPL.Algebra
import Context
import State
import Utils
import SPL.Printer

type Cv = Context Type
type Cf = Context (Type, Type)
type SPLC = (Cv, Cf)

splcv :: State Cv a -> State SPLC a
splcv = stl

splcf :: State Cf a -> State SPLC a
splcf = str

forgetv :: State SPLC a -> State SPLC a
forgetv (ST f) = ST $ \c@(cv, _) -> case f c of
    Left (a, (_, cf)) -> Left (a, (cv, cf))
    Right e -> Right e

forgetf :: State SPLC a -> State SPLC a
forgetf (ST f) = ST $ \c@(_, cf) -> case f c of
    Left (a, (cv, _)) -> Left (a, (cv, cf))
    Right e -> Right e

caddvar :: String -> Type -> State SPLC ()
caddvar i t = splcv $ cadd i t ("redefined variable " ++ i)

caddfun :: String -> [(Type, String)] -> Type -> State SPLC ()
caddfun i as t = splcf $
    cadd i (combineTypes (map fst as), t) ("redefined function " ++ i)

instance DistinctSequence Type where
    createN n = TPoly ("?" ++ show n)

isFlexible :: String -> Bool
isFlexible s = case s of
    '?' : r -> True
    _ -> False

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

combineTypes :: [Type] -> Type
combineTypes l = case l of
    [] -> TVoid
    a : r -> case r of
        [] -> a
        _ -> TTuple a (combineTypes r)

unify :: Type -> Type -> Maybe (Context Type)
unify t u = let (b, c) = apply (unify' t u) cnew in
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

unifiable :: Type -> Type -> Bool
unifiable t u = case unify t u of
    Nothing -> False
    Just _ -> True

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
checkApp (t, t') a = case unify t a of
    Nothing -> fail $ "application mismatch: `" ++ simplePrint t ++
        "' does not cover `" ++ simplePrint a ++ "'"
    Just c -> return $ treplace t' >!> c

op1Type :: Op1 -> (Type, Type)
op1Type o = case o of
    ONot -> (TBool, TBool)
    ONeg -> (TInt, TInt)

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

data StmtT =
    StmtsT [StmtT]
    | VarDeclT Type String ExpT
    | FunDeclT Type String [(Type, String)] StmtT
    | FunCallT String [ExpT]
    | ReturnT (Maybe ExpT)
    | AssignT String [Field] ExpT
    | IfT ExpT StmtT (Maybe StmtT)
    | WhileT ExpT StmtT
    deriving (Eq, Show)

data ExpT =
    EIntT Int Type
    | EBoolT Bool Type
    | ECharT Char Type
    | ENilT Type
    | ETupleT ExpT ExpT Type
    | EIdT String [Field] Type
    | EFunCallT String [ExpT] Type
    | EOp1T Op1 ExpT Type
    | EOp2T Op2 ExpT ExpT Type
    deriving (Eq, Show)

getType :: ExpT -> Type
getType e = case e of
    EIntT _ t -> t
    EBoolT _ t -> t
    ECharT _ t -> t
    ENilT t -> t
    ETupleT _ _ t -> t
    EIdT _ _ t -> t
    EFunCallT _ _ t -> t
    EOp1T _ _ t -> t
    EOp2T _ _ _ t -> t

initContext :: [Stmt] -> State SPLC ()
initContext l = case l of
    [] -> do
        splcv (return ())
        splcf $ do
            cadd "isEmpty" (TList (TPoly "t"), TBool) "?"
            cadd "read" (TVoid, TPoly "t") "?"
            cadd "print" (TPoly "t", TVoid) "?"
    s : r -> case s of
        VarDecl t i _ -> do
            caddvar i t
            initContext r
        FunDecl t i as _ -> do
            caddfun i as t
            rec
        _ -> rec
        where
        rec = initContext r

guaranteeReturn :: Stmt -> Bool
guaranteeReturn s = case s of
    Stmts l -> guaranteeReturn' l
    If e s' m -> case m of
        Nothing -> False
        Just s'' -> guaranteeReturn s' && guaranteeReturn s''
    Return m -> True
    _ -> False
    where
    guaranteeReturn' l = case l of
        [] -> False
        s' : l' -> guaranteeReturn s' || guaranteeReturn' l'


guaranteeReturns :: [Stmt] -> [Stmt]
guaranteeReturns l = case l of
    [] -> []
    s : r -> case s of
        FunDecl t i as b ->
            if t == TVoid || guaranteeReturn b then
                rest
            else
                error $ "function " ++ i ++ " may not return a value"
        _ -> rest
        where
        rest = s : guaranteeReturns r

annotateProgram :: [Stmt] -> [StmtT]
annotateProgram l = let
    l' = guaranteeReturns l
    in (do
        initContext l'
        splcv cdown
        splcf cdown
        annotateMulti [] l'
    ) >!> (cnew, cnew)

annotateMulti :: [Type] -> [Stmt] -> State SPLC [StmtT]
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

applyFun :: String -> [Exp] -> State SPLC ([ExpT], Type)
applyFun i as = do
    t <- splcf (clookupe i)
    t <- splcv (freshen t)
    es <- mapM annotateE as
    a <- checkApp t (combineTypes (map getType es))
    return (es, a)

annotateS :: [Type] -> Stmt -> State SPLC StmtT
annotateS l s = case s of
    Stmts s -> do
        s <- annotateMulti l s
        return $ StmtsT s
    VarDecl t i e -> do
        b <- checkPoly l t
        if b then do
            e <- annotateE e
            let et = getType e in
                if unifiable et t then do
                    caddvar i t
                    return (VarDeclT t i e)
                else
                    fail $ "assignment mismatch: `" ++
                        simplePrint et ++ "' does not cover `" ++
                        simplePrint t ++ "'"
        else
            fail ("free polymorphic variable " ++ i)
    FunDecl t i as b -> do
        caddfun i as t
        forgetv $ do
            splcv cdown
            splcv (mapM_ addArg as)
            splcf cdown
            splcv cdown
            b <- annotateS (t : l) b
            return $ FunDeclT t i as b
            where
            addArg (t, i) =
                cadd i t ("duplicate formal arguments for function " ++ i)
    FunCall i as -> do
        (es, _) <- applyFun i as
        return $ FunCallT i es
    Return m -> case l of
        [] -> fail "return outside function"
        a : l' -> case m of
            Nothing -> return $ ReturnT Nothing
            Just e -> do
                e <- annotateE e
                let t = getType e in
                    if unifiable t a then
                        return $ ReturnT (Just e)
                    else
                        fail $ "invalid return type `" ++
                            simplePrint t ++ "'; expected `" ++
                            simplePrint a ++ "'"
    Assign i fs e -> do
        vt <- idType i fs
        e <- annotateE e
        let t = getType e in
            if unifiable t vt then
                return $ AssignT i fs e
            else
                fail $ "assignment mismatch: `" ++ simplePrint t ++
                    "' does not cover `" ++ simplePrint vt ++ "'"
    If e s m -> do
        e <- annotateE e
        s <- indiff (annotateS l s)
        case m of
            Nothing -> return $ IfT e s Nothing
            Just m -> do
                m <- indiff (annotateS l m)
                return $ IfT e s (Just m)
    While e s -> do
        e <- annotateE e
        s <- indiff (annotateS l s)
        return $ WhileT e s

freshen :: (Type, Type) -> State Cv (Type, Type)
freshen t = ST $ \cv -> let
    (r, (cv', _)) = apply (freshen' t) (cv, cnew)
    in Left (r, cv')
    where
    freshen' (t1, t2) = do
        t1 <- freshenT t1
        t2 <- freshenT t2
        return (t1, t2)
    freshenT t = case t of
        TTuple t1 t2 -> do
            (t1, t2) <- freshen' (t1, t2)
            return (TTuple t1 t2)
        TList t -> do
            t <- freshenT t
            return (TList t)
        TPoly i -> do
            m <- str (clookup i)
            case m of
                Nothing -> do
                    t <- stl (fresh)
                    str (cadd i t "?")
                    return t
                Just t -> return t
        _ -> return t

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
annotateE e = case e of
    EInt i -> return $ EIntT i TInt
    EBool b -> return $ EBoolT b TBool
    EChar a -> return $ ECharT a TChar
    ENil -> splcv $ do
        a <- fresh
        return $ ENilT (TList a)
    ETuple e1 e2 -> do
        e1 <- annotateE e1
        e2 <- annotateE e2
        return $ ETupleT e1 e2 (TTuple (getType e1) (getType e2))
    EId i fs -> do
        t <- idType i fs
        return $ EIdT i fs t
    EFunCall i as -> do
        (es, a) <- applyFun i as
        return $ EFunCallT i es a
    EOp1 o e -> do
        e <- annotateE e
        a <- checkApp (op1Type o) (getType e)
        return $ EOp1T o e a
    EOp2 o e1 e2 -> do
        e1 <- annotateE e1
        e2 <- annotateE e2
        ft <- splcv (op2Type o)
        a <- checkApp ft (TTuple (getType e1) (getType e2))
        return $ EOp2T o e1 e2 a
