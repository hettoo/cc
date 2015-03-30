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

instance DistinctSequence Type where
    createN n = TPoly ("?" ++ show n)

isFlexible :: String -> Bool
isFlexible s = case s of
    '?' : r -> True
    _ -> False

fieldType :: Field -> State (Context Type) (Type, Type)
fieldType f = case f of
    Head -> fresh >>- \a -> (TList a, a)
    Tail -> fresh >>- \a -> (TList a, TList a)
    First -> fresh >>= \a -> fresh >>- \b -> (TTuple a b, a)
    Second -> fresh >>= \a -> fresh >>- \b -> (TTuple a b, b)

combineTypes :: [Type] -> Type
combineTypes l = case l of
    [] -> TVoid
    a : r -> case r of
        [] -> a
        _ -> TTuple a (combineTypes r)

unify :: Type -> Type -> Maybe (Context Type)
unify = unify' cnew
    where
    unify' c t u = case rewrite (t, u) of
        (TTuple t1 t2, TTuple t1' t2') ->
            case unify' c t1 t1' of
                Nothing -> Nothing
                Just c' -> unify' c' t2 t2'
        (TList t', TList t'') -> unify' c t' t''
        (TPoly i, TPoly j) ->
            if i == j then
                Just c
            else
                if isFlexible i then
                    caddr c i u
                else
                    if isFlexible j then
                        caddr c j t
                    else
                        Nothing
        (TPoly i, _) -> if isFlexible i then caddr c i u else Nothing
        (_, TPoly j) -> if isFlexible j then caddr c j t else Nothing
        (t', u') -> if t' == u' then Just c else Nothing
        where
        rewrite (t, u) = case (clookup c (name t), clookup c (name u)) of
            (Nothing, Nothing) -> (t, u)
            (Nothing, Just u') -> (t, u')
            (Just t', Nothing) -> (t', u)
            (Just t', Just u') -> (t', u')
        name t = case t of
            TPoly i -> i
            _ -> ""

treplace :: Context Type -> Type -> Type
treplace c t = case t of
    TTuple t1 t2 -> TTuple (treplace c t1) (treplace c t2)
    TList t' -> TList (treplace c t')
    _ -> creplace c findPoly t
    where
    findPoly t = case t of
        TPoly a -> Just a
        _ -> Nothing

checkApp :: (Type, Type) -> Type -> Type
checkApp (t, t') a = case unify t a of
    Nothing -> error $ "application mismatch: `" ++ simplePrint t ++
        "' does not cover `" ++ simplePrint a ++ "'"
    Just c -> treplace c t'

op1Type :: Op1 -> (Type, Type)
op1Type o = case o of
    ONot -> (TBool, TBool)
    ONeg -> (TInt, TInt)

op2Type :: Op2 -> State SPLC (Type, Type)
op2Type o = splcv $ case o of
    OCons -> fresh >>- \a -> (TTuple a (TList a), TList a)
    OAnd -> return (TTuple TBool TBool, TBool)
    OOr -> return (TTuple TBool TBool, TBool)
    OEq -> fresh >>- \a -> (TTuple a a, TBool)
    ONeq -> fresh >>- \a -> (TTuple a a, TBool)
    OLt -> fresh >>- \a -> (TTuple a a, TBool)
    OGt -> fresh >>- \a -> (TTuple a a, TBool)
    OLe -> fresh >>- \a -> (TTuple a a, TBool)
    OGe -> fresh >>- \a -> (TTuple a a, TBool)
    OPlus -> fresh >>- \a -> (TTuple a a, a)
    OMinus -> fresh >>- \a -> (TTuple a a, a)
    OTimes -> fresh >>- \a -> (TTuple a a, a)
    ODiv -> fresh >>- \a -> (TTuple a a, a)
    OMod -> fresh >>- \a -> (TTuple a a, a)

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

unMaybe :: Maybe a -> a
unMaybe m = case m of
    Nothing -> error "unexpected emptiness"
    Just a -> a

initContext :: [Stmt] -> SPLC
initContext l = case l of
    [] -> (cnew, (addRead . addPrint . addIsEmpty) cnew)
        where
        addIsEmpty c = unMaybe $ cadd c "isEmpty" (TList (TPoly "t"), TBool)
        addRead c = unMaybe $ cadd c "read" (TVoid, TPoly "t")
        addPrint c = unMaybe $ cadd c "print" (TPoly "t", TVoid)
    s : r -> case s of
        VarDecl t i _ -> case cadd n i t of
            Nothing -> error $ "redefined variable " ++ i
            Just n' -> (n', m)
        FunDecl t i as _ -> case cadd m i (combineTypes (map fst as), t) of
            Nothing -> error $ "redefined function " ++ i
            Just m' -> (n, m')
        _ -> p
        where
        p@(n, m) = initContext r

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
    ST f = annotateMulti [] l'
    in fst . f $ pair (cdown, cdown) (initContext l')

annotateMulti :: [Type] -> [Stmt] -> State SPLC [StmtT]
annotateMulti l = mapM (annotateS' l)

annotateS :: Stmt -> State SPLC StmtT
annotateS = annotateS' []

checkPoly :: [Type] -> Type -> State SPLC Bool
checkPoly l t = checkPoly' (listPoly t) (concat (map listPoly l))
    where
    checkPoly' r l = case r of
        [] -> return True
        a : r' -> ST $ \c@(cv, _) ->
            if a `elem` l || cfindf cv (\t -> a `elem` listPoly t) then
                let ST f = checkPoly' r' l in f c
            else
                (False, c)
    listPoly t = case t of
        TPoly _ -> [t]
        TTuple t1 t2 -> listPoly t1 ++ listPoly t2
        TList t' -> listPoly t'
        _ -> []

annotateS' :: [Type] -> Stmt -> State SPLC StmtT
annotateS' l s = case s of
    Stmts s -> annotateMulti l s >>- StmtsT
    VarDecl t i e -> checkPoly l t >>= \b ->
        if b then
            annotateE e >>= \e' -> let et = getType e' in
                case unify et t of
                    Nothing -> error $ "assignment mismatch: `" ++
                        simplePrint et ++ "' does not cover `" ++
                        simplePrint t ++ "'"
                    Just _ -> ST $ \c@(cv, cf) -> case cadd cv i t of
                        Nothing -> error $ "redefined variable " ++ i
                        Just cv' -> (VarDeclT t i e', (cv', cf))
        else
            error ("free polymorphic variable " ++ i)
    FunDecl t i as b -> ST $ \c@(cv, cf) ->
        case cadd cf i (combineTypes (map fst as), t) of
            Nothing -> error $ "redefined function " ++ i
            Just cf' -> case foldr addArg (Just (cdown cv)) as of
                Nothing ->
                    error $ "duplicate formal arguments for function " ++ i
                Just cv' -> let ST f = annotateS' (t : l) b in
                    (FunDeclT t i as (fst (f (cv'', cf''))), (cv, cf''))
                    where
                    cv'' = cdown cv'
                    cf'' = cdown cf'
            where
            addArg (t, i) m = case m of
                Nothing -> Nothing
                Just cv -> cadd cv i t
    FunCall i as -> mapM annotateE as >>- \es -> FunCallT i es
    Return m -> case l of
        [] -> error "return outside function"
        a : l' -> case m of
            Nothing -> return $ ReturnT Nothing
            Just e -> annotateE e >>- \e' -> let t = getType e' in
                case unify t a of
                    Just _ -> ReturnT (Just e')
                    Nothing -> error $ "invalid return type `" ++
                        simplePrint t ++ "'; expected `" ++ simplePrint a ++ "'"
    Assign i fs e -> idType i fs >>= \vt -> annotateE e >>- \e' ->
        let t = getType e' in case unify t vt of
            Nothing -> error $ "assignment mismatch: `" ++ simplePrint t ++
                "' does not cover `" ++ simplePrint vt ++ "'"
            Just _ -> AssignT i fs e'
    If e s m -> annotateE e >>= \e' -> indiff (annotateS' l s) >>= \s' ->
        case m of
            Nothing -> return $ IfT e' s' Nothing
            Just m' -> indiff (annotateS' l m') >>- \m'' -> IfT e' s' (Just m'')
    While e s -> annotateE e >>= \e' ->
        indiff (annotateS' l s) >>- \s' -> WhileT e' s'

freshen :: (Type, Type) -> State Cv (Type, Type)
freshen t = let ST f = freshen' t in ST $ \cv -> right fst (f (cv, cnew))
    where
    freshen' (t1, t2) = freshenT t1 >>= \t1' ->
        freshenT t2 >>- \t2' -> (t1', t2')
    freshenT t = case t of
        TTuple t1 t2 -> freshenT t1 >>= \t1' ->
            freshenT t2 >>- \t2' -> (TTuple t1' t2')
        TList t' -> freshenT t' >>- \t'' -> TList t''
        TPoly i -> ST $ \(cv, r) -> case clookup r i of
            Nothing -> let
                ST f = fresh
                p@(t', cv') = f cv
                in (t', (cv', unMaybe (cadd r i t')))
            Just t' -> (t', (cv, r))
        _ -> return t

idType :: String -> [Field] -> State SPLC Type
idType i fs = splcv . indiff $ idType' i fs
    where
    idType' i fs = ST $ \cv -> case fs of
        [] -> (clookupe cv i, cv)
        f : r -> let
            ST g = fieldType f
            ST h = idType' i r
            in h (unMaybe (cadd (crem cv i) i
                (checkApp (fst (g cv)) (clookupe cv i))))

annotateE :: Exp -> State SPLC ExpT
annotateE e = case e of
    EInt i -> return $ EIntT i TInt
    EBool b -> return $ EBoolT b TBool
    EChar a -> return $ ECharT a TChar
    ENil -> splcv $ fresh >>- \t -> ENilT (TList t)
    ETuple e1 e2 -> annotateE e1 >>= \e1' -> annotateE e2 >>- \e2' ->
        ETupleT e1' e2' (TTuple (getType e1') (getType e2'))
    EId i fs -> idType i fs >>- \t -> EIdT i fs t
    EFunCall i as -> ST $ \(cv, cf) -> let
        ST f = freshen (clookupe cf i)
        (t, cv') = f cv
        ST g = mapM annotateE as
        (es, c') = g (cv', cf)
        in (EFunCallT i es (checkApp t (combineTypes (map getType es))), c')
    EOp1 o e -> annotateE e >>- \e' ->
        EOp1T o e' (checkApp (op1Type o) (getType e'))
    EOp2 o e1 e2 -> annotateE e1 >>= \e1' -> annotateE e2 >>= \e2' ->
        op2Type o >>- \ft ->
            EOp2T o e1' e2' (checkApp ft (TTuple (getType e1') (getType e2')))
