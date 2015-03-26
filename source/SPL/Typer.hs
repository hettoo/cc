module SPL.Typer where
import SPL.Algebra
import Context
import Utils
import SPL.Printer

type Cv = Context Type
type Cf = Context (Type, Type)
type SPLC = (Cv, Cf)

instance DistinctSequence Type where
    createN n = TPoly ("?" ++ show n)

isFlexible :: String -> Bool
isFlexible s = case s of
    '?' : r -> True
    _ -> False

fieldType :: SPLC -> Field -> ((Type, Type), SPLC)
fieldType c@(cv, cf) f = case f of
    Head -> ((TList a, a), (cv', cf))
    Tail -> ((TList a, TList a), (cv', cf))
    First -> ((TTuple a b, a), (cv'', cf))
    Second -> ((TTuple a b, b), (cv'', cf))
    where
    (a, cv') = fresh cv
    (b, cv'') = fresh cv'

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
    Nothing -> error ("application mismatch: `" ++ simplePrint t ++
        "' does not cover `" ++ simplePrint a ++ "'")
    Just c -> treplace c t'

op1Type :: Op1 -> (Type, Type)
op1Type o = case o of
    ONot -> (TBool, TBool)
    ONeg -> (TInt, TInt)

op2Type :: SPLC -> Op2 -> ((Type, Type), SPLC)
op2Type c@(cv, cf) o = case o of
    OCons -> ((TTuple a (TList a), TList a), (cv', cf))
    OAnd -> ((TTuple TBool TBool, TBool), c)
    OOr -> ((TTuple TBool TBool, TBool), c)
    OEq -> ((TTuple a a, TBool), (cv', cf))
    ONeq -> ((TTuple a a, TBool), (cv', cf))
    OLt -> ((TTuple a a, TBool), (cv', cf))
    OGt -> ((TTuple a a, TBool), (cv', cf))
    OLe -> ((TTuple a a, TBool), (cv', cf))
    OGe -> ((TTuple a a, TBool), (cv', cf))
    OPlus -> ((TTuple a a, a), (cv', cf))
    OMinus -> ((TTuple a a, a), (cv', cf))
    OTimes -> ((TTuple a a, a), (cv', cf))
    ODiv -> ((TTuple a a, a), (cv', cf))
    OMod -> ((TTuple a a, a), (cv', cf))
    where
    (a, cv') = fresh cv

sideMap :: (c -> a -> (b, c)) -> c -> [a] -> ([b], c)
sideMap f c l = case l of
    [] -> ([], c)
    a : r -> let
        (b, c') = f c a
        (bs, c'') = sideMap f c' r
        in (b : bs, c'')

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
        VarDecl t i _ -> (unMaybe (cadd n i t), m)
        FunDecl t i as _ ->
            (n, unMaybe (cadd m i (combineTypes (map fst as), t)))
        _ -> p
        where
        p@(n, m) = initContext r

annotateProgram :: [Stmt] -> [StmtT]
annotateProgram l = fst $ annotateMulti []
    (pair (cdown, cdown) (initContext l)) l

annotateMulti :: [Type] -> SPLC -> [Stmt] -> ([StmtT], SPLC)
annotateMulti l = sideMap (annotateS' l)

annotateS :: SPLC -> Stmt -> (StmtT, SPLC)
annotateS = annotateS' []

checkPoly :: [Type] -> Context Type -> Type -> Bool
checkPoly l c t = checkPoly' (listPoly t) (concat (map listPoly l)) c
    where
    checkPoly' r l c = case r of
        [] -> True
        a : r' -> if a `elem` l || cfindf c (\t -> a `elem` listPoly t) then
                checkPoly' r' l c
            else
                False
    listPoly t = case t of
        TPoly _ -> [t]
        TTuple t1 t2 -> listPoly t1 ++ listPoly t2
        TList t' -> listPoly t'
        _ -> []

annotateS' :: [Type] -> SPLC -> Stmt -> (StmtT, SPLC)
annotateS' l c@(cv, cf) s = case s of
    Stmts s -> let (s', c') = annotateMulti l c s in (StmtsT s', c)
    VarDecl t i e ->
        if checkPoly l cv t then
            case unify et t of
                Nothing -> error ("assignment mismatch: `" ++ simplePrint et ++
                    "' does not cover `" ++ simplePrint t ++ "'")
                Just _ -> case cadd cv' i t of
                    Nothing -> error ("redefined variable " ++ i)
                    Just cv'' -> (VarDeclT t i e', (cv'', cf'))
                -- TODO: apply unification?
        else
            error ("unbounded polymorphic variable " ++ i)
        where
        (e', (cv', cf')) = ae e
        et = getType e'
    FunDecl t i as b ->
        case cadd cf i (combineTypes (map fst as), t) of
            Nothing -> error ("redefined function" ++ i)
            Just cf' -> case foldr addArg (Just (cdown cv)) as of
                Nothing ->
                    error ("duplicate formal arguments for function " ++ i)
                Just cv' -> (FunDeclT t i as
                    (fst (annotateS' (t : l) (cv'', cf'') b)), (cv, cf''))
                    where
                    cv'' = cdown cv'
                    cf'' = cdown cf'
            where
            addArg (t, i) m = case m of
                Nothing -> Nothing
                Just cv -> cadd cv i t
    FunCall i as -> (FunCallT i es, c')
        where
        (es, c') = sideMap annotateE c as
    Return m -> (ReturnT m', c')
        where
        (t, (m', c')) = case m of
            Nothing -> (TVoid, (Nothing, c))
            Just e -> case l of
                [] -> error "return outside function"
                a : l' -> case unify t a of
                    Just _ -> (t, left Just r)
                    Nothing -> error ("invalid return type `" ++
                        simplePrint t ++ "'; expected `" ++
                        simplePrint a ++ "'")
                where
                r = ae e
                t = getType (fst r)
    Assign i fs e -> case unify t vt of
        Nothing -> error ("assignment mismatch: `" ++ simplePrint t ++
            "' does not cover `" ++ simplePrint vt ++ "'")
        Just _ -> (AssignT i fs e', c')
        -- TODO: apply unification?
        where
        vt = idType c i fs
        (e', c') = ae e
        t = getType e'
    If e s m ->
        (IfT e' (fst $ annotateS' l c' s) (fmap (fst . annotateS' l c') m), c')
        where
        (e', c') = ae e
    While e s -> (WhileT e' (fst $ annotateS' l c' s), c')
        where
        (e', c') = ae e
    where
    ae = annotateE c

freshen :: Context Type -> (Type, Type) -> ((Type, Type), Context Type)
freshen = freshen' cnew
    where
    freshen' r c (t1, t2) = let (r', (t1', c')) = freshenT r c t1 in
        left (\t2' -> (t1', t2')) (snd (freshenT r' c' t2))
    freshenT r c t = case t of
        TTuple t1 t2 -> let
            (r', (t1', c')) = freshenT r c t1
            (r'', (t2', c'')) = freshenT r' c' t2
            in (r'', (TTuple t1' t2', c''))
        TList t' -> let (r', (t'', c')) = freshenT r c t' in
            (r', (TList t'', c'))
        TPoly i -> case clookup r i of
            Nothing -> (unMaybe (cadd r i t'), f)
                where
                f@(t', _) = fresh c
            Just t' -> (r, (t', c))
        _ -> (r, (t, c))

idType :: SPLC -> String -> [Field] -> Type
idType c@(cv, cf) i fs = case fs of
    [] -> clookupe cv i
    f : r -> idType (unMaybe (cadd (crem cv i) i
        (checkApp (fst $ fieldType c f) (clookupe cv i))), cf) i r

annotateE :: SPLC -> Exp -> (ExpT, SPLC)
annotateE c@(cv, cf) e = case e of
    EInt i -> (EIntT i TInt, c)
    EBool b -> (EBoolT b TBool, c)
    EChar a -> (ECharT a TChar, c)
    ENil -> (ENilT (TList t), (cv', cf))
        where
        (t, cv') = fresh cv
    ETuple e1 e2 -> (ETupleT r1 r2 (TTuple (getType r1) (getType r2)), c2)
        where
        (r1, c1) = annotateE c e1
        (r2, c2) = annotateE c1 e2
    EId i fs -> (EIdT i fs (idType c i fs), c)
    EFunCall i as -> (EFunCallT i es
        (checkApp t (combineTypes (map getType es))), c')
        where
        (t, cv') = freshen cv (clookupe cf i)
        (es, c') = sideMap annotateE (cv', cf) as
    EOp1 o e -> (EOp1T o e' (checkApp (op1Type o) (getType e')), c')
        where
        (e', c') = annotateE c e
    EOp2 o e1 e2 -> (EOp2T o r1 r2 (checkApp ft t), c3)
        where
        (r1, c1) = annotateE c e1
        (r2, c2) = annotateE c1 e2
        (ft, c3) = op2Type c2 o
        t = TTuple (getType r1) (getType r2)
