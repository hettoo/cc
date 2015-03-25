module SPL.Typer where
import SPL.Algebra
import Context
import Utils

type Cv = Context Type
type Cf = Context (Type, Type)
type SPLC = (Cv, Cf)

instance DistinctSequence Type where
    createN n = TPoly ("?" ++ show n)

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

covers :: Bool -> Type -> Type -> Maybe (Context Type)
covers = covers' cnew
    where
    covers' c b t u = case (t, u) of
        (TTuple t1 t2, TTuple t1' t2') ->
            case covers' c b t1 t1' of
                Nothing -> Nothing
                Just c' -> covers' c' b t2 t2'
        (TList t', TList t'') -> covers' c b t' t''
        (TPoly i, TPoly j) ->
            if b then
                if i == j then
                    Just c
                else
                    Nothing
            else
                case clookup c i of
                    Nothing -> Just (caddr c i u)
                    Just t' -> Just c -- TODO: unsure about this
        (TPoly i, _) -> Just (caddr c i u)
        _ -> if t == u then Just c else Nothing

treplace :: Context Type -> Type -> Type
treplace c t = case t of
    TTuple t1 t2 -> TTuple (treplace c t1) (treplace c t2)
    TList t' -> TList (treplace c t')
    _ -> creplace c findPoly t
    where
    findPoly t = case t of
        TPoly a -> Just a
        _ -> Nothing

-- TODO: create fresh variables here
checkApp :: (Type, Type) -> Type -> Type
checkApp (t, t') a = case covers False t a of
    Nothing -> error ("application mismatch: " ++ show t ++
        " does not cover " ++ show a)
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

initContext :: [Stmt] -> SPLC
initContext l = case l of
    [] -> (cnew, (addRead . addPrint . addIsEmpty) cnew)
        where
        addIsEmpty c = cadd c "isEmpty" (TList (TPoly "t"), TBool)
        addRead c = cadd c "read" (TVoid, TPoly "t")
        addPrint c = cadd c "print" (TPoly "t", TVoid)
    s : r -> case s of
        VarDecl t i _ -> (cadd n i t, m)
        FunDecl t i as _ -> (n, cadd m i (combineTypes (map fst as), t))
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

annotateS' :: [Type] -> SPLC -> Stmt -> (StmtT, SPLC)
annotateS' l c@(cv, cf) s = case s of
    Stmts s -> let (s', c') = annotateMulti l c s in (StmtsT s', c)
    VarDecl t i e -> (VarDeclT t i e', (cadd cv' i t, cf'))
        where
        (e', (cv', cf')) = ae e
    FunDecl t i as b ->
        (FunDeclT t i as (fst (annotateS' (t : l) (cv', cf') b)), (cv, cf'))
        where
        cv' = cdown (foldr (\(t, i) cv -> cadd cv i t) (cdown cv) as)
        cf' = cdown (cadd cf i (combineTypes (map fst as), t))
    FunCall i as -> (FunCallT i es, c')
        where
        (es, c') = sideMap annotateE c as
    Return m -> (ReturnT m', c')
        where
        (t, (m', c')) = case m of
            Nothing -> (TVoid, (Nothing, c))
            Just e -> case l of
                [] -> error "return outside function"
                a : l' -> case covers True t a of
                    Just _ -> (t, left Just r)
                    Nothing -> error ("invalid return type " ++ show t ++
                        "; expected " ++ show a)
                where
                r = ae e
                t = getType (fst r)
    Assign i fs e -> (AssignT i fs e', c')
        where
        (e', c') = ae e
    If e s m ->
        (IfT e' (fst $ annotateS' l c' s) (fmap (fst . annotateS' l c') m), c')
        where
        (e', c') = ae e
    While e s -> (WhileT e' (fst $ annotateS' l c' s), c')
        where
        (e', c') = ae e
    where
    ae = annotateE c

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
    EId i fs -> (EIdT i fs t, c)
        where
        t = case fs of
            [] -> clookupe cv i
            f : r -> (getType . fst) (annotateE (cadd (crem cv i) i
                (checkApp (fst $ fieldType c f) (clookupe cv i)), cf) (EId i r))
    EFunCall i as -> (EFunCallT i es
        (checkApp (clookupe cf i) (combineTypes (map getType es))), c')
        where
        (es, c') = sideMap annotateE c as
    EOp1 o e -> (EOp1T o e' (checkApp (op1Type o) (getType e')), c')
        where
        (e', c') = annotateE c e
    EOp2 o e1 e2 -> (EOp2T o r1 r2 (checkApp ft t), c3)
        where
        (r1, c1) = annotateE c e1
        (r2, c2) = annotateE c1 e2
        (ft, c3) = op2Type c2 o
        t = TTuple (getType r1) (getType r2)
