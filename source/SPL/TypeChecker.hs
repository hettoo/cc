module SPL.TypeChecker where
import SPL.Algebra
import Context
import Utils

type Cv = Context Type
type Cf = Context (Type, Type)
type SPLC = (Cv, Cf)

instance DistinctSequence Type where
    createN n = TPoly ("?" ++ show n)

fieldType :: Context Type -> Field -> (Type, Type)
fieldType c f = case f of
    Head -> (TList a, a)
    Tail -> (TList a, TList a)
    First -> (TTuple a b, a)
    Second -> (TTuple a b, b)
    where
    a = fresh c
    b = fresh c -- I know!

combineTypes :: [Type] -> Type
combineTypes l = case l of
    [] -> TVoid
    a : r -> case r of
        [] -> a
        _ -> TTuple a (combineTypes r)

covers :: Type -> Type -> Bool
covers t u = case (t, u) of
    (TTuple t1 t2, TTuple t1' t2') -> covers t1 t1' && covers t2 t2'
    (TList t', TList t'') -> covers t' t''
    (TPoly a, t) -> True -- TODO: keep track of these
    _ -> t == u

checkApp :: (Type, Type) -> Type -> Type
checkApp (t, t') a = if covers t a then t' else -- TODO: unify things
    error ("application mismatch: " ++ show t ++
        " does not cover " ++ show a)

op1Type :: Op1 -> (Type, Type)
op1Type o = case o of
    ONot -> (TBool, TBool)
    ONeg -> (TInt, TBool)

op2Type :: Context Type -> Op2 -> (Type, Type)
op2Type c o = case o of
    OCons -> (TTuple a (TList a), TList a)
    OAnd -> (TTuple TBool TBool, TBool)
    OOr -> (TTuple TBool TBool, TBool)
    OEq -> (TTuple a a, TBool)
    ONeq -> (TTuple a a, TBool)
    OLt -> (TTuple a a, TBool)
    OGt -> (TTuple a a, TBool)
    OLe -> (TTuple a a, TBool)
    OGe -> (TTuple a a, TBool)
    OPlus -> (TTuple a a, a)
    OMinus -> (TTuple a a, a)
    OTimes -> (TTuple a a, a)
    ODiv -> (TTuple a a, a)
    OMod -> (TTuple a a, a)
    where
    a = fresh c

expType :: SPLC -> Exp -> Type
expType c@(cv, cf) e = case e of
    EInt i -> TInt
    EBool b -> TBool
    EChar c -> TChar
    ENil -> TList (fresh cv)
    ETuple e1 e2 -> TTuple (expType c e1) (expType c e2)
    EId i fs -> case fs of
        [] -> clookupe cv i
        f : r -> case (clookupe cv i, fieldType cv f) of
            (t, (t', t'')) ->
                if t == t' then
                    expType (cadd (crem cv i) i t'', cf) (EId i r)
                else
                    error ("invalid use of field " ++ show f)
    EFunCall i as ->
        checkApp (clookupe cf i) (combineTypes (map (expType c) as))
    EOp1 o e -> checkApp (op1Type o) (expType c e)
    EOp2 o e1 e2 ->
        checkApp (op2Type cv o) (TTuple (expType c e1) (expType c e2))

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

initContext :: [Stmt] -> SPLC
initContext l = case l of
    [] -> (cnew, cnew)
    s : r -> case s of
        VarDecl t i _ -> (cadd n i t, m)
        FunDecl t i as _ -> (n, cadd m i (combineTypes (map fst as), t))
        _ -> p
        where
        p@(n, m) = initContext r

annotateProgram :: [Stmt] -> [StmtT]
annotateProgram l = fst $ annotateMulti (pair (cdown, cdown) (initContext l)) l

annotateMulti :: SPLC -> [Stmt] -> ([StmtT], SPLC)
annotateMulti c = foldr (\s (l, c) ->
    let (r, c') = annotateS c s in (r : l, c')) ([], c)

annotateS :: SPLC -> Stmt -> (StmtT, SPLC)
annotateS c@(cv, cf) s = case s of
    Stmts l -> let (l', c') = annotateMulti c l in (StmtsT l', c')
    VarDecl t i e -> (VarDeclT t i (ae e), (cadd cv i (expType c e), cf))
    FunDecl t i as b ->
        (FunDeclT t i as (fst (annotateS (cv', cf') b)), (cv, cf'))
        where
        cv' = foldr (\(t, i) cv -> cadd cv i t) cv as
        cf' = cadd cf i (combineTypes (map fst as), t)
    FunCall i as -> (FunCallT i (map ae as), c)
    Return m -> (ReturnT (fmap ae m), c)
    Assign i fs e -> (AssignT i fs (ae e), c)
    If e s m ->
        (IfT (ae e) (fst $ annotateS c s) (fmap (fst . annotateS c) m), c)
    While e s -> (WhileT (ae e) (fst $ annotateS c s), c)
    where
    ae = annotateE c

annotateE :: SPLC -> Exp -> ExpT
annotateE c e = case e of
    EInt i -> EIntT i r
    EBool b -> EBoolT b r
    EChar c -> ECharT c r
    ENil -> ENilT r
    ETuple e1 e2 -> ETupleT (annotateE c e1) (annotateE c e2) r
    EId i fs -> EIdT i fs r
    EFunCall i as -> EFunCallT i (map (annotateE c) as) r
    EOp1 o e -> EOp1T o (annotateE c e) r
    EOp2 o e1 e2 -> EOp2T o (annotateE c e1) (annotateE c e2) r
    where
    r = expType c e
