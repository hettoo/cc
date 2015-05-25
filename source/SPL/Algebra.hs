{-# LANGUAGE DeriveFunctor #-}
module SPL.Algebra where
import Fix

data PStmt f e =
    Stmts [PStmt f e]
    | DataDecl String [String] [(String, [(Type, String)])]
    | VarDecl Type String (Maybe e)
    | FunDecl Type String [(Type, String)] (PStmt f e)
    | FunCall String [e]
    | Return (Maybe e)
    | Assign String [f] e
    | If e (PStmt f e) (Maybe (PStmt f e))
    | While e (PStmt f e)
    | Case e [(String, PStmt f e)]
    deriving (Eq, Show)

type Stmt = PStmt String Exp
type StmtT = PStmt (String, Type) ExpT

data Type =
    TPoly String
    | TCustom String [Type]
    | TInt
    | TBool
    | TChar
    | TVoid
    deriving (Eq, Show)

tTuple :: Type -> Type -> Type
tTuple t1 t2 = TCustom "Tuple" [t1, t2]

tList :: Type -> Type
tList t = TCustom "List" [t]

data Op1 =
    ONot
    | ONeg
    deriving (Eq, Show)

data Op2 =
    OCons
    | OAnd
    | OOr
    | OEq
    | ONeq
    | OLt
    | OGt
    | OLe
    | OGe
    | OPlus
    | OMinus
    | OTimes
    | ODiv
    | OMod
    deriving (Eq, Show)

data PExp f e =
    EInt Int
    | EBool Bool
    | EChar Char
    | ENil
    | ETuple e e
    | EId String [f]
    | ECons String [e]
    | EFunCall String [e]
    | EOp1 Op1 e
    | EOp2 Op2 e e
    deriving (Eq, Show, Functor)

data PExpT e = PExpT {expC :: PExp (String, Type) e, typeC :: Type}

type Exp = Fix (PExp String)
type ExpT = Fix PExpT

expt :: PExp (String, Type) ExpT -> Type -> ExpT
expt e t = Fix $ PExpT {expC = e, typeC = t}

getType :: ExpT -> Type
getType = typeC . unFix

combineTypes :: [Type] -> Type
combineTypes l = case l of
    [] -> TVoid
    a : r -> if null r then a else tTuple a (combineTypes r)
