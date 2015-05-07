module SPL.Algebra where

data PStmt e =
    Stmts [PStmt e]
    | DataDecl String [String] [(String, [(Type, String)])]
    | VarDecl Type String e
    | FunDecl Type String [(Type, String)] (PStmt e)
    | FunCall String [e]
    | Return (Maybe e)
    | Assign String [Field] e
    | If e (PStmt e) (Maybe (PStmt e))
    | While e (PStmt e)
    | Case e [(String, PStmt e)]
    deriving (Eq, Show)

type Stmt = PStmt Exp
type StmtT = PStmt ExpT

data Type =
    TPoly String
    | TCustom String [Type]
    | TInt
    | TBool
    | TChar
    | TTuple Type Type
    | TList Type
    | TVoid
    deriving (Eq, Show)

data Field =
    Head
    | Tail
    | First
    | Second
    deriving (Eq, Show)

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

data Exp =
    EInt Int
    | EBool Bool
    | EChar Char
    | ENil
    | ETuple Exp Exp
    | EId String [Field]
    | ECons String [Exp]
    | EFunCall String [Exp]
    | EOp1 Op1 Exp
    | EOp2 Op2 Exp Exp
    deriving (Eq, Show)

data ExpT =
    EIntT Int Type
    | EBoolT Bool Type
    | ECharT Char Type
    | ENilT Type
    | ETupleT ExpT ExpT Type
    | EIdT String [Field] Type
    | EConsT String [ExpT] Type
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
    EConsT _ _ t -> t
    EFunCallT _ _ t -> t
    EOp1T _ _ t -> t
    EOp2T _ _ _ t -> t

combineTypes :: [Type] -> Type
combineTypes l = case l of
    [] -> TVoid
    a : r -> case r of
        [] -> a
        _ -> TTuple a (combineTypes r)
