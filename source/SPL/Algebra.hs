module SPL.Algebra where

data StmtG e =
    Stmts [StmtG e]
    | VarDecl Type String e
    | FunDecl Type String [(Type, String)] (StmtG e)
    | FunCall String [e]
    | Return (Maybe e)
    | Assign String [Field] e
    | If e (StmtG e) (Maybe (StmtG e))
    | While e (StmtG e)
    deriving (Eq, Show)

type Stmt = StmtG Exp

data Type =
    TPoly String
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
    | EFunCall String [Exp]
    | EOp1 Op1 Exp
    | EOp2 Op2 Exp Exp
    deriving (Eq, Show)
