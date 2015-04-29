module SPL.Algebra where

data Stmt =
    Stmts [Stmt]
    | VarDecl Type String Exp
    | FunDecl Type String [(Type, String)] Stmt
    | FunCall String [Exp]
    | Return (Maybe Exp)
    | Assign String [Field] Exp
    | If Exp Stmt (Maybe Stmt)
    | While Exp Stmt
    deriving (Eq, Show)

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

combineTypes :: [Type] -> Type
combineTypes l = case l of
    [] -> TVoid
    a : r -> case r of
        [] -> a
        _ -> TTuple a (combineTypes r)
