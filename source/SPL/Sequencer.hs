module SPL.Sequencer where
import SPL.Algebra
import SPL.Typer

genSeq :: [StmtT] -> String
genSeq ss = unlines $ concatMap genSeqStmt ss
-- genSeq (FunCallT "print" [EIntT x TInt]) = unlines ["ldc " ++ x, "trap 0"]

genSeqStmt s = case s of
    StmtsT ss -> concatMap genSeqStmt ss
    VarDeclT _ _ _ -> ["halt"] -- not yet implemented
    FunDeclT _ _ _ _ -> ["halt"] -- not yet implemented
    FunCallT "print" [e] -> case getType e of
        TInt -> genSeqExp e ++ ["trap 0"]
        TChar -> genSeqExp e ++ ["trap 1"]
    FunCallT _ _ -> ["halt"] -- not yet implemented
    ReturnT _ -> ["halt"] -- not yet implemented
    AssignT _ _ _ -> ["halt"] -- not yet implemented
    IfT _ _ _ -> ["halt"] -- not yet implemented
    WhileT _ _ -> ["halt"] -- not yet implemented

genSeqExp e = case e of
    EIntT x TInt -> ["ldc " ++ show x]
    EBoolT x TBool -> case x of
        True -> ["ldc 1"]
        False -> ["ldc 0"]
    ECharT x TChar -> ["ldc " ++ show x]
    ENilT _ -> ["nop"]
    ETupleT e1 e2 _ -> genSeqExp e1 ++ genSeqExp e2
    EIdT _ _ _ -> ["halt"] -- not yet implemented
    EFunCallT _ _ _ -> ["halt"] -- not yet implemented
    EOp1T op e _ -> genSeqExp e ++ genSeqOp op
    EOp2T op e1 e2 _ -> genSeqExp e1 ++ genSeqOp e2

genSeqOp _ = ["halt"]
