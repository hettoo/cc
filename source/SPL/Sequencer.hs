module SPL.Sequencer where
import SPL.Algebra
import SPL.Typer
import Control.Monad.ST
import Data.Map (Map)
import qualified Data.Map as M

seq :: [StmtT] -> String
seq ss = unlines $ concatMap seqStmt ss

type Instruction = String

type FunId = String
type VarId = String
type Label = String
type HeapAddr = Int
type FunTab = Map FunId Label
type VarTab = Map VarId HeapAddr

type Sequencer = State (FunTab, VarTab) [Instruction]

seqStmt :: Stmt -> Sequencer
seqStmt s = case s of
    StmtsT ss -> sequence $ map seqStmt ss
    VarDeclT t id e -> halt "not yet implemented" 
    FunDeclT _ _ _ _ -> halt "not yet implemented" 
    FunCallT "print" [e] -> do
        s <- seqExp e
        return $ s ++ case getType e of
            TInt -> ["trap 0"]
            TChar -> ["trap 1"]
    FunCallT _ _ -> halt "not yet implemented" 
    ReturnT _ -> halt "not yet implemented" 
    AssignT _ _ _ -> halt "not yet implemented" 
    IfT _ _ _ -> halt "not yet implemented" 
    WhileT _ _ -> halt "not yet implemented" 

seqExp :: ExpT -> Sequencer 
seqExp e = case e of
    EIntT x TInt -> return ["ldc " ++ show x]
    EBoolT x TBool -> case x of
        True -> return ["ldc -1"]
        False -> return ["ldc 0"]
    ECharT x TChar -> return ["ldc " ++ show x]
    ENilT _ -> return ["nop"]
    ETupleT e1 e2 _ -> do
        s1 <- seqExp e1 
        s2 <- seqExp e2
        return $ s1 ++ s2
    EIdT _ _ _ -> halt "not yet implemented" 
    EFunCallT id as _ -> do
        s <- sequence $ map seqExp as
        c <- seqFunCall id
        return $ s ++ c
    EOp1T op e _ -> do
        s <- seqExp e
        return $ s ++ case op of
            ONot -> ["not"]
            ONeg -> ["sub"]
    EOp2T OCons _ _ -> halt "not yet implemented"
    EOp2T op e1 e2 _ -> do
        s1 <- seqExp e1
        s2 <- seqExp e2
        return $ s1 ++ s2 ++ case op of
            OAnd -> ["and"]
            OOr -> ["or"]
            OEq -> ["eq"]
            ONeq -> ["ne"]
            OLt -> ["lt"]
            OGt -> ["gt"]
            OLe -> ["le"]
            OGe -> ["ge"]
            OPlus -> ["add"]
            OMinus -> ["sub"]
            OTimes -> ["mul"]
            ODiv -> ["div"]
            OMod -> ["mod"]

seqVar :: VarId -> Sequencer
seqVar id = do
    (_, varTab) <- get
    return ["ldc " ++ M.lookup id varTab]

seqFunCall :: FunId -> Sequencer
seqFunCall id = do
    (funTab, _) <- get
    return ["bsr " ++ M.lookup id funTab]

comment :: [Instruction] String -> [Instruction]
comment (i:is) c = (i ++ " ; " ++ c) : is

halt :: String -> Sequencer
halt reason = return $ comment ["halt"] reason
