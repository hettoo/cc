module SPL.Sequencer where
import SPL.Algebra
import SPL.Typer
import State
import Context
import Todo
import Control.Monad

type Instruction = String

type HeapAddr = Int
type VarTab = Context HeapAddr

type Label = String
type FunTab = Context Label

type Call = (String, [Type]) -- TODO: allow polymorphic outputs

type Sequencer = State (Todo Call, FunTab, VarTab) [Instruction]

seqf :: (a -> Sequencer) -> [a] -> Sequencer
seqf f = foldl (\s a -> s >> f a) (return [])

seqOutput :: [StmtT] -> String
seqOutput ss = unlines $ seqTodo ss >!> (todo ("main", []) tnew, cnew, cnew)
-- TODO: evaluate global variables first

seqTodo :: [StmtT] -> Sequencer
seqTodo ss = do
    (t, _, _) <- getState
    case getTodo t of
        (Nothing, _) -> return []
        (Just c, t') -> do
            setTodo (const t')
            l <- seqStmt (findFunction c ss)
            l' <- seqTodo ss
            return $ callLabel c : l ++ l'

callLabel :: Call -> String
callLabel (s, t) = s -- TODO: encode types

findFunction :: Call -> [StmtT] -> StmtT
findFunction c@(i, as) l = case l of
    s : r -> case s of
        FunDeclT _ i' as' b ->
            if i == i' && as == map fst as' then -- TODO: unify
                b
            else
                rec
        _ -> rec
        where
        rec = findFunction c r

mainType :: [StmtT] -> Type
mainType l = case l of
    s : r -> case s of
        FunDeclT t "main" _ _ -> t
        _ -> mainType r

setTodo :: (Todo Call -> Todo Call) -> Sequencer
setTodo f = ST $ \(t, ft, vt) -> Left ([], (f t, ft, vt))

addCall :: Call -> Sequencer
addCall c = setTodo (todo c)

seqStmt :: StmtT -> Sequencer
seqStmt s = case s of
    StmtsT ss -> seqf seqStmt ss
    VarDeclT t id e -> halt "not yet implemented"
    FunDeclT _ _ _ _ -> halt "nested functions are impossible"
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
        s <- seqf seqExp as
        c <- seqFunCall id
        return $ s ++ c
    EOp1T op e _ -> do
        s <- seqExp e
        return $ s ++ case op of
            ONot -> ["not"]
            ONeg -> ["sub"]
    EOp2T OCons _ _ _ -> halt "not yet implemented"
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

seqVar :: String -> Sequencer
seqVar id = do
    (_, _, varTab) <- getState
    return ["ldc " ++ show (clookupe id >!> varTab)]

seqFunCall :: String -> Sequencer
seqFunCall id = do
    (_, funTab, _) <- getState
    return ["bsr " ++ (clookupe id >!> funTab)]

comment :: [Instruction] -> String -> [Instruction]
comment (i:is) c = (i ++ " ; " ++ c) : is

halt :: String -> Sequencer
halt reason = return $ comment ["halt"] reason
