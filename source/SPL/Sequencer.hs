module SPL.Sequencer where
import SPL.Algebra
import SPL.Typer
import SPL.Printer
import Context
import Todo
import Endo
import State
import Control.Monad

data Command =
    LABEL String |
    LDC String |
    STL Integer |
    OP1 String |
    OP2 String |
    PRINTI |
    PRINTC |
    BRA String |
    BRF String |
    JSR |
    RET |
    HALT String
    deriving Show

stackChange :: Command -> Int
stackChange c = case c of
    LABEL _ -> 0
    LDC _ -> 1
    STL _ -> -1
    OP1 _ -> -1
    OP2 _ -> -2
    PRINTI -> -1
    PRINTC -> -1
    BRA _ -> 0
    BRF _ -> -1
    JSR -> 0
    RET -> -1
    HALT _ -> 0

type Call = (String, [Type]) -- TODO: allow polymorphic outputs
type SP = Int
type VarContext = Context SP
type SO = (Todo Call, SP, VarContext, Int, [Command])
type Sequencer = Endo SO

gtodo :: (Todo Call -> Todo Call) -> Sequencer
gtodo = globalizef (\(t, _, _, _, _) -> t)
    (\t (_, sp, vc, f, l) -> (t, sp, vc, f, l))

gsp :: (SP -> SP) -> Sequencer
gsp = globalizef (\(_, sp, _, _, _) -> sp)
    (\sp (t, _, vc, f, l) -> (t, sp, vc, f, l))

gvc :: Endo VarContext -> Sequencer
gvc = globalize (\(_, _, vc, _, _) -> vc)
    (\vc (t, sp, _, f, l) -> (t, sp, vc, f, l))

gfresh :: (Int -> Int) -> Sequencer
gfresh = globalizef (\(_, _, _, f, _) -> f)
    (\f (t, sp, vc, _, l) -> (t, sp, vc, f, l))

gcmd :: ([Command] -> [Command]) -> Sequencer
gcmd = globalizef (\(_, _, _, _, l) -> l)
    (\l (t, sp, vc, f, _) -> (t, sp, vc, f, l))

addCmd :: Command -> Sequencer
addCmd c = do
    gcmd $ \l -> l ++ [c]
    gsp $ \sp -> sp + stackChange c

discard :: Int -> Sequencer
discard n = case n of
    0 -> eId
    n -> do
        addCmd $ STL 0 -- TODO: proper way to pop and discard?
        discard (n - 1) -- TODO: all in one command?

seqOutput :: [StmtT] -> String
seqOutput l = stateOutput $ globals l >> seqTodo l >@>
    (todo ("main", []) tnew, 0, cnew, 0, [])

stateOutput :: SO -> String
stateOutput (_, _, _, _, l) = unlines (map cmdOutput l)

cmdOutput :: Command -> String
cmdOutput c = case c of
    LABEL s -> s ++ ":"
    LDC s -> "ldc " ++ s
    STL i -> "stl " ++ (show i)
    OP1 s -> s
    OP2 s -> s
    PRINTI -> "trap 0"
    PRINTC -> "trap 1"
    BRA s -> "bra " ++ s
    BRF s -> "brf " ++ s
    JSR -> "jsr"
    RET -> "ret"
    HALT s -> "halt" ++ if s == "" then "" else " ; " ++ s

globals :: [StmtT] -> Sequencer
globals l = case l of
    [] -> eId
    s : r -> do
        case s of
            VarDeclT t i e ->
                seqVariable t i e
            _ -> eId
        globals r

seqTodo :: [StmtT] -> Sequencer
seqTodo l = do
    (_, _, vc, _, _) <- getState
    seqTodo' vc l
    where
    seqTodo' vc l = do
        (t, _, _, _, _) <- getState
        case getTodo t of
            (Nothing, _) -> eId
            (Just c@(i, as), t') -> do
                gtodo $ const t'
                gvc . st $ const vc
                addCmd $ LABEL (callLabel c)
                seqFunction c l
                seqTodo' vc l
                if i == "main" then
                    addCmd $ HALT "program end"
                else
                    eId

callLabel :: Call -> String
callLabel (s, l) = s ++ "_" ++ show (length l) ++ typesLabel l
    where
    typesLabel l = case l of
        [] -> ""
        a : r -> "_" ++ simplePrint a ++ typesLabel r

seqVariable :: Type -> String -> ExpT -> Sequencer
seqVariable t i e = do
    seqExp e
    (_, sp, _, _, _) <- getState
    gvc (cadd i sp ("redefined variable " ++ i))

seqFunction :: Call -> [StmtT] -> Sequencer
seqFunction c@(i, as) l = case i of -- TODO: unification
    "isEmpty" -> eId -- TODO
    "read" -> eId -- TODO
    "print" -> eId -- TODO
    _ -> seqStmt (findFunction c l)

findFunction :: Call -> [StmtT] -> StmtT
findFunction c@(i, as) l = case l of
    [] -> error $ "function " ++ show c ++ " not found"
    s : r -> case s of
        FunDeclT t i' as' b ->
            if i == i' {-&& as == map fst as'-} then -- TODO: unify
                case (i, t) of
                    ("main", _) -> b
                    (_, TVoid) -> StmtsT [b, ReturnT Nothing] -- just to be sure
                    _ -> b
            else
                rec
        _ -> rec
        where
        rec = findFunction c r

flowLabel :: Int -> String
flowLabel i = "_f" ++ show i

seqStmt :: StmtT -> Sequencer
seqStmt s = case s of
    StmtsT l -> endoSeq seqStmt l
    VarDeclT t i e -> seqVariable t i e
    FunDeclT _ _ _ _ -> addCmd $ HALT "nested functions are impossible"
    FunCallT "print" [e] -> do
        seqExp e
        case getType e of
            TInt -> addCmd PRINTI
            TChar -> addCmd PRINTC
            _ -> addCmd $ HALT ("print not yet implemented for " ++ show e)
    FunCallT id as -> seqFunCall id as
    ReturnT m -> do
        case m of
            Just e -> seqExp e
            Nothing -> eId
        addCmd RET
    AssignT _ _ _ -> addCmd $ HALT "assignment not yet implemented"
    IfT c b m -> case m of
        Nothing -> do
            seqExp c
            (_, _, _, f, _) <- getState
            gfresh (+1)
            addCmd $ BRF (flowLabel f)
            seqStmt b
            addCmd $ LABEL (flowLabel f)
        Just e -> do
            seqExp c
            (_, _, _, f, _) <- getState
            gfresh (+2)
            addCmd $ BRF (flowLabel f)
            seqStmt b
            addCmd $ BRA (flowLabel (f + 1))
            addCmd $ LABEL (flowLabel f)
            seqStmt e
            addCmd $ LABEL (flowLabel (f + 1))
    WhileT c b -> do
        (_, _, _, f, _) <- getState
        gfresh (+2)
        addCmd $ LABEL (flowLabel f)
        seqExp c
        addCmd $ BRF (flowLabel (f + 1))
        seqStmt b
        addCmd $ BRA (flowLabel f)
        addCmd $ LABEL (flowLabel (f + 1))

seqExp :: ExpT -> Sequencer
seqExp e = case e of
    EIntT x TInt -> addCmd $ LDC (show x)
    EBoolT x TBool -> addCmd $ LDC $ case x of
        True -> "-1"
        False -> "0"
    ECharT x TChar -> addCmd $ LDC (show x)
    ENilT _ -> addCmd $ HALT "nil not yet implemented"
    ETupleT e1 e2 _ -> do -- TODO: probably not like this
        seqExp e1
        seqExp e2
    EIdT _ _ _ -> addCmd $ HALT "variables not yet implemented"
    EFunCallT id as _ -> do
        endoSeq seqExp as
        seqFunCall id as
        discard 1
    EOp1T op e _ -> do
        seqExp e
        addCmd $ OP1 $ case op of
            ONot -> "not"
            ONeg -> "sub"
    EOp2T OCons _ _ _ -> addCmd $ HALT "cons not yet implemented"
    EOp2T op e1 e2 _ -> do
        seqExp e1
        seqExp e2
        addCmd $ OP2 $ case op of
            OAnd -> "and"
            OOr -> "or"
            OEq -> "eq"
            ONeq -> "ne"
            OLt -> "lt"
            OGt -> "gt"
            OLe -> "le"
            OGe -> "ge"
            OPlus -> "add"
            OMinus -> "sub"
            OTimes -> "mul"
            ODiv -> "div"
            OMod -> "mod"

seqFunCall :: String -> [ExpT] -> Sequencer
seqFunCall i as = let c = (i, map getType as) in do
    gtodo (todo c)
    endoSeq seqExp as
    addCmd $ LDC (callLabel c)
    addCmd JSR
    gsp $ \sp -> sp - length as + 1 -- TODO: clean up variables from the stack
