module SPL.Sequencer where
import SPL.Algebra
import SPL.Typer
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

stackChange :: Command -> (Int, Int)
stackChange c = case c of
    LABEL _ -> (0, 0)
    LDC _ -> (0, 1)
    STL _ -> (1, 0)
    OP1 _ -> (1, 0)
    OP2 _ -> (2, 0)
    PRINTI -> (1, 0)
    PRINTC -> (1, 0)
    BRA _ -> (0, 0)
    BRF _ -> (1, 0)
    JSR -> (1, 1)
    RET -> (1, 0)
    HALT _ -> (0, 0)

type Call = (String, [Type]) -- TODO: allow polymorphic outputs
type SP = Int
type VarContext = Context SP
type SO = (Todo Call, SP, VarContext, [Command])
type Sequencer = Endo SO

gtodo :: (Todo Call -> Todo Call) -> Sequencer
gtodo = globalize (\(t, _, _, _) -> t) (\t (_, sp, vc, l) -> (t, sp, vc, l))

gsp :: (SP -> SP) -> Sequencer
gsp = globalize (\(_, sp, _, _) -> sp) (\sp (t, _, vc, l) -> (t, sp, vc, l))

gvc :: (VarContext -> VarContext) -> Sequencer
gvc = globalize (\(_, _, vc, _) -> vc) (\vc (t, sp, _, l) -> (t, sp, vc, l))

gcmd :: ([Command] -> [Command]) -> Sequencer
gcmd = globalize (\(_, _, _, l) -> l) (\l (t, sp, vc, _) -> (t, sp, vc, l))

addCmd :: Command -> Sequencer
addCmd c = do
    gcmd $ \l -> l ++ [c]
    let (s, a) = stackChange c in
        gsp $ \sp -> sp + a - s

makeCall :: Call -> Sequencer
makeCall c@(i, as) = do
    addCmd $ LDC (callLabel c)
    addCmd JSR
    gsp $ \sp -> sp - length as + 1

discard :: Sequencer
discard = addCmd $ STL 0 -- TODO: proper way to pop and discard

seqOutput :: [StmtT] -> String
seqOutput l = stateOutput $ defaults >> (seqTodo l) >@>
    (todo ("main", []) tnew, 0, cnew, [])

stateOutput :: SO -> String
stateOutput (_, _, _, l) = unlines (map cmdOutput l)

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

defaults :: Sequencer
defaults = eId -- TODO

seqTodo :: [StmtT] -> Sequencer
seqTodo l = do
    (t, _, _, _) <- getState
    case getTodo t of
        (Nothing, _) -> eId
        (Just c@(i, as), t') -> do
            gtodo (const t')
            addCmd $ LABEL (callLabel c)
            seqFunction c l
            seqTodo l
            if i == "main" then
                addCmd $ HALT "program end"
            else
                eId

callLabel :: Call -> String
callLabel (s, t) = s -- TODO: encode types

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

seqStmt :: StmtT -> Sequencer
seqStmt s = case s of
    StmtsT l -> endoSeq seqStmt l
    VarDeclT t id e -> addCmd $ HALT "variables not yet implemented"
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
    IfT c b m -> case m of -- TODO: fresh labels
        Nothing -> do
            seqExp c
            addCmd $ BRF "_endif"
            seqStmt b
            addCmd $ LABEL "_endif"
        Just e -> do
            seqExp c
            addCmd $ BRF "_else"
            seqStmt b
            addCmd $ BRA "_endif"
            addCmd $ LABEL "_else"
            seqStmt e
            addCmd $ LABEL "_endif"
    WhileT c b -> do -- TODO: fresh labels
        addCmd $ LABEL "_while"
        seqExp c
        addCmd $ BRF "_endwhile"
        seqStmt b
        addCmd $ BRA "_while"
        addCmd $ LABEL "_endwhile"

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
        discard
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
