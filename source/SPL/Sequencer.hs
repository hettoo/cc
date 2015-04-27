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
    LDR String |
    STR String |
    LDS Int |
    STS Int |
    OP1 String |
    OP2 String |
    PRINTI |
    PRINTC |
    BRA String |
    BRF String |
    JSR |
    LINK |
    UNLINK |
    RET |
    HALT String
    deriving Show

stackChange :: Command -> Int
stackChange c = case c of
    LABEL _ -> 0
    LDC _ -> 1
    LDR _ -> 1
    STR _ -> -1
    LDS _ -> 1
    STS _ -> -1
    OP1 _ -> 0
    OP2 _ -> -1
    PRINTI -> -1
    PRINTC -> -1
    BRA _ -> 0
    BRF _ -> -1
    JSR -> -1
    LINK -> 1
    UNLINK -> 0 -- we reset things manually after a function
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

seqOutput :: [StmtT] -> String
seqOutput l = stateOutput $ globals l >> seqMain l >> seqTodo l >@>
    (tnew, 0, cnew, 0, [])

stateOutput :: SO -> String
stateOutput (_, _, _, _, l) = unlines (map cmdOutput l)

cmdOutput :: Command -> String
cmdOutput c = case c of
    LABEL s -> s ++ ":"
    LDC s -> "ldc " ++ s
    LDR s -> "ldr " ++ s
    STR s -> "str " ++ s
    LDS i -> "lds " ++ (show i)
    STS i -> "sts " ++ (show i)
    OP1 s -> s
    OP2 s -> s
    PRINTI -> "trap 0"
    PRINTC -> "trap 1"
    BRA s -> "bra " ++ s
    BRF s -> "brf " ++ s
    JSR -> "jsr"
    RET -> "ret"
    HALT s -> "halt" ++ if s == "" then "" else " ; " ++ s
    LINK -> "link 0"
    UNLINK -> "unlink"

globals :: [StmtT] -> Sequencer
globals l = case l of
    [] -> eId
    s : r -> do
        case s of
            VarDeclT t i e -> seqNewVariable t i e
            _ -> eId
        globals r

seqMain :: [StmtT] -> Sequencer
seqMain l = do
    (_, _, vc, _, _) <- getState
    seqStmt True (fst $ findFunction ("main", []) l)
    gvc . st $ const vc

seqTodo :: [StmtT] -> Sequencer
seqTodo l = do
    (_, sp, vc, _, _) <- getState
    seqTodo' sp vc l
    where
    seqTodo' sp vc l = do
        (t, _, _, _, _) <- getState
        case getTodo t of
            (Nothing, _) -> eId
            (Just c@(i, as), t') -> do
                gtodo $ const t'
                gsp $ const (sp + length as)
                gvc . st $ const vc
                addCmd $ LABEL (callLabel c)
                seqFunction c l
                seqTodo' sp vc l

callLabel :: Call -> String
callLabel (s, l) = s ++ "_" ++ show (length l) ++
    foldr (\a r -> "_" ++ simplePrint a ++ r) "" l

addVariable :: String -> Int -> Sequencer
addVariable i o = do
    (_, sp, _, _, _) <- getState
    gvc (cadd i (sp + o) ("redefined variable " ++ i))

seqNewVariable :: Type -> String -> ExpT -> Sequencer
seqNewVariable t i e = do
    seqExp e
    addVariable i 0

seqFunction :: Call -> [StmtT] -> Sequencer
seqFunction c@(i, as) l = case i of -- TODO: unification
    "isEmpty" -> eId -- TODO
    "read" -> eId -- TODO
    "print" -> eId -- TODO
    _ -> let (b, as') = findFunction c l in do
        addVariables (-length as') as'
        addCmd LINK
        seqStmt False b
        where
        addVariables n l = case l of
            [] -> eId
            a : r -> do
                addVariable a n
                addVariables (n + 1) r

findFunction :: Call -> [StmtT] -> (StmtT, [String])
findFunction c@(i, as) l = case l of
    [] -> error $ "function " ++ show c ++ " not found"
    s : r -> case s of
        FunDeclT t i' as' b ->
            if i == i' {-&& as == map fst as'-} then -- TODO: unify
                case t of
                    TVoid -> (StmtsT [b, ReturnT Nothing], n) -- just to be sure
                    _ -> (b, n)
            else
                rec
            where
            n = map snd as'
        _ -> rec
        where
        rec = findFunction c r

flowLabel :: Int -> String
flowLabel i = "_f" ++ show i

varPos :: String -> State SO Int
varPos i = do
    (_, sp, vc, _, _) <- getState
    return $ (clookupe i >!> vc) - sp

seqStmt :: Bool -> StmtT -> Sequencer
seqStmt main s = case s of
    StmtsT l -> endoSeq (seqStmt main) l
    VarDeclT t i e -> seqNewVariable t i e
    FunDeclT _ _ _ _ -> addCmd $ HALT "nested functions are impossible"
    FunCallT "print" [e] -> do
        seqExp e
        case getType e of
            TInt -> addCmd PRINTI
            TChar -> addCmd PRINTC
            _ -> addCmd $ HALT ("print not yet implemented for " ++ show e)
    FunCallT i as -> seqFunCall i as
    ReturnT m -> do
        case m of
            Just e -> do
                seqExp e
                if main then
                    eId -- TODO: print according to the return type
                else
                    addCmd $ STR "RR"
            Nothing -> eId
        if main then
            addCmd $ HALT "program end"
        else do
            addCmd $ UNLINK
            addCmd RET
    AssignT i fs e -> do -- TODO: fields
        seqExp e
        p <- varPos i
        addCmd $ STS p
    IfT c b m -> case m of
        Nothing -> do
            seqExp c
            (_, _, _, f, _) <- getState
            gfresh (+1)
            addCmd $ BRF (flowLabel f)
            seqStmt main b
            addCmd $ LABEL (flowLabel f)
        Just e -> do
            seqExp c
            (_, _, _, f, _) <- getState
            gfresh (+2)
            addCmd $ BRF (flowLabel f)
            seqStmt main b
            addCmd $ BRA (flowLabel (f + 1))
            addCmd $ LABEL (flowLabel f)
            seqStmt main e
            addCmd $ LABEL (flowLabel (f + 1))
    WhileT c b -> do
        (_, _, _, f, _) <- getState
        gfresh (+2)
        addCmd $ LABEL (flowLabel f)
        seqExp c
        addCmd $ BRF (flowLabel (f + 1))
        seqStmt main b
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
    EIdT i fs t -> do
        p <- varPos i
        addCmd $ LDS p
    EFunCallT id as _ -> do
        seqFunCall id as
        addCmd $ LDR "RR"
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
    gsp $ \sp -> sp - length as
