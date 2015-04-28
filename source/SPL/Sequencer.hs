module SPL.Sequencer where
import SPL.Algebra
import SPL.Typer
import SPL.Printer
import Context
import Todo
import Endo
import State
import Utils
import Data.Char
import Control.Monad

data Command =
    LABEL String |
    AJS Int |
    LDC String |
    LDR String |
    STR String |
    LDS Int |
    STS Int |
    LDH Int Int |
    STH Int |
    OP1 String |
    OP2 String |
    PRINTI |
    PRINTC |
    BRA String |
    BRF String |
    JSR |
    LINK Int |
    UNLINK Int |
    RET |
    HALT String
    deriving Show

stackChange :: Command -> Int
stackChange c = case c of
    LABEL _ -> 0
    AJS n -> n
    LDC _ -> 1
    LDR _ -> 1
    STR _ -> -1
    LDS _ -> 1
    STS _ -> -1
    LDH _ n -> n - 1
    STH n -> 1 - n
    OP1 _ -> 0
    OP2 _ -> -1
    PRINTI -> -1
    PRINTC -> -1
    BRA _ -> 0
    BRF _ -> -1
    JSR -> -1
    LINK n -> n + 1
    UNLINK n -> -n - 1
    RET -> -1
    HALT _ -> 0

cmdOutput :: Command -> String
cmdOutput c = case c of
    LABEL s -> s ++ ":"
    AJS n -> "ajs " ++ show n
    LDC s -> "ldc " ++ s
    LDR s -> "ldr " ++ s
    STR s -> "str " ++ s
    LDS n -> "lds " ++ show n
    STS n -> "sts " ++ show n
    LDH m n -> "ldmh " ++ show m ++ " " ++ show n
    STH n -> "stmh " ++ show n
    OP1 s -> s
    OP2 s -> s
    PRINTI -> "trap 0"
    PRINTC -> "trap 1"
    BRA s -> "bra " ++ s
    BRF s -> "brf " ++ s
    JSR -> "jsr"
    RET -> "ret"
    HALT s -> "halt" ++ if s == "" then "" else " ; " ++ s
    LINK n -> "link " ++ show n
    UNLINK _ -> "unlink"

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

addCmds :: [Command] -> Sequencer
addCmds cs = foldl (>>) eId $ map addCmd cs

seqOutput :: [StmtT] -> String
seqOutput l = stateOutput $ program >@> (tnew, 0, cnew, 0, [])
    where
    program = do
        globals l
        gvc cdown
        seqMain l
        seqTodo l

stateOutput :: SO -> String
stateOutput (_, _, _, _, l) = unlines (map cmdOutput l)

globals :: [StmtT] -> Sequencer
globals l = endoSeq declareGlobal l >> endoSeq setGlobal l
    where
    declareGlobal s = case s of
        VarDeclT _ i _ -> do
            addCmd $ LDC "0" -- whatever
            addVariable i 0
        _ -> eId
    setGlobal s = case s of
        VarDeclT t i e -> seqNewVariable l False t i e
        _ -> eId

seqMain :: [StmtT] -> Sequencer
seqMain l = do
    (_, _, vc, _, _) <- getState
    let (s, _, _) = findFunction ("main", []) l in
        seqStmt l True s
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
    foldr (\a r -> "_" ++ overloadPrint a ++ r) "" l
    where
    overloadPrint t = case t of
        TPoly s -> "POLY" -- will eventually never show up
        TInt -> "Int"
        TBool -> "Bool"
        TChar -> "Char"
        TTuple u v -> "T_" ++ overloadPrint u ++ "_" ++ overloadPrint v ++ "_ET"
        TList u -> "L_" ++ overloadPrint u ++ "_EL"
        TVoid -> "Void"

addVariable :: String -> Int -> Sequencer
addVariable i o = do
    (_, sp, _, _, _) <- getState
    gvc (cadd i (sp + o) ("redefined variable " ++ i))

setVariable :: String -> Sequencer
setVariable i = do
    p <- varPos i
    addCmd $ STS p

seqNewVariable :: [StmtT] -> Bool -> Type -> String -> ExpT -> Sequencer
seqNewVariable l main t i e = do
    seqExp l e
    if main then
        addVariable i 0
    else
        setVariable i

varDecls :: StmtT -> [(String, ExpT)]
varDecls t = case t of
    StmtsT l -> concat $ map varDecls l
    VarDeclT _ i e -> [(i, e)]
    _ -> []

seqFunction :: Call -> [StmtT] -> Sequencer
seqFunction c@(i, as) l = -- TODO: unification
    let
        (b, names, _) = findFunction c l
        namesp = reverse names
        namesi = reverse (map fst (varDecls b))
        names' = namesi ++ namesp
    in do
    addVariables (-length names') namesi
    gvc cdown
    addVariables (-length namesp) namesp
    seqStmt l False b
    where
    addVariables n l = case l of
        [] -> eId
        a : r -> do
            addVariable a n
            addVariables (n + 1) r

findFunction :: Call -> [StmtT] -> (StmtT, [String], Type)
findFunction c@(i, as) l = case l of
    [] -> error $ "function " ++ show c ++ " not found"
    s : r -> case s of
        FunDeclT t i' as' b ->
            if i == i' {-&& as == map fst as'-} then -- TODO: unify
                case t of
                    TVoid -> (StmtsT [b, ReturnT Nothing], n, t)
                    _ -> (b, n, t)
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

seqPrint :: Type -> Sequencer
seqPrint t = case t of
    TBool -> seqIf printTrue (Just printFalse)
        where
        printTrue = seqPrintStr "True"
        printFalse = seqPrintStr "False"
    TInt -> addCmd PRINTI
    TChar -> addCmd PRINTC
    TList t' -> do
        addCmd $ LDC (enc '[')
        addCmd PRINTC
        testNonEmpty
        seqIf printElement (Just . addCmd $ AJS (-1))
        seqWhile testNonEmpty printNextElement
        addCmd $ LDC (enc ']')
        addCmd PRINTC
        where
        testNonEmpty = addCmd $ LDS 0
        printElement = do
            addCmd $ LDH 0 2
            seqPrint t'
        printNextElement = do
            seqPrintStr ", "
            printElement
    TTuple t1 t2 -> do
        addCmd $ LDC (enc '(')
        addCmd PRINTC
        addCmd $ LDH 0 2
        addCmd $ LDS (-1)
        seqPrint t1
        seqPrintStr ", "
        seqPrint t1
        addCmd $ LDC (enc ')')
        addCmd PRINTC
        addCmd $ AJS (-1)

enc :: Char -> String
enc = show . ord

seqPrintStr :: String -> Sequencer
seqPrintStr s = addCmds $ concatMap (\c -> [LDC $ enc c, PRINTC]) s

seqStmt :: [StmtT] -> Bool -> StmtT -> Sequencer
seqStmt ss main s = case s of
    StmtsT l -> endoSeq (seqStmt ss main) l
    VarDeclT t i e -> seqNewVariable ss main t i e
    FunDeclT _ _ _ _ -> eId
    FunCallT i as -> seqFunCall True ss i as
    ReturnT m -> do
        case m of
            Just e -> do
                seqExp ss e
                if main then
                    seqPrint $ getType e
                else
                    addCmd $ STR "RR"
            Nothing -> eId
        if main then
            addCmd $ HALT "program end"
        else do
            addCmd RET
    AssignT i fs e -> do -- TODO: fields
        seqExp ss e
        setVariable i
    IfT c b m -> do
        seqExp ss c
        seqIf (seqStmt ss main b) (fmap (seqStmt ss main) m)
    WhileT c b -> seqWhile (seqExp ss c) (seqStmt ss main b)

seqIf :: Sequencer -> Maybe Sequencer -> Sequencer
seqIf b m = do
    (_, _, _, f, _) <- getState
    case m of
        Nothing -> do
            gfresh (+1)
            addCmd $ BRF (flowLabel f)
            (_, sp, _, _, _) <- getState
            b
            addCmd $ LABEL (flowLabel f)
            gsp $ const sp
        Just e -> do
            gfresh (+2)
            addCmd $ BRF (flowLabel f)
            (_, sp, _, _, _) <- getState
            b
            addCmd $ BRA (flowLabel (f + 1))
            addCmd $ LABEL (flowLabel f)
            gsp $ const sp
            e
            addCmd $ LABEL (flowLabel (f + 1))
            gsp $ const sp

seqWhile :: Sequencer -> Sequencer -> Sequencer
seqWhile c b = do
    (_, _, _, f, _) <- getState
    gfresh (+2)
    addCmd $ LABEL (flowLabel f)
    c
    addCmd $ BRF (flowLabel (f + 1))
    b
    addCmd $ BRA (flowLabel f)
    addCmd $ LABEL (flowLabel (f + 1))

seqExp :: [StmtT] -> ExpT -> Sequencer
seqExp l e = case e of
    EIntT x TInt -> addCmd $ LDC (show x)
    EBoolT x TBool -> addCmd $ LDC $ case x of
        True -> "-1"
        False -> "0"
    ECharT x TChar -> addCmd $ LDC (enc x)
    ENilT _ -> addCmd $ LDC "0"
    ETupleT e1 e2 _ -> do
        seqExp l e1
        seqExp l e2
        addCmd $ STH 2
    EIdT i fs t -> do -- TODO: fields
        p <- varPos i
        addCmd $ LDS p
    EFunCallT i as _ -> do
        seqFunCall False l i as
        addCmd $ LDR "RR"
    EOp1T op e _ -> do
        seqExp l e
        applyOp1 op (getType e)
    EOp2T OCons e1 e2 _ -> do
        seqExp l e2
        seqExp l e1
        addCmd $ STH 2
    EOp2T op e1 e2 _ -> do
        seqExp l e1
        seqExp l e2
        applyOp2 op (getType e1, getType e2)

op1Tuple :: Op1 -> (Type, Type) -> Sequencer
op1Tuple op (t1, t2) = do
    addCmd $ LDH 0 2
    addCmd $ LDS (-1)
    applyOp1 op t1
    addCmd $ LDS (-1)
    applyOp1 op t2
    addCmd $ STH 2
    addCmd $ STR "R5"
    addCmd $ AJS (-2)
    addCmd $ LDR "R5"

applyOp1 :: Op1 -> Type -> Sequencer
applyOp1 op t = case t of
    TList t' -> eId -- TODO
    TTuple t1 t2 -> op1Tuple op ((t1, t2))
    _ -> addCmd $ OP1 $ case op of
        ONot -> "not"
        ONeg -> "neg"

op2Tuple :: Op2 -> ((Type, Type), (Type, Type)) -> Sequencer
op2Tuple op ((t1, t2), (t1', t2')) = do
    addCmd $ LDH 0 2
    addCmd $ LDS (-2)
    addCmd $ LDH 0 2
    addCmd $ LDS (-1)
    addCmd $ LDS (-4)
    applyOp2 op (t1, t1')
    addCmd $ LDS (-1)
    addCmd $ LDS (-4)
    applyOp2 op (t2, t2')
    addCmd $ STH 2
    addCmd $ STR "R5"
    addCmd $ AJS (-5)
    addCmd $ LDR "R5"

op2BoolTuple :: Bool -> Bool -> Op2 -> ((Type, Type), (Type, Type)) -> Sequencer
op2BoolTuple firstQuick quickValue op ((t1, t2), (t1', t2')) = do
    addCmd $ LDH 0 2
    addCmd $ LDS (-2)
    addCmd $ LDH 0 2
    addCmd $ LDS (-1)
    addCmd $ LDS (-4)
    applyOp2 op (t1, t1')
    seqIf (if firstQuick then quick else slow)
        (Just $ if firstQuick then slow else quick)
    where
    quick = do
        addCmd $ AJS (-5)
        addCmd . LDC $ if quickValue then "-1" else "0"
    slow = do
        addCmd $ LDS (-2)
        applyOp2 op (t2, t2')
        addCmd $ STR "R5"
        addCmd $ AJS (-4)
        addCmd $ LDR "R5"

applyOp2 :: Op2 -> (Type, Type) -> Sequencer
applyOp2 op t = case t of
    (TList t1, TList t2) -> case op of
        _ -> eId -- TODO
    (TTuple t1 t2, TTuple t1' t2') -> case op of
        OLt -> op2BoolTuple True True op ((t1, t2), (t1', t2'))
        OGt -> op2BoolTuple True True op ((t1, t2), (t1', t2'))
        OLe -> op2BoolTuple True True op ((t1, t2), (t1', t2'))
        OGe -> op2BoolTuple True True op ((t1, t2), (t1', t2'))
        OEq -> op2BoolTuple False False op ((t1, t2), (t1', t2'))
        ONeq -> op2BoolTuple True False op ((t1, t2), (t1', t2'))
        OPlus -> op2Tuple op ((t1, t2), (t1', t2'))
        OMinus -> op2Tuple op ((t1, t2), (t1', t2'))
        OTimes -> op2Tuple op ((t1, t2), (t1', t2'))
        ODiv -> op2Tuple op ((t1, t2), (t1', t2'))
        OMod -> op2Tuple op ((t1, t2), (t1', t2'))
    _ -> do
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

seqFunCall :: Bool -> [StmtT] -> String -> [ExpT] -> Sequencer
seqFunCall discard l i as =
    let
        c = (i, map getType as)
        (b, names, rt) = findFunction c l
        n = length as + length (varDecls b)
        as' = zip names as
    in case (c, as) of
        (("print", [t]), [e]) -> do
            seqExp l e
            seqPrint t
        (("read", []), []) -> eId -- TODO
        (("isEmpty", [TList _]), [e]) -> do
            seqExp l e
            seqIf (addCmd $ LDC "0") (Just . addCmd $ LDC "-1")
            doDiscard
        _ -> do
            gtodo (todo c)
            addCmd $ LINK n
            flip endoSeq as' $ \(_, e) -> seqExp l e
            addCmd $ LDC (callLabel c)
            addCmd JSR
            addCmd $ UNLINK n
            gsp (\sp -> sp - 1)
            case rt of
                TVoid -> eId
                _ -> doDiscard
        where
        doDiscard =
            if discard then
                addCmd $ AJS (-1)
            else
                eId
