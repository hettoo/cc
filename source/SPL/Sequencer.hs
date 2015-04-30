module SPL.Sequencer where
import SPL.Algebra
import SPL.Unifier
import SPL.Parser
import SPL.Typer
import SPL.Std
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
    SWP |
    LDC String |
    LDR String |
    STR String |
    LDS Int |
    STS Int |
    LDA Int |
    STA Int |
    LDH Int Int |
    STH Int |
    LDRR String String |
    OP1 String |
    OP2 String |
    PRINTI |
    PRINTC |
    READI |
    READC |
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
    SWP -> 0
    LDC _ -> 1
    LDR _ -> 1
    STR _ -> -1
    LDS _ -> 1
    STS _ -> -1
    LDA _ -> 0
    STA _ -> -2
    LDH _ n -> n - 1
    STH n -> 1 - n
    LDRR _ _ -> 0
    OP1 _ -> 0
    OP2 _ -> -1
    PRINTI -> -1
    PRINTC -> -1
    READI -> 1
    READC -> 1
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
    SWP -> "swp"
    LDC s -> "ldc " ++ s
    LDR s -> "ldr " ++ s
    STR s -> "str " ++ s
    LDS n -> "lds " ++ show n
    STS n -> "sts " ++ show n
    LDA n -> "lda " ++ show n
    STA n -> "sta " ++ show n
    LDH m n -> "ldmh " ++ show m ++ " " ++ show n
    STH n -> "stmh " ++ show n
    LDRR s t -> "ldrr " ++ s ++ " " ++ t
    OP1 s -> s
    OP2 s -> s
    PRINTI -> "trap 0"
    PRINTC -> "trap 1"
    READI -> "trap 10"
    READC -> "trap 11"
    BRA s -> "bra " ++ s
    BRF s -> "brf " ++ s
    JSR -> "jsr"
    RET -> "ret"
    HALT s -> "halt" ++ if s == "" then "" else " ; " ++ s
    LINK n -> "link " ++ show n
    UNLINK _ -> "unlink"

type Call = (String, [Type]) -- TODO: allow polymorphic outputs
type SP = Int
type VarContext = Context (SP, Bool)
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
        globals l'
        gvc cdown
        let (_, _, t) = findFunction ("main", []) l in
            case t of
                TVoid -> seqStmt l' (FunCallT "main" [])
                _ -> seqStmt l' (FunCallT "print" [EFunCallT "main" [] t])
        addCmd $ HALT "program end"
        seqTodo l'
    l' = l ++ annotateProgram (parseSPL' True stdSPL)

stateOutput :: SO -> String
stateOutput (_, _, _, _, l) = unlines (map cmdOutput l)

globals :: [StmtT] -> Sequencer
globals l = do
    addCmd $ LDRR "R7" "SP"
    setGlobals l 0
    where
    setGlobals l n = case l of
        [] -> eId
        s : r -> case s of
            VarDeclT t i e -> do
                seqExp l e
                addVariable i n True
                setGlobals r (n + 1)
            _ -> setGlobals r n

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
        TInt -> "Int"
        TBool -> "Bool"
        TChar -> "Char"
        TTuple u v -> "T_" ++ overloadPrint u ++ "_" ++ overloadPrint v ++ "_ET"
        TList u -> "L_" ++ overloadPrint u ++ "_EL"
        TPoly p -> "P_" ++ map (\c -> if c == '?' then '_' else c) p ++ "_EP"
        -- ^empty list trick :(
        TVoid -> "Void"

addVariable :: String -> Int -> Bool -> Sequencer
addVariable i o b = do
    (_, sp, _, _, _) <- getState
    gvc (cadd i (if b then o + 1 else sp + o, b) ("redefined variable " ++ i))

getVariable :: String -> Sequencer
getVariable i = do
    (p, b) <- varPos i
    if b then addCmd $ LDS p
        else do
            addCmd $ LDR "R7"
            addCmd $ LDA p

setVariable :: String -> Sequencer
setVariable i = do
    (p, b) <- varPos i
    if b then addCmd $ STS p
        else do
            addCmd $ LDR "R7"
            addCmd $ SWP
            addCmd $ STA p

varPos :: String -> State SO (Int, Bool)
varPos i = do
    (_, sp, vc, _, _) <- getState
    let (p, b) = clookupe i >!> vc in
        return $ if b then (p, False) else (p - sp, True)

varDecls :: StmtT -> [(String, ExpT)]
varDecls t = case t of
    StmtsT l -> concat $ map varDecls l
    VarDeclT _ i e -> [(i, e)]
    _ -> []

seqFunction :: Call -> [StmtT] -> Sequencer
seqFunction c@(i, as) l =
    let
        (b, names, _) = findFunction c l
        namesp = names
        namesi = reverse (map fst (varDecls b))
        names' = namesi ++ namesp
    in do
    addVariables (-length names') namesi
    gvc cdown
    addVariables (-length namesp) namesp
    seqStmt l b
    where
    addVariables n l = case l of
        [] -> eId
        a : r -> do
            addVariable a n False
            addVariables (n + 1) r

findFunction :: Call -> [StmtT] -> (StmtT, [String], Type)
findFunction c@(i, as) l = case l of
    [] -> error $ "function " ++ show c ++ " not found"
    s : r -> case s of
        FunDeclT t i' as' b ->
            if i == i' then
                case unifyAll (combineTypes as) (combineTypes $ map fst as') of
                    Nothing -> rec
                    Just c -> case t of
                        TVoid -> (StmtsT [b', ReturnT Nothing], n, t')
                        _ -> (b', n, t')
                        where
                        b' = applyUnificationS c b
                        t' = applyUnificationT c t
                        n = map snd as'
            else
                rec
        _ -> rec
        where
        rec = findFunction c r

flowLabel :: Int -> String
flowLabel i = "_f" ++ show i

enc :: Char -> String
enc = show . ord

seqPrintStr :: String -> Sequencer
seqPrintStr s = addCmds $ concatMap (\c -> [LDC $ enc c, PRINTC]) s

seqStmt :: [StmtT] -> StmtT -> Sequencer
seqStmt ss s = case s of
    StmtsT l -> endoSeq (seqStmt ss) l
    VarDeclT t i e -> do
        seqExp ss e
        setVariable i
    FunDeclT _ _ _ _ -> eId
    FunCallT i as -> seqFunCall ss i as
    ReturnT m -> do
        case m of
            Just e -> do
                seqExp ss e
                addCmd $ STR "RR"
            Nothing -> eId
        addCmd RET
    AssignT i fs e ->
        case fs of
            [] -> do
                seqExp ss e
                setVariable i
            _ -> do
                getVariable i
                setFields fs
                setVariable i
        where
        setFields fs = case fs of
            f : r -> do
                addCmd $ LDH 0 2
                case r of
                    [] -> case f of
                        First -> do
                            seqExp ss e
                            addCmd $ LDS (-1)
                            addCmd $ STH 2
                            addCmd $ STR "R5"
                            addCmd $ AJS (-2)
                            addCmd $ LDR "R5"
                        Second -> do
                            addCmd $ AJS (-1)
                            seqExp ss e
                            addCmd $ STH 2
                        Head -> do
                            addCmd $ AJS (-1)
                            seqExp ss e
                            addCmd $ STH 2
                        Tail -> do
                            seqExp ss e
                            addCmd $ LDS (-1)
                            addCmd $ STH 2
                            addCmd $ STR "R5"
                            addCmd $ AJS (-2)
                            addCmd $ LDR "R5"
                    _ -> case f of
                        First -> do
                            addCmd $ LDS (-1)
                            setFields r
                            addCmd $ LDS (-1)
                            addCmd $ STH 2
                            addCmd $ STR "R5"
                            addCmd $ AJS (-2)
                            addCmd $ LDR "R5"
                        Second -> do
                            setFields r
                            addCmd $ STH 2
                        Head -> do
                            setFields r
                            addCmd $ STH 2
                        Tail -> do
                            addCmd $ LDS (-1)
                            setFields r
                            addCmd $ LDS (-1)
                            addCmd $ STH 2
                            addCmd $ STR "R5"
                            addCmd $ AJS (-2)
                            addCmd $ LDR "R5"
    IfT c b m -> do
        seqExp ss c
        seqIf (seqStmt ss b) (fmap (seqStmt ss) m)
    WhileT c b -> seqWhile (seqExp ss c) (seqStmt ss b)

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
            (_, spStart, _, _, _) <- getState
            b
            addCmd $ BRA (flowLabel (f + 1))
            (_, spEnd, _, _, _) <- getState
            addCmd $ LABEL (flowLabel f)
            gsp $ const spStart
            e
            addCmd $ LABEL (flowLabel (f + 1))
            gsp $ const spEnd

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
    EIdT i fs t -> do
        getVariable i
        flip endoSeq fs $ \f -> case f of
            First -> do
                addCmd $ LDH 0 2
                addCmd $ AJS (-1)
            Second -> do
                addCmd $ LDH 0 2
                addCmd $ STR "R5"
                addCmd $ AJS (-1)
                addCmd $ LDR "R5"
            Head -> do
                addCmd $ LDH 0 2
                addCmd $ STR "R5"
                addCmd $ AJS (-1)
                addCmd $ LDR "R5"
            Tail -> do
                addCmd $ LDH 0 2
                addCmd $ AJS (-1)
    EFunCallT i as _ -> do
        seqFunCall l i as
        addCmd $ LDR "RR"
    EOp1T op e _ -> case getType e of
        TList _ -> stdop1
        TTuple _ _ -> stdop1
        _ -> do
            seqExp l e
            addCmd $ OP1 (op1name op)
        where
        stdop1 = do
            seqFunCall l ("_op_" ++ op1name op) [e]
            addCmd $ LDR "RR"
    EOp2T OCons e1 e2 _ -> do
        seqExp l e2
        seqExp l e1
        addCmd $ STH 2
    EOp2T op e1 e2 _ -> case getType e of
        TList _ -> stdop2
        TTuple _ _ -> stdop2
        _ -> do
            seqExp l e1
            seqExp l e2
            addCmd $ OP2 (op2name op)
        where
        stdop2 = do
            seqFunCall l ("_op_" ++ op2name op) [e1, e2]
            addCmd $ LDR "RR"

op1name :: Op1 -> String
op1name op = case op of
    ONot -> "not"
    ONeg -> "neg"

op2name :: Op2 -> String
op2name op = case op of
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

seqFunCall :: [StmtT] -> String -> [ExpT] -> Sequencer
seqFunCall l i as =
    let
        c = (i, map getType as)
        (b, names, rt) = findFunction c l
        n = length as + length (varDecls b)
        as' = zip names as
    in case (c, as) of
        (("print", [t]), [e]) -> case t of
            TList _ -> stdprint
            TTuple _ _ -> stdprint
            TBool -> stdprint
            TInt -> do
                seqExp l e
                addCmd PRINTI
            TChar -> do
                seqExp l e
                addCmd PRINTC
            TPoly _ -> eId -- dummy for empty list code
            where
            stdprint = seqFunCall l "_print" as
        (("read", []), []) -> do -- TODO: other types
            addCmd $ READI
            addCmd $ STR "RR"
        (("isEmpty", [TList _]), [e]) -> do
            seqExp l e
            seqIf (addCmd $ LDC "0") (Just . addCmd $ LDC "-1")
            addCmd $ STR "RR"
        _ -> do
            gtodo (todo c)
            addCmd $ LINK n
            flip endoSeq as' $ \(_, e) -> seqExp l e
            addCmd $ LDC (callLabel c)
            addCmd JSR
            addCmd $ UNLINK n
            gsp (\sp -> sp - (length as'))
