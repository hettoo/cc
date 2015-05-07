module SPL.Sequencer where
import SPL.Algebra
import SPL.Unifier
import SPL.Parser
import SPL.Typer
import SPL.Std
import Context
import Todo
import State
import Data.Char
import Data.List

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

type Call = (String, [Type])
type SP = Int
type VarContext = Context (SP, Bool)
data SO = SO {
    todoCalls :: Todo Call,
    sp :: SP,
    vc :: VarContext,
    freshNum :: Int,
    cmds :: [Command]}
type Sequencer = State SO ()

gtodo :: State (Todo Call) a -> State SO a
gtodo = stWrap todoCalls (\x c -> c {todoCalls = x})

gsp :: State SP a -> State SO a
gsp = stWrap sp (\x c -> c {sp = x})

gvc :: State VarContext a -> State SO a
gvc = stWrap vc (\x c -> c {vc = x})

gfresh :: State Int a -> State SO a
gfresh = stWrap freshNum (\x c -> c {freshNum = x})

gcmd :: State [Command] a -> State SO a
gcmd = stWrap cmds (\x c -> c {cmds = x})

addCmd :: Command -> Sequencer
addCmd c = do
    gcmd . st $ \l -> l ++ [c]
    gsp . st $ \sp -> sp + stackChange c

addCmds :: [Command] -> Sequencer
addCmds cs = foldl (>>) ids $ map addCmd cs

seqOutput :: [StmtT] -> String
seqOutput l = stateOutput $ program l >@> initialState

program :: [StmtT] -> Sequencer
program l = do
    globals l'
    gvc cdown
    let (_, _, t) = findFunction ("main", []) l in
        case t of
            TVoid -> seqStmt l' (FunCall "main" [])
            _ -> seqStmt l' (FunCall "print" [EFunCallT "main" [] t])
    addCmd $ HALT "program end"
    seqTodo l'
    where
    l' = l ++ annotateProgram (parseSPL' True stdSPL)

initialState :: SO
initialState = SO {todoCalls = tnew, sp = 0, vc = cnew, freshNum = 0, cmds = []}

stateOutput :: SO -> String
stateOutput = unlines . (map cmdOutput) . cmds

globals :: [StmtT] -> Sequencer
globals l = do
    addCmd $ LDRR "R7" "SP"
    setGlobals l 0
    where
    setGlobals l n = case l of
        [] -> ids
        s : r -> case s of
            VarDecl t i e -> do
                seqExp l e
                addVariable i n True
                setGlobals r (n + 1)
            _ -> setGlobals r n

seqTodo :: [StmtT] -> Sequencer
seqTodo l = do
    sp <- gsp getState
    vc <- gvc getState
    seqTodo' sp vc l
    where
    seqTodo' sp vc l = do
        t <- gtodo getState
        case getTodo t of
            (Nothing, _) -> ids
            (Just c@(i, as), t') -> do
                gtodo . st $ const t'
                gsp . st $ const (sp + length as)
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
    sp <- gsp getState
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
    sp <- gsp getState
    vc <- gvc getState
    let (p, b) = clookupe i >!> vc in
        return $ if b then (p, False) else (p - sp, True)

varDecls :: StmtT -> [(String, ExpT)]
varDecls t = case t of
    Stmts l -> concat $ map varDecls l
    VarDecl _ i e -> [(i, e)]
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
        [] -> ids
        a : r -> do
            addVariable a n False
            addVariables (n + 1) r

findFunction :: Call -> [StmtT] -> (StmtT, [String], Type)
findFunction c@(i, as) l = case l of
    [] -> error $ "function " ++ show c ++ " not found"
    s : r -> case s of
        FunDecl t i' as' b ->
            if i == i' then
                case unifyAll (combineTypes as) (combineTypes $ map fst as') of
                    Nothing -> rec
                    Just c -> case t of
                        TVoid -> (Stmts [b', Return Nothing], n, t')
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
    Stmts l -> sequence_ $ map (seqStmt ss) l
    VarDecl t i e -> do
        seqExp ss e
        setVariable i
    FunDecl _ _ _ _ -> ids
    FunCall i as -> seqFunCall ss i as
    Return m -> do
        case m of
            Just e -> do
                seqExp ss e
                addCmd $ STR "RR"
            Nothing -> ids
        addCmd RET
    Assign i fs e ->
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
    If c b m -> do
        seqExp ss c
        seqIf (seqStmt ss b) (fmap (seqStmt ss) m)
    While c b -> seqWhile (seqExp ss c) (seqStmt ss b)

seqIf :: Sequencer -> Maybe Sequencer -> Sequencer
seqIf b m = do
    f <- gfresh getState
    case m of
        Nothing -> do
            gfresh . st $ (+1)
            addCmd $ BRF (flowLabel f)
            sp <- gsp getState
            b
            addCmd $ LABEL (flowLabel f)
            gsp . st $ const sp
        Just e -> do
            gfresh . st $ (+2)
            addCmd $ BRF (flowLabel f)
            spStart <- gsp getState
            b
            addCmd $ BRA (flowLabel (f + 1))
            spEnd <- gsp getState
            addCmd $ LABEL (flowLabel f)
            gsp . st $ const spStart
            e
            addCmd $ LABEL (flowLabel (f + 1))
            gsp . st $ const spEnd

seqWhile :: Sequencer -> Sequencer -> Sequencer
seqWhile c b = do
    f <- gfresh getState
    gfresh . st $ (+2)
    addCmd $ LABEL (flowLabel f)
    c
    addCmd $ BRF (flowLabel (f + 1))
    b
    addCmd $ BRA (flowLabel f)
    addCmd $ LABEL (flowLabel (f + 1))

consIndex :: [StmtT] -> String -> Int
consIndex l i = case l of
    a : r -> case a of
        DataDecl _ _ cs -> case findIndex (\t -> fst t == i) cs of
            Just n -> n
            Nothing -> consIndex r i

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
        sequence_ $ map seqField fs
        where
        seqField f = case f of
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
    EConsT i as t -> do
        n <- return $ consIndex l i
        addCmd $ LDC (show n)
        sequence_ $ map (seqExp l) as
        addCmd $ STH (length as + 1)
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
            TPoly _ -> ids -- dummy for empty list code
            where
            stdprint = seqFunCall l "_print" as
        (("read", []), []) -> do
            addCmd $ READI
            addCmd $ STR "RR"
        (("isEmpty", [TList _]), [e]) -> do
            seqExp l e
            seqIf (addCmd $ LDC "0") (Just . addCmd $ LDC "-1")
            addCmd $ STR "RR"
        _ -> do
            gtodo . st $ todo c
            addCmd $ LINK n
            sequence $ map (\(_, e) -> seqExp l e) as'
            addCmd $ LDC (callLabel c)
            addCmd JSR
            addCmd $ UNLINK n
            gsp . st $ \sp -> sp - (length as')
