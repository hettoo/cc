{-# LANGUAGE FlexibleInstances #-}
module SPL.PrinterT where
import SPL.Algebra
import SPL.Printer
import SPL.Typer
import Utils

elseIfChainT :: Maybe StmtT -> ([(ExpT, StmtT)], Maybe StmtT)
elseIfChainT m = case m of
    Nothing -> ([], Nothing)
    Just (IfT c b m') -> left ((:) (c, b)) (elseIfChainT m')
    Just s -> ([], Just s)

chainStmtsT :: ([(ExpT, StmtT)], Maybe StmtT) -> [StmtT]
chainStmtsT (l, m) = case l of
    [] -> case m of
        Nothing -> []
        Just s -> [s]
    (e, s) : r -> s : chainStmtsT (r, m)

instance PrettyPrinter StmtT where
    prettyPrint' n stmt = case stmt of
        StmtsT l -> "{\n" ++ prettyPrint' (n + 1) l ++ prettyPrint' n () ++ "}"
        _ -> prettyPrint' n () ++ case stmt of
            DataDeclT i as l -> "data " ++ i ++
                foldr (\a r -> " " ++ a ++ r) "" as ++ " =" ++ cons l ++ ";\n"
                where
                cons l = case l of
                    [] -> ""
                    (c, ts) : r -> " " ++ c ++
                        foldr (\t r -> " " ++ simplePrint t ++ r) "" ts ++
                        case r of
                            [] -> ""
                            _ -> " |" ++ cons r
            VarDeclT t s e -> simplePrint t ++ " " ++ s ++ " = " ++
                simplePrint e ++ ";\n"
            FunDeclT t s a b -> simplePrint t ++ " " ++ s ++
                " (" ++ simplePrint a ++ ")" ++ blockPrint True b [b]
            FunCallT s l -> s ++ "(" ++ simplePrint l ++ ");\n"
            ReturnT m -> "return" ++ (case m of
                Nothing -> ""
                Just e -> " " ++ simplePrint e) ++ ";\n"
            AssignT s l e -> s ++ simplePrint l ++ " = " ++
                simplePrint e ++ ";\n"
            IfT c b m -> printIfChain c b (elseIfChainT m)
                (b : (chainStmtsT . elseIfChainT) m)
            WhileT e b -> "while (" ++ simplePrint e ++ ")" ++
                blockPrint True b [b]
        where
        printIfChain c b p h = "if (" ++ simplePrint c ++ ")" ++ case p of
            ([], Nothing) -> blockPrint True b h
            _ -> blockPrint False b h ++ printElseIfChain p h
        printElseIfChain p h = case p of
            ([], m) -> printElse m h
            ((c, b) : r, m) -> "else if (" ++ simplePrint c ++ ")" ++
                case (r, m) of
                    ([], Nothing) -> blockPrint True b h
                    _ -> blockPrint False b h ++ printElseIfChain (r, m) h
        printElse m h = case m of
            Nothing -> ""
            Just e -> "else" ++ blockPrint True e h
        blockPrint b s l = case foldl (||) False (map checkStmts l') of
                True -> " " ++ prettyPrint' n w ++ case b of
                    True -> "\n"
                    False -> " "
                False -> "\n" ++ prettyPrint' (n + 1) s' ++
                    case b of
                        True -> ""
                        False -> prettyPrint' n ()
                where
                w = case checkStmts s of
                    True -> s
                    False -> StmtsT [s]
                s' = makeBlock s
                l' = map makeBlock l
                checkStmts s = case s of
                    StmtsT _ -> True
                    _ -> False
        makeBlock s = case s of
            IfT _ _ _ -> StmtsT [s]
            WhileT _ _ -> StmtsT [s]
            _ -> s

instance PrettyPrinter [StmtT] where
    prettyPrint' = prettyPrint'' DSPre
        where
        prettyPrint'' s n l = case l of
            [] -> ""
            a : r -> case a of
                FunDeclT _ _ _ _ | s /= DSPre ->
                    "\n" ++ e ++ prettyPrint'' DSFun n r
                VarDeclT _ _ _ | not (s `elem` [DSPre, DSVar]) ->
                    "\n" ++ e ++ prettyPrint'' DSVar n r
                DataDeclT _ _ _ | not (s `elem` [DSPre, DSData]) ->
                    "\n" ++ e ++ prettyPrint'' DSData n r
                FunDeclT _ _ _ _ -> e ++ prettyPrint'' DSFun n r
                VarDeclT _ _ _ -> e ++ prettyPrint'' DSVar n r
                DataDeclT _ _ _ -> e ++ prettyPrint'' DSData n r
                _ | not (s `elem` [DSPre, DSNormal]) ->
                    "\n" ++ e ++ prettyPrint'' DSNormal n r
                _ -> e ++ prettyPrint'' DSNormal n r
                where
                e = prettyPrint' n a

instance SimplePrinter ExpT where
    simplePrint e = "(" ++ (case e of
        EIntT n _ -> show n
        EBoolT b _ -> show b
        ECharT c _ -> show c
        ENilT _ -> "[]"
        ETupleT a b _ -> "(" ++ simplePrint a ++ ", " ++ simplePrint b ++ ")"
        EIdT s l _ -> s ++ simplePrint l
        EFunCallT s l _ -> s ++ "(" ++ simplePrint l ++ ")"
        EOp1T o a _ -> simplePrint o ++ simplePrint a
        EOp2T o a b _ -> simplePrint a ++ " " ++ simplePrint o
            ++ " " ++ simplePrint b) ++ " @ " ++ simplePrint (getType e) ++ ")"
        where
        wrap s b = case b of
            True -> "(" ++ s ++ ")"
            False -> s

instance SimplePrinter [ExpT] where
    simplePrint l = case l of
        [] -> ""
        e : r -> simplePrint e ++ case r of
            [] -> ""
            _ -> ", " ++ simplePrint r
