{-# LANGUAGE FlexibleInstances #-}
module SPL.Printer where
import SPL.Algebra
import Utils

class PrettyPrinter a where
    prettyPrint' :: Int -> a -> String
    prettyPrint :: a -> String
    prettyPrint = prettyPrint' 0

class SimplePrinter a where
    simplePrint :: a -> String

instance PrettyPrinter () where
    prettyPrint' n _ = concatMap (replicate n) "    "

elseIfChain :: Maybe Stmt -> ([(Exp, Stmt)], Maybe Stmt)
elseIfChain m = case m of
    Nothing -> ([], Nothing)
    Just (If c b m') -> left ((:) (c, b)) (elseIfChain m')
    Just s -> ([], Just s)

chainStmts :: ([(Exp, Stmt)], Maybe Stmt) -> [Stmt]
chainStmts (l, m) = case l of
    [] -> case m of
        Nothing -> []
        Just s -> [s]
    (e, s) : r -> s : chainStmts (r, m)

instance PrettyPrinter Stmt where
    prettyPrint' n stmt = case stmt of
        Stmts l -> "{\n" ++ prettyPrint' (n + 1) l ++ prettyPrint' n () ++ "}"
        _ -> prettyPrint' n () ++ case stmt of
            DataDecl i as l -> "data " ++ i ++
                foldr (\a r -> " " ++ a ++ r) "" as ++ " =" ++ cons l ++ ";\n"
                where
                cons l = case l of
                    [] -> ""
                    (c, ts) : r -> " " ++ c ++
                        foldr (\t r -> " " ++ simplePrint t ++ r) "" ts ++
                        case r of
                            [] -> ""
                            _ -> " |" ++ cons r
            VarDecl t s e -> simplePrint t ++ " " ++ s ++ " = " ++
                simplePrint e ++ ";\n"
            FunDecl t s a b -> simplePrint t ++ " " ++ s ++
                " (" ++ simplePrint a ++ ")" ++ blockPrint True b [b]
            FunCall s l -> s ++ "(" ++ simplePrint l ++ ");\n"
            Return m -> "return" ++ (case m of
                Nothing -> ""
                Just e -> " " ++ simplePrint e) ++ ";\n"
            Assign s l e -> s ++ simplePrint l ++ " = " ++
                simplePrint e ++ ";\n"
            If c b m -> printIfChain c b (elseIfChain m)
                (b : (chainStmts . elseIfChain) m)
            While e b -> "while (" ++ simplePrint e ++ ")" ++
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
                    False -> Stmts [s]
                s' = makeBlock s
                l' = map makeBlock l
                checkStmts s = case s of
                    Stmts _ -> True
                    _ -> False
        makeBlock s = case s of
            If _ _ _ -> Stmts [s]
            While _ _ -> Stmts [s]
            _ -> s

instance SimplePrinter [(Type, String)] where
    simplePrint l = case l of
        [] -> ""
        (t, s) : r -> simplePrint t ++ " " ++ s ++ case r of
            [] -> ""
            _ -> ", " ++ simplePrint r

data DeclState =
    DSPre
    | DSNormal
    | DSData
    | DSFun
    | DSVar
    deriving (Eq, Show)

instance PrettyPrinter [Stmt] where
    prettyPrint' = prettyPrint'' DSPre
        where
        prettyPrint'' s n l = case l of
            [] -> ""
            a : r -> case a of
                FunDecl _ _ _ _ | s /= DSPre ->
                    "\n" ++ e ++ prettyPrint'' DSFun n r
                VarDecl _ _ _ | not (s `elem` [DSPre, DSVar]) ->
                    "\n" ++ e ++ prettyPrint'' DSVar n r
                DataDecl _ _ _ | not (s `elem` [DSPre, DSData]) ->
                    "\n" ++ e ++ prettyPrint'' DSData n r
                FunDecl _ _ _ _ -> e ++ prettyPrint'' DSFun n r
                VarDecl _ _ _ -> e ++ prettyPrint'' DSVar n r
                DataDecl _ _ _ -> e ++ prettyPrint'' DSData n r
                _ | not (s `elem` [DSPre, DSNormal]) ->
                    "\n" ++ e ++ prettyPrint'' DSNormal n r
                _ -> e ++ prettyPrint'' DSNormal n r
                where
                e = prettyPrint' n a

instance SimplePrinter Field where
    simplePrint f = "." ++ case f of
        Head -> "hd"
        Tail -> "tl"
        First -> "fst"
        Second -> "snd"

instance SimplePrinter [Field] where
    simplePrint l = case l of
        [] -> ""
        f : r -> simplePrint f ++ simplePrint r

instance SimplePrinter Type where
    simplePrint t = case t of
        TPoly s -> s
        TCustom s l -> "\\" ++ s ++
            foldr (\a r -> " " ++ simplePrint a ++ r) "" l ++ "/"
        TInt -> "Int"
        TBool -> "Bool"
        TChar -> "Char"
        TTuple u v -> "(" ++ simplePrint u ++ ", " ++ simplePrint v ++ ")"
        TList u -> "[" ++ simplePrint u ++ "]"
        TVoid -> "Void"

instance SimplePrinter Op1 where
    simplePrint o = case o of
        ONot -> "!"
        ONeg -> "-"

instance SimplePrinter Op2 where
    simplePrint o = case o of
        OCons -> ":"
        OAnd -> "&&"
        OOr -> "||"
        OEq -> "=="
        ONeq -> "!="
        OLt -> "<"
        OGt -> ">"
        OLe -> "<="
        OGe -> ">="
        OPlus -> "+"
        OMinus -> "-"
        OTimes -> "*"
        ODiv -> "/"
        OMod -> "%"

stronger1 :: Op1 -> Exp -> Bool
stronger1 _ e = case e of
    EOp2 _ _ _ -> True
    _ -> False

data Side =
    SLeft
    | SRight
    deriving Eq

stronger2 :: Op2 -> Exp -> Side -> Bool
stronger2 o e s = case e of
    EOp2 o' a b -> (if s == assoc o then (>) else (>=))
        (strength o) (strength o')
    _ -> False

assoc :: Op2 -> Side
assoc o = case o of
    OCons -> SRight
    _ -> SLeft

strength :: Op2 -> Int
strength o = case o of
    OCons -> 0
    OAnd -> 1
    OOr -> 1
    OEq -> 2
    ONeq -> 2
    OLt -> 3
    OGt -> 3
    OLe -> 3
    OGe -> 3
    OPlus -> 4
    OMinus -> 4
    OTimes -> 5
    ODiv -> 5
    OMod -> 5

instance SimplePrinter Exp where
    simplePrint e = case e of
        EInt n -> show n
        EBool b -> show b
        EChar c -> show c
        ENil -> "[]"
        ETuple a b -> "(" ++ simplePrint a ++ ", " ++ simplePrint b ++ ")"
        EId s l -> s ++ simplePrint l
        EFunCall s l -> s ++ "(" ++ simplePrint l ++ ")"
        EOp1 o a -> simplePrint o ++ wrap (simplePrint a) (stronger1 o a)
        EOp2 o a b -> wrap (simplePrint a) (stronger2 o a SLeft) ++
            " " ++ simplePrint o ++ " " ++
            wrap (simplePrint b) (stronger2 o b SRight)
        where
        wrap s b = case b of
            True -> "(" ++ s ++ ")"
            False -> s

instance SimplePrinter [Exp] where
    simplePrint l = case l of
        [] -> ""
        e : r -> simplePrint e ++ case r of
            [] -> ""
            _ -> ", " ++ simplePrint r
