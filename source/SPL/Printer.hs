{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module SPL.Printer where
import SPL.Algebra
import Fix
import Utils
import Data.List

class PrettyPrinter a where
    prettyPrint' :: Int -> a -> String
    prettyPrint :: a -> String
    prettyPrint = prettyPrint' 0

class SimplePrinter a where
    simplePrint :: a -> String

instance PrettyPrinter () where
    prettyPrint' n _ = concatMap (replicate n) "    "

elseIfChain :: Maybe (PStmt e) -> ([(e, PStmt e)], Maybe (PStmt e))
elseIfChain m = case m of
    Nothing -> ([], Nothing)
    Just (If c b m') -> left ((:) (c, b)) (elseIfChain m')
    Just s -> ([], Just s)

chainStmts :: ([(e, PStmt e)], Maybe (PStmt e)) -> [PStmt e]
chainStmts (l, m) = case l of
    [] -> case m of
        Nothing -> []
        Just s -> [s]
    (e, s) : r -> s : chainStmts (r, m)

instance (SimplePrinter e, SimplePrinter [e]) => PrettyPrinter (PStmt e) where
    prettyPrint' n stmt = case stmt of
        Stmts l -> "{\n" ++ prettyPrint' (n + 1) l ++ prettyPrint' n () ++ "}"
        _ -> prettyPrint' n () ++ case stmt of
            DataDecl i as l -> "data " ++ i ++
                foldr (\a r -> " " ++ a ++ r) "" as ++ " = " ++
                intercalate " | " (map cons l) ++ ";\n"
                where
                cons (c, ts) = c ++ " " ++ simplePrint ts
            VarDecl t s e -> simplePrint t ++ " " ++ s ++ " = " ++
                simplePrint e ++ ";\n"
            FunDecl t s as b -> simplePrint t ++ " " ++ s ++
                " " ++ simplePrint as ++ blockPrint True b [b]
            FunCall s l -> s ++ "(" ++ simplePrint l ++ ");\n"
            Return m -> "return" ++ (case m of
                Nothing -> ""
                Just e -> " " ++ simplePrint e) ++ ";\n"
            Assign s l e -> s ++ concatMap ('.' :) l ++ " = " ++
                simplePrint e ++ ";\n"
            Case e bs -> "case " ++ simplePrint e ++ " {" ++
                foldr singleCase "" bs ++ "\n" ++ prettyPrint' n () ++ "}\n"
                where
                singleCase (i, b) r = "\n" ++ prettyPrint' (n + 1) () ++ i ++
                    " " ++ prettyPrint' (n + 1) (makeBlockStrong b) ++ r
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
                True -> " " ++
                    prettyPrint' n (if checkStmts s then s else Stmts [s]) ++
                    if b then "\n" else " "
                False -> "\n" ++ prettyPrint' (n + 1) s' ++
                    if b then "" else prettyPrint' n ()
                where
                s' = makeBlock s
                l' = map makeBlock l
                checkStmts s = case s of
                    Stmts _ -> True
                    _ -> False
        makeBlock s = case s of
            Case _ _ -> Stmts [s]
            If _ _ _ -> Stmts [s]
            While _ _ -> Stmts [s]
            _ -> s
        makeBlockStrong s = case s of
            Stmts _ -> s
            _ -> Stmts [s]

instance SimplePrinter [(Type, String)] where
    simplePrint l = "(" ++ intercalate ", "
        (map (\(t, s) -> simplePrint t ++ " " ++ s) l) ++ ")"

data DeclState =
    DSPre
    | DSNormal
    | DSData
    | DSFun
    | DSVar
    deriving (Eq, Show)

instance (SimplePrinter e, SimplePrinter [e]) => PrettyPrinter [PStmt e] where
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

instance SimplePrinter Type where
    simplePrint t = case t of
        TPoly s -> s
        TCustom s l -> case s of
            "List" -> let [u] = l in "[" ++ simplePrint u ++ "]"
            "Tuple" -> let [u, v] = l in
                "(" ++ simplePrint u ++ ", " ++ simplePrint v ++ ")"
            _ -> "\\" ++ s ++ foldr (\a r -> " " ++ simplePrint a ++ r) "" l ++
                "/"
        TInt -> "Int"
        TBool -> "Bool"
        TChar -> "Char"
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

data Side =
    SLeft
    | SRight
    deriving Eq

class Strength e where
    stronger1 :: Op1 -> e -> Bool
    stronger2 :: Op2 -> e -> Side -> Bool

instance Strength Exp where
    stronger1 _ e = case unFix e of
        EOp2 _ _ _ -> True
        _ -> False
    stronger2 o e s = case unFix e of
        EOp2 o' a b -> (if s == assoc o then (>) else (>=))
            (strength o) (strength o')
        _ -> False

instance Strength ExpT where
    stronger1 _ _ = False
    stronger2 _ _ _ = False

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

instance (SimplePrinter e, SimplePrinter [e], Strength e) =>
    SimplePrinter (PExp e) where
    simplePrint e = case e of
        EInt n -> show n
        EBool b -> show b
        EChar c -> ['\'', c, '\'']
        ENil -> "[]"
        ETuple a b -> "(" ++ simplePrint a ++ ", " ++ simplePrint b ++ ")"
        EId s l -> s ++ concatMap ('.' :) l
        ECons i as -> i ++ if null as then "" else "(" ++ simplePrint as ++ ")"
        EFunCall s l -> s ++ "(" ++ simplePrint l ++ ")"
        EOp1 o a -> simplePrint o ++ wrap (simplePrint a) (stronger1 o a)
        EOp2 o a b -> wrap (simplePrint a) (stronger2 o a SLeft) ++
            " " ++ simplePrint o ++ " " ++
            wrap (simplePrint b) (stronger2 o b SRight)
        where
        wrap s b = if b then "(" ++ s ++ ")" else s

instance (SimplePrinter e, SimplePrinter [e], Strength e) =>
    SimplePrinter (PExpT e) where
    simplePrint p = "(" ++ simplePrint (expC p) ++
        " @ " ++ simplePrint (typeC p) ++ ")"

instance SimplePrinter (f (Fix f)) => SimplePrinter (Fix f) where
    simplePrint = simplePrint . unFix

instance SimplePrinter (Fix f) => SimplePrinter [Fix f] where
    simplePrint l = case l of
        [] -> ""
        e : r -> simplePrint e ++ if null r then "" else ", " ++ simplePrint r
