{-# LANGUAGE FlexibleInstances #-}
module SPL.Printer where
import SPL.Structure

class PrettyPrinter a where
    prettyPrint' :: Int -> a -> String
    prettyPrint :: a -> String
    prettyPrint = prettyPrint' 0

class SimplePrinter a where
    simplePrint :: a -> String

instance PrettyPrinter () where
    prettyPrint' n _ = concatMap (replicate n) "    "

instance PrettyPrinter Stmt where
    prettyPrint' n stmt = case stmt of
        Stmts l -> "{\n" ++ prettyPrint' (n + 1) l ++ prettyPrint' n () ++ "}"
        _ -> prettyPrint' n () ++ case stmt of
            VarDecl t s e -> simplePrint t ++ " " ++ s ++ " = " ++
                simplePrint e ++ ";\n"
            FunDecl t s a b -> simplePrint t ++ " " ++ s ++
                " (" ++ simplePrint a ++ ")" ++ blockPrint True (Stmts b) ++ "\n"
            FunCall s l -> s ++ "(" ++ simplePrint l ++ ");\n"
            Return m -> "return" ++ (case m of
                Nothing -> ""
                Just e -> " " ++ simplePrint e ++ "") ++ ";\n"
            Assign s l e -> s ++ simplePrint l ++ " = " ++
                simplePrint e ++ ";\n"
            If c b m -> "if (" ++ simplePrint c ++ ")" ++
                case m of
                    Nothing -> blockPrint True b
                    Just e -> blockPrint False b ++ "else" ++ blockPrint True e
            While e b -> "while (" ++ simplePrint e ++ ")" ++ blockPrint True b
        where
        blockPrint b s = case s of
                Stmts _ -> " " ++ prettyPrint' n s ++ case b of
                    True -> "\n"
                    False -> " "
                _ -> "\n" ++ prettyPrint' (n + 1) s ++ "" ++ case b of
                    True -> ""
                    False -> prettyPrint' n ()

instance SimplePrinter [(Type, String)] where
    simplePrint l = case l of
        [] -> ""
        (t, s) : r -> simplePrint t ++ " " ++ s ++ case r of
            [] -> ""
            _ -> ", " ++ simplePrint r

instance PrettyPrinter [Stmt] where
    prettyPrint' n l = case l of
        [] -> ""
        a : r -> prettyPrint' n a ++ prettyPrint' n r

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
        TInt -> "Int"
        TBool -> "Bool"
        TChar -> "Char"
        TTuple u v -> "(" ++ simplePrint u ++ ", " ++ simplePrint v ++ ")"
        TList u -> "[" ++ simplePrint u ++ "]"
        TVoid -> "Void"

instance SimplePrinter Exp where
    simplePrint e = case e of
        EInt n -> show n
        EBool b -> show b
        EChar c -> show c
        ENil -> "[]"
        ECons a b -> simplePrint a ++ " : " ++ simplePrint b
        ETuple a b -> "(" ++ simplePrint a ++ ", " ++ simplePrint b ++ ")"
        EId s l -> s ++ simplePrint l
        EFunCall s l -> s ++ "(" ++ simplePrint l ++ ")"
        EAnd a b -> simplePrint a ++ " && " ++ simplePrint b
        EOr a b -> simplePrint a ++ " || " ++ simplePrint b
        EEq a b -> simplePrint a ++ " == " ++ simplePrint b
        ENeq a b -> simplePrint a ++ " != " ++ simplePrint b
        ELt a b -> simplePrint a ++ " < " ++ simplePrint b
        EGt a b -> simplePrint a ++ " > " ++ simplePrint b
        ELe a b -> simplePrint a ++ " <= " ++ simplePrint b
        EGe a b -> simplePrint a ++ " >= " ++ simplePrint b
        EPlus a b -> simplePrint a ++ " + " ++ simplePrint b
        EMinus a b -> simplePrint a ++ " - " ++ simplePrint b
        ETimes a b -> simplePrint a ++ " * " ++ simplePrint b
        EDiv a b -> simplePrint a ++ " / " ++ simplePrint b
        EMod a b -> simplePrint a ++ " % " ++ simplePrint b
        ENot a -> "!" ++ simplePrint a
        ENeg a -> "-" ++ simplePrint a

instance SimplePrinter [Exp] where
    simplePrint l = case l of
        [] -> ""
        e : r -> simplePrint e ++ case r of
            [] -> ""
            _ -> ", " ++ simplePrint r
