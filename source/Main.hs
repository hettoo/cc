module Main where
import SPL.Algebra
import Parser.Parser
import SPL.Parser
import SPL.Printer
import SPL.TypeChecker

testPrinter :: String -> Bool
testPrinter s = p == (parseSPL . prettyPrint) p
    where
    p = parseSPL s

testParser :: [Stmt] -> Bool
testParser l = s == (prettyPrint . parseSPL) s
    where
    s = prettyPrint l

main = do
    s <- getContents
    putStr . prettyPrint . parseSPL $ s
    putStrLn $ "Parser |= Printer: " ++ show (testPrinter s)
