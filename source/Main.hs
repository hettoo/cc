module Main where
import SPL.Structure
import Parser
import SPL.PreParser
import SPL.Parser
import SPL.Printer

parseSPL :: String -> [Stmt]
parseSPL = parse $ pPre >@ parse pSPL

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
