module Main where
import System.Environment
import SPL.Algebra
import Parser.Parser
import SPL.Parser
import SPL.Printer
import SPL.Typer
import SPL.PrinterT
import SPL.Sequencer
import State

testPrinter :: String -> Bool
testPrinter s = p == (parseSPL . prettyPrint) p
    where
    p = parseSPL s

testParser :: [Stmt] -> Bool
testParser l = s == (prettyPrint . parseSPL) s
    where
    s = prettyPrint l

lookupPath :: [String] -> FilePath
lookupPath args = case args of
   [] -> "a.ssm"
   ("-o" : (path : _)) -> path
   (_ : a) -> lookupPath a

main = do
    args <- getArgs
    s <- getContents
    let p = annotateProgram . parseSPL $ s
    writeFile (lookupPath args) $ seqOutput p
    putStr . prettyPrint $ p
    --putStr . prettyPrint . parseSPL $ s
    --putStrLn $ "Parser |= Printer: " ++ show (testPrinter s)
