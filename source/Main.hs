module Main where
import Tokenizer
import MealyFormula

--main = print $ norm layoutMarker
main = do
    s <- getContents
    print $ tokenize s
