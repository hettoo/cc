module Main where
import Tokenizer
import MealyFormula

main = do
    s <- getContents
    print $ tokenize s
