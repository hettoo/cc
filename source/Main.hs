module Main where
import Tokenizer
import MealyFormula

--main = print $ norm layoutMarker
main = print $ tokenize "=?/*/a*//x//f/**/\n"
