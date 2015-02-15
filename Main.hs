module Main where
import Tokenizer
import MealyFormula

--main = print $ norm layoutMarker
main = print $ layoutMarks "?/*/a*//x//f/**/\n"
