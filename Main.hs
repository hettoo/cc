module Main where
import Tokenizer
import MealyFormula

--main = print $ norm commentMarker
main = print $ (unabort . commentMarks) "?/*/a*//x//f/**/\n"
