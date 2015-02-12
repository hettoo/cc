module Tokenizer where
import Mealy

data Token = TBD

type TData = (String, Int, Int)
type AToken = (Token, TData)

type TState = (String, Int, Int)
type Command = TState -> TState

type TMealy q = Maybe Char -> (Command, Maybe Token, q)

tdata :: TState -> TData
tdata (w, c, l) = (reverse w, c, l)

readc :: Maybe Char -> Bool -> Command
readc a b (w, c, l) = case b of
    True -> as w
    False -> as []
    where
    as u = ((case a of
        Nothing -> u
        Just b -> b : u), c + 1, l)

tokenize :: (q -> TMealy q) -> q -> String -> [AToken]
tokenize m i s = tokenizef i is $ (map Just s) ++ [Nothing]
    where
    is = ([], 0, 0)
    tokenizef _ _ [] = []
    tokenizef q d (a : r) = case m i a of
        (f, x, p) -> case x of
            Nothing -> advance True
            Just t -> (t, tdata d) : advance False
            where
            advance b = tokenizef p (f $ readc a b d) r
