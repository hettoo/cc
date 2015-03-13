{-# LANGUAGE MultiParamTypeClasses #-}
module CharParser where
import Parser
import Listify
import Utils
import Data.Char

data CharState = CharState {
    pos :: (Int, Int),
    aerror :: Maybe (String, (Int, Int))}

instance ParserState CharState Char where
    initial = CharState (1, 1) Nothing
    update s c = case pos s of
        (a, b) -> case c of
            '\n' -> s {pos = (a + 1, 1)}
            _ -> s {pos = (a, b + 1)}
    merge s t = case (aerror s, aerror t) of
        (e@(Just (m1, (a1, b1))), Just (m2, (a2, b2))) ->
            if m2 == "" || a1 > a2 || (a1 == a2 && b1 > b2) then
                t {aerror = e}
            else
                t
        (_, Just _) -> t
        (Just t, _) -> s {aerror = Just t}
        _ -> t
    setError s e = s {aerror = Just (e, pos s)}
    getError s = case aerror s of
        Nothing -> Nothing
        Just (e, (a, b)) -> Just $ e ++ " at " ++ show a ++ ":" ++ show b

newtype CharReState = Re CharState

instance ParserState CharReState CharState where
    initial = Re initial
    update (Re s) t = Re t {aerror = aerror s}
    merge (Re s) (Re t) = Re (merge s t)
    setError (Re s) e = Re (setError s e)
    getError (Re s) = getError s

type CharParser v = Parser Char Char CharState v
type CharReParser v = Parser Char CharState CharReState v

ws :: ParserState s b =>
    Parser Char b s (Char, [Char])
ws = plus (satisfy ((flip elem) [' ', '\n', '\r', '\t']))

ows :: ParserState s b =>
    Parser Char b s [Char]
ows = opt ws >@ listify

nalphanum_ :: ParserState s b =>
    Parser Char b s (Maybe Char)
nalphanum_ = sep (satisfy (\c -> not (c == '_' || isAlphaNum c)))

(.*?*.) :: ParserState s b =>
    Parser Char b s v -> Parser Char b s w -> Parser Char b s (v, w)
(.*?*.) p q = (p .*- ows) .*. q
infixl 7 .*?*.

(-*?*.) :: ParserState s b =>
    Parser Char b s v -> Parser Char b s w -> Parser Char b s w
(-*?*.) p q = (p .*. ows) -*. q
infixl 7 -*?*.

(.*?*-) :: ParserState s b =>
    Parser Char b s v -> Parser Char b s w -> Parser Char b s v
(.*?*-) p q = p .*- (ows .*. q)
infixl 7 .*?*-

neCommaList :: ParserState s b =>
    Parser Char b s v -> Parser Char b s (v, [v])
neCommaList p = p .*?*. star (sym ',' -*?*. p)

commaList :: ParserState s b =>
    Parser Char b s v -> Parser Char b s [v]
commaList p = opt (neCommaList p) >@ listify
