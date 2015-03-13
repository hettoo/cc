{-# LANGUAGE MultiParamTypeClasses #-}
module CharParser where
import Parser
import Listify
import Utils
import Data.Char

data CharState = CharState {
    pos :: (Int, Int),
    merror :: Maybe (String, (Int, Int))}

type CharParser v = Parser Char CharState v

instance ParserState CharState Char where
    initial = CharState (1, 1) Nothing
    update s c = case pos s of
        (a, b) -> case c of
            '\n' -> s {pos = (a + 1, 1)}
            _ -> s {pos = (a, b + 1)}
    merge s t = case (merror s, merror t) of
        (e@(Just (_, (a1, b1))), Just (_, (a2, b2))) ->
            if a1 > a2 || (a1 == a2 && b1 > b2) then t {merror = e} else t
        (_, Just _) -> t
        (Just t, _) -> s {merror = Just t}
        _ -> t
    setError s e = s {merror = Just (e, pos s)}
    getError s = case merror s of
        Nothing -> Nothing
        Just (e, (a, b)) -> Just $ e ++ " at " ++ show a ++ ":" ++ show b

ws :: CharParser (Char, [Char])
ws = plus (satisfy ((flip elem) [' ', '\n', '\r', '\t']))

ows :: CharParser [Char]
ows = opt ws >@ listify

nalphanum_ :: CharParser (Maybe Char)
nalphanum_ = sep (satisfy (\c -> not (c == '_' || isAlphaNum c)))

(.*?*.) :: CharParser v -> CharParser w -> CharParser (v, w)
(.*?*.) p q = (p .*- ows) .*. q
infixl 7 .*?*.

(-*?*.) :: CharParser v -> CharParser w -> CharParser w
(-*?*.) p q = (p .*. ows) -*. q
infixl 7 -*?*.

(.*?*-) :: CharParser v -> CharParser w -> CharParser v
(.*?*-) p q = p .*- (ows .*. q)
infixl 7 .*?*-

neCommaList :: CharParser v -> CharParser (v, [v])
neCommaList p = p .*?*. star (sym ',' -*?*. p)

commaList :: CharParser v -> CharParser [v]
commaList p = opt (neCommaList p) >@ listify
