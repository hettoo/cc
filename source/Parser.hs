{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Parser where
import Listify
import Utils

type Parser a s v = [a] -> s -> (s, Maybe (v, [a]))

class ParserState s a | s -> a where
    initial :: s
    update :: s -> a -> s
    merge :: s -> s -> s
    setError :: s -> String -> s
    getError :: s -> Maybe String

parse :: (ParserState s a, Eq a, Eq v, Show v) =>
    Parser a s v -> [a] -> v
parse p l = case p l initial of
    (_, Just (v, r)) -> v
    (s, _) -> error $ "parser error" ++ case getError s of
        Nothing -> ""
        Just e -> ": " ++ e

-- Main parser building blocks

nop :: Parser a s v
nop _ s = (s, Nothing)

yield :: v -> Parser a s v
yield v l s = (s, Just (v, l))

injectError :: ParserState s a =>
    Parser a s v -> String -> Parser a s v
injectError p e l s = p l (setError s e)

skip :: ParserState s a =>
    Parser a s a
skip l s = case l of
    a : r -> yield a r (update s a)
    _ -> yieldError "unexpected eof" l s

eof :: ParserState s a =>
    Parser a s ()
eof l = (if isEmpty l then yield () else yieldError "expected eof") l

satisfy :: (ParserState s a, Show a) =>
    (a -> Bool) -> Parser a s a
satisfy f l s = case l of
    a : r -> case f a of
        True -> yield a r (update s a)
        False -> yieldError ("unexpected symbol " ++ show a) l s
    _ -> yieldError "unexpected eof" l s

phantom :: Parser a s v -> Parser a s v
phantom p l s = pair (const s, fmap (\t -> (fst t, l))) (p l s)

cond :: ParserState s a =>
    Parser a s v -> Parser a s v -> Parser a s v
cond p q l s = case p l s of
    (t, Nothing) -> left (merge t) (q l s)
    x -> x

(>.) :: Parser a s v -> Parser a s (v -> w) -> Parser a s w
(>.) p q l s = case p l s of
    (t, Just (v, l')) -> pair (id, fmap (\(f, l'') -> (f v, l''))) (q l' t)
    (t, Nothing) -> (t, Nothing)
infixl 7 >.

-- Some useful abbreviations

(.*.) :: ParserState s a =>
    Parser a s v -> Parser a s w -> Parser a s (v, w)
(.*.) p q = cond (p >. (q >@ \w v -> (v, w))) nop
infixl 7 .*.

(.*-) :: ParserState s a =>
    Parser a s v -> Parser a s w -> Parser a s v
(.*-) p q = p .*. q >@ fst
infixl 7 .*-

(-*.) :: ParserState s a =>
    Parser a s v -> Parser a s w -> Parser a s w
(-*.) p q = p .*. q >@ snd
infixl 7 -*.

(>@) :: Parser a s v -> (v -> w) -> Parser a s w
(>@) p f = p >. yield f
infixl 6 >@

(>!) :: Parser a s v -> w -> Parser a s w
(>!) p w = p >@ const w
infixl 6 >!

(\/) :: ParserState s a =>
    Parser a s v -> Parser a s v -> Parser a s v
(\/) = cond
infixr 5 \/

(\+/) :: ParserState s a =>
    Parser a s v -> Parser a s w -> Parser a s (Either v w)
(\+/) p q = (p >@ Left) \/ (q >@ Right)
infixr 5 \+/

yieldError :: ParserState s a =>
    String -> Parser a s v
yieldError = injectError nop

sep :: ParserState s a =>
    Parser a s v -> Parser a s (Maybe v)
sep p = phantom p >@ Just \/ eof >! Nothing

opt :: ParserState s a =>
    Parser a s v -> Parser a s (Maybe v)
opt p = p >@ Just \/ injectError (yield Nothing) ""

plus :: ParserState s a =>
    Parser a s v -> Parser a s (v, [v])
plus p = p .*. (opt (plus p >@ uncurry (:)) >@ listify)

star :: ParserState s a =>
    Parser a s v -> Parser a s [v]
star p = opt (plus p) >@ listify

anything :: (ParserState s a, Show a) =>
    Parser a s a
anything = satisfy $ const True

sym :: (Eq a, ParserState s a, Show a) =>
    a -> Parser a s a
sym a = satisfy ((==) a)

nsym :: (Eq a, ParserState s a, Show a) =>
    a -> Parser a s a
nsym a = satisfy ((/=) a)

sseq :: (Eq a, ParserState s a, Show a) =>
    [a] -> Parser a s [a]
sseq l = case l of
    [] -> yield []
    a : r -> sym a .*. sseq r >@ uncurry (:)
