{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Parser.Parser where
import Listify
import Utils

type Parser a b s v = [(a, b)] -> s -> (s, Maybe (v, [(a, b)]))

class ParserState s b | s -> b where
    initial :: s
    update :: s -> b -> s
    merge :: s -> s -> s
    setError :: s -> String -> s
    getError :: s -> Maybe String

parse :: (ParserState s b, Eq a, Eq v, Show v) =>
    Parser a b s v -> [(a, b)] -> v
parse p l = case p l initial of
    (_, Just (v, r)) -> v
    (s, _) -> error $ "parser error" ++ case getError s of
        Nothing -> ""
        Just e -> ": " ++ e

-- Main parser building blocks

nop :: Parser a b s v
nop _ s = (s, Nothing)

yield :: v -> Parser a b s v
yield v l s = (s, Just (v, l))

injectError :: ParserState s b =>
    Parser a b s v -> String -> Parser a b s v
injectError p e l s = p l (setError s e)

skip :: ParserState s b =>
    Parser a b s a
skip l s = case l of
    (a, b) : r -> yield a r (update s b)
    _ -> yieldError "unexpected eof" l s

eof :: ParserState s b =>
    Parser a b s ()
eof l = (if null l then yield () else yieldError "expected eof") l

satisfy :: (ParserState s b, Show a) =>
    (a -> Bool) -> Parser a b s a
satisfy f l s = case l of
    (a, b) : r -> (case f a of
        True -> yield a r
        False -> yieldError ("unexpected symbol " ++ show a) l) (update s b)
    _ -> yieldError "unexpected eof" l s

phantom :: Parser a b s v -> Parser a b s v
phantom p l s = pair (const s, fmap (\t -> (fst t, l))) (p l s)

cond :: ParserState s b =>
    Parser a b s v -> Parser a b s v -> Parser a b s v
cond p q l s = case p l s of
    (t, Nothing) -> left (merge t) (q l s)
    x -> x

reveal :: Parser a b s v -> Parser a b s (v, s)
reveal p l s = case p l s of
    (t, Just (v, r)) -> (t, Just ((v, t), r))
    (t, Nothing) -> (t, Nothing)

(>.) :: ParserState s b =>
    Parser a b s v -> Parser a b s (v -> w) -> Parser a b s w
(>.) p q l s = case p l s of
    (t, Just (v, l')) -> pair (merge t, fmap (\(f, l'') -> (f v, l''))) (q l' t)
    (t, Nothing) -> (t, Nothing)
infixl 7 >.

-- Some useful abbreviations

(.*.) :: ParserState s b =>
    Parser a b s v -> Parser a b s w -> Parser a b s (v, w)
(.*.) p q = cond (p >. (q >@ \w v -> (v, w))) nop
infixl 7 .*.

(.*-) :: ParserState s b =>
    Parser a b s v -> Parser a b s w -> Parser a b s v
(.*-) p q = p .*. q >@ fst
infixl 7 .*-

(-*.) :: ParserState s b =>
    Parser a b s v -> Parser a b s w -> Parser a b s w
(-*.) p q = p .*. q >@ snd
infixl 7 -*.

(>@) :: ParserState s b =>
    Parser a b s v -> (v -> w) -> Parser a b s w
(>@) p f = p >. yield f
infixl 6 >@

(>!) :: ParserState s b =>
    Parser a b s v -> w -> Parser a b s w
(>!) p w = p >@ const w
infixl 6 >!

(\/) :: ParserState s b =>
    Parser a b s v -> Parser a b s v -> Parser a b s v
(\/) = cond
infixr 5 \/

(\+/) :: ParserState s b =>
    Parser a b s v -> Parser a b s w -> Parser a b s (Either v w)
(\+/) p q = (p >@ Left) \/ (q >@ Right)
infixr 5 \+/

yieldError :: ParserState s b =>
    String -> Parser a b s v
yieldError = injectError nop

sep :: ParserState s b =>
    Parser a b s v -> Parser a b s (Maybe v)
sep p = phantom p >@ Just \/ eof >! Nothing

opt :: ParserState s b =>
    Parser a b s v -> Parser a b s (Maybe v)
opt p = p >@ Just \/ injectError (yield Nothing) ""

plus :: ParserState s b =>
    Parser a b s v -> Parser a b s (v, [v])
plus p = p .*. (opt (plus p >@ uncurry (:)) >@ listify)

star :: ParserState s b =>
    Parser a b s v -> Parser a b s [v]
star p = opt (plus p) >@ listify

anything :: (ParserState s b, Show a) =>
    Parser a b s a
anything = satisfy $ const True

sym :: (Eq a, ParserState s b, Show a) =>
    a -> Parser a b s a
sym a = satisfy ((==) a)

nsym :: (Eq a, ParserState s b, Show a) =>
    a -> Parser a b s a
nsym a = satisfy ((/=) a)

sseq :: (Eq a, ParserState s b, Show a) =>
    [a] -> Parser a b s [a]
sseq l = case l of
    [] -> yield []
    a : r -> sym a .*. sseq r >@ uncurry (:)
