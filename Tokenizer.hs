module Tokenizer where
import SemiLattice
import Mealy

data Token = TAssign
           | TSemicolon
           | TComma
           | TLeftParenthesis
           | TRightParenthesis
           | TLeftCBracket
           | TRightCBracket
           | TLeftBracket
           | TRightBracket
           | TVoid
           | TIf
           | TElse
           | TWhile
           | TReturn
           | TNil
           | TBool
           | TBasicType
           | TOp1
           | TOp2
           | TChar
           | TField
           | TInt
           | TId
           deriving (Eq, Show)

-- Final tokens will be annotated with the corresponding string and its
-- location.
type TData = (String, Int, Int)
type AToken = (Token, TData)

-- We need to add some special tokens as tokenizer commands.
data CToken = NextLine
            | Clear
            | Token Token
            deriving (Eq, Show)

-- The state representation of the tokenizer happens to match the
-- representation of the annotated token data.
type TState = (String, Int, Int)

-- However, strings corresponding to tokens are recorded in reverse.
tdata :: TState -> TData
tdata (w, c, l) = (reverse w, c, l)

-- The character advance command is executed first on each new character.
readc :: Maybe Char -> Bool -> TState -> TState
readc a b (w, c, l) = case b of
    True -> as w
    False -> as []
    where
    as u = ((case a of
        Nothing -> u
        Just b -> b : u), c + 1, l)

-- Further effects depend on the command in the token.
command :: CToken -> TState -> TState
command c = case c of
    NextLine -> \(u, c, l) -> (u, 0, l + 1)
    Clear -> \(u, c, l) -> ("", c, l)
    _ -> id

-- We create some abbreviations for practical reasons.
type A = Maybe Char
type B = SimpleLattice CToken
type Var = Int

type TMealy q = Mealy A B q

tokenize :: TMealy q -> q -> String -> [AToken]
tokenize m i s = tokenize' i ([], 0, 0) $ (map Just s) ++ [Nothing]
    where
    tokenize' _ _ [] = []
    tokenize' q state (a : r) = case m q a of
        (s, p) -> case s of
            Bottom -> advance id True
            Val c -> case c of
                Token t -> (t, tdata state) : r
                _ -> r
                where
                r = advance (command c) False
            Top -> error "Overspecified tokenizer"
            where
            advance f b = tokenize' p (f $ readc a b state) r

type TFormula = MealyFormula A B Int

tseq :: String -> TFormula -> TFormula
tseq l f = foldr (Trans . Just) f l

tmseq :: [String] -> TFormula -> TFormula
tmseq l f = foldr (\s -> Add (tseq s f)) FF l

tsum :: [A] -> TFormula -> TFormula
tsum l t = foldr (\a -> Add (Trans a t)) FF l

tosum :: [A] -> B -> TFormula
tosum l b = foldr (\a -> Add (Out a b)) FF l

topt :: (TFormula -> TFormula) -> TFormula -> TFormula
topt f g = f g `Add` g

tot :: A -> B -> TFormula -> TFormula
tot a b f = Trans a f `Add` Out a b

sub :: Eq a => [a] -> [a] -> [a]
sub a b = filter (\c -> not $ c `elem` b) a

mc :: [Char] -> [A]
mc = map Just

call = ['\0'..'\255']
cnum = ['0'..'9']
calpha = ['a'..'z'] ++ ['A'..'Z']
calphanum = calpha ++ cnum
calphanum_ = '_' : calphanum
cwhite = [' ', '\t', '\n']
mcall = Nothing : mc call

nt :: Token -> B
nt = Val . Token

taccept :: Token -> TFormula
taccept = tosum mcall . nt

tokenize_default = tokenize synthesize $
    tseq "=" (taccept TAssign)
    `Add`
    tseq ";" (taccept TSemicolon)
    `Add`
    tseq "," (taccept TComma)
    `Add`
    tseq "(" (taccept TLeftParenthesis)
    `Add`
    tseq ")" (taccept TRightParenthesis)
    `Add`
    tseq "{" (taccept TLeftCBracket)
    `Add`
    tseq "}" (taccept TRightCBracket)
    `Add`
    tseq "[" (taccept TLeftBracket)
    `Add`
    tseq "]" (taccept TRightBracket)
    `Add`
    tseq "Void" (taccept TVoid)
    `Add`
    tseq "if" (taccept TIf)
    `Add`
    tseq "else" (taccept TElse)
    `Add`
    tseq "while" (taccept TWhile)
    `Add`
    tseq "return" (taccept TReturn)
    `Add`
    tseq "[]" (taccept TNil)
    `Add`
    tmseq ["True", "False"] (taccept TBool)
    `Add`
    tmseq ["Int", "Bool", "Char"] (taccept TBasicType)
    `Add`
    tmseq ["!", "-"] (taccept TOp1)
    `Add`
    tmseq ["+", "-", "*", "/", "%", "==", "<", ">", "<=", ">=", "!=", "&&",
           "||", ":"] (taccept TOp2)
    `Add`
    tmseq (map (\c -> ['\'', c, '\'']) cnum) (taccept TChar)
    `Add`
    (Nu 0 $ tosum (mcall `sub` mc ['.']) (nt TField)
        `Add` tmseq [".hd", ".tl", ".fst", ".snd"] (Var 0))
    `Add`
    (topt (tseq "-") $ tsum (mc cnum) $ Nu 0 $
        tosum (mcall `sub` mc cnum) (nt TInt)
        `Add` tsum (mc cnum) (Var 0))
    `Add`
    (tsum (mc calpha) $ Nu 0 $ tosum (mcall `sub` mc calphanum_) (nt TId)
        `Add` tsum (mc calphanum_) (Var 0))
    `Add`
    (Nu 0 $ tsum (mcall `sub` mc cwhite) (Var 0) `Add` tsum (mc cwhite)
        (Nu 1 $ tsum (mcall `sub` mc cwhite) (Var 0)
            `Add` tosum (mcall `sub` mc cwhite) (Val Clear)
            `Add` tsum (mc cwhite) (Var 1)))
    `Add`
    (Nu 0 $ tseq "\n" (tosum mcall $ Val NextLine)
        `Add` (Nu 1 $ tseq "\n" (Var 1)
            `Add` tsum (mcall `sub` mc ['\n']) (Var 0)))
    `Add`
    (Nu 0 $ tsum (mcall `sub` mc ['/']) (Var 0) `Add` tseq "/"
        (tsum (mcall `sub` mc ['/']) (Var 0) `Add` tseq "/"
            (Nu 1 $ tsum (mcall `sub` mc ['\n']) (Var 1)
                `Add` tot (Just '\n') (Val Clear) (Var 0))))
    `Add`
    (Nu 0 $ tsum (mcall `sub` mc ['/']) (Var 0) `Add` tseq "/"
        (Nu 1 $ tsum (mcall `sub` mc ['/', '*']) (Var 0)
            `Add` tseq "/" (Var 1) `Add` tseq "*"
                (Nu 2 $ tsum (mcall `sub` mc ['*']) (Var 2)
                    `Add` tseq "*" (Out (Just '/') (Val Clear)
                        `Add` tsum mcall (Var 0)))))
