module Tokenizer where
import Mealy
import MealyFormula
import MealyExpression
import JSL
import Utils

type Var = Int

instance Fresh Int where
    fresh f = case freshest f of
        Just n -> n + 1
        Nothing -> 0

type APos a = (a, (Int, Int))

index :: Mealy TChar (APos TChar) (Int, Int)
index = ((1, 1), \p a -> ((a, p), advance a p))
    where
    advance a (l, c) = case a of
        Char '\n' -> (l + 1, 1)
        _ -> (l, c + 1)

data LayoutClass = Slash
                 | Star
                 | LineEnd
                 | Blank
                 | StarSlash
                 | NotSlash
                 | NotStar
                 | NotLineEnd
                 | NotStarNotSlash
                 deriving (Eq, Enum, Bounded, Show)

layoutClassChars :: LayoutClass -> TChar -> Bool
layoutClassChars c a = case c of
    Slash -> a == Char '/'
    Star -> a == Char '*'
    LineEnd -> a == Char '\n' || a == EOF
    Blank -> a `elem` EOF : map Char [' ', '\t', '\n', '\r']
    StarSlash -> layoutClassChars Slash a || layoutClassChars Star a
    NotSlash -> not $ layoutClassChars Slash a
    NotStar -> not $ layoutClassChars Star a
    NotLineEnd -> not $ layoutClassChars LineEnd a
    NotStarNotSlash -> not $ layoutClassChars StarSlash a

data LayoutMark = Start -- comment started on the previous character
                | End -- exact comment end
                | Single -- whitespace character
                deriving (Eq, Show)

type LML = SimpleLattice LayoutMark

layoutMarker :: MealySynth LayoutClass LML Var
layoutMarker = synthesize $ star $
    star ([Blank] .|. Single \/ skip [NotSlash]) .*.
        skip [Slash] .*.
            (skip [NotStarNotSlash]
            \/ ([Slash] .|. Start .*. star (skip [NotLineEnd]) .*.
                [LineEnd] .|. End)
            \/ [Star] .|. Start .*.
                star (skip [NotStar] \/ skip [Star, NotSlash]) .*.
                [Star, Slash] .|. End)

layoutMarks :: String -> [(LML, APos TChar)]
layoutMarks s =
    mealyList layoutMarker -*- mealyId
    -.- mealySingle (classify layoutClassChars) -*- index
    -<- map double (tstring s)

-- This is ugly.
unifyBlank :: [(LML, APos TChar)] -> [APos Char]
unifyBlank = noQuote True
    where
    noQuote v l = case l of
        [] -> []
        (p : r@((Val Start, _) : _)) -> blank v p quote False r
        p@(Val Single, _) : r -> blank v p noQuote False r
        p : r -> c (snd p) : noQuote True r
    quote v l = case l of
        [] -> []
        p@(Val End, _) : r -> blank v p noQuote v r
        p : r -> blank v p quote v r
    blank v (_, a) f w r = case v of
        True -> (' ', snd a) : f w r
        False -> f w r
    c (Char c, d) = (c, d)

data CharClass = CAssign
               | CWhite
               | COther
               deriving (Eq, Enum, Bounded, Show)

classChars :: CharClass -> Char -> Bool
classChars c a = case c of
    CAssign -> a == '='
    CWhite -> a == ' '
    COther -> not $ classChars CAssign a || classChars CWhite a

data Token = TAssign
           | TWhite
           | TError
           deriving (Eq, Show)

type TL = SimpleLattice Token

tokenizer :: MealySynth CharClass TL Var
tokenizer = synthesize $ star $
    [CAssign] .|. TAssign \/ [CWhite] .|. TWhite \/ [COther] .|. TError

-- This is also ugly.
normalize :: [APos (SimpleLattice a, b)] -> [APos (a, [b])]
normalize = snd . normalize'
    where
    normalize' l = case l of
        [] -> ([], [])
        ((Val a, b), p) : r -> case normalize' r of
            (s, t) -> ([], ((a, b : s), p) : t)
        ((_, b), _) : r -> case normalize' r of
            (s, t) -> (b : s, t)

tokenize :: String -> [APos (Token, String)]
tokenize s = normalize $
    (mealyList tokenizer -*- mealyId
    -.- mealySingle (pair (classify classChars, id) . double))
        -*- mealyId
    -<- (unifyBlank . layoutMarks) s
