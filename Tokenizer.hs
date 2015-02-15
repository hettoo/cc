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
                 | Newline
                 | Blank
                 | StarSlash
                 | NotSlash
                 | NotStar
                 | NotNewline
                 | NotStarNotSlash
                 deriving (Eq, Enum, Bounded, Show)

layoutClassChars :: LayoutClass -> TChar -> Bool
layoutClassChars c a = case c of
    Slash -> a == Char '/'
    Star -> a == Char '*'
    Newline -> a == Char '\n'
    Blank -> a `elem` EOF : map Char [' ', '\t', '\n', '\r']
    StarSlash -> layoutClassChars Slash a || layoutClassChars Star a
    NotSlash -> not $ layoutClassChars Slash a
    NotStar -> not $ layoutClassChars Star a
    NotNewline -> not $ layoutClassChars Newline a
    NotStarNotSlash -> not $ layoutClassChars StarSlash a

data LayoutMark = Start -- comment started on the previous character
                | End -- exact comment end
                | Single -- whitespace character
                deriving (Eq, Show)

type LML = SimpleLattice LayoutMark

layoutMarker :: MealySynth LayoutClass LML Var
layoutMarker = synthesize $ star
    (star ([Blank] .|. Single .+. skip [NotSlash]) .*.
        skip [Slash] .*.
            (skip [NotStarNotSlash]
            .+. ([Slash] .|. Start .*. star (skip [NotNewline]) .*.
                [Newline] .|. End)
            .+. [Star] .|. Start .*.
                star (skip [NotStar] .+. skip [Star, NotSlash]) .*.
                [Star, Slash] .|. End))

layoutMarks :: String -> [(LML, APos TChar)]
layoutMarks s =
    mealyList layoutMarker -*- mealyId -.-
    mealySingle (classify layoutClassChars) -*- index
    -!- map double (tstring s)
