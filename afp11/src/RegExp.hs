{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
module RegExp(RegExp(), (><), star, plus, try, str, symbol, matches, symbols, minmax, mininf, rempty, bot, top, wildcard,
    structuralEq, normalize, derivative) where
import Numeric.Natural
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List (intercalate)

data Spread = Finite Natural | Infinite deriving (Eq, Ord) -- Reverse order from Maybe

instance Show Spread where

    show :: Spread -> String
    show (Finite n) = show n
    show Infinite = ""

uplus :: Spread -> Spread -> Spread
uplus (Finite a) (Finite b) = Finite (a + b)
uplus _ _ = Infinite

uminus :: Spread -> Natural -> Spread
uminus (Finite a) b = Finite (a - b)
uminus _ _ = Infinite

umul :: Spread -> Spread -> Spread
umul (Finite a) (Finite b) = Finite (a * b)
umul _ _ = Infinite

data RegExp s
    = Bot
    | Str [s]
    | Wildcard
    | Concat (NonEmpty (RegExp s))
    | Alter (Set (RegExp s))
    | MinMax (RegExp s) Natural Spread
    | Top
    deriving (Ord)

instance (Show s) => Show (RegExp s) where

    show :: RegExp s -> String
    show Bot = "⊥"
    show (Str s) = concat ["(", show s, ")"]
    show Wildcard = "(.)"
    show (Concat as) = concat ["(", concatMap show as, ")"]
    show (Alter as) = concat ["(", intercalate "|" (show <$> Set.toAscList as), ")"]
    show (MinMax a 0 Infinite) = show a ++ "*"
    show (MinMax a 1 Infinite) = show a ++ "+"
    show (MinMax a 0 (Finite 1)) = show a ++ "?"
    show (MinMax a n k) = concat ["(", show a, "){", show n, ",", show (k `uplus` Finite n), "}"]
    show Top = "⊤"

normalize :: (Ord s) => RegExp s -> RegExp s
normalize (Concat as) = mconcat $ NE.toList (normalize <$> as)
normalize (Alter as) = Alter (normalize `Set.map` as)
normalize (MinMax a n k) = minspread n k (normalize a)
normalize a = a

structuralEq :: Eq s => RegExp s -> RegExp s -> Bool
Bot `structuralEq` Bot = True
Str a `structuralEq` Str a' = a == a'
Wildcard `structuralEq` Wildcard = True
Concat as `structuralEq` Concat as' = and $ zipWith structuralEq (NE.toList as) (NE.toList as')
Alter as `structuralEq` Alter as' = and $ zipWith structuralEq (Set.toAscList as) (Set.toAscList as')
MinMax a n k `structuralEq` MinMax a' n' k' = (a `structuralEq` a') && (n == n') && (k == k')
Top `structuralEq` Top = True
_ `structuralEq` _ = False

instance (Ord s) => Eq (RegExp s) where

    (==) :: RegExp s -> RegExp s -> Bool
    r == r' = normalize r `structuralEq` normalize r'

pattern MinMax' :: RegExp s -> Natural -> Natural -> RegExp s
pattern MinMax' a n k = MinMax a n (Finite k)

pattern Empty :: RegExp s
pattern Empty = Str []

makeConcat :: NonEmpty (RegExp s) -> RegExp s
makeConcat (a :| []) = a
makeConcat as = Concat as

concatMixNoMinMax :: RegExp s -> RegExp s -> Maybe (RegExp s)
concatMixNoMinMax Bot _ = return Bot
concatMixNoMinMax _ Bot = return Bot
concatMixNoMinMax Empty b = return b
concatMixNoMinMax a Empty = return a
concatMixNoMinMax Top b | matchesEmpty b = return Top
concatMixNoMinMax a Top | matchesEmpty a = return Top
concatMixNoMinMax (Str a) (Str b) = return $ Str (a <> b)
concatMixNoMinMax _ _ = Nothing

concatMix :: (Ord s) => RegExp s -> RegExp s -> Maybe (RegExp s)
concatMix (MinMax a n k) (MinMax a' n' k') | a == a' = return $ MinMax a (n + n') (k `uplus` k')
concatMix a (MinMax a' n k) | a == a' = return $ MinMax a (n + 1) k
concatMix (MinMax a n k) a' | a == a' = return $ MinMax a (n + 1) k
concatMix a b = concatMixNoMinMax a b

concatPrepend :: (RegExp s -> RegExp s -> Maybe (RegExp s)) -> RegExp s -> NonEmpty (RegExp s) -> NonEmpty (RegExp s)
concatPrepend mix a (b NE.:| bs) = case mix a b of
    Just c -> case bs of
        [] -> c :| []
        (b':bs') -> concatPrepend mix c (b' NE.:| bs')
    Nothing -> a NE.:| (b:bs)

concatAppend :: (RegExp s -> RegExp s -> Maybe (RegExp s)) -> NonEmpty (RegExp s) -> RegExp s -> NonEmpty (RegExp s)
concatAppend mix (a :| []) b = case mix a b of
            Just a' -> a' NE.:| []
            Nothing -> a NE.:| [b]
concatAppend mix (a :| (a':as)) b = concatPrepend mix a $ concatAppend mix (a' :| as) b

concatPlus :: (RegExp s -> RegExp s -> Maybe (RegExp s)) -> NonEmpty (RegExp s) -> NonEmpty (RegExp s) -> NonEmpty (RegExp s)
concatPlus mix (a :| []) bs = concatPrepend mix a bs
concatPlus mix (a :| (a':as)) bs = concatPrepend mix a $ concatPlus mix (a' :| as) bs

concatBase :: (RegExp s -> RegExp s -> Maybe (RegExp s)) -> RegExp s -> RegExp s -> RegExp s
concatBase mix (Concat as) (Concat bs) = makeConcat (concatPlus mix as bs)
concatBase mix a (Concat bs) = makeConcat (concatPrepend mix a bs)
concatBase mix (Concat as) b = makeConcat (concatAppend mix as b)
concatBase mix a b = makeConcat (concatPrepend mix a (return b))

-- Concat that doesn't combine MinMaxs, used in MinMax derivative
concatNoMinMax :: RegExp s -> RegExp s -> RegExp s
concatNoMinMax = concatBase concatMixNoMinMax

instance (Ord s) => Semigroup (RegExp s) where

    (<>) :: RegExp s -> RegExp s -> RegExp s
    (<>) = concatBase concatMix

instance (Ord s) => Monoid (RegExp s) where
    mempty :: RegExp s
    mempty = Empty

(><) :: (Ord s) => RegExp s -> RegExp s -> RegExp s
Bot >< b = b
a >< Bot = a
Top >< _ = Top
_ >< Top = Top
Empty >< b | matchesEmpty b = b
a >< Empty | matchesEmpty a = a
Empty >< (MinMax a 1 u) = MinMax a 0 (u `uplus` Finite 1)
(MinMax a 1 u) >< Empty = MinMax a 0 (u `uplus` Finite 1)
(Alter as) >< (Alter bs) = Alter (as <> bs)
(Alter as) >< b = Alter (Set.insert b as)
a >< (Alter bs) = Alter (Set.insert a bs)
a >< b = Alter $ Set.fromList [a, b]

alters :: (Ord s, Foldable l) => l (RegExp s) -> RegExp s
alters = foldr (><) Bot

matchesEmpty :: RegExp s -> Bool
matchesEmpty Empty = True
matchesEmpty Top = True
matchesEmpty (MinMax _ 0 _) = True
matchesEmpty (MinMax a _ _) = matchesEmpty a
matchesEmpty (Concat as) = all matchesEmpty as
matchesEmpty (Alter as) = any matchesEmpty as
matchesEmpty _ = False

derivative :: (Ord s) => s -> RegExp s -> RegExp s
derivative _ Bot = Bot
derivative _ (Str []) = Bot
derivative _ (MinMax' _ 0 0) = Bot
derivative _ Wildcard = Empty
derivative _ Top = Top
derivative x (Concat (a NE.:| [])) = derivative x a
derivative x (Alter as) = alters $ Set.map (derivative x) as
derivative x (MinMax a 0 k) = derivative x (a `concatNoMinMax` minspread 0 (k `uminus` 1) a)
derivative x (MinMax a n k) = derivative x (a `concatNoMinMax` minspread (n - 1) k a)
derivative x (Str (x':xs)) | x == x' = Str xs
                           | otherwise = Bot
derivative x (Concat (a NE.:| (a':as'))) = t >< d
    where
        t = derivative x a <> as
        d = if matchesEmpty a then derivative x as else Bot
        as = makeConcat $ a' NE.:| as'


matches :: (Ord s) => RegExp s -> [s] -> Bool
matches Bot _ = False
matches Top _ = True
matches Empty s = null s
matches r [] = matchesEmpty r
matches r (x:xs) = matches (derivative x r) xs


minspread :: (Ord s) => Natural -> Spread -> RegExp s -> RegExp s
minspread 1 (Finite 0) a = a
minspread 0 (Finite 0) _ = Empty
minspread _ _ Empty = Empty
minspread 0 _ Bot = Empty
minspread _ _ Bot = Bot
minspread _ _ Top = Top
minspread n Infinite Wildcard = minspread' n 0 Wildcard <> Top
minspread n k (MinMax a 0 k') = minspread 0 (k' `umul` (k `uplus` Finite n)) a
minspread n k (MinMax a 1 k') = minspread n ((k' `uplus` Finite 1) `umul` (k `uplus` Finite n)) a
minspread n k a = MinMax a n k

minspread' :: (Ord s) => Natural -> Natural -> RegExp s -> RegExp s
minspread' n k = minspread n (Finite k)

minmax :: (Ord s) => Natural -> Natural -> RegExp s -> RegExp s
minmax l u a | u >= l = minspread' l (u-l) a
             | otherwise = Bot

mininf :: (Ord s) => Natural -> RegExp s -> RegExp s
mininf l = minspread l Infinite

rempty :: RegExp s
rempty = Empty

bot :: RegExp s
bot = Bot

top :: RegExp s
top = Top

star :: (Ord s) => RegExp s -> RegExp s
star = mininf 0

plus :: (Ord s) => RegExp s -> RegExp s
plus = mininf 1

try :: (Ord s) => RegExp s -> RegExp s
try = minspread' 0 1

str :: [s] -> RegExp s
str = Str

symbol :: s -> RegExp s
symbol = str . return

symbols :: (Ord s) => [s] -> RegExp s
symbols = alters . (symbol <$>)

wildcard :: RegExp s
wildcard = Wildcard