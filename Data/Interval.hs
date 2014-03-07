module Data.Interval (
    Interval(Interval),
    lower,
    upper,
    minus,
    sup,
    inf,
    interior,
    adjacent,
    overlap,
    boundedBelow,
    boundedStrictlyBelow,
    boundedAbove,
    boundedStrictlyAbove,
    intersectBelow,
    intersectStrictlyBelow,
    intersectAbove,
    intersectStrictlyAbove) where

import Control.Applicative
import Control.Lens
import Data.Maybe

data Interval = Interval { _lower :: Int, _upper :: Int }
$(makeLenses ''Interval)

instance Show Interval where
    show (Interval a b) = concat ["[", show a, ", ", show b, "]"]

minus :: Interval -> Fold Interval Interval
minus (Interval a b) f i = l *> u
    where   l = (to (intersectStrictlyBelow a) . _Just) f i
            u = (to (intersectStrictlyAbove b) . _Just) f i

sup :: Interval -> Interval -> Interval
sup (Interval a b) (Interval c d) = Interval (min a c) (max b d)

inf :: Interval -> Interval -> Maybe Interval
inf (Interval a b) (Interval c d)
    | x <= y     = Just (Interval x y)
    | otherwise = Nothing
    where   x = max a c
            y = min b d

interior :: Interval -> Maybe Interval
interior (Interval a b)
    | a' <= b'  = Just (Interval a' b')
    | otherwise = Nothing
    where   a' = a + 1
            b' = b - 1

adjacent :: Interval -> Interval -> Bool
adjacent (Interval a b) = overlap (Interval (a - 1) (b + 1))

overlap :: Interval -> Interval -> Bool
overlap x y = isJust (inf x y)

boundedBelow :: Int -> Interval -> Bool
boundedBelow i = (i <=) . view lower

boundedStrictlyBelow :: Int -> Interval -> Bool
boundedStrictlyBelow i = (i <) . view lower

boundedAbove :: Int -> Interval -> Bool
boundedAbove i = (<= i) . view upper

boundedStrictlyAbove :: Int -> Interval -> Bool
boundedStrictlyAbove i = (< i) . view upper

intersectBelow :: Int -> Interval -> Maybe Interval
intersectBelow i (Interval a b)
    | i >= a    = Just (Interval a (min i b))
    | otherwise = Nothing

intersectAbove :: Int -> Interval -> Maybe Interval
intersectAbove i (Interval a b)
    | i <= b    = Just (Interval (max i a) b)
    | otherwise = Nothing

intersectStrictlyBelow :: Int -> Interval -> Maybe Interval
intersectStrictlyBelow i = intersectBelow (i - 1)

intersectStrictlyAbove :: Int -> Interval -> Maybe Interval
intersectStrictlyAbove i = intersectAbove (i + 1)
