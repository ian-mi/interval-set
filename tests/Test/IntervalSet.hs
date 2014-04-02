module Test.IntervalSet where

import Data.Interval as I
import Data.IntervalSet as IS
import Test.Interval

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Test.SmallCheck
import Test.SmallCheck.Series

intervalAbove :: Monad m => Int -> Series m Interval
intervalAbove i = cons2 f
    where f (Positive x) (Positive y) = let a = i + x in Interval a (a + y)

intervalsAbove :: Monad m => Int -> Series m [Interval]
intervalsAbove i = do
    x@(Interval _ b) <- intervalAbove i
    xs <- decDepth (intervalsAbove b) <|> pure []
    return (x:xs)

increasingIntervals :: Monad m => Series m [Interval]
increasingIntervals = do
    x@(Interval _ b) <- series
    xs <- decDepth (intervalsAbove b) <|> pure []
    return (x:xs)

instance Monad m => Serial m IntervalSet where
    series = do
        is <- pure [] \/ decDepth increasingIntervals
        return (appEndo (foldMap (Endo . insert) is) Empty)

unionInterval :: IntervalSet -> Interval -> Int -> Bool
unionInterval s i x = IS.elem x (IS.insert i s) == (IS.elem x s || I.elem x i)
