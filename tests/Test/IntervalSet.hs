module Test.IntervalSet where

import Data.Interval
import Data.IntervalSet
import Test.Interval

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Test.SmallCheck
import Test.SmallCheck.Series

balancedSubtrees :: Monad m => Interval -> Int -> Int -> Series m Tree
balancedSubtrees (Interval a b) lmin rmin = return Leaf \/ do
    m@(Interval c d) <- subintervals (Interval (a + lmin) (b - rmin))
    let lrmin = max (c - a - (b - c)) 1
    let rlmin = max (b - d - (d - a)) 1
    l <- balancedSubtrees (Interval a c) lmin lrmin
    r <- balancedSubtrees (Interval d b) rlmin rmin
    return (Node l m r)

instance Monad m => Serial m IntervalSet where
    series = return Empty \/ do
        i <- series
        t <- balancedSubtrees i 1 1
        return (IntervalSet i t)
