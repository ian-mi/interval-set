module Test.Balance where

import Data.Interval
import Data.IntervalSet

balanced :: IntervalSet -> Bool
balanced Empty = True
balanced (IntervalSet i t) = balancedTree i 1 1 t

balancedTree :: Interval -> Int -> Int -> Tree -> Bool
balancedTree (Interval a b) lmin rmin Leaf = b - a >=max lmin rmin
balancedTree (Interval a b) lmin rmin (Node l (Interval c d) r) =
        balancedTree (Interval a c) lmin lrmin l &&
        balancedTree (Interval d b) rlmin rmin r
        where
                lrmin = max (c - a - (b - c)) 1
                rlmin = max (b - d - (d - a)) 1

balancedIntervalUnion :: IntervalSet -> Interval -> Bool
balancedIntervalUnion s i = balanced (insert i s)

balancedIntervalComplement ::IntervalSet ->Interval ->Bool
balancedIntervalComplement s i = balanced (delete i s)