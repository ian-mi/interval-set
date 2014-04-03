module Test.Validity where

import Data.Interval
import Data.IntervalSet

valid :: IntervalSet -> Bool
valid Empty = True
valid (IntervalSet i t) = validTree i t

validTree :: Interval -> Tree -> Bool
validTree _ Leaf = True
validTree (Interval a b) (Node l (Interval c d) r)
    = and [a < c, d < b, validTree (Interval a c) l, validTree (Interval d b) r]

validIntervalUnion :: IntervalSet -> Interval -> Bool
validIntervalUnion s i = valid (insert i s)

validIntervalComplement ::IntervalSet ->Interval ->Bool
validIntervalComplement s i = valid (delete i s)