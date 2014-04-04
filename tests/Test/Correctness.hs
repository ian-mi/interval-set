module Test.Correctness where

import Data.Interval as I
import Data.IntervalSet as IS

intervalUnion :: IntervalSet -> Interval -> Int -> Bool
intervalUnion s i x = IS.elem x (IS.insert i s) == (IS.elem x s || I.elem x i)

intervalComplement ::IntervalSet ->Interval ->Int ->Bool
intervalComplement s i x = IS.elem x (IS.delete i s) ==(IS.elem x s &&not (I.elem x i))

intervalIntersection ::IntervalSet ->Interval ->Int ->Bool
intervalIntersection s i x = IS.elem x (IS.intersect i s) ==(IS.elem x s &&I.elem x i)