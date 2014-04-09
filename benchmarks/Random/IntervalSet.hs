module Random.IntervalSet where

import Data.Interval as I
import Data.IntervalSet as IS

import Data.Foldable
import Data.Monoid
import Data.Random
import Data.Random.Distribution.Exponential

subintervals ::Interval ->Double ->Double ->RVar [Interval]
subintervals (Interval a b) p n = do
        lp ::Double <-exponential p
        let c = a + ceiling lp
        ln ::Double <-exponential n
        let d = c + ceiling ln
        if d >=b then
                return [Interval a (min b c)] else
                fmap (Interval a c :) (subintervals (Interval d b) p n)

subset ::Interval -> RVar IntervalSet
subset i = do
        is <-subintervals i k k
        return (appEndo (foldMap (Endo . IS.insert) is) IS.Empty)
        where
                k = sqrt l / 2
                l = fromInteger (fromIntegral (I.length i))
