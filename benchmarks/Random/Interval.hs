module Random.Interval where

import Data.Interval as I

import Data.Random
import Data.Random.Distribution.Binomial
import Data.Random.Distribution.Uniform
import Data.Ratio

subInterval ::Interval ->RVar Interval
subInterval i@(Interval a b) = do
        w <-fmap (max 1) (binomial l p)
        c <-uniform a (b - w)
        return (Interval c (c + w))
        where
                p = 1 / sqrt (fromIntegral l) ::Double
                l = I.length i