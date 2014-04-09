module Main where

import Benchmark.IntervalSet
import Random.Interval
import Random.IntervalSet

import Criterion.Main
import Data.Interval
import Data.IntervalSet as IS
import Data.Random

main ::IO ()
main = do
        s <-sample (subset domain)
        i <-sample (subInterval domain)
        defaultMain [
                benchInsert s i,
                benchDelete s i,
                benchIntersect s i
                ]
        where
                domain = Interval 0 (10^8)

benchInsert ::IntervalSet ->Interval ->Benchmark
benchInsert s i = bench "Insert" (nf (uncurry IS.insert) (i, s))

benchDelete ::IntervalSet ->Interval ->Benchmark
benchDelete s i = bench "Delete" (nf (uncurry IS.delete) (i, s))

benchIntersect ::IntervalSet ->Interval ->Benchmark
benchIntersect s i = bench "Intersect" (nf (uncurry IS.intersect) (i, s))