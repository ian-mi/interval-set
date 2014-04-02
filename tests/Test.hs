module Main where

import Test.IntervalSet
import Test.Tasty
import Test.Tasty.SmallCheck

main :: IO ()
main = defaultMain (testProperty "Union Interval" unionInterval)
