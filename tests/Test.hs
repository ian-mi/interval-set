module Main where

import Test.Balance
import Test.Correctness
import Test.IntervalSet
import Test.Tasty
import Test.Tasty.SmallCheck
import Test.Validity

main :: IO ()
main = defaultMain $ localOption (SmallCheckDepth 7) $ testGroup "Tests" [
        testGroup "Correctness" [
                testProperty "Interval Union" intervalUnion,
                testProperty "Interval Complement" intervalComplement,
                testProperty "Interval Intersection" intervalIntersection
                ],
        testGroup "Validity" [
                testProperty "Valid" valid,
                testProperty "Interval Union" validIntervalUnion,
                testProperty "Interval Complement" validIntervalComplement,
                testProperty "Interval Intersection" validIntervalIntersection
                ],
        testGroup "Balance" [
                testProperty "Balanced" balanced,
                testProperty "Interval Union" balancedIntervalUnion,
                testProperty "Interval Complement" balancedIntervalComplement,
                testProperty "Interval Intersection" balancedIntervalIntersection
                ]
        ]
