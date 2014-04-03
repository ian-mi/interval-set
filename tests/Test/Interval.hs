module Test.Interval where

import Data.Interval

import Control.Monad
import Test.SmallCheck
import Test.SmallCheck.Series

instance Monad m => Serial m Interval where
    series = cons2 f
        where f a (Positive l) = Interval a (a + l)

subintervals :: Monad m => Interval -> Series m Interval
subintervals (Interval a b) = do
    a' <- msum [return x | x <- [a .. b - 1]]
    b' <- msum [return x | x <- [a' + 1 .. b]]
    return (Interval a' b')
