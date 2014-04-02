module Test.Interval where

import Data.Interval

import Test.SmallCheck
import Test.SmallCheck.Series

instance Monad m => Serial m Interval where
    series = cons2 f
        where f a (Positive l) = Interval a (a + l)
