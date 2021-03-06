module Data.Interval where

data Interval = Interval {-# UNPACK #-} !Int {-# UNPACK #-} !Int

instance Show Interval where
    show (Interval a b) = concat ["[", show a, ", ", show b, ")"]

{-# INLINE length #-}
length :: Interval -> Int
length (Interval a b) = b - a

{-# INLINE elem #-}
elem :: Int -> Interval -> Bool
elem n (Interval a b) = (n >= a) && (n < b)

{-# INLINE contains #-}
contains ::Interval ->Interval ->Bool
contains (Interval a b) (Interval c d) = a <=c &&b >=d

{-# INLINE intersect #-}
intersect ::Interval ->Interval ->Maybe Interval
intersect (Interval a b) (Interval c d)
        | a' < b' = Just (Interval a' b')
        | otherwise = Nothing
        where
                a' = max a c
                b' = min b d