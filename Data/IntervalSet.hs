module Data.IntervalSet where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Interval
import Data.Maybe
import Data.Monoid

import qualified Data.IntMap as IM

newtype IntervalSet = IntervalSet { _intervalSet :: IM.IntMap Int }
$(makeLenses ''IntervalSet)

instance Show IntervalSet where
    show s = show (s ^.. intervals)

intervals :: Fold IntervalSet Interval
intervals = intervalSet . ifolded . withIndex . to (uncurry Interval)

above :: Int -> Fold IntervalSet Interval
above i = strictlyAbove (i - 1)

strictlyAbove :: Int -> Fold IntervalSet Interval
strictlyAbove i = to (i,) . unfolded f
    where f (j, s) = do
            (a, b) <- views intervalSet (IM.lookupGT j) s
            return (Interval a b, (b, s))

notBelow :: Int -> Fold IntervalSet Interval
notBelow i f s
    | Just (a, b) <- views intervalSet (IM.lookupLT i) s, b > i
        = f (Interval a b) *> strictlyAbove b f s
    | otherwise = above i f s

notStrictlyBelow :: Int -> Fold IntervalSet Interval
notStrictlyBelow i f s
    | Just (a, b) <- views intervalSet (IM.lookupLT i) s, b >= i
        = f (Interval a b) *> strictlyAbove b f s
    | otherwise = above i f s

containedBy :: Interval -> Fold IntervalSet Interval
containedBy (Interval a b) = takingWhile (boundedAbove b) (above a)

strictlyContainedBy :: Interval -> Fold IntervalSet Interval
strictlyContainedBy (interior -> Just i) = containedBy i
strictlyContainedBy _ = ignored

containing :: Interval -> IntervalSet -> Maybe Interval
containing (Interval a b) (IntervalSet s)
    | Just (c, d) <- IM.lookupLE a s, d >= b = Just (Interval c d)
    | otherwise = Nothing

intersecting :: Interval -> Fold IntervalSet Interval
intersecting (Interval a b) =
    takingWhile (not . boundedStrictlyBelow b) (notStrictlyBelow a)

touching :: Interval -> Fold IntervalSet Interval
touching (Interval a b) = intersecting (Interval (a - 1) (b + 1))

deleteInterval :: Interval -> State IntervalSet ()
deleteInterval = (intervalSet %=) . IM.delete . view lower

insertInterval :: Interval -> State IntervalSet ()
insertInterval (Interval a b) = intervalSet %= IM.insert a b

addInterval :: Interval -> State IntervalSet ()
addInterval i = do
    t <- gets (toListOf (touching i))
    mapM_ deleteInterval t
    insertInterval (foldBy sup i t)

removeInterval :: Interval -> State IntervalSet [Interval]
removeInterval i = do
    t <- gets (toListOf (touching i))
    mapM_ deleteInterval t
    t ^! folded . minus i . act insertInterval
    return (t ^.. folded . to (inf i) . _Just)

nonEmpty :: Iso' IntervalSet (Maybe IntervalSet)
nonEmpty = iso g (fromMaybe (IntervalSet IM.empty))
    where g s = if views intervalSet IM.null s then Nothing else Just s

minusSet :: IntervalSet -> Fold Interval Interval
minusSet s f i
    | Just (Interval l _) <- listToMaybe is,
      Just j <- intersectStrictlyBelow l i = f j <* r is
    | otherwise = r is
    where   is = s ^.. intersecting i
            r js
                | [] <- js = f i
                | [Interval _ u] <- js,
                  Just j <- intersectStrictlyAbove u i = f j
                | Interval _ a : js'@(Interval b _ : _) <- js
                    = f (Interval (a + 1) (b - 1)) <* r js'
                | otherwise = pure undefined

setFromInterval :: Interval -> IntervalSet
setFromInterval (Interval a b) = IntervalSet (IM.singleton a b)
