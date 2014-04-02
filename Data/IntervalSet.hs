module Data.IntervalSet where

import Data.Interval as I

import Control.Applicative
import Control.Lens
import Data.List

data IntervalSet = IntervalSet {-# UNPACK #-} !Interval Tree | Empty
data Tree = Node Tree {-# UNPACK #-} !Interval Tree | Leaf deriving Show
data TTree = TNode TTree {-# UNPACK #-} !Interval TTree | TLeaf Tree

instance Show IntervalSet where
    show s = concat ["{", intercalate ", " (s ^.. intervals . to show), "}"]

intervals :: Fold IntervalSet Interval
intervals f = foldIntervals (<*) (coerce . f) (pure undefined)

mkTree :: Interval -> TTree -> Interval -> TTree -> Tree
mkTree (Interval a b) l m@(Interval c d) r
    = Node (flatten (Interval a c) l) m (flatten (Interval d b) r)

flatten :: Interval -> TTree -> Tree
flatten _ (TLeaf t) = t
flatten i@(Interval a b) (TNode l m@(Interval c d) r)
    |   ul + um + 1 < ur,
        Just (rl, rm@(Interval e f), rr) <- pivotT i r,
        e - a < ur = mkTree i (TNode l m rl) rm rr
    |   ur + um + 1 < ul,
        Just (ll, lm@(Interval e f), lr) <- pivotT i l,
        b - f < ul = mkTree i ll lm (TNode lr m r)
    |   otherwise = mkTree i l m r
    where
        ul = c - a
        um = d - c
        ur = b - d

pivotT :: Interval -> TTree -> Maybe (TTree, Interval, TTree)
pivotT i (TLeaf t) = pivot i t
pivotT i@(Interval a b) (TNode l m@(Interval c d) r)
    |   ul + um + 1 < ur,
        Just (rl, rm@(Interval e f), rr) <- pivotT i r,
        e - a < ur = Just (TNode l m rl, rm, rr)
    |   ur + um + 1 < ul,
        Just (ll, lm@(Interval e f), lr) <- pivotT i l,
        b - f < ul = Just (ll, lm, TNode lr m r)
    |   otherwise = Just (l, m, r)
    where
        ul = c - a
        um = d - c
        ur = b - d

pivot :: Interval -> Tree -> Maybe (TTree, Interval, TTree)
pivot _ Leaf = Nothing
pivot i@(Interval a b) (Node l m@(Interval c d) r)
    |   ul + um + 1 < ur,
        Just (rl, rm@(Interval e f), rr) <- pivot i r,
        e - a < ur = Just (TNode (TLeaf l) m rl, rm, rr)
    |   ur + um + 1 < ul,
        Just (ll, lm@(Interval e f), lr) <- pivot i l,
        b - f < ul = Just (ll, lm, TNode lr m (TLeaf r))
    |   otherwise = Just (TLeaf l, m, TLeaf r)
    where
        ul = c - a
        um = d - c
        ur = b - d

balance :: Interval -> Tree -> Interval -> Tree -> Tree
balance i l m r = flatten i (TNode (TLeaf l) m (TLeaf r))

insertTrees :: Interval -> Tree -> Tree -> TTree
insertTrees i Leaf t = insertTree i t
insertTrees i@(Interval x y) (Node ll lm@(Interval c d) lr) r
    | x <= c = insertTrees i ll r
    | x <= d = TNode (TLeaf ll) (Interval c x) (insertTree i r)
    | otherwise = TNode (TLeaf ll) lm (insertTrees i lr r)

insertTree :: Interval -> Tree -> TTree
insertTree _ Leaf = TLeaf Leaf
insertTree i@(Interval x y) (Node l (Interval c d) r)
    | x <= c, y >= d = insertTrees i l r
    | x <= c = TNode (insertTree i l) (Interval (max c y) d) (TLeaf r)
    | y >= d = TNode (TLeaf l) (Interval c (min d x)) (insertTree i r)
    | otherwise =
        TNode (TLeaf l) (Interval c x) (TNode (TLeaf Leaf) (Interval y d) (TLeaf r))

insertContained :: Interval -> Interval -> Tree -> Tree
insertContained _ _ Leaf = Leaf
insertContained i@(Interval x y) j@(Interval a b) t@(Node l m@(Interval c d) r)
    | y <= c = Node (insertContained i (Interval a c) l) m r
    | x >= d = Node l m (insertContained i (Interval d b) r)
    | otherwise = flatten j (insertTree i t)

insert :: Interval -> IntervalSet -> IntervalSet
insert i Empty = IntervalSet i Leaf
insert i@(Interval x y) (IntervalSet j@(Interval a b) t)
    | x > b = let j' = Interval a y in
        IntervalSet j' (balance j' t (Interval b x) Leaf)
    | y < a = let j' = Interval x b in
        IntervalSet j' (balance j' Leaf (Interval y a) t)
    | x >= a && y <= b = IntervalSet j (insertContained i j t)
    | otherwise = IntervalSet j' (flatten j' (insertTree i t))
    where j' = Interval (min a x) (max b y)

foldSet :: (Interval -> Tree -> a) -> a -> IntervalSet -> a
foldSet _ v Empty = v
foldSet f v (IntervalSet i t) = f i t

foldTree :: (a -> Interval -> a -> a) -> a -> Tree -> a
foldTree _ v Leaf = v
foldTree f v (Node l m r) = f (foldTree f v l) m (foldTree f v r)

fold :: (Interval -> a -> b) -> b -> (a -> Interval -> a -> a) -> a -> IntervalSet -> b
fold g e f v = foldSet (\i -> g i . foldTree f v) e

foldIntervals :: (a -> a -> a) -> (Interval -> a) -> a -> IntervalSet -> a
foldIntervals g f v = fold (flip ($)) v h f
    where   h fl (Interval c d) fr (Interval a b) = g (fl il) (fr ir)
                where   il = Interval a c
                        ir = Interval d b

foldIntervalsWithComplement ::
    (a -> Interval -> a -> a) -> (Interval -> a) -> a -> IntervalSet -> a
foldIntervalsWithComplement g f v = fold (flip ($)) v h f
    where   h fl im@(Interval c d) fr (Interval a b) = g (fl il) im (fr ir)
                where   il = Interval a c
                        ir = Interval d b

elem x = fold g False f True
    where   g i v
                | I.elem x i = v
                | otherwise = False
            f l (Interval a b) r
                | x < a = l
                | x >= b = r
                | otherwise = False
