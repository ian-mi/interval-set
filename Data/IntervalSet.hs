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

deleteTree ::Interval ->Interval ->Tree ->(Interval, TTree)
deleteTree i@(Interval x y) j@(Interval a b) Leaf
        | x > a, y < b = (j, TLeaf (Node Leaf i Leaf))
        | x > a, x < b = (Interval a x, TLeaf Leaf)
        | y > a, y < b = (Interval y b, TLeaf Leaf)
        | otherwise = (j, TLeaf Leaf)
deleteTree i@(Interval x y) j@(Interval a b) t@(Node l m@(Interval c d) r)
        | x >=b ||y <=a ||I.contains m i = (j, TLeaf t)
        | I.contains i ir = (il', l')
        | I.contains i il = (ir', r')
        | otherwise = (Interval a' b', TNode l' (Interval c' d') r')
        where
                il = Interval a c
                ir = Interval d b
                (il'@(Interval a' c'), l') = deleteTree i il l
                (ir'@(Interval d' b'), r') = deleteTree i ir r

delete :: Interval -> IntervalSet -> IntervalSet
delete _ Empty = Empty
delete i@(Interval x y) s@(IntervalSet j@(Interval a b) t)
        | I.contains i j = Empty
        | otherwise = IntervalSet j' (flatten j' t')
        where (j', t') = deleteTree i j t

intersectTree ::Interval ->Interval ->Tree ->Maybe (Interval, TTree)
intersectTree i j Leaf
        | Just j' <-I.intersect i j = Just (j', TLeaf Leaf)
        | otherwise = Nothing
intersectTree i j@(Interval a b) (Node l m@(Interval c d) r)
        | Just j' <-I.intersect i j = case intersectTree i jl l of
                Just l'
                        | Just r' <-intersectTree i jr r ->Just (combineTTrees l' r')
                        | otherwise ->Just l' 
                otherwise ->intersectTree i jr r
        | otherwise = Nothing
        where
                jl = Interval a c
                jr = Interval d b

{-# INLINE combineTTrees #-}
combineTTrees ::(Interval, TTree) ->(Interval, TTree) ->(Interval, TTree)
combineTTrees (Interval a b, l) (Interval c d, r)
        = (Interval a d, TNode l (Interval b c) r)

intersect ::Interval ->IntervalSet ->IntervalSet
intersect _ Empty = Empty
intersect i (IntervalSet j t)
        | Just (j', t') <-intersectTree i j t = IntervalSet j' (flatten j' t')
        | otherwise = Empty

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
    a ->(Interval ->a ->a) ->(a ->Interval ->a ->a) ->(Interval ->a) ->IntervalSet ->a
foldIntervalsWithComplement v h g f = fold h' v g' f
    where
                g' fl im@(Interval c d) fr (Interval a b) = g (fl il) im (fr ir)
                        where
                                il = Interval a c
                                ir = Interval d b
                h' i k = h i (k i)

elem x = fold g False f True
    where   g i v
                | I.elem x i = v
                | otherwise = False
            f l (Interval a b) r
                | x < a = l
                | x >= b = r
                | otherwise = False

intervals :: Fold IntervalSet Interval
intervals f = foldIntervals (<*) (coerce . f) (pure undefined)
