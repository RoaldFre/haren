{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveFunctor #-}
-- | Module for making Bounding Volume Hierarchies (Containing at least one element.)
module BVH (
    buildBVH,

    -- from module Boxes
    Boxable(..)
) where

import Boxes
import Ray
import Geometry
import Intersection
import Math

import Data.List hiding (transpose, intersect)
import Data.Maybe
import GHC.Exts

-- | Bounding Volume Hierarchy.
data BVH a = 
        BVHleaf [Boxed a]
      | BVHnode Box (BVH a) (BVH a)
instance (Show a) => Show (BVH a) where
    show bvh = show' "" bvh
     where
        show' t (BVHleaf []) = t ++ "BVHleaf []"
        show' t (BVHleaf xs) = t ++ "BVHleaf [\n"
                            ++ showlist (t ++ "        ") xs ++ "\n" 
                            ++ t ++ "       ]"
        show' t (BVHnode b l r) = t ++ "BVHnode <" ++ show b ++ "> (\n"
                            ++ show' newt l
                            ++ "\n)(\n"
                            ++ show' newt r 
                        where newt = t ++ "        "
        showlist _ [] = ""
        showlist t [x] = t ++ show x
        showlist t (x:xs) = t ++ show x ++ ",\n" ++ showlist t xs

instance (Boxable a) => Boxable (BVH a) where
    box (BVHleaf boxeds) = box boxeds
    box (BVHnode b _ _) = b


instance (Geometry a) => Geometry (BVH a) where
    boundingBox (BVHleaf boxeds) = box boxeds
    boundingBox (BVHnode b _ _) = box b
    intersectGeom bvh ray = concatMap (\g -> intersectGeom g ray) $ bvhPotentialHits ray bvh


instance (Intersectable t a) => Intersectable t (BVH a) where
    intersect ray bvh = concatMap (intersect ray) $ bvhPotentialHits ray bvh


-- | Returns a list of all elements whose bounding box got hit by the given 
-- ray.
bvhPotentialHits :: Ray -> BVH a -> [a]
bvhPotentialHits ray (BVHleaf boxeds) = map unbox $ filter (ray `hitsBoxed`) boxeds
bvhPotentialHits ray (BVHnode b bvh1 bvh2)
    | ray `hitsBox` b = concatMap (bvhPotentialHits ray) [bvh1, bvh2]
    | otherwise       = []




-- | Build a Bounding Volume Hierarchy (top-down) from the list of boxables.
-- This will keep partitioning until there are at most 'n' elements in each 
-- leaf. In the pathological case where it is impossible to partition a 
-- list further (eg when it holds more than 'n' items with the exact same 
-- box), then that list will be put in a leaf in its entirety.
buildBVH :: (Boxable a) => Int -> [a] -> BVH a
buildBVH n xs = buildBVH' $ map mkBoxed xs
    where
        buildBVH' :: (Boxable a) => [Boxed a] -> BVH a
        buildBVH' xs2
           | length xs2 <= n = BVHleaf xs2
           | otherwise       = case partitionBoxeds xs2 of
                Nothing          -> BVHleaf xs2
                Just (_, p1, p2) -> BVHnode (box (fromWL p1, fromWL p2))
                                            (buildBVH' $ unbox $ fromWL p1)
                                            (buildBVH' $ unbox $ fromWL p2)

-- | A thingamajing with an associated length of said thingamajing stored 
-- for O(1) access.
data WithLength a = WL {
        wlLength :: Int,
        fromWL   :: a
    } deriving (Functor, Show)

-- | A partition of a bunch of boxeds, each part wrapped in a boxed itself. 
-- For each 'bunch of boxeds', the number of elements inside is stored as 
-- well, The first element of the tuple is the cost of the partition.
type BoxedPartition a = (Flt, WithLength (Boxed [Boxed a]), WithLength (Boxed [Boxed a]))


-- | Return the best partition of the list of boxeds.
partitionBoxeds :: (Boxable a) => [Boxed a] -> Maybe (BoxedPartition a)
partitionBoxeds boxeds = bestPartition $ catMaybes $ [partx, party, partz]
    where
        sortedx = sortWith (\b -> f3x (centroid $ box b)) boxeds
        sortedy = sortWith (\b -> f3y (centroid $ box b)) boxeds
        sortedz = sortWith (\b -> f3z (centroid $ box b)) boxeds
        partx = bestPartition $ makePartitions sortedx
        party = bestPartition $ makePartitions sortedy
        partz = bestPartition $ makePartitions sortedz

-- | Creates all partitions that can be made by splitting the given list 
-- at some position. (So not *all* permutations are considered! Thus, this 
-- only really makes sense if the list is somehow 'cleverly ordererd')
makePartitions :: Boxable a => [Boxed a] -> [BoxedPartition a]
makePartitions xs = zipWith costOfPartition bs1 bs2
    where
        -- A bit or reversed-list-magic to line up the 'matching' 
        -- partitions/sublists. Need tail to get good 'match' and
        -- and avoid duplicates.
        bs1 = tail $ accumulateBoxeds xs
        bs2 = reverse $ tail $ accumulateBoxeds $ reverse xs 
        costOfPartition p1@(WL n1 b1) p2@(WL n2 b2) = (cost, p1, p2)
            where cost = (surfaceArea $ box b1) * (fromIntegral n1)
                       + (surfaceArea $ box b2) * (fromIntegral n2)

bestPartition :: Boxable a => [BoxedPartition a] -> Maybe (BoxedPartition a)
bestPartition xs
    | null xs   = Nothing
    | otherwise = Just $ foldl1 bestPart xs
    where bestPart p1@(c1,_,_) p2@(c2,_,_) = if c1 < c2 then p1 else p2

-- | The result when acting on a list
-- [Boxed a, Boxed b, Boxed c]
-- will be the list (where the WithLength is suppressed)
-- [Boxed (bounding box of a b c) [a,b,c]
-- ,Boxed (bounding box of   b c)   [b,c]
-- ,Boxed (bounding box of     c)     [c]]
accumulateBoxeds :: [Boxed a] -> [WithLength (Boxed [Boxed a])]
accumulateBoxeds [] = []
accumulateBoxeds (boxed:boxeds) = accum [WL 1 (mkBoxed [boxed])] boxeds
    where
        accum :: [WithLength (Boxed [Boxed a])] -> [Boxed a] -> [WithLength (Boxed [Boxed a])]
        accum acc []     = acc
        accum acc (b:bs) = accum newacc bs
            where 
                newacc = (WL (1 + l) (Boxed (box (a,b)) (b:as))) : acc
                l = wlLength $ head acc
                a = fromWL $ head acc
                as = unbox $ fromWL $ head acc


-- vim: expandtab smarttab sw=4 ts=4
