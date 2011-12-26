{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveFunctor #-}
-- | Module for making Bounding Volume Hierarchies (Containing at least one element.)
module BVH (
    --BVH(..), -- TODO hide this?
    buildBVH,
    buildBVHfast,

    -- from module Boxes
    Boxable(..)
) where

import Boxes
import Ray
import Geometry
import Intersection
import Math

import Data.List hiding (transpose, intersect)
import Control.Applicative
import GHC.Exts

-- | Bounding Volume Hierarchy. (Contains at least one element.)
data BVH a = 
        BVHleaf [Boxed a]
      | BVHnode (Boxed (BVH a)) (Boxed (BVH a))
instance (Show a) => Show (BVH a) where
    show bvh = show' "" bvh
     where
        show' t (BVHleaf []) = t ++ "BVHleaf []"
        show' t (BVHleaf xs) = t ++ "BVHleaf [\n" ++ showlist (t ++ "        ") xs ++ "\n" ++ t ++ "       ]"
        show' t (BVHnode l r) = t ++ "BVHnode (\n" ++ show' newt (unbox l) ++ "\n)(\n"
                                                   ++ show' newt (unbox r) where newt = t ++ "        "
        showlist t [] = ""
        showlist t [x] = t ++ show x
        showlist t (x:xs) = t ++ show x ++ ",\n" ++ showlist t xs
        --tab = "  "

instance (Boxable a) => Boxable (BVH a) where
    box (BVHleaf b) = box b
    box (BVHnode b1 b2) = box (b1, b2)


-- for optimizing a single geometry
instance (Geometry a) => Geometry (BVH a) where
    boundingBox (BVHleaf b) = box b
    boundingBox (BVHnode b1 b2) = box (b1, b2)

    intersectGeom bvh ray = concatMap (\g -> intersectGeom g ray) $ bvhPotentialHits ray bvh


instance (Intersectable t a) => Intersectable t (BVH a) where
    intersect ray bvh = concatMap (intersect ray) $ bvhPotentialHits ray bvh


-- | Returns a list of all elements whose bounding box got hit by the given 
-- ray.
-- TODO: I'm not testing if I won't hit with the *entire* bvh alltogether 
-- (== potentially slight performance drop for small, 'dense' trees)
bvhPotentialHits :: Ray -> BVH a -> [a]
bvhPotentialHits ray (BVHleaf b) = map unbox $ filter (ray `hitsBoxed`) b
bvhPotentialHits ray (BVHnode b1 b2) =
    concatMap (bvhPotentialHits ray . unbox) $ filter (ray `hitsBoxed`) [b1, b2]




-- | Build a Bounding Volume Hierarchy (top-down) from the list of boxables.
-- This will keep partitioning until there are at most 'n' elements in each 
-- leaf. In the pathological case where it is impossible to partition a 
-- list further (eg when it holds more than 'n' items with the exact same 
-- box), then that list will be put in a leaf in its entirety.
buildBVH :: (Boxable a) => Int -> [a] -> BVH a
buildBVH n xs = buildBVH' $ WL (length xs) (map mkBoxed xs)
    where
        buildBVH' :: (Boxable a) => WithLength [Boxed a] -> BVH a
        buildBVH' withLength@(WL l xs)
           | l <= n    = BVHleaf xs
           -- | otherwise = case bestPartition $ partitionBoxeds withLength of
           | otherwise = case bestPartition $ partitionBoxeds withLength of
                Nothing       -> BVHleaf xs
                Just (p1, p2) -> BVHnode (buildBVH' <$> swapBoxedAndWithLength p1)
                                         (buildBVH' <$> swapBoxedAndWithLength p2)
        swapBoxedAndWithLength (WL n b) = Boxed (thebox b) (WL n (unbox b))

--TODO speed up the normal one
buildBVHfast :: (Boxable a) => Int -> [a] -> BVH a
buildBVHfast n xs = buildBVH' $ WL (length xs) (map mkBoxed xs)
    where
        buildBVH' :: (Boxable a) => WithLength [Boxed a] -> BVH a
        buildBVH' withLength@(WL l xs)
           | l <= n    = BVHleaf xs
           | otherwise = case bestPartition $ partitionBoxedsFast withLength of
                Nothing       -> BVHleaf xs
                Just (p1, p2) -> BVHnode (buildBVH' <$> swapBoxedAndWithLength p1)
                                         (buildBVH' <$> swapBoxedAndWithLength p2)
        swapBoxedAndWithLength (WL n b) = Boxed (thebox b) (WL n (unbox b))



-- | A thingamajing with an associated length of said thingamajing stored 
-- for O(1) access.
data WithLength a = WL {
    wlLength :: Int,
    fromWL   :: a 
} deriving Functor
toWL :: [a] -> WithLength [a]
toWL xs = WL (length xs) xs

-- | A partition of a bunch of boxeds, each part wrapped in a boxed itself. 
-- For each 'bunch of boxeds', the number of elements inside is stored as 
-- well.
type BoxedPartition a = (WithLength (Boxed [Boxed a]), WithLength (Boxed [Boxed a]))

-- | Generate a list of possible Partitions from a list of boxeds. 
partitionBoxeds :: (Boxable a) => WithLength [Boxed a] -> [BoxedPartition a]
partitionBoxeds boxedsWL = map (mapPair (fmap mkBoxed)) $
    partitionList sortedx ++ partitionList sortedy ++ partitionList sortedz
    where
        sortedx = fmap (sortWith (\b -> f3x (min b))) boxedsWL
        sortedy = fmap (sortWith (\b -> f3y (min b))) boxedsWL
        sortedz = fmap (sortWith (\b -> f3z (min b))) boxedsWL
        min boxed = boxmin where (Box boxmin boxmax) = box boxed


-- Only partitions dividing the three axes in the center are generated (so 
-- this will run in O(n) instead of O(n log n) when sorting).  Partitions 
-- where one of the parts is empty are suppressed.
partitionBoxedsFast :: (Boxable a) => WithLength [Boxed a] -> [BoxedPartition a]
partitionBoxedsFast (WL 0 [])  = []
partitionBoxedsFast (WL 1 [_]) = []
partitionBoxedsFast (WL _ xs)  = map (mapPair (fmap mkBoxed . toWL)) $
                     filter (\(a, b) -> not (null a)  &&  not (null b))
                        [partition (\b -> f3x (min b) < thresholdx) xs
                        ,partition (\b -> f3y (min b) < thresholdy) xs
                        ,partition (\b -> f3z (min b) < thresholdz) xs]
    where
        min boxed = boxmin where (Box boxmin boxmax) = box boxed
        (Box totalmin totalmax) = box xs
        (F3 thresholdx thresholdy thresholdz) = totalmin .+. (totalmax .* 0.5)



mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

-- | No trivial partitions "((0,[]), (n,xs))" are created.
partitionList :: WithLength [a] -> [(WithLength [a], WithLength [a])]
partitionList (WL 0 []) = []
partitionList (WL n (x:xs)) = partitionList' (WL 1 [x]) (WL (n - 1) xs)
    where
        partitionList' (WL n1 xs) (WL 0  [])   = []
        partitionList' (WL n1 xs) (WL n2 (y:ys)) = (WL n1 xs, WL n2 (y:ys)) :
                            partitionList' (WL (n1 + 1) (y:xs)) (WL (n2 - 1) ys)


costOfPartition :: BoxedPartition a -> Flt
costOfPartition ((WL n1 b1), (WL n2 b2)) = 
    (halfSurfaceArea $ box b1) * (fromIntegral n1)
    + (halfSurfaceArea $ box b2) * (fromIntegral n2)
    where 
        halfSurfaceArea (Box p1 p2) = x*y + y*z + z*x
            where (F3 x y z) = p2 .-. p1

bestPartition :: [BoxedPartition a] -> Maybe (BoxedPartition a)
bestPartition [] = Nothing
bestPartition [p] = Just p
bestPartition (p1:p2:ps) = if costOfPartition p1 < costOfPartition p2
    then bestPartition (p1:ps)
    else bestPartition (p2:ps)




-- vim: expandtab smarttab sw=4 ts=4
