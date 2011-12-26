{-# LANGUAGE TypeSynonymInstances, DeriveFunctor #-}

module Boxes where

import Math
import Ray

import Data.List

-- | Axis aligned box.
data Box = Box {
        boxMin :: !Pt3,
        boxMax :: !Pt3
    } deriving Show

getBoxVertices :: Box -> [Pt3]
getBoxVertices (Box p1 p2) = [p1 .+. (F3 x y z) | x <- [0, f3x (p2 .-. p1)], 
                                                  y <- [0, f3y (p2 .-. p1)],
                                                  z <- [0, f3z (p2 .-. p1)]]



class Boxable a where
    box :: a -> Box -- ^ Surrounding box for 'a'

instance Boxable Box where
    box = id

instance Boxable Pt3 where
    -- be careful with floating point precision!
    box p = Box (p .-. eps) (p .+. eps)
        where eps = epsilon *. (F3 1 1 1)

instance (Boxable a, Boxable b) => Boxable (a, b) where
    box (a, b) = Box lo hi
     where
        Box lo1 hi1 = box a
        Box lo2 hi2 = box b
        lo = tupleFromList $ zipWith min2 (tupleToList lo1) (tupleToList lo2)
        hi = tupleFromList $ zipWith max2 (tupleToList hi1) (tupleToList hi2)
        min2 x y = if x < y then x else y
        max2 x y = if x > y then x else y

instance (Boxable a) => Boxable [a] where
    box [] = error "Can't box an empty list of boxables!"
    box boxables = foldl1' (\b1 b2 -> box (b1, b2)) (map box boxables)



-- | Store a precomputed bounding box -> O(1) retrieval time
data Boxed a = Boxed {
        thebox :: !Box,
        unbox  :: a
    } deriving Functor
instance (Show a) => Show (Boxed a) where
    show b = "Box <" ++ show p1 ++ "#" ++ show p2 ++ "> containing " ++ show (unbox b)
        where Box p1 p2 = thebox b
instance Boxable (Boxed a) where
    box = thebox

-- | Put the argument in a box. -- TODO  *unless* it already is boxed.!!!!!
mkBoxed :: (Boxable a) => a -> Boxed a
mkBoxed x = Boxed (box x) x



hitsBoxed :: Ray -> Boxed a -> Bool
ray `hitsBoxed` boxed = ray `hitsBox` (box boxed)
--ray `hitsBoxed` boxed = True -- DEBUG

-- TODO: make this beautiful, loose the ugly imperative feel! ;P
hitsBox :: Ray -> Box -> Bool
ray `hitsBox` (Box p1 p2)
    | tfar1 < tnear1   ||  tfar1 < 0  =  False
    | tfar2 < tnear2   ||  tfar2 < 0  =  False
    | tfar3 < tnear3   ||  tfar3 < 0  =  False
    | otherwise                       =  True
    where
        distFromSlabs dir bound1 bound2 = if t1 < t2 then (t1, t2) else (t2, t1)
            where
                t1 = bound1 / dir
                t2 = bound2 / dir
        [dists1, dists2, dists3] = zipWith3 distFromSlabs 
                                       (tupleToList (rayDir ray)) 
                                       (tupleToList (p1 .-. rayOrigin ray))
                                       (tupleToList (p2 .-. rayOrigin ray))
        shrink (x,y) (a,b) = (maximum [x,a], minimum [y,b])
        (tnear1, tfar1) = shrink (rayNear ray, rayFar ray) dists1
        (tnear2, tfar2) = shrink (tnear1, tfar1) dists2
        (tnear3, tfar3) = shrink (tnear2, tfar2) dists3

-- vim: expandtab smarttab sw=4 ts=4
