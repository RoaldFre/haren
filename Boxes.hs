{-# LANGUAGE TypeSynonymInstances, DeriveFunctor #-}

module Boxes (
    Boxable(..),
    Boxed(..),
    mkBoxed,
    hitsBoxed,

    module Geometry.Box
) where

import Math
import Ray
import {-# SOURCE #-} Geometry.Box

import Data.List

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


-- vim: expandtab smarttab sw=4 ts=4
