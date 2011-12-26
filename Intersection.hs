{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances #-}

module Intersection (
    Intersection(..),
    intPos, intDist, intTotDist, intDir, intNorm, intTexUV,

    Intersectable(..),
    AnyIntersectable(..),

    module Ray,
) where

import Math
import Ray
import Geometry

data Intersection t = Intersection {
        intThing   :: t,               -- ^ The thing that got intersected
        intGeomInt :: GeomIntersection -- ^ Details of the intersection
    } deriving Show
-- Lift the GeomIntersection acessors:
intPos     = giPos     . intGeomInt
intDist    = giDist    . intGeomInt
intTotDist = giTotDist . intGeomInt
intDir     = giDir     . intGeomInt
intNorm    = giNorm    . intGeomInt
intTexUV   = giTexUV   . intGeomInt

instance Ord (Intersection t) where
    i1 <= i2  =  intGeomInt i1 <= intGeomInt i2
instance Eq (Intersection t) where
    i1 == i2  =  intGeomInt i1 == intGeomInt i2


-- | Top level class of all possible things that are intersectable. This 
-- can be individual objects, but also entire scenes.
class Intersectable t a where
    intersect :: Ray -> a -> [Intersection t]





-- TODO IS THIS CORRECT?:

-- Existential intersectable (i11e cfr i18n: intersectable is too long to 
-- type and screws over my already too-wide formatting :P)
data AnyIntersectable t = forall a . (Intersectable t a, Show a) => MkAnyI11e a

instance (Intersectable t) (AnyIntersectable t) where
    intersect ray (MkAnyI11e intersectable) = intersect ray intersectable
instance Show (AnyIntersectable t) where
    show (MkAnyI11e intersectable) = "AnyI11e " ++ show intersectable



-- vim: expandtab smarttab sw=4 ts=4
