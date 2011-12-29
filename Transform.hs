{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor #-}

module Transform (
    transfoM4s, --TODO remove
    Transformation(..),
    Transformed(..), -- TODO can be hidden altogether?
) where

import Geometry
import Intersection
import Boxes
import Math

data Transformation = Identity
                    | Translation Vec3
                    | Rotation Vec3 Flt
                    | Scale Flt Flt Flt
                    | Transformation `After` Transformation -- Composition
                    deriving Show

-- | Returns matrices for the normal and inverse transformation.
transfoM4s :: Transformation -> (M4, M4)
transfoM4s Identity              = (m4id, m4id)
transfoM4s (Translation v)       = trans3M4s v
transfoM4s (Rotation axis angle) = rotM4s axis angle
transfoM4s (Scale x y z)         = scalexyzM4s x y z
transfoM4s (t1 `After` t2)       = (m1 .*. m2, m2inv .*. m1inv)
    where
        (m1, m1inv) = transfoM4s t1
        (m2, m2inv) = transfoM4s t2


-- | Transformed thingamajings, represented by transformations and the 
-- untransformed thingamajing.
data Transformed a = Transformed {
        tTrans    :: M4,
        tInvTrans :: M4,
        tOriginal :: a
    } deriving (Show, Functor)
instance (Ord a) => Ord (Transformed a) where
    t1 <= t2  =  (tOriginal t1) <= (tOriginal t2)
-- Prerequisite for Ord...
instance (Eq a) => Eq (Transformed a) where
    t1 == t2  =  (tOriginal t1) == (tOriginal t2)


-- Intersectable
instance (Intersectable t a) => (Intersectable t) (Transformed a) where
    intersect ray (Transformed trans invTrans thing) = globalInts
     where
        localInts = intersect (transformRay invTrans ray) thing
        globalInts = map (transformInt (rayOrigin ray) (trans, invTrans)) localInts

transformRay :: M4 -> Ray -> Ray
transformRay trans (Ray origin dir near far dist) =
    Ray (trans `multPt` origin) (trans `multVec` dir) near far dist

transformInt :: Pt3 -> (M4, M4) -> Intersection t -> Intersection t
transformInt originalOrigin (trans, invTrans) int =
    -- TODO: this isn't very nice imo...
    int {intGeomInt = (intGeomInt int) {giDir = newDir, giPos = newPos, giNorm = newNorm}}
    where 
        newDir = trans `multVec` (intDir int)
        --newPos = trans `multPt` (intPos int),
        newPos = originalOrigin .+. (newDir .* intDist int) 
        newNorm = normalize $ (transpose invTrans) `multVec` (intNorm int)


-- Boxable
instance (Boxable a) => Boxable (Transformed a) where
    box to = transformBox (tTrans to) $ box $ tOriginal to

transformBox :: M4 -> Box -> Box
transformBox trans b = box $ map (trans `multPt`) $ getBoxVertices b


-- vim: expandtab smarttab sw=4 ts=4
