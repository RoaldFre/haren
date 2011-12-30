{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor #-}

module Transform (
    Transformation(..),
    TransformationGraph,
    flattenTransfoGraph,
    module Graph,
) where

import Geometry
import Intersection
import Boxes
import Math
import Graph

data Transformation = Identity
                    | Translation Vec3
                    | Rotation Vec3 Flt
                    | Scale Flt
                    | ScaleXYZ Flt Flt Flt
                    | Transformation `After` Transformation -- Composition
                    deriving Show

-- | Returns matrices for the normal and inverse transformation.
transfoM4s :: Transformation -> (M4, M4)
transfoM4s Identity              = (m4id, m4id)
transfoM4s (Translation v)       = trans3M4s v
transfoM4s (Rotation axis angle) = rotM4s axis angle
transfoM4s (Scale factor)        = transfoM4s $ ScaleXYZ factor factor factor
transfoM4s (ScaleXYZ x y z)      = scalexyzM4s x y z
transfoM4s (t1 `After` t2)       = (m1 .*. m2, m2inv .*. m1inv)
    where
        (m1, m1inv) = transfoM4s t1
        (m2, m2inv) = transfoM4s t2


-- | Transformed thingamajings, represented by transformations and the 
-- untransformed thingamajing.
data Transformed a = Transformed {
        tTrans     :: M4,
        _tInvTrans :: M4,
        tOriginal  :: a
    } deriving (Show, Functor)
instance (Ord a) => Ord (Transformed a) where
    t1 <= t2  =  (tOriginal t1) <= (tOriginal t2)
-- Prerequisite for Ord...
instance (Eq a) => Eq (Transformed a) where
    t1 == t2  =  (tOriginal t1) == (tOriginal t2)


-- Boxable
instance (Boxable a) => Boxable (Transformed a) where
    box t = transformBox (tTrans t) $ box $ tOriginal t

transformBox :: M4 -> Box -> Box
transformBox trans b = box $ map (trans `multPt`) $ getBoxVertices b


-- Geometry
instance (Geometry a) => Geometry (Transformed a) where
    boundingBox t = transformBox (tTrans t) $ boundingBox $ tOriginal t
    intersectGeom (Transformed trans invTrans original) ray =
        map (transformGeomInt (rayOrigin ray) (trans, invTrans)) originalInts
        where originalInts = intersectGeom original ray

transformGeomInt :: Pt3 -> (M4, M4) -> GeomIntersection -> GeomIntersection
transformGeomInt originalOrigin (trans, invTrans) gi =
    gi {giDir = newDir, giPos = newPos, giNorm = newNorm}
    where 
        newDir = trans `multVec` (giDir gi)
        newPos = originalOrigin .+. (newDir .* giDist gi) 
        newNorm = normalize $ (transpose invTrans) `multVec` (giNorm gi)


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
transformInt originalOrigin trans int =
    int {intGeomInt = transformGeomInt originalOrigin trans $ intGeomInt int}


-- Graph
type TransformationGraph l = Graph l Transformation
flattenTransfoGraph :: TransformationGraph l -> [Transformed l]
flattenTransfoGraph graph = 
    map mkTrans $ flattenGraph multTuples (m4id, m4id) matricesGraph
    where
        mkTrans ((m,mInv), leaf) = Transformed m mInv leaf
        matricesGraph = transfoM4s `fmap` graph
        multTuples (a, b) (x, y) = (a .*. x, y .*. b)
        -- Note that the matrix for the child gets multiplied to the right 
        -- for the normal transformation, and to the left for the inverse.

-- vim: expandtab smarttab sw=4 ts=4
