{-# LANGUAGE ExistentialQuantification #-}

module Geometry (
    Geometry(..),
    GeomIntersection(..),
    makeGeomInt,
    AnyGeom(..),

    -- from module Boxes
    Box,
    
    -- from module Ray
    Ray(..),

    -- from module Math
    Flt, Pt3, Vec3, UVec3, Pt2
) where

import Math
import Boxes
import Ray

class Geometry a where
    boundingBox :: a -> Box
    intersectGeom :: a -> Ray -> [GeomIntersection]

data GeomIntersection = GeomIntersection {
        giPos     :: Pt3,      -- ^ Position of the intersection
        giDist    :: Flt,      -- ^ Distance of the (last) intersecting ray
        giTotDist :: Flt,      -- ^ Total accumulated distance traveled by rays till we got here
        giDir     :: Vec3,     -- ^ Direction of intersecting ray, *NOT* normalised
        giNorm    :: UVec3,    -- ^ Normal vector of intersection surface, normalised
        giTexUV   :: Maybe Pt2 -- ^ Texture coordinate
    } deriving Show
makeGeomInt :: Ray -> Flt -> UVec3 -> Maybe Pt2 -> GeomIntersection
makeGeomInt ray dist = 
    GeomIntersection (walk ray dist) dist (dist + (rayDist ray)) (rayDir ray)

-- | Note: this ordering only really makes sense for intersections of the same ray.
instance Ord GeomIntersection where
    gi1 <= gi2  =  giDist gi1 <= giDist gi2
-- Prerequisite for Ord...
instance Eq GeomIntersection where
    gi1 == gi2  =  giDist gi1 == giDist gi2


-- Existential geometry
data AnyGeom = forall a . (Geometry a, Show a) => MkAnyGeom a
instance Geometry AnyGeom where
    boundingBox (MkAnyGeom g) = boundingBox g
    intersectGeom (MkAnyGeom g) = intersectGeom g
instance Show AnyGeom where
    show (MkAnyGeom geom) = "AnyGeom " ++ show geom

instance Boxable AnyGeom where
    box (MkAnyGeom geom) = boundingBox geom








-- vim: expandtab smarttab sw=4 ts=4
