{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, Rank2Types #-}

module Object (
    Object(..),

    ObjIntersection,
    intMat,

    module Geometry,
    module Material
) where

import Geometry
import Material
import Boxes
import Intersection

data Object = Object {
    objGeom :: AnyGeom,
    objMat  :: AnyMat
} deriving Show

instance (Intersectable Object) Object where
    intersect ray o@(Object geom mat) = map (Intersection o) $ intersectGeom geom ray

instance Boxable Object where
    box obj = box $ objGeom obj

type ObjIntersection = Intersection Object
intMat :: ObjIntersection -> AnyMat
intMat = objMat . intThing

-- vim: expandtab smarttab sw=4 ts=4
