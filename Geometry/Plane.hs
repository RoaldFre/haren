module Geometry.Plane (
    Plane,
    mkPlane,
    module Geometry
) where

import Geometry
import Math
import Boxes

-- | A two-sided plane.
-- Plane origin direction1 direction2 normal
data Plane = Plane Pt3 Vec3 Vec3 UVec3 deriving Show
mkPlane :: Pt3 -> Vec3 -> Vec3 -> Plane
mkPlane origin dir1 dir2 = Plane origin dir1 dir2 $ normalize $ dir1 .^. dir2
       
instance Geometry Plane where
    boundingBox (Plane o d1 d2 _) = box [o, o.+.d1, o.+.d2, o.+.d1.+.d2]
    intersectGeom (Plane o d1 d2 normal) ray@(Ray e d min max _)
        | t < min   || t > max   = []
        | gamma < 0 || gamma > 1 = []
        | beta < 0  || beta > 1  = []
        | otherwise              = [makeGeomInt ray t n (Just $ F2 beta gamma)| n <- ns]
        where
            -- e + t*d = o + beta*d1 + gamma*d2
            F3 t beta gamma = solve3x3 (M3 ((-1)*.d) d1 d2) (e .-. o)
            ns = [normal, (-1)*.normal]

-- vim: expandtab smarttab sw=4 ts=4
