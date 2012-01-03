module Geometry.Plane (
    mkPlane,
    mkPlane2,
    module Geometry
) where

import Geometry
import Math
import Boxes

-- | A plane (with several normals -> 1 or 2 sided).
-- Plane origin direction1 direction2 normals
data Plane = Plane Pt3 Vec3 Vec3 [UVec3] deriving Show

-- TODO returun AnyGeom from mkPlane, not a bare Plane!

-- | Single sided plane
mkPlane :: Pt3 -> Vec3 -> Vec3 -> Vec3 -> Plane
mkPlane origin dir1 dir2 normalDir = Plane origin dir1 dir2 [normal]
    where 
        normal = if norm .*. normalDir > 0 then norm else inv norm
        norm = normalize $ dir1 .^. dir2
       
-- | Double sided plane
mkPlane2 :: Pt3 -> Vec3 -> Vec3 -> Plane
mkPlane2 origin dir1 dir2 = Plane origin dir1 dir2 [norm, inv norm]
    where norm = normalize $ dir1 .^. dir2

instance Geometry Plane where
    boundingBox (Plane o d1 d2 _) = box [o, o.+.d1, o.+.d2, o.+.d1.+.d2]
    intersectGeom (Plane o d1 d2 ns) ray@(Ray e d mint maxt _)
        | t < mint  || t > maxt  = []
        | gamma < 0 || gamma > 1 = []
        | beta < 0  || beta > 1  = []
        | otherwise              = [makeGeomInt ray t n (Just $ F2 beta gamma)| n <- ns]
        where
            -- e + t*d = o + beta*d1 + gamma*d2
            F3 t beta gamma = solve3x3 (M3 ((-1)*.d) d1 d2) (e .-. o)

-- vim: expandtab smarttab sw=4 ts=4
