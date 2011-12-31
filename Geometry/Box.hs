module Geometry.Box (
    Box(..),
    mkBox,
    getBoxVertices,
    hitsBox,
) where

import Math
import Geometry

import Data.List

-- | Axis aligned box.
data Box = Box {
        boxMin :: !Pt3,
        boxMax :: !Pt3
    } deriving Show

instance Geometry Box where
    boundingBox = id
    intersectGeom = intersectBox

mkBox :: Pt3 -> Pt3 -> AnyGeom
mkBox p1 p2 = MkAnyGeom $ Box p1 p2

getBoxVertices :: Box -> [Pt3]
getBoxVertices (Box p1 p2) = [p1 .+. (F3 x y z) | x <- [0, f3x (p2 .-. p1)], 
                                                  y <- [0, f3y (p2 .-. p1)],
                                                  z <- [0, f3z (p2 .-. p1)]]

-- Note: compiler will/should specialize this to loose all the 
-- normal-dragging-along code from intersectBox. (TODO: verify)
hitsBox :: Ray -> Box -> Bool
ray `hitsBox` box = not $ null $ intersectBox box ray

-- TODO: make this beautiful, loose the ugly imperative feel! ;P
intersectBox :: Box -> Ray -> [GeomIntersection]
intersectBox (Box p1 p2) ray
    | (dist far1) < (dist near1)   ||  (dist far1) < 0  =  []
    | (dist far2) < (dist near2)   ||  (dist far2) < 0  =  []
    | (dist far3) < (dist near3)   ||  (dist far3) < 0  =  []
    | otherwise = mkGeomInts ray [near3, far3]
    where
        distFromSlabs dir bound1 bound2 = if t1 < t2 then (t1, t2) else (t2, t1)
            where
                t1 = bound1 / dir
                t2 = bound2 / dir
        [dists1, dists2, dists3] = zipWith3 distFromSlabs 
                                       (tupleToList (rayDir ray)) 
                                       (tupleToList (p1 .-. rayOrigin ray))
                                       (tupleToList (p2 .-. rayOrigin ray))
        initialDistsAndNorms = addNorm ray f3zero (rayNear ray, rayFar ray)
        distsAndNorms1 = addNorm ray f3e1 dists1
        distsAndNorms2 = addNorm ray f3e2 dists2
        distsAndNorms3 = addNorm ray f3e3 dists3
        (near1, far1) = shrink initialDistsAndNorms distsAndNorms1
        (near2, far2) = shrink (near1, far1) distsAndNorms2
        (near3, far3) = shrink (near2, far2) distsAndNorms3


type DistAndNorm = (Flt, UVec3)
dist :: DistAndNorm -> Flt
dist = fst

addNorm :: Ray -> UVec3 -> (Flt, Flt) -> (DistAndNorm, DistAndNorm)
addNorm ray norm (t1, t2) = ((t1, norm1), (t2, norm2))
    where
        norm1 = if (rayDir ray .*. norm < 0) then norm else inv norm
        norm2 = inv norm1

shrink :: (DistAndNorm, DistAndNorm) -> (DistAndNorm, DistAndNorm) -> (DistAndNorm, DistAndNorm)
shrink (near1, far1)  (near2, far2)  =  (near, far)
    where
        near = if (dist near1) > (dist near2) then near1 else near2
        far  = if (dist far1 ) < (dist far2 ) then far1  else far2

mkGeomInts :: Ray -> [DistAndNorm] -> [GeomIntersection]
mkGeomInts ray dns =
    map (\(t, n) -> makeGeomInt ray t n Nothing) dns

-- vim: expandtab smarttab sw=4 ts=4
