module Geometry.Sphere (
    Sphere(..)
    module Geometry
) where

import Geometry

-- | Sphere with radius 1 at origin
data Sphere = Sphere deriving Show

instance Geometry Sphere where
    boundingBox Sphere = Box (F3 (-1) (-1) (-1)) (F3 1 1 1)
    intersectGeom Sphere ray@(Ray e d min max _) =
        [makeGeomInt ray t (sphereNormal t) Nothing | t <- ts, min < t, t < max]
        where
            ts = solveQuadEq
                    (d .*. d)
                    (2 *. d .*. e)
                    (e.^2 - 1)
            sphereNormal t = (e .+. t*.d)

solveQuadEq :: Flt -> Flt -> Flt -> [Flt]
solveQuadEq a b c
    | d < 0     = []
    | d > 0     = [(-b - sqrt(d))/(2*a), (-b + sqrt(d))/(2*a)]
    | otherwise = [-b/(2*a)]
    where
        d = b^2 - 4*a*c

-- vim: expandtab smarttab sw=4 ts=4
