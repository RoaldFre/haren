module Geometry.Triangles (
    Vertex(..),
    Triangle(..),
    mkTriangle,

    TriangleMesh(..),
    optimizeTriangleMesh,

    module Geometry
) where

import Math
import Geometry
import Boxes
import BVH

data Vertex = Vertex {
        vPos  :: !Pt3,
        vNorm :: !UVec3
    } deriving Show

data Triangle = Triangle !Vertex !Vertex !Vertex  -- ^ Triangle with the given vertices
        deriving Show

-- | Make a triangle with the given set of vertexpoints. The normals will 
-- automatically be computed (giving flat shading) based on the order in 
-- which the vertexpoints are given (righthanded).
mkTriangle :: Pt3 -> Pt3 -> Pt3 -> Triangle
mkTriangle p1 p2 p3 = Triangle (Vertex p1 n) (Vertex p2 n) (Vertex p3 n)
    where n = normalize $ (p2 .-. p1) .^. (p3 .-. p1)

instance Geometry Triangle where
    boundingBox (Triangle v1 v2 v3) = box $ map vPos [v1, v2, v3]
    intersectGeom (Triangle v1 v2 v3) ray@(Ray e d mint maxt _)
        -- | See pp206-208 of Fundamentals of Computer Graphics (Peter 
        -- Shirley, 2nd edition) for algorithm.
        | t < mint  || t > maxt         = []
        | gamma < 0 || gamma > 1        = []
        | beta < 0  || beta > 1 - gamma = []
        | otherwise                     = [makeGeomInt ray t n Nothing]
        where
            -- e + t*d = o + beta*(-d1) + gamma*(-d2)
            F3 t beta gamma = solve3x3 (M3 d d1 d2) diff
            d1 = (vPos v1) .-. (vPos v2) -- (inverse of) 1st basis vector, for beta
            d2 = (vPos v1) .-. (vPos v3) -- (inverse of) 2nd basis vector, for gamma
            diff = (vPos v1) .-. e -- vector to travel
            alpha = 1 - beta - gamma
            n = normalize $
                alpha*.(vNorm v1) .+. beta*.(vNorm v2) .+. gamma*.(vNorm v3)

data TriangleMesh = TriangleMesh [Triangle] deriving Show
instance Geometry TriangleMesh where
    boundingBox (TriangleMesh triangles) =
        box [boundingBox triangle | triangle <- triangles]
    intersectGeom (TriangleMesh triangles) ray =
        concatMap (\t -> intersectGeom t ray) triangles


-- | Optimize the given trianglemesh for raytracing. Triangles will be 
-- placed in a BVH, with each leaf containing at most the given number of 
-- triangles (modulo pathological cases).
optimizeTriangleMesh:: Int -> TriangleMesh -> AnyGeom
optimizeTriangleMesh n (TriangleMesh triangles) =
    MkAnyGeom $! buildBVH n $ MkAnyGeom `fmap` triangles

-- vim: expandtab smarttab sw=4 ts=4
