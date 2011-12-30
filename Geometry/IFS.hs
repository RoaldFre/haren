module Geometry.IFS (
    Attractor,
    mkIFS,
    sierpinski2D,
    sierpinskiPiramid, sierpinskiPiramidStd,
    sierpinskiTetraeder, sierpinskiTetraederStd,

    module Geometry
) where

import Math
import Geometry
import Geometry.Triangles
import Transform
import BVH

-- | (Fixed point, scale factor)
-- Scale factor is 'inverted', ie 2 means halve the length.
type Attractor = (Pt3, Flt)
attrToTrans :: Attractor -> Transformation
attrToTrans (point, scale) =
    Translation point `After` Scale (1/scale) `After` Translation (inv point)

mkIFS :: [Attractor] -> AnyGeom -> Int -> AnyGeom
mkIFS attractors unit depth =
    MkAnyGeom $ buildBVHfast 1 $ flattenTransfoGraph $ mkIFS' depth $ Leaf unit
    where
        mkIFS' 0 graph = graph
        mkIFS' d graph = Fork $ map (\a -> Node (attrToTrans a) subGraph) attractors
            where subGraph = mkIFS' (d-1) graph

-- | Create a 2D Sierpinski Triangle in the xy plane, centered in the 
-- origin, by iterating the given starting geometry for the given number of 
-- iterations. The attractors fill the (-1, -1, 0) to (1, sqrt(3) - 1, 0) 
-- bounding box.
sierpinski2D :: AnyGeom -> Int -> AnyGeom
sierpinski2D = mkIFS $
    [((F3 (-1)  (-1)   0), 2)
    ,((F3   1   (-1)   0), 2)
    ,((F3   0  (sq3-1) 0), 2)]
    where sq3 = sqrt 3



-- | Create a Sierpinski Piramid by iterating the given starting geometry 
-- for the given number of iterations. The attractors fill the
-- (-sqrt(1/2), 0, -sqrt(1/2) to (sqrt(1/2), 1, sqrt(1/2)) bounding box.
sierpinskiPiramid :: AnyGeom -> Int -> AnyGeom
sierpinskiPiramid = mkIFS $
    [((F3 (-x)  0 (-x)), 2)
    ,((F3 (-x)  0   x ), 2)
    ,((F3   x   0 (-x)), 2)
    ,((F3   x   0   x ), 2)
    ,((F3   0   1   0 ), 2)]
    where x = sqrt 0.5

-- | Create a sierpinskiPiramid with as initial geometry a piramid 
-- with the attractors as vertices.
sierpinskiPiramidStd :: Int -> AnyGeom
sierpinskiPiramidStd = sierpinskiPiramid $ MkAnyGeom $
    TriangleMesh [mkTriangle lf rf t
                 ,mkTriangle rf rb t
                 ,mkTriangle rb lb t
                 ,mkTriangle lb rf t
                 ,mkTriangle rf lf lb
                 ,mkTriangle lb rb rf]
    where
        x  = sqrt 0.5
        lf = F3 (-x)  0   x  -- left front
        lb = F3 (-x)  0 (-x) -- left back
        rf = F3   x   0   x  -- right front
        rb = F3   x   0 (-x) -- right back
        t  = F3   0   1   0  -- top



-- | Create a Sierpinski Tetraeder by iterating the given starting geometry 
-- for the given number of iterations. The attractors fill the
-- (-sqrt(1/2), 0, -sqrt(1/2) to (sqrt(1/2), 1, sqrt(1/2)) bounding box.
sierpinskiTetraeder :: AnyGeom -> Int -> AnyGeom
sierpinskiTetraeder = mkIFS $
    [((F3 (-x/2)  0 ( x*1/3)), 2)
    ,((F3 ( x/2)  0 ( x*1/3)), 2)
    ,((F3    0    0 (-x*2/3)), 2)
    ,((F3    0    1     0   ), 2)]
    where x = sqrt (3 / 2)

-- | Create a sierpinskiTetraeder with as initial geometry a tetraeder
-- with the attractors as vertices.
sierpinskiTetraederStd :: Int -> AnyGeom
sierpinskiTetraederStd = sierpinskiTetraeder $ MkAnyGeom $
    TriangleMesh [mkTriangle l r t
                 ,mkTriangle r b t
                 ,mkTriangle b l t
                 ,mkTriangle r l b]
    where
        x = sqrt (3 / 2)
        l = F3 (-x/2)  0 ( x*1/3) -- left
        r = F3 ( x/2)  0 ( x*1/3) -- right
        b = F3    0    0 (-x*2/3) -- back
        t = F3    0    1     0    -- top

-- vim: expandtab smarttab sw=4 ts=4
