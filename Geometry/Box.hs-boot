module Geometry.Box where

import Ray
import Math

data Box = Box {
        boxMin :: !Pt3,
        boxMax :: !Pt3
    }
instance Show Box

hitsBox :: Ray -> Box -> Bool
getBoxVertices :: Box -> [Pt3]
centroid :: Box -> Pt3
surfaceArea :: Box -> Flt
