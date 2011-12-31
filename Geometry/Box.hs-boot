module Geometry.Box where

import Ray
import Math

data Box = Box {
        boxMin :: !Pt3,
        boxMax :: !Pt3
    }

hitsBox :: Ray -> Box -> Bool
getBoxVertices :: Box -> [Pt3]
