module Ray where

import Math

data Ray = Ray {
        rayOrigin :: !Pt3,
        rayDir    :: !Vec3, -- ^ *NOT* neccesarily normalised (eg for transformed rays)
        rayNear   :: !Flt,  -- ^ near clipping distance
        rayFar    :: !Flt,  -- ^ far clipping distance
        rayDist   :: !Flt   -- ^ Total distance traversed by the light before it became 
                            --   this ray. Eg if this ray is a reflected 
                            --   ray, the distance here is the distance of 
                            --   the original ray before it reflected and 
                            --   became this ray.
    } deriving Show

walk :: Ray -> Flt -> Pt3
walk ray dist = (rayOrigin ray) .+. (rayDir ray) .* dist


-- vim: expandtab smarttab sw=4 ts=4
