module Light where

import Math
import Color

data LightType = PointSource Pt3        -- ^ Pointsource position
        | SoftBox Pt3 Vec3 Vec3 Int Int -- ^ SoftBox origin side1 side2 numRays1 numRays2
        deriving Show

-- Distribute the given number of rays across the surface of the softbox in 
-- each direction. The resulting number of rays is at least the given 
-- number here, but potentially more! -- TODO give upper bound (too lazy to 
-- calculate right now)
mkSoftBox :: Pt3 -> Vec3 -> Vec3 -> Int -> LightType
mkSoftBox origin dir1 dir2 totRays = SoftBox origin dir1 dir2 n1 n2
    where
        width  = (len dir1)
        height = (len dir2)
        averageLength = sqrt (width * height)
        n  = fromIntegral totRays
        n1 = ceiling $ (sqrt n) * width / averageLength
        n2 = ceiling $ (sqrt n) * height / averageLength

data Light = Light {
        lightType  :: LightType,
        lightColor :: Color
    } deriving Show

-- vim: expandtab smarttab sw=4 ts=4
