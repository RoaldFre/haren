module Light (
    Light(..),
    --LightType,
    LightType(..), -- Export everything for now, read comment in Haray.hs 
                   -- on how this can/will be improved in the future!
    mkPointSource,
    mkSoftBox,

    -- from module Color
    Color(..)
) where

import Math
import Color

data Light = Light {
        lightType  :: LightType,
        lightColor :: Color
    } deriving Show

data LightType =
            PointSource !Pt3
            -- ^ PointSource position
            | SoftBox !Pt3 !Vec3 !Vec3 !UVec3 !Int !Int
            -- ^ SoftBox origin side1 side2 normal numRays1 numRays2
        deriving Show

mkPointSource :: Pt3 -> LightType
mkPointSource = PointSource

-- Distribute the given number of rays across the surface of the softbox in 
-- each direction. The resulting number of rays is at least the given 
-- number here, but potentially more! -- TODO give upper bound (too lazy to 
-- calculate right now)
mkSoftBox :: Pt3  -- ^ Origin of the softbox
          -> Vec3 -- ^ Direction and length of one side of the softbox
          -> Vec3 -- ^ Direction and length of other side of the softbox
          -> Pt3  -- ^ Where the softbox is pointing to (to determine direction)
          -> Int  -- ^ Approximate umber of rays that will be created (lower bound)
          -> LightType
mkSoftBox origin side1 side2 pointsTo totRays = SoftBox origin side1 side2 norm n1 n2
    where
        width  = (len side1)
        height = (len side2)
        averageLength = sqrt (width * height)
        n  = fromIntegral totRays
        n1 = ceiling $ (sqrt n) * width / averageLength
        n2 = ceiling $ (sqrt n) * height / averageLength
        norm = if norm' .*. (pointsTo .-. origin) > 0 then norm' else inv norm'
            where norm' = normalize side1 .^. side2


-- vim: expandtab smarttab sw=4 ts=4
