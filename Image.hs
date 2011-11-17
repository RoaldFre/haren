module Image where

import Math


{-

-- TODO: Pick proper convention (0 to res-1) or (1 to res) and check if 
-- everything complies with this!
newtype Pixel = Pixel (Int, Int) deriving Show

newtype Resolution = Resolution (Int, Int) deriving Show

-- | RGB triplet, components in the range [0..1]. Not a newtype so we can 
-- reuse our triplet math.
type Color = (Flt, Flt, Flt)
black = (0, 0, 0) :: Color
white = (1, 1, 1) :: Color
red   = (1, 0, 0) :: Color
green = (0, 1, 0) :: Color
blue  = (0, 0, 1) :: Color

data Image = Image {
        imgRes :: Resolution,
        imgMap :: RT (Pixel -> Color)
    }

flipHoriz :: Resolution -> Pixel -> Pixel
flipHoriz (Resolution (ni, nj)) (Pixel (i, j)) = Pixel (i, nj - j - 1)

-}

-- vim: expandtab smarttab sw=4 ts=4
