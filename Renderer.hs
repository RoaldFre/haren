{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Renderer (
	Renderer(..),
	Pixel(..),
	pixToPt,
	Resolution(..),
	resToPix,
	flipHoriz,

	-- TODO export full modules?
	Color(..),
	Scene(..),
) where

import Color
import Scene
import Math

import Data.Ix

class (Monad m, Functor m) => Renderer c m | m -> c, c -> m where
    colorPixel :: Pixel -> m Color
    getResolution :: m Resolution
    run :: Scene -> c -> m a -> a -- ^ scene -> config -> computation -> result

{-
class (Monad m, Functor m) => RendererST c m s | m -> c, c -> m where
    colorPixel_ :: Pixel -> m Color 
    getResolution_ :: m Resolution
    run_ :: Scene -> c -> m a -> a -- ^ scene -> config -> computation -> result
-}


-- TODO: Pick proper convention (0 to res-1) or (1 to res) and check if 
-- everything complies with this!
-- | ( 0,  0) is the top left corner of the image
-- | (nx, ny) is the bottom right corner of the image
newtype Pixel = Pixel (Int, Int) deriving (Show, Ord, Eq, Ix)

pixToPt :: Pixel -> Pt2
pixToPt (Pixel (x, y)) = F2 (fromIntegral x) (fromIntegral y)

newtype Resolution = Resolution (Int, Int) deriving Show
resToPix :: Resolution -> Pixel
resToPix (Resolution pair) = Pixel pair

flipHoriz :: Resolution -> Pixel -> Pixel
flipHoriz (Resolution (ni, nj)) (Pixel (i, j)) = Pixel (i, nj - j - 1)

-- vim: expandtab smarttab sw=4 ts=4
