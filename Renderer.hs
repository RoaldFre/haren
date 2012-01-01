{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Renderer (
	Renderer(..),
	Pixel(..),
	pixToPt,
	Resolution(..),
	resToPix,
	flipHoriz,

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

newtype Pixel = Pixel (Int, Int) deriving (Show, Ord, Eq, Ix)

pixToPt :: Pixel -> Pt2
pixToPt (Pixel (x, y)) = F2 (fromIntegral x) (fromIntegral y)

newtype Resolution = Resolution (Int, Int) deriving Show
resToPix :: Resolution -> Pixel
resToPix (Resolution pair) = Pixel pair

flipHoriz :: Resolution -> Pixel -> Pixel
flipHoriz (Resolution (_, nj)) (Pixel (i, j)) = Pixel (i, nj - j - 1)

-- vim: expandtab smarttab sw=4 ts=4
