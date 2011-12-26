module OutputHaras (renderImage) where

import Haras
import Math

import Control.Applicative
import Data.Array.IArray
import Data.Array.Unboxed


renderImage :: FilePath -> Image -> IO ()
renderImage fileName image =
    writeFile fileName $ renderPPMstr image

renderPPMstr :: Image -> String
renderPPMstr image =
    headerPPM res ++ "\n" ++ (unlines $ map (renderRow image) [0 .. y-2])
    where
        Pixel (x, y) = fst $ snd $ bounds image
        res = Resolution (x-1, y-1)

renderRow :: Image -> Int -> String
renderRow image row =
    unwords $ map (pixelToPPM image) [Pixel (j, row) | j <- [0 .. x-2]]
    where
        Pixel (x, _) = fst $ snd $ bounds image

headerPPM :: Resolution -> String
headerPPM (Resolution (nx, ny)) = 
    "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255"

pixelToPPM :: Image -> Pixel -> String
pixelToPPM image pixel = component r ++ " " ++ component g ++ " " ++ component b
    where 
        component = show . round . (* 255) . min 1 . max 0
        r = image ! (pixel, Red)
        g = image ! (pixel, Green)
        b = image ! (pixel, Blue)

-- vim: expandtab smarttab sw=4 ts=4
