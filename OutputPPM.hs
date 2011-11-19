module OutputPPM (renderPPM) where

import Renderer
import Math

import Types

import System.Random
import Control.Monad.State
import Control.Applicative

renderPPM :: (Renderer c m) => FilePath -> Scene -> c -> IO ()
renderPPM fileName scene conf = writeFile fileName $ run scene conf renderPPMstr

renderPPMstr :: (Renderer c m) => m String
renderPPMstr = do
    res@(Resolution (nx, _)) <- getResolution
    massiveString <- unlines <$> mapM renderRow [0 .. nx-1]
    return $ headerPPM res ++ "\n" ++ massiveString

renderRow :: (Renderer c m) => Int -> m String
renderRow row = do
    Resolution (_, ny) <- getResolution
    colorRow <- mapM colorPixel [Pixel (row, j) | j <- [0 .. ny-1]]
    return $ unwords $ map colorToPPM colorRow

headerPPM :: Resolution -> String
headerPPM (Resolution (nx, ny)) = 
    "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255"

colorToPPM :: Color -> String
colorToPPM (Flt3 r g b) = component r ++ " " ++ component g ++ " " ++ component b
    where component = show . round . (* 255) . min 1 . max 0


-- vim: expandtab smarttab sw=4 ts=4
