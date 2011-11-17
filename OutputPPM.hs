module OutputPPM (renderPPM) where

import Image
import Math

import Haray
import Types

import System.Random
import Control.Monad.State
import Control.Applicative

{-
renderPPM :: FilePath -> Image -> IO ()
renderPPM fileName = (writeFile fileName) . renderPPMstr
-}

renderPPM :: FilePath -> Image -> IO ()
renderPPM fileName img = writeFile fileName $ evalState (renderPPMstr img) state
    where
        state = RayTraceState (Scene [] []) 5 (mkStdGen 0)

renderPPMstr :: Image -> RT String
renderPPMstr image = do
    massiveString <- unlines <$> mapM (renderRow image) [0 .. nx-1]
    return $ headerPPM res ++ "\n" ++ massiveString

{-
    headerPPM res ++ "\n" ++ 
        unlines <$> [
            unwords <$> [
                colorToPPM <$> imageMap (Pixel (i,j)) | i <- [0 .. nx-1] ]
                                                    | j <- [0 .. ny-1] ]
-}
    where
        --res@(Resolution (nx, ny)) = imgRes image
        res@(Resolution (nx, ny)) = Resolution (600,600) ------------------------ TODO
        imageMap = imgMap image
        pixelRow i = [Pixel (i,j) | j <- [0 .. ny-1]]





renderRow :: Image -> Int -> RT String
renderRow image row = do
    colorRow <- mapM imageMap pixelRow
    return $ unwords $ map colorToPPM colorRow
    where
        --res@(Resolution (nx, ny)) = imgRes image
        res@(Resolution (nx, ny)) = Resolution (600,600) ------------------------ TODO
        imageMap = imgMap image
        pixelRow = [Pixel (row, j) | j <- [0 .. ny-1]]



headerPPM :: Resolution -> String
headerPPM (Resolution (nx, ny)) = 
    "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255"

colorToPPM :: Color -> String
colorToPPM (r, g, b) = component r ++ " " ++ component g ++ " " ++ component b
    where component = show . round . (* 255) . min 1 . max 0


-- vim: expandtab smarttab sw=4 ts=4
