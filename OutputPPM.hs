module OutputPPM (renderPPM) where

import Image
import Math

renderPPM :: FilePath -> Image -> IO ()
renderPPM fileName = (writeFile fileName) . renderPPMstr

renderPPMstr :: Image -> String
renderPPMstr image =
    headerPPM res ++ "\n" ++ 
        unlines [
            unwords [
                colorToPPM $ imageMap (Pixel (i,j)) | i <- [0 .. nx-1] ]
                                                    | j <- [0 .. ny-1] ]
    where
        res@(Resolution (nx, ny)) = imgRes image
        imageMap = imgMap image

headerPPM :: Resolution -> String
headerPPM (Resolution (nx, ny)) = 
    "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255"

colorToPPM :: Color -> String
colorToPPM (r, g, b) = component r ++ " " ++ component g ++ " " ++ component b
    where component = show . round . (* 255) . min 1 . max 0


-- vim: expandtab smarttab sw=4 ts=4
