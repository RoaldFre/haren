{-# LANGUAGE BangPatterns #-}
module OutputPPM (renderPPM) where

import Renderer
import Math

import Control.Applicative
import Control.Monad
import GHC.Exts

renderPPM :: (Renderer c m) => Int -> FilePath -> Scene -> c -> IO ()
renderPPM chunksize fileName scene conf =
    writeFile fileName $ run scene conf stringAction
    where
        stringAction = chunkParallel chunksize (renderPPMstr res)
        res = run scene conf getResolution -- bit of a hack here ...

chunkParallel :: (Renderer c m) => Int -> [m String] -> m String
chunkParallel chunksize !actions = result
    where
        chunked = splitEvery chunksize actions -- [[m Str]]
        batches = map (mapM id) chunked        -- [m [Str]]
        renderedBatches = renderPar batches    -- m [[Str]]
        result = fmap (concat . concat) renderedBatches -- m Str

renderPPMstr :: (Renderer c m) => Resolution -> [m String]
renderPPMstr res@(Resolution (_,ny)) =
    (return $ headerPPM res ++ "\n") : rows
    where
        rowsLists = map (renderRow res) [0 .. ny-1]
        rows = foldr (\a b -> a ++ [return "\n"] ++ b) [] rowsLists

-- A list of strings where each string is the color of a single pixel
renderRow :: (Renderer c m) => Resolution -> Int -> [m String]
renderRow (Resolution (nx, _)) row =
    map (\p -> colorToPPM <$> colorPixel p) [Pixel (j, row) | j <- [0 .. nx-1]]

headerPPM :: Resolution -> String
headerPPM (Resolution (nx, ny)) = 
    "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255"

-- | Appends a space (to prepare for a possible next color)
colorToPPM :: Color -> String
colorToPPM (Color r g b) = component r ++ " " ++ component g ++ " " ++ component b ++ " "
    where component = show . (round :: Flt -> Int) . (* 255) . min 1 . max 0


-- From http://www.haskell.org/haskellwiki/Data.List.Split
-- TODO read up on build/foldr/fusion and use it!
splitEvery :: Int -> [e] -> [[e]]
splitEvery i list = map (take i) (build (splitter list)) where
    splitter [] _ n = n
    splitter l c n  = l `c` splitter (drop i l) c n

-- vim: expandtab smarttab sw=4 ts=4
