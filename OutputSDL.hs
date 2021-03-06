module OutputSDL (renderSDL, RenderMode(..)) where

import Foreign
import Data.List
import Control.Monad.Reader
import Graphics.UI.SDL as SDL hiding (Pixel, Color)
import System.Exit

import Renderer

import Math -- TODO, this is only for Flt, but should get that from Renderer...

data RenderMode = PerPixel | PerLine | PerLines Int

chunkSize :: RenderMode -> Resolution -> Int
chunkSize PerPixel _ = 1
chunkSize PerLine (Resolution (nx, _)) = nx
chunkSize (PerLines n) res = n * (chunkSize PerLine res)

renderSDL :: (Renderer c m) => RenderMode -> Scene -> c -> IO ()
renderSDL renderMode scene conf = do
    let (Resolution (nx, ny)) = run scene conf getResolution
    screen <- setVideoMode nx ny 32 [SWSurface]
    setCaption "haren" []
    sequence_ $ run scene conf (renderSDLactions renderMode screen)
    putStrLn "All done!"
    quitHandler screen

renderSDLactions :: (Renderer c m) => RenderMode -> Surface -> m [IO ()]
renderSDLactions renderMode screen  = do
    res@(Resolution (nx, ny)) <- getResolution
    let pixels = [Pixel (i, j) | j <- [0 .. ny - 1], i <- [0 .. nx - 1]]
    putPixelActions <- mapM pixToPutPix pixels
    let n = chunkSize renderMode res
    return $ sprinkle n [SDL.flip screen, pollForQuit] putPixelActions
    where 
        pixToPutPix :: (Renderer c m) => Pixel -> m (IO ())
        pixToPutPix pixel = do
            color <- colorPixel pixel
            return $ putPixel screen (pixel, color)

-- | 'Sprinkle' the first given list at every n'th position in the second 
-- list, including at the very beginnig and the very end
--sprinkle :: Int -> [a] -> [a] -> [a]
sprinkle :: Int -> [a] -> [a] -> [a]
sprinkle _ insertion [] = insertion
sprinkle n insertion xs = insertion ++ chunk ++ sprinkle n insertion rest
    where (chunk, rest) = splitAt n xs

putPixel :: Surface -> (Pixel, Color) -> IO ()
putPixel s ((Pixel (x,y)), (Color r g b)) = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    pixelCol <- mapRGB (surfaceGetPixelFormat s) r8 g8 b8
    pokeElemOff pixels ((y * surfaceGetWidth s) + x) pixelCol
    where
        [r8, g8, b8] = map channelToWord [r, g, b]
        channelToWord = fromIntegral . (round :: Flt -> Int) . (* 255) . max 0 . min 1

pollForQuit :: IO ()
pollForQuit = do
    e <- pollEvent
    case e of
        Quit    -> exitSuccess
        NoEvent -> return ()
        _       -> pollForQuit

quitHandler :: Surface -> IO ()
quitHandler screen = do
    e <- waitEvent
    case e of
        Quit        -> return ()
        VideoExpose -> SDL.flip screen
        _           -> quitHandler screen


-- vim: expandtab smarttab sw=4 ts=4
