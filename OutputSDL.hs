module OutputSDL (renderSDL) where

import Foreign
import Data.Word
import Control.Monad.Reader
import Graphics.UI.SDL hiding (Pixel, Color)
import Graphics.UI.SDL.Types
import System.Exit

import Image
import Math

renderSDL :: Image -> IO ()
renderSDL image = do
    screen <- setVideoMode nx ny 32 [SWSurface]
    setCaption "haren" []
    let pixels = [Pixel (i, j) | j <- [0 .. ny - 1], i <- [0 .. nx - 1]]
    let putPixelActions = map (putPixel screen) $ zip pixels (map (imgMap image) pixels)
    let actions = sprinkle [Graphics.UI.SDL.flip screen, pollForQuit] putPixelActions
    sequence actions
    quitHandler
    where Resolution (nx, ny) = imgRes image

-- | 'Sprinkle' the first given list between every element of the second 
-- list, including at the very beginnig and the very end
sprinkle :: [a] -> [a] -> [a]
sprinkle insertion [] = insertion
sprinkle insertion (x:xs) = insertion ++ [x] ++ sprinkle insertion xs

putPixel :: Surface -> (Pixel, Color) -> IO ()
putPixel s ((Pixel (x,y)), (r,g,b)) = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    pixelCol <- mapRGB (surfaceGetPixelFormat s) r8 g8 b8
    pokeElemOff pixels ((y * surfaceGetWidth s) + x) pixelCol
    where
        [r8, g8, b8] = map channelToWord [r, g, b]
        channelToWord = fromIntegral . round . (* 255) . max 0 . min 1

pollForQuit :: IO ()
pollForQuit = do
    e <- pollEvent
    case e of
        Quit    -> exitSuccess --TODO: nicer way?
        NoEvent -> return ()
        _       -> pollForQuit

quitHandler :: IO ()
quitHandler = do
    e <- waitEvent
    case e of
        Quit -> return ()
        otherwise -> quitHandler


-- vim: expandtab smarttab sw=4 ts=4
