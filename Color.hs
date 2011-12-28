{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Color where

import Math
import Control.Parallel.Strategies hiding (dot)
import Control.DeepSeq

-- | Lazy RGB triplet, components in the range [0..1].
data Color = Color {
        cRed   :: Flt,
        cGreen :: Flt,
        cBlue  :: Flt
    }

instance (NumTuple Flt) Color where
    tupleToList (Color r g b) = [r, g, b]
    tupleFromList [r, g, b] = Color r g b
    tupleFromList _ = error "Invalid number of elements in list for this color!"
instance Show Color where
    show = showTuple -- TODO: specify this somehow at the level of NumTuple
instance Mult Color Color Flt where
    (.*.) = dot
instance Mult Color Flt Color where (.*.) = (.*)
instance Mult Flt Color Color where (.*.) = (*.)
instance Div Color Flt
instance Add Color where (.+.) = addt
instance Sub Color where (.-.) = subt
instance NFData Color where rnf = rnfTuple

black = Color 0 0 0
white = Color 1 1 1
red   = Color 1 0 0
green = Color 0 1 0
blue  = Color 0 0 1

-- vim: expandtab smarttab sw=4 ts=4
