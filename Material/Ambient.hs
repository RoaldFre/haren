module Material.Ambient (
    mkAmbient
) where

import Material

data Ambient = Ambient Color deriving Show
mkAmbient :: Color -> AnyMat
mkAmbient col = MkAnyMat $ Ambient col

instance Material Ambient where
    colorMaterial _ (Ambient col) _ = return col

-- vim: expandtab smarttab sw=4 ts=4
