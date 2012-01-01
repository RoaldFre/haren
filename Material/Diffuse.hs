module Material.Diffuse (
    mkDiffuse,
) where

import Material
import Math

data Diffuse = Diffuse deriving Show
mkDiffuse :: AnyMat
mkDiffuse = MkAnyMat Diffuse

instance Material Diffuse where
    colorMaterialSingleLight int Diffuse (ilDir, ilCol) =
        return $ ilCol .* (ilDir .*. (intNorm int))

-- vim: expandtab smarttab sw=4 ts=4
