module Material.Diffuse (
    mkDiffuse,
) where

import Material
import Math

data Diffuse = Diffuse deriving Show
mkDiffuse :: AnyMat
mkDiffuse = MkAnyMat Diffuse

instance Material Diffuse where
    colorMaterialSingleLight int Diffuse (ilDir, ilCol)
        | cosTheta < 0 = return black -- Possible if we got hit from inside.
        | otherwise    = return $ ilCol .* cosTheta
        where cosTheta = ilDir .*. (intNorm int)

-- vim: expandtab smarttab sw=4 ts=4
