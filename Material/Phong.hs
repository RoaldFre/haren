module Material.Phong (
    mkPhong,
) where

import Material
import Math

data Phong = Phong Flt deriving Show -- ^ Phong exponent
mkPhong :: Flt -> AnyMat
mkPhong phongExponent = MkAnyMat $ Phong phongExponent

instance Material Phong where
    colorMaterialSingleLight int (Phong p) (ilDir, ilCol) =
        return $ ilCol .* ((h .*. n) ** p)
        where
            n = intNorm int
            h = normalize $ ilDir .-. (intDir int)

-- vim: expandtab smarttab sw=4 ts=4
