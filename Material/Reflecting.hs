module Material.Reflecting (
    mkReflecting,
) where

import Haray
import Ray
import Material
import Math

data Reflecting = Reflecting deriving Show
mkReflecting :: AnyMat
mkReflecting = MkAnyMat Reflecting

instance Material Reflecting where
    colorMaterial int Reflecting _ =
        black `orRecurseOn` (colorRay ray)
        where
            ray = Ray (intPos int) reflectedDir epsilon infinity (intTotDist int)
            reflectedDir = reflect (intDir int) (intNorm int)

-- vim: expandtab smarttab sw=4 ts=4
