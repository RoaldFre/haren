module Material.Glossy (
    mkGlossy,
) where

import Haray
import Ray
import Material
import Math

data Glossy = Glossy Flt Int deriving Show -- ^ Glossiness and number of samples
mkGlossy :: Flt -> Int -> AnyMat
mkGlossy glossiness samples = MkAnyMat $ Glossy glossiness samples

instance Material Glossy where
    colorMaterial int (Glossy stdev n) _ =
        black `orRecurseOn` do
            dus <- getStdNorms n stdev
            dvs <- getStdNorms n stdev
            let perturbs = perturb (zip dus dvs) reflectedDir
            let directions = filter (\d -> (intNorm int) .*. d > 0) perturbs -- Only reflect *away*!
            let rays = map makeReflRay directions
            colorRays n rays
        where
            makeReflRay dir = Ray (intPos int) dir epsilon infinity (intTotDist int)
            reflectedDir = reflect (intDir int) (intNorm int)

-- vim: expandtab smarttab sw=4 ts=4
