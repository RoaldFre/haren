module Material.Dielectric (
    mkDielectric,
) where

import Haray
import Ray
import Material
import Math

import Data.Maybe

data Dielectric = Dielectric Flt (Flt, Flt, Flt) deriving Show
mkDielectric :: Flt -> (Flt, Flt, Flt) -> AnyMat
mkDielectric n attenuation = MkAnyMat $ Dielectric n attenuation

instance Material Dielectric where
    colorMaterial int (Dielectric n (ar, ag, ab)) _ = do
        if intDir int .*. intNorm int < 0
        then do 
            -- Entering the object
            --  always have a refracted ray entering the object (attenuate).
            --  reflected ray goes away from the object (no attenuation).
            refrCol <- transformColorRay attenuate $ fromJust refractedRay
            reflCol <- black `orRecurseOn` colorRay reflectedRay
            return $ combine reflCol refrCol
        else do
            -- Exiting the object:
            --  reflected ray stays within the object (attenuate)
            --  if no total internal refl: refracted ray leaves obj (no attenuation)
            reflCol <- black `orRecurseOn` transformColorRay attenuate reflectedRay
            case refractedRay of
                Just refrRay -> colorRay refrRay >>= (\refrCol ->
                                return $ combine reflCol refrCol)
                Nothing      -> return reflCol -- total internal reflection
     where
        nfactor = if (intDir int .*. intNorm int < 0)
                    then 1/n -- entering the object
                    else n   -- leaving the object
        refractedRay = mkRefractedRay int nfactor
        reflectedRay = Ray (intPos int) reflectedDir epsilon infinity (intTotDist int)
        reflectedDir = reflect (intDir int) (intNorm int)
        c = if (intDir int .*. intNorm int < 0)
                    then -(intDir int .*. intNorm int)
                    else (rayDir $ fromJust refractedRay) .*. intNorm int
        r0 = ((n - 1) / (n + 1))^2 -- reflectance at normal incidence 
                                   -- (invar under n <-> 1/n)
        r = r0 + (1 - r0) * (1 - c)^5 -- Schlick's approx. to Fresnel's eq.
        combine reflCol refrCol = r *. reflCol  .+.  (1 - r) *. refrCol
        attenuate i col = col .***. 
                (tupleFromList $ map (\a -> exp(-a * (intDist i))) $ [ar,ag,ab])

-- | n is the ratio of the indices of refraction of the material being 
-- exited (as determined by the direction vector) to the index of 
-- refraction of the material being entered.
mkRefractedRay :: Intersection i -> Flt -> Maybe Ray
mkRefractedRay int n
    | cosSq < 0 = Nothing -- Total internal reflection
    | otherwise = Just refrRay
    where
        norm  = if intDir int .*. intNorm int < 0 then intNorm int else inv $ intNorm int
        dir   = normalize $ intDir int
        cosSq = 1  -  n^2 * (1 - (dir .*. norm)^2)
        refrDir = normalize $ n*.(dir .-. norm.*(dir .*. norm)) .-. norm.*(sqrt cosSq)
        refrRay = Ray (intPos int) refrDir epsilon infinity (intTotDist int)

-- vim: expandtab smarttab sw=4 ts=4
