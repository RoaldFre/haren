{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Materials where

import Haray
import Ray
import Material
import Math

import Data.Maybe
import Control.Applicative
import Data.List hiding (transpose, intersect)

data Diffuse    = Diffuse deriving Show
data Phong      = Phong Flt deriving Show           -- ^ Phong exponent
data Reflecting = Reflecting deriving Show
data Glossy     = Glossy Flt Int deriving Show      -- ^ Glossiness and number of samples
data Dielectric = Dielectric Flt deriving Show      -- ^ index of refraction
data Texture    = Texture Tex deriving Show

type Tex = (Pt2 -> Color) -- ^ function of (u,v) to colors, u and v in [0,1]
instance Show Tex where
    show _ = "<<Texture>>"

checkers :: Color -> Color -> Int -> Int -> Tex
checkers c1 c2 nu nv (F2 u v)
    | uParity + vParity == 1  =  c1
    | otherwise               =  c2
    where
        uParity = (floor $ (fromIntegral nu) * u) `mod` 2 :: Int
        vParity = (floor $ (fromIntegral nv) * v) `mod` 2 :: Int



instance Material Diffuse where
    colorMaterialSingleLight int Diffuse (ilDir, ilCol) =
        return $ ilCol .* (ilDir .*. (intNorm int))
instance Material Phong where
    colorMaterialSingleLight int (Phong p) (ilDir, ilCol) =
        return $ ilCol .* ((h .*. n) ** p)
        where
            n = intNorm int
            h = normalize $ ilDir .-. (intDir int)
instance Material Reflecting where
    colorMaterial int Reflecting _ =
        black `orRecurseOn` (colorRay ray)
        where
            ray = Ray (intPos int) reflectedDir epsilon infinity (intTotDist int)
            reflectedDir = reflect (intDir int) (intNorm int)
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
instance Material Texture where
    colorMaterialSingleLight int (Texture f) (ilDir, ilCol) = case intTexUV int of
        Nothing -> error "Trying to map texture on something without texture coordinates!"
        Just uv -> return $ (ilCol .***. f uv) .* (ilDir .*. (intNorm int))
instance Material Dielectric where
    colorMaterial int (Dielectric n) _ = do
        --reflCol <- black `orRecurseOn` colorRay reflectedRay
        if intDir int .*. intNorm int < 0
            then do
                colorRay (fromJust refractedRay)
            else
                case refractedRay of
                    Just refrRay -> return green -- colorRay refrRay
                    Nothing      -> return red -- total internal reflection



        {-
        then do -- Entering the object
                -- We always have a refracted ray entering the object.
            {-
            refrCol <- case intersectFirst (MkAnyI11e $ intObj int) (fromJust refractedRay) of
                Just int2 -> color int2 -- TODO ATTENUATION! NOT IN COLOR INTERSECTION!!
                Nothing   -> return black -- TODO possibly due to epsilon errors...
                -}
            refrCol <- colorRay (fromJust refractedRay)
                --Nothing   -> error "refracted ray could not escape!" -- DEBUG, TODO
                --Nothing   -> colorRay $ fromJust refractedRay
                        -- We couldn't escape from ourself! (ie we aren't a 
                        -- closed geometry) Look for other intersections.
            return $ r *. reflCol  .+.  (1 - r) *. refrCol
        else -- Exiting the object
            case refractedRay of
                Just refrRay -> colorRay refrRay >>= (\refrCol ->
                                return (r *. reflCol  .+.  (1 - r) *. refrCol))
                Nothing      -> return reflCol -- total internal reflection
                --TODO r == 1 in those two cases? (or return r *. reflCol)
        -}

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
        r0 = sq $ (n - 1) / (n + 1) -- reflectance at normal incidence 
                                    -- (invar under n <-> 1/n)
        --r = r0 + (1 - r0) * (1 - c)^5 -- Schlick's approx. to Fresnel's eq.
        r = 0.1 :: Flt -- XXX DEBUG TODO

-- | n is the ratio of the indices of refraction of the material being 
-- exited (as determined by the direction vector) to the index of 
-- refraction of the material being entered.
mkRefractedRay :: Intersection i -> Flt -> Maybe Ray
mkRefractedRay int n
    | cosSq < 0 = Nothing -- Total internal reflection
    | otherwise = Just refrRay
    where
        norm  = intNorm int
        dir   = normalize $ intDir int
        cosSq = 1  -  (sq n) * (1 - sq (dir .*. norm))
        refrDir = normalize $ n*.(dir .-. norm.*(dir .*. norm)) .-. norm.*(sqrt cosSq) -- TODO Normalized?
        refrRay = Ray (intPos int) refrDir epsilon infinity (intTotDist int)













data ColoredMaterial = ColMat Color AnyMat deriving Show -- ^ Modulate the material with the given color
type WeightedMaterial = (Flt, AnyMat)
type CombinedMaterial = [AnyMat]

instance Material ColoredMaterial where
    colorMaterial int (ColMat col mat) incidentLights =
        (col .***.) <$> colorMaterial int mat incidentLights

instance Material WeightedMaterial where
    colorMaterial int (weight, mat) incidentLights =
        (weight *.) <$> colorMaterial int mat incidentLights

instance Material CombinedMaterial where
    colorMaterial int materials incidentLights = do
        contributions <- mapM (\m -> colorMaterial int m incidentLights) materials
        return $ foldl' (.+.) black contributions


-- vim: expandtab smarttab sw=4 ts=4
