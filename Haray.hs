 -- TODO: separate types in other file, or make huge explicit export list 
 -- so we hide internal stuff
module Haray where
--module Haray (raytrace, testImage, test) where

import Types
import Math
import Image

-- import Data.List
import Data.Maybe

import Control.Monad.State
import Control.Applicative

import Prelude hiding (concatMap)
import Data.Foldable hiding (minimum, concat)

data RaytraceConfig = RaytraceConfig {
        maxIter :: Int
    } deriving Show

data RaytraceState = RaytraceState {
        rayTrStScene :: Scene,
        rayTrStDepth :: Int
    } deriving Show

type RTState a = State RaytraceState a

orRecurseOn :: a -> RTState a -> RTState a
defaultValue `orRecurseOn` recursion = do
    depth <- gets rayTrStDepth
    if depth <= 0
        then return defaultValue
        else do
            modify (\s -> s {rayTrStDepth = depth - 1})
            recursion

walk :: Ray -> Flt -> Point
walk ray dist = (rayOrigin ray) .+. (rayDir ray) .* dist

makeIntersection :: Ray -> Flt -> UnitVector -> Material -> Intersection
makeIntersection ray dist normal mat = 
    Intersection (walk ray dist) dist (rayDir ray) normal mat

intersectFirst :: [Object] -> Ray -> Maybe Intersection
intersectFirst objs ray =
    case intersectWith ray objs of
        []   -> Nothing
        ints -> Just (minimum ints)

intersectWith :: Ray -> [Object] -> [Intersection]
intersectWith ray objs = concatMap (intersectWith1 ray) objs

intersectWith1 :: Ray -> Object -> [Intersection]
intersectWith1 ray@(Ray e d min max) (Object (Sphere r c) mat) =
    [makeIntersection ray t (sphereNormal t) mat | t <- ts, min < t, t < max]
    where
        ts = solveQuadEq
                (d .*. d)
                (2 *. d .*. (e .-. c))
                ((e .-. c).^2 - r^2)
        sphereNormal t = (e .+. t*.d .-. c) ./ r


cameraSystem :: Camera -> CoordSyst
cameraSystem cam = (u, v, w)
    where
        w = (-1) *. (camDir cam)
        u = normalize $ (camUp cam) .^. w
        v = w .^. u

cameraRay :: Resolution -> Camera -> Pixel -> Ray
cameraRay (Resolution (nx, ny)) cam (Pixel (i, j)) =
    Ray (camPos cam) dir 0 infinity
    where
        -- See p164 and 203-204 of Fundamentals of Computer 
        -- Graphics (Peter Shirley, 2nd ed) for drawing and info.
        -- We choose n = 1.
        nxFlt = fromIntegral ny
        nyFlt = fromIntegral nx
        iFlt = fromIntegral i
        jFlt = fromIntegral j
        (u, v, w) = cameraSystem cam
        top = tan ((camFovy cam) * pi / 360) -- theta/2
        right = top * nxFlt / nyFlt
        us = right * ((2*iFlt + 1)/nxFlt - 1)
        vs =  top  * ((2*jFlt + 1)/nyFlt - 1)
        dir = normalize $ us*.u .+. vs*.v .-. w -- ws = -n = -1


-- | Calculate the Color of the given Intersection
color :: Intersection -> RTState Color
color int =
--TODO STRICT?
    foldlM (addWeightedPureColor int) black (intMat int)

addWeightedPureColor :: Intersection -> Color -> MaterialComponent -> RTState Color
addWeightedPureColor int col (MaterialComponent (weight, pureMat)) = do
    incidentLight <- incidentDirectLight int
    pureCol <- colorPure int incidentLight pureMat
    return $ col  .+.  weight *. pureCol


-- | Calculate the Color of a PureMaterial under the given light at a given 
-- Intersection
colorPure :: Intersection -> [IncidentLight] -> PureMaterial -> RTState Color
colorPure int incidentLights pureMat@(PureMaterial matType matCol) = do
--TODO: this is messy hack to get it to compile
    contributions <- mapM (colorMaterialType int matType) incidentLights
    let total = foldl' (.+.) black contributions
    return $ matCol .***. total 

{-
    return $ matCol .***. (foldl' (.+.) black $
                        map (colorMaterialType int matType) incidentLights)
-}

colorMaterialType :: Intersection -> MaterialType -> IncidentLight -> RTState Color
colorMaterialType int Diffuse (ilDir, ilCol) =
    return $ ilCol .* (ilDir .*. (intNorm int))
colorMaterialType int (Phong p) (ilDir, ilCol) =
    return $ ilCol .* ((h .*. n) ** p)
    where
        n = intNorm int
        h = normalize $ ilDir .-. (intDir int)
{- Alternative (... with a branch instruction TODO: benchmark):
    (max 0 ((refl .*. ilDir) ** p)) *. ilCol
    where refl = reflect (intDir int) (intNorm int)
-}
colorMaterialType int Reflecting (ilDir, ilCol) =
    black `orRecurseOn` ((ilCol .***.) <$> (colorRay ray))
    where
        ray = Ray (intPos int) reflectedDir epsilon infinity
        reflectedDir = reflect (intDir int) (intNorm int)
    --TODO attenuation?



colorRay :: Ray -> RTState Color
colorRay ray = do
    scene <- gets rayTrStScene
    case (intersectFirst (sObjs scene) ray) of
        Nothing -> return black
        Just int -> color int


-- | Returns the incident light from the scene that's hitting the given 
-- intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight :: Intersection -> RTState [IncidentLight]
incidentDirectLight int = do
    scene <- gets rayTrStScene
    concat <$> mapM (incidentDirectLight1 int) (sLights scene)

-- | Returns the incident light from the given Light that's hitting the 
-- given intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight1 :: Intersection -> Light -> RTState [IncidentLight]
incidentDirectLight1 int light = do
    scene <- gets rayTrStScene
    return $ mapMaybe (propagateShadowRay (sObjs scene) light) shadowRays
    where
        allRays = spawnShadowRays light (intPos int)
        shadowRays = filter correctSide allRays
        correctSide ray = (rayDir ray) .*. (intNorm int) > 0
    
-- | Spawn Rays from the given Point to the given light.
spawnShadowRays :: Light -> Point -> [Ray]
spawnShadowRays (Light lightType _) point = spawnShadowRaysFromType lightType point

spawnShadowRaysFromType :: LightType -> Point -> [Ray]
spawnShadowRaysFromType (PointSource lightPos) point = [ray]
    where
        diff = lightPos .-. point
        distance = len diff
        direction = diff ./ distance
        ray = Ray point direction epsilon distance
{-
spawnShadowRaysFromType (Softbox lightPos) point =
    concat [spawnShadowRaysFromType (PointSource pos) point | pos <- positions]
    where
        positions = 
        --| Softbox Point Vector Vector Int -- ^ Softbox origin side1 side2 numRays
-}
        
-- TODO: Softbox as concatmap over random pointsources.

-- | Propagate the given ray from the given light through the scene. Return 
-- the resulting incident light of the lightray, or Nothing if it is 
-- blocked.
propagateShadowRay :: [Object] -> Light -> Ray -> Maybe IncidentLight
propagateShadowRay objs light ray =
    case (intersectWith ray objs) of
        [] -> Just ((rayDir ray), color)
        _  -> Nothing
    where
        color = (lightColor light) .* (attenuation (lightType light) ray)

attenuation :: LightType -> Ray -> Flt
attenuation (Directional _) _ = 1
attenuation _             ray = 1 -- / (rayFar ray)^2 -- 1/(((rayFar ray) * (rayFar ray)) faster?

raytrace :: RaytraceConfig -> Resolution -> Camera -> Scene -> Image
raytrace conf res cam scene = Image res map
    where
        map pixel = ((evalState . colorRay) . (cameraRay res cam) . (flipHoriz res)) pixel state
        depth = maxIter conf
        state = RaytraceState scene depth


{-
raytrace :: RaytraceConfig -> Resolution -> Camera -> Scene -> Image
raytrace conf res cam scene = Image res map
    where
        map = (colorRay depth scene) . (cameraRay res cam) . (flipHoriz res)
        depth = maxIter conf
-}


testImage = raytrace conf res cam scene
    where
        cam = camLookingAt (0,15,0) (0,0,10) e2 20
        geom1 = Sphere 1.0 (   0, 0, 10)
        geom2 = Sphere 1.0 (-1.1, 0, 12)
        geom3 = Sphere 1.0 ( 1.1, 0, 12)
        mc1 = MaterialComponent (0.1, PureMaterial Diffuse red)
        mc2 =  MaterialComponent (1, PureMaterial (Phong 50) blue)
        mc3 =  MaterialComponent (1, PureMaterial Reflecting $ 0.8 *. white)
        mat = [mc1, mc2, mc3]
        objs = [Object geom1 mat, Object geom2 mat, Object geom3 mat]
        res = Resolution (600, 600)
        lights = [Light (PointSource (10,10,10)) (white)
                 ,Light (PointSource (-10,10,10)) (0.5 *. white)]
        scene = Scene lights objs
        conf = RaytraceConfig 5

-- vim: expandtab smarttab sw=4 ts=4
