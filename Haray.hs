{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

--module Haray where
module Haray (RayTraceConfig (..), RayTracer, rayTrace) where

import Types
import Math
import Renderer

import Data.Maybe
import System.Random

import Control.Monad.State
import Control.Applicative

import Prelude hiding (concatMap)
import Data.Foldable hiding (minimum, concat)

data RayTraceConfig = RayTraceConfig {
        confDepth :: Int,
        confSeed  :: Int,
        confRes   :: Resolution, -- TODO, this and cam are shared with rasterizer, not part of raytracer per se
        confCam   :: Camera
    } deriving Show

data RayTraceState = RayTraceState {
        stateScene  :: Scene,
        stateDepth  :: Int,
        stateRndGen :: StdGen, -- ^ Random number generator
        stateRes    :: Resolution,
        stateCam    :: Camera,
        stateMaxDepth :: Int
    } deriving Show

newtype RayTracer a = RT (State RayTraceState a)
    deriving (Monad, Functor)
getRes    = RT $ gets stateRes
getDepth  = RT $ gets stateDepth
decDepth  = getDepth >>= (\d -> RT $ modify (\s -> s {stateDepth = d - 1}))
getScene  = RT $ gets stateScene
getRndGen = RT $ gets stateRndGen
getCam    = RT $ gets stateCam
setRndGen new = RT $ modify (\s -> s {stateRndGen = new})
resetDepth = RT $ gets stateMaxDepth >>= \d -> modify (\s -> s {stateDepth = d})

instance (Renderer RayTraceConfig) RayTracer where
    colorPixel = rayTrace
    getResolution = getRes
    run scene conf (RT s) = evalState s (mkInitialState scene conf)

mkInitialState :: Scene -> RayTraceConfig -> RayTraceState
mkInitialState scene conf =
    RayTraceState scene depth rndGen res cam depth
    where
        depth = confDepth conf
        rndGen = mkStdGen $ confSeed conf
        res = confRes conf
        cam = confCam conf


-- | Return the default value if getDepth is not zero, or use the given 
-- 'recursion' and apply that after decrementing depth.
orRecurseOn :: a -> RayTracer a -> RayTracer a
defaultValue `orRecurseOn` recursion = do
    depth <- getDepth
    if depth <= 0
        then return defaultValue
        else decDepth >> recursion

getRandomR :: Random a => (a, a) -> RayTracer a
getRandomR range = do
    gen <- getRndGen
    let (rand, newGen) = randomR range gen
    setRndGen newGen
    return rand

getRandom :: Random a => RayTracer a
getRandom = do
    gen <- getRndGen
    let (rand, newGen) = random gen
    setRndGen newGen
    return rand

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
        (u, v, w) = camUVW cam
        top = tan ((camFovy cam) * pi / 360) -- theta/2
        right = top * nxFlt / nyFlt
        us = right * ((2*iFlt + 1)/nxFlt - 1)
        vs =  top  * ((2*jFlt + 1)/nyFlt - 1)
        dir = normalize $ us*.u .+. vs*.v .-. w -- ws = -n = -1


-- | Calculate the Color of the given Intersection
color :: Intersection -> RayTracer Color
color int =
--TODO STRICT//seq?
    foldlM (addWeightedPureColor int) black (intMat int)

addWeightedPureColor :: Intersection -> Color -> MaterialComponent -> RayTracer Color
addWeightedPureColor int col (MaterialComponent (weight, pureMat)) = do
    incidentLight <- incidentDirectLight int
    pureCol <- colorPure int incidentLight pureMat
    return $ col  .+.  weight *. pureCol


-- | Calculate the Color of a PureMaterial under the given light at a given 
-- Intersection
colorPure :: Intersection -> [IncidentLight] -> PureMaterial -> RayTracer Color
colorPure int incidentLights pureMat@(PureMaterial matType matCol) = do
    contributions <- mapM (colorMaterialType int matType) incidentLights
    let total = foldl' (.+.) black contributions
    return $ matCol .***. total 


colorMaterialType :: Intersection -> MaterialType -> IncidentLight -> RayTracer Color
colorMaterialType int Diffuse (ilDir, ilCol) =
    return $ ilCol .* (ilDir .*. (intNorm int))
colorMaterialType int (Phong p) (ilDir, ilCol) =
    return $ ilCol .* ((h .*. n) ** p)
    where
        n = intNorm int
        h = normalize $ ilDir .-. (intDir int)

colorMaterialType int Reflecting (ilDir, ilCol) =
    black `orRecurseOn` ((ilCol .***.) <$> (colorRay ray))
    where
        ray = Ray (intPos int) reflectedDir epsilon infinity
        reflectedDir = reflect (intDir int) (intNorm int)
    --TODO attenuation?


colorRay :: Ray -> RayTracer Color
colorRay ray = do
    scene <- getScene
    case (intersectFirst (sObjs scene) ray) of
        Nothing -> return black --(r,g,b)
        Just int -> color int


-- | Returns the incident light from the scene that's hitting the given 
-- intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight :: Intersection -> RayTracer [IncidentLight]
incidentDirectLight int = do
    scene <- getScene
    concat <$> mapM (incidentDirectLight1 int) (sLights scene)

-- | Returns the incident light from the given Light that's hitting the 
-- given intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight1 :: Intersection -> Light -> RayTracer [IncidentLight]
incidentDirectLight1 int light = do
    scene <- getScene
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


rayTrace :: Pixel -> RayTracer Color
rayTrace pixel = do
    res <- getRes -- TODO: getResolution from Renderer?
    cam <- getCam
    resetDepth
    (colorRay . (cameraRay res cam) . (flipHoriz res)) pixel


-- vim: expandtab smarttab sw=4 ts=4
