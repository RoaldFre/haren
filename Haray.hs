module Haray where
--module Haray (raytrace, testImage, test) where

import Types
import Math
import Image

-- import Data.List
import Data.Maybe
import System.Random

import Control.Monad.State
import Control.Applicative

import Prelude hiding (concatMap)
import Data.Foldable hiding (minimum, concat)


{-
newtype RTImage = RTImage (Pixel -> RT Color)
unwrapRTColor :: (RT Color) -> Color
unwrapRTColor rtsCol = evalState rtsCol state aaaaaaaaaaargh
-}


-- | Inject the default value into the state if stateDepth is not zero, 
-- or use the given 'recursion' and apply that on a state with decremented 
-- depth.
orRecurseOn :: a -> RT a -> RT a
defaultValue `orRecurseOn` recursion = do
    depth <- gets stateDepth
    if depth <= 0
        then return defaultValue
        else modify (\s -> s {stateDepth = depth - 1}) >> recursion

getRandomR :: Random a => (a, a) -> RT a
getRandomR range = do
    gen <- gets stateRndGen
    let (rand, newGen) = randomR range gen
    modify (\s -> s {stateRndGen = newGen})
    return rand

getRandom :: Random a => RT a
getRandom = do
    gen <- gets stateRndGen
    let (rand, newGen) = random gen
    modify (\s -> s {stateRndGen = newGen})
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
color :: Intersection -> RT Color
color int =
--TODO STRICT//seq?
    foldlM (addWeightedPureColor int) black (intMat int)

addWeightedPureColor :: Intersection -> Color -> MaterialComponent -> RT Color
addWeightedPureColor int col (MaterialComponent (weight, pureMat)) = do
    incidentLight <- incidentDirectLight int
    pureCol <- colorPure int incidentLight pureMat
    return $ col  .+.  weight *. pureCol


-- | Calculate the Color of a PureMaterial under the given light at a given 
-- Intersection
colorPure :: Intersection -> [IncidentLight] -> PureMaterial -> RT Color
colorPure int incidentLights pureMat@(PureMaterial matType matCol) = do
    contributions <- mapM (colorMaterialType int matType) incidentLights
    let total = foldl' (.+.) black contributions
    return $ matCol .***. total 


colorMaterialType :: Intersection -> MaterialType -> IncidentLight -> RT Color
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


colorRay :: Ray -> RT Color
colorRay ray = do
    r <- getRandom
    g <- getRandom
    b <- getRandom

    (.+. ((r,g,b) .* 1)) <$> color (Intersection (0.1,0.1,9) 9 (0.1,0.1,1) (0.1,0.2,-1) mat)
        where mat = [MaterialComponent (0.1, PureMaterial Diffuse red)]
        
{-
        intPos  :: Point,       -- ^ Position of the intersection
        intDist :: Flt,         -- ^ Distance of intersecting ray
        intDir  :: UnitVector,  -- ^ Direction of intersecting ray
        intNorm :: UnitVector,  -- ^ Normal vector of intersection surface
        intMat  :: Material     -- ^ Material of intersection surface
 
    scene <- gets stateScene
    case (intersectFirst (sObjs scene) ray) of
        Nothing -> return black --(r,g,b)
        Just int -> (.+. ((r,g,b) .* 1)) <$> color int
-}


-- | Returns the incident light from the scene that's hitting the given 
-- intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight :: Intersection -> RT [IncidentLight]
incidentDirectLight int = do
    scene <- gets stateScene
    concat <$> mapM (incidentDirectLight1 int) (sLights scene)

-- | Returns the incident light from the given Light that's hitting the 
-- given intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight1 :: Intersection -> Light -> RT [IncidentLight]
incidentDirectLight1 int light = do
    scene <- gets stateScene
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

raytrace :: RaytraceConfig -> Resolution -> Camera -> Scene -> Pixel -> RT Color
raytrace conf res cam scene pixel =
    colorRay $ (cameraRay res cam) $ (flipHoriz res) pixel

   --     depth = maxIter conf

        --state = RayTraceState scene depth (mkStdGen 0)


testImage = Image res $ raytrace conf res cam scene
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
        state = RayTraceState scene 5 (mkStdGen 0)


testu :: Flt -> Color
testu factor = (evalState test2 state) .* factor
    where state = RayTraceState (Scene [] []) 1 (mkStdGen 0)


test :: RT (Double, Double, Double)
test = do
    r <- getRandom
    g <- getRandom
    b <- getRandom
    return (r,g,b)

test2 :: RT Color
test2 = test >> test

-- vim: expandtab smarttab sw=4 ts=4
