 -- TODO: separate types in other file, or make huge explicit export list 
 -- so we hide internal stuff
module Haray where
--module Haray (raytrace, testImage, test) where

import Types
import Math
import Image

import Data.List
import Data.Maybe

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
color :: Scene -> Intersection -> Color
color scene int =
    foldl' addWeightedPureColor black (intMat int)
    where
        addWeightedPureColor col (MaterialComponent (weight, pureMat)) =
            col .+. weight *. (colorPure int incidentLight pureMat)
        incidentLight = incidentDirectLight scene int

-- | Calculate the Color of a PureMaterial under the given light at a given 
-- Intersection
colorPure :: Intersection -> [IncidentLight] -> PureMaterial -> Color
colorPure int incidentLights pureMat =
    foldl' (.+.) black $ map (colorPure1 int pureMat) incidentLights

colorPure1 :: Intersection -> PureMaterial -> IncidentLight -> Color
colorPure1 int (PureMaterial Diffuse matCol) (ilDir, ilCol) =
    (ilDir) .*. (intNorm int) *. ilCol


colorRay :: Scene -> Ray -> Color
colorRay scene ray =
    case (intersectFirst (sObjs scene) ray) of
        Nothing -> black
        Just int -> color scene int


-- | Returns the incident light from the scene that's hitting the given 
-- intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight :: Scene  -> Intersection -> [IncidentLight]
incidentDirectLight scene int =
    concatMap (incidentDirectLight1 scene int) $ sLights scene

-- | Returns the incident light from the given Light that's hitting the 
-- given intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight1 :: Scene  -> Intersection -> Light -> [IncidentLight]
incidentDirectLight1 scene int light = 
    mapMaybe (propagateShadowRay (sObjs scene) light) shadowRays
    where
        allRays = spawnShadowRays light (intPos int)
        shadowRays = filter correctSide allRays
        correctSide ray = (rayDir ray) .*. (intNorm int) > 0
    
-- | Spawn Rays from the given Point to the given light.
spawnShadowRays :: Light -> Point -> [Ray]
spawnShadowRays (Light (PointSource lightPos) _) point = [ray]
    where
        diff = lightPos .-. point
        distance = len diff
        direction = diff ./ distance
        ray = Ray point direction epsilon distance
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
attenuation _             ray = 1 -- / (rayFar ray)^2
    

raytrace :: Resolution -> Camera -> Scene -> Image
raytrace res cam scene = Image res map
    where map = (colorRay scene) . (cameraRay res cam) . (flipHoriz res)


testImage = raytrace res cam scene
    where
        cam = Camera zero e3 e2 20
        geom1 = Sphere 1.0 (0,0,10)
        geom2 = Sphere 1.0 (-1,2,20)
        geom3 = Sphere 0.4 (1,1,9)
        mat = [MaterialComponent (1, PureMaterial Diffuse white)]
        objs = [Object geom1 mat, Object geom2 mat, Object geom3 mat]
        res = Resolution (300,300)
        lights = [Light (PointSource (10,10,0)) (white)]
        scene = Scene lights objs

-- vim: expandtab smarttab sw=4 ts=4
