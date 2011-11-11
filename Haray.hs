 -- TODO: separate types in other file, or make huge explicit export list 
 -- so we hide internal stuff
module Haray where

import Math
import Image

import Data.List
import Data.Maybe


data MaterialType = Diffuse | Phong Flt deriving Show
data PureMaterial = PureMaterial MaterialType Color deriving Show
-- | [(weight, pureMaterial)]
newtype MaterialComponent = MaterialComponent (Flt, PureMaterial) deriving Show
type Material = [MaterialComponent]

data Geometry = Sphere Flt Point
    | Triangle Point Point Point
        deriving Show

data Object = Object Geometry Material deriving Show

data Ray = Ray {
        rayOrigin :: Point,
        rayDir    :: UnitVector,
        rayNear   :: Flt, -- ^ near clipping distance
        rayFar    :: Flt  -- ^ far clipping distance
    } deriving Show

data Intersection = Intersection {
        intPos  :: Point,       -- ^ Position of the intersection
        intDist :: Flt,         -- ^ Distance of intersecting ray
        intDir  :: UnitVector,  -- ^ Direction of intersecting ray
        intNorm :: UnitVector,  -- ^ Normal vector of intersection surface
        intMat  :: Material     -- ^ Material of intersection surface
    } deriving Show

data Camera = Camera {
    --TODO: u,v,w instead of pos, dir, up?
        camPos  :: UnitVector,
        camDir  :: UnitVector,
        camUp   :: UnitVector,
        camFovy :: Flt -- ^ in degrees
    } deriving Show

type CoordSyst = (UnitVector, UnitVector, UnitVector)

data LightType = Directional Vector       -- ^ directional light, no attenuation
        | PointSource Point               -- ^ Pointsource position
        | Softbox Point Vector Vector Int -- ^ Softbox origin side1 side2 numRays
        deriving Show

data Light = Light {
        lightType  :: LightType,
        lightColor :: Color
    } deriving Show

-- | *Inverse* direction and color of lightray incident on a point of a 
-- surface.
type IncidentLight = (UnitVector, Color)

data Scene = Scene {
        sLights :: [Light],
        sObjs   :: [Object]
    }


-- | Note: this ordering only really makes sense for intersections of the same ray.
instance Ord Intersection where
    i1 <= i2  =  intDist i1 <= intDist i2
-- Prerequisite for Ord...
instance Eq Intersection where
    i1 == i2  =  intDist i1 == intDist i2

walk :: Ray -> Flt -> Point
walk ray dist = (rayOrigin ray) .+. (rayDir ray) .* dist

makeIntersection :: Ray -> Flt -> UnitVector -> Material -> Intersection
makeIntersection ray dist normal mat = 
    Intersection (walk ray dist) dist (rayDir ray) normal mat

intersectFirst :: [Object] -> Ray -> Maybe Intersection
intersectFirst objs r =
    case [i | i <-  concatMap (intersectWith r) objs] of
        [] -> Nothing
        ints  -> Just (minimum ints)

intersectWith :: Ray -> Object -> [Intersection]
intersectWith ray@(Ray e d min max) (Object (Sphere r c) mat) =
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
        ray = Ray lightPos direction epsilon distance
-- TODO: Softbox as concatmap over random pointsources.

-- | Propagate the given ray from the given light through the scene. Return 
-- the resulting incident light of the lightray, or Nothing if it is 
-- blocked.
propagateShadowRay :: [Object] -> Light -> Ray -> Maybe IncidentLight
propagateShadowRay objs light ray =
    case intersectFirst objs ray of
        Just _  -> Nothing
        Nothing -> Just ((rayDir ray), color)
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
        cam = Camera zero e3 e2 30
        geom1 = Sphere 1 (0,0,10)
        geom2 = Sphere 1 (0,2,20)
        mat = [MaterialComponent (1, PureMaterial Diffuse white)]
        objs = [Object geom1 mat, Object geom2 mat]
        res = Resolution (200,200)
        lights = [Light (PointSource (0,0,0)) (white)]
        scene = Scene lights objs


-- vim: expandtab smarttab sw=4 ts=4
