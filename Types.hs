{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}

module Types where

import Math


data MaterialType = Diffuse
        | Phong Flt      -- ^ Phong exponent
        | Reflecting
        | Refracting Flt -- ^ index of refraction
        deriving Show
data PureMaterial = PureMaterial MaterialType Color deriving Show
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

data CameraGaze = CameraGaze {
        cgPos  :: Point,
        cgDir  :: UnitVector,
        cgUp   :: UnitVector,
        cgFovy :: Flt -- ^ in degrees
    } deriving Show

data Camera = Camera {
        camPos  :: Point,
        camUVW  :: CoordSyst,
        camFovy :: Flt -- ^ in degrees
    } deriving Show

camFromCamGaze :: CameraGaze -> Camera
camFromCamGaze (CameraGaze pos dir up fovy) = Camera pos (u, v, w) fovy
    where
        w = (-1) *. dir
        u = normalize $ up .^. w
        v = w .^. u

camLookingAt :: Point -> Point -> UnitVector -> Flt -> Camera
camLookingAt pos lookAt up fovy =
    camFromCamGaze $ CameraGaze pos (direction pos lookAt) up fovy

type CoordSyst = (UnitVector, UnitVector, UnitVector)

data LightType = Directional Vector       -- ^ directional light, no attenuation
        | PointSource Point               -- ^ Pointsource position
        | Softbox Point Vector Vector Int -- ^ Softbox origin side1 side2 numRays
        deriving Show

data Light = Light {
        lightType  :: LightType,
        lightColor :: Color
    } deriving Show

-- | (direction pointing *to* the lightsource, color of incident light)
type IncidentLight = (UnitVector, Color)

data Scene = Scene {
        sLights :: [Light],
        sObjs   :: [Object]
    } deriving Show

-- | Note: this ordering only really makes sense for intersections of the same ray.
instance Ord Intersection where
    i1 <= i2  =  intDist i1 <= intDist i2
-- Prerequisite for Ord...
instance Eq Intersection where
    i1 == i2  =  intDist i1 == intDist i2


-- TODO: Pick proper convention (0 to res-1) or (1 to res) and check if 
-- everything complies with this!
newtype Pixel = Pixel (Int, Int) deriving Show

newtype Resolution = Resolution (Int, Int) deriving Show

-- | Lazy RGB triplet, components in the range [0..1].
data Color = Color Flt Flt Flt

instance (NumTuple Flt) Color where
    toList (Color r g b) = [r, g, b]
    fromList [r, g, b] = Color r g b
instance Show Color where
    show = showTuple -- TODO: specify this somehow at the level of NumTuple

black = Color 0 0 0
white = Color 1 1 1
red   = Color 1 0 0
green = Color 0 1 0
blue  = Color 0 0 1

flipHoriz :: Resolution -> Pixel -> Pixel
flipHoriz (Resolution (ni, nj)) (Pixel (i, j)) = Pixel (i, nj - j - 1)

-- vim: expandtab smarttab sw=4 ts=4
