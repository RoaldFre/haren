module Types where

import Math
import Image

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

