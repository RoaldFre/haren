{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, DeriveFunctor, ExistentialQuantification #-}

module Types where

import Math
import Data.List hiding (intersect)


data MaterialType = Diffuse
        | Phong Flt      -- ^ Phong exponent
        | Reflecting
        | Refracting Flt -- ^ index of refraction
        deriving Show
data PureMaterial = PureMaterial MaterialType Color deriving Show
newtype MaterialComponent = MaterialComponent (Flt, PureMaterial) deriving Show
type Material = [MaterialComponent]




data Ray = Ray {
        rayOrigin :: Pt3,
        rayDir    :: Vec3, -- ^ *NOT* neccesarily normalised (eg for transformed rays)
        rayNear   :: Flt,  -- ^ near clipping distance
        rayFar    :: Flt   -- ^ far clipping distance
    } deriving Show

data Intersection = Intersection {
        intPos  :: Pt3,     -- ^ Position of the intersection
        intDist :: Flt,     -- ^ Distance of intersecting ray
        intDir  :: Vec3,    -- ^ Direction of intersecting ray, *NOT* normalised
        intNorm :: UVec3,   -- ^ Normal vector of intersection surface, normalised
	-- Note to self: Keep this as the last element to use some curry 
	-- tricks. TODO clean?
        intMat  :: Material -- ^ Material of intersection surface
    } deriving Show

-- | Note: this ordering only really makes sense for intersections of the same ray.
instance Ord Intersection where
    i1 <= i2  =  intDist i1 <= intDist i2
-- Prerequisite for Ord...
instance Eq Intersection where
    i1 == i2  =  intDist i1 == intDist i2





class Geometry a where
    boundingBox :: a -> Box
    intersect :: a -> Ray -> [Material -> Intersection]

-- Existential geometry
data AnyGeom = forall a . Geometry a => MkAnyGeom a
instance Geometry AnyGeom where
    boundingBox (MkAnyGeom g) = boundingBox g
    intersect (MkAnyGeom g) = intersect g
instance Show AnyGeom where
    show (MkAnyGeom geom) = "AnyGeom TODO" -- TODO


-- | Sphere with radius 1 at origin
data Sphere = Sphere



instance Geometry Sphere where
    boundingBox Sphere = Box (F3 (-1) (-1) (-1)) (F3 1 1 1)
    intersect Sphere ray@(Ray e d min max) =
        [makeIntersection ray t (sphereNormal t) | t <- ts, min < t, t < max]
        where
            ts = solveQuadEq
                    (d .*. d)
                    (2 *. d .*. e)
                    (e.^2 - 1)
            sphereNormal t = (e .+. t*.d)

solveQuadEq :: Flt -> Flt -> Flt -> [Flt]
solveQuadEq a b c
    | d < 0     = []
    | d > 0     = [(-b - sqrt(d))/(2*a), (-b + sqrt(d))/(2*a)]
    | otherwise = [-b/(2*a)]
    where
        d = b^2 - 4*a*c

walk :: Ray -> Flt -> Pt3
walk ray dist = (rayOrigin ray) .+. (rayDir ray) .* dist

makeIntersection :: Ray -> Flt -> UVec3 -> Material -> Intersection
makeIntersection ray dist normal mat = 
    Intersection (walk ray dist) dist (rayDir ray) normal mat

data Vertex = Vertex {
        vPos  :: !Pt3,
        vNorm :: !UVec3
    } deriving Show

data Triangle = Triangle !Vertex !Vertex !Vertex  -- ^ Triangle with the given vertices
        deriving Show
data TriangleMesh = TriangleMesh [Triangle]
        deriving Show



data Object = Object AnyGeom Material deriving Show

--TODO define all box stuf only for raytracer? But we make Geometry 
--implement Boxable, and geometry is defined here, so that's not *that* 
--nice imo

-- | Axis aligned box. -- TODO: strict in points?
data Box = Box Pt3 Pt3 deriving Show

class Boxable a where
    box :: a -> Box -- ^ Surrounding box for 'a'

instance Boxable Box where
    box = id

instance Boxable Pt3 where
    box p = Box p p

instance (Boxable a, Boxable b) => Boxable (a, b) where
    box (a, b) = Box min max
     where
        Box min1 max1 = box a
        Box min2 max2 = box b
        min = tupleFromList $ zipWith minim (tupleToList min1) (tupleToList min2)
        max = tupleFromList $ zipWith maxim (tupleToList max1) (tupleToList max2)
        minim a b = if a < b then a else b
        maxim a b = if a > b then a else b

instance (Boxable a) => Boxable [a] where
    box [] = error "Can't box an empty list of boxables!"
    box boxables = foldl1' (\b1 b2 -> box (b1, b2)) (map box boxables)

instance Boxable AnyGeom where
    box (MkAnyGeom geom) = boundingBox geom

instance Boxable Object where
    box (Object geom _) = box geom



data CameraGaze = CameraGaze {
        cgPos  :: Pt3,
        cgDir  :: UVec3,
        cgUp   :: UVec3,
        cgFovy :: Flt -- ^ in degrees
    } deriving Show

data Camera = Camera {
        camPos  :: Pt3,
        camUVW  :: CoordSyst,
        camFovy :: Flt -- ^ in degrees
    } deriving Show

camFromCamGaze :: CameraGaze -> Camera
camFromCamGaze (CameraGaze pos dir up fovy) = Camera pos (u, v, w) fovy
    where
        w = (-1::Flt) .*. dir
        u = normalize $ up .^. w
        v = w .^. u

camLookingAt :: Pt3 -> Pt3 -> UVec3 -> Flt -> Camera
camLookingAt pos lookAt up fovy =
    camFromCamGaze $ CameraGaze pos (direction pos lookAt) up fovy

type CoordSyst = (UVec3, UVec3, UVec3)

data LightType = Directional Vec3   -- ^ directional light, no attenuation
        | PointSource Pt3           -- ^ Pointsource position
        | Softbox Pt3 Vec3 Vec3 Int -- ^ Softbox origin side1 side2 numRays
        deriving Show

data Light = Light {
        lightType  :: LightType,
        lightColor :: Color
    } deriving Show

data ObjectGraph a = Node a (ObjectGraph a)
                   | Fork [ObjectGraph a]
                   | Leaf Object
                   deriving (Show, Functor)
{-
instance Functor ObjectGraph where
    fmap f (Leaf obj) = Leaf obj
    fmap f (Fork graphs) = Fork $ map (fmap f) graphs
    fmap f (Node x graph) = Node (f x) (f `fmap` graph)
-}

-- | Flatten or 'fold' the object graph starting with the given initial 
-- value and proceeding with the given folding function. If the graph 
-- contains loops, the resulting list will be infinite.
-- The first argument given to the supplied function will be the object 
-- that sits closest to the root in the tree, the second argument will be 
-- its child.
flattenObjectGraph :: (a -> a -> a) -> a -> ObjectGraph a -> [(a, Object)]
flattenObjectGraph f initial (Leaf obj) = [(initial, obj)]
flattenObjectGraph f initial (Fork graphs) =
        concatMap (flattenObjectGraph f initial) graphs
flattenObjectGraph f initial (Node x subGraph) =
        flattenObjectGraph f (f initial x) subGraph

type SceneGraph = ObjectGraph Transformation

data Transformation = Translation Vec3
                    | Rotation Vec3 Flt
                    | Scale Flt Flt Flt
                    deriving Show

-- | Returns matrices for the normal and inverse transformation.
transfoM4s :: Transformation -> (M4, M4)
transfoM4s (Translation v)       = trans3M4s v
transfoM4s (Rotation axis angle) = rotM4s axis angle
transfoM4s (Scale x y z)         = scalexyzM4s x y z

data Scene = Scene {
        sLights :: [Light],
        sGraph  :: SceneGraph
    } deriving Show



-- TODO: Pick proper convention (0 to res-1) or (1 to res) and check if 
-- everything complies with this!
newtype Pixel = Pixel (Int, Int) deriving Show

newtype Resolution = Resolution (Int, Int) deriving Show

-- | Lazy RGB triplet, components in the range [0..1].
data Color = Color Flt Flt Flt

instance (NumTuple Flt) Color where
    tupleToList (Color r g b) = [r, g, b]
    tupleFromList [r, g, b] = Color r g b
instance Show Color where
    show = showTuple -- TODO: specify this somehow at the level of NumTuple
instance Mult Color Color Flt where
    (.*.) = dot -- TODO: this is a bit contrived and probably not really needed
instance Mult Color Flt Color where (.*.) = (.*)
instance Mult Flt Color Color where (.*.) = (*.)
instance Div Color Flt
instance Add Color where (.+.) = addt
instance Sub Color where (.-.) = subt

black = Color 0 0 0
white = Color 1 1 1
red   = Color 1 0 0
green = Color 0 1 0
blue  = Color 0 0 1

flipHoriz :: Resolution -> Pixel -> Pixel
flipHoriz (Resolution (ni, nj)) (Pixel (i, j)) = Pixel (i, nj - j - 1)

-- vim: expandtab smarttab sw=4 ts=4
