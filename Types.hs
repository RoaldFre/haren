{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, DeriveFunctor, ExistentialQuantification #-}

module Types where

import Math
import Data.List hiding (intersect)
import Data.Ix
import qualified Data.Foldable as F
import qualified Data.Sequence as S


type Tex = (Pt2 -> Color)
instance Show Tex where
    show _ = "<<Texture>>"

data MaterialType = Diffuse
        | Phong Flt      -- ^ Phong exponent
        | Reflecting
        | Glossy Flt Int -- ^ Glossiness and number of samples
        | Refracting Flt -- ^ index of refraction
        | Texture Tex -- ^ function of (u,v) to colors, u and v in [0,1]
        deriving Show
data PureMaterial = PureMaterial MaterialType Color deriving Show
newtype MaterialComponent = MaterialComponent (Flt, PureMaterial) deriving Show
type Material = [MaterialComponent]

checkers :: Color -> Color -> Int -> Int -> Tex
checkers c1 c2 nu nv (F2 u v)
    | uParity + vParity == 1  =  c1
    | otherwise               =  c2
    where
        uParity = (floor $ (fromIntegral nu) * u) `mod` 2
        vParity = (floor $ (fromIntegral nv) * v) `mod` 2


data Ray = Ray {
        rayOrigin :: !Pt3,
        rayDir    :: !Vec3, -- ^ *NOT* neccesarily normalised (eg for transformed rays)
        rayNear   :: !Flt,  -- ^ near clipping distance
        rayFar    :: !Flt,  -- ^ far clipping distance
        rayDist   :: !Flt   -- ^ Total distance traversed by the light before it became 
                            --   this ray. Eg if this ray is a reflected 
                            --   ray, the distance here is the distance of 
                            --   the original ray before it reflected and 
                            --   became this ray.
    } deriving Show

-- TODO strict?
data Intersection = Intersection {
        intPos     :: Pt3,       -- ^ Position of the intersection
        intDist    :: Flt,       -- ^ Distance of the (last) intersecting ray
        intTotDist :: Flt,       -- ^ Total accumulated distance traveled by rays till we got here
        intDir     :: Vec3,      -- ^ Direction of intersecting ray, *NOT* normalised
        intNorm    :: UVec3,     -- ^ Normal vector of intersection surface, normalised
        intTexUV   :: Maybe Pt2, -- ^ Texture coordinate
    -- Note to self: Keep intMat this as the last element to use some curry 
    -- tricks. TODO nicer alternative?
        intMat  :: Material -- ^ Material of intersection surface
    } deriving Show

-- | Note: this ordering only really makes sense for intersections of the same ray.
instance Ord Intersection where
    i1 <= i2  =  intDist i1 <= intDist i2
-- Prerequisite for Ord...
instance Eq Intersection where
    i1 == i2  =  intDist i1 == intDist i2

makeIntersection :: Ray -> Flt -> UVec3 -> Maybe Pt2 -> Material -> Intersection
makeIntersection ray dist normal texcoord mat = 
    Intersection (walk ray dist) dist (dist + (rayDist ray)) (rayDir ray) normal texcoord mat



class Geometry a where
    boundingBox :: a -> Box
    intersectGeom :: a -> Ray -> [Material -> Intersection]

-- Existential geometry
data AnyGeom = forall a . (Geometry a, Show a) => MkAnyGeom a
instance Geometry AnyGeom where
    boundingBox (MkAnyGeom g) = boundingBox g
    intersectGeom (MkAnyGeom g) = intersectGeom g
instance Show AnyGeom where
    show (MkAnyGeom geom) = "AnyGeom " ++ show geom


-- | Sphere with radius 1 at origin
data Sphere = Sphere deriving Show

instance Geometry Sphere where
    boundingBox Sphere = Box (F3 (-1) (-1) (-1)) (F3 1 1 1)
    intersectGeom Sphere ray@(Ray e d min max _) =
        [makeIntersection ray t (sphereNormal t) Nothing | t <- ts, min < t, t < max]
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

data Vertex = Vertex {
        vPos  :: !Pt3,
        vNorm :: !UVec3
    } deriving Show

data Triangle = Triangle !Vertex !Vertex !Vertex  -- ^ Triangle with the given vertices
        deriving Show

instance Geometry Triangle where
    boundingBox (Triangle v1 v2 v3) = box $ map vPos [v1, v2, v3]
{-
    intersectGeom (Triangle v1 v2 v3) ray@(Ray origin dir min max)
        -- | See pp206-208 of Fundamentals of Computer Graphics (Peter 
        -- Shirley, 2nd edition) for algorithm.
--        | abs m < smallest              = [] -- TODO: use epsilon to do relative compare?
        | t < min   || t > max          = []
        | gamma < 0 || gamma > 1        = []
        | beta < 0  || beta > 1 - gamma = []
        | otherwise                     = [makeIntersection ray t n]
        where
            t     = -(f*(a*k - j*b) + e*(j*c - a*l) + d*(b*l - k*c)) / m
            beta  =  (j*(e*i - h*f) + k*(g*f - d*i) + l*(d*h - e*g)) / m
            gamma =  (i*(a*k - j*b) + h*(j*c - a*l) + g*(b*l - k*c)) / m
            m     =   a*(e*i - h*f) + b*(g*f - d*i) + c*(d*h - e*g)
            F3 a b c = (vPos v1) .-. (vPos v2) -- 1st basis vector, for beta
            F3 d e f = (vPos v1) .-. (vPos v3) -- 2nd basis vector, for gamma
            F3 g h i = dir                     -- ray direction vector
            F3 j k l = (vPos v1) .-. origin    -- vector to travel
            alpha = 1 - beta - gamma
            n = normalize $
                alpha*.(vNorm v1) .+. beta*.(vNorm v2) .+. gamma*.(vNorm v3)
-}
--TODO: this code is too slow: :-(
--{-
    intersectGeom (Triangle v1 v2 v3) ray@(Ray e d min max _)
        -- | See pp206-208 of Fundamentals of Computer Graphics (Peter 
        -- Shirley, 2nd edition) for algorithm.
--        | abs m < smallest              = [] -- TODO: use epsilon to do relative compare?
        | t < min   || t > max          = []
        | gamma < 0 || gamma > 1        = []
        | beta < 0  || beta > 1 - gamma = []
        | otherwise                     = [makeIntersection ray t n Nothing]
        where
            -- e + t*d = o + beta*(-d1) + gamma*(-d2)
            F3 t beta gamma = solve3x3 (M3 d d1 d2) diff
            d1 = (vPos v1) .-. (vPos v2) -- (inverse of) 1st basis vector, for beta
            d2 = (vPos v1) .-. (vPos v3) -- (inverse of) 2nd basis vector, for gamma
            diff = (vPos v1) .-. e -- vector to travel
            alpha = 1 - beta - gamma
            n = normalize $
                alpha*.(vNorm v1) .+. beta*.(vNorm v2) .+. gamma*.(vNorm v3)
---}

data TriangleMesh = TriangleMesh [Triangle] deriving Show

--instance Geometry TriangleMesh where
--only use accelerated structure for boundingBox and intersect, still make 
--it a geometry later on for rasterizing, though. <- TODO

-- | A two-sided plane.
-- Plane origin direction1 direction2 normal
data Plane = Plane Pt3 Vec3 Vec3 UVec3 deriving Show
mkPlane :: Pt3 -> Vec3 -> Vec3 -> Plane
mkPlane origin dir1 dir2 = Plane origin dir1 dir2 $ normalize $ dir1 .^. dir2
       
instance Geometry Plane where
    boundingBox (Plane o d1 d2 _) = box [o, o.+.d1, o.+.d2, o.+.d1.+.d2]
    intersectGeom (Plane o d1 d2 normal) ray@(Ray e d min max _)
        | t < min   || t > max   = []
        | gamma < 0 || gamma > 1 = []
        | beta < 0  || beta > 1  = []
        | otherwise              = [makeIntersection ray t n (Just $ F2 beta gamma)| n <- ns]
        where
            -- e + t*d = o + beta*d1 + gamma*d2
            F3 t beta gamma = solve3x3 (M3 ((-1)*.d) d1 d2) (e .-. o)
            ns = [normal, (-1)*.normal]

data Object = Object AnyGeom Material deriving Show

--TODO define all box stuf only for raytracer? But we make Geometry 
--implement Boxable, and geometry is defined here, so that's not *that* 
--nice imo

-- | Axis aligned box.
data Box = Box !Pt3 !Pt3 deriving Show

class Boxable a where
    box :: a -> Box -- ^ Surrounding box for 'a'

instance Boxable Box where
    box = id

instance Boxable Pt3 where
    -- be careful with floating point precision!
    box p = Box (p .-. eps) (p .+. eps)
        where eps = epsilon *. (F3 1 1 1)

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

instance (Boxable a) => Boxable (S.Seq a) where
    box xs
        | S.length xs <= 0 = error "Can't box an empty sequence of boxables!"
        | otherwise        = F.foldl1 (\b1 b2 -> box (b1, b2)) (fmap box xs)

instance Boxable AnyGeom where
    box (MkAnyGeom geom) = boundingBox geom

instance Boxable Object where
    box (Object geom _) = box geom



data CameraGaze = CameraGaze {
        cgPos  :: Pt3,
        cgDir  :: Vec3,
        cgUp   :: Vec3,
        cgFovy :: Flt -- ^ in degrees
    } deriving Show

data Camera = Camera {
        camPos  :: Pt3,
        camUVW  :: CoordSyst, -- ^ w is the reverse of the looking direction
        camFovy :: Flt -- ^ in degrees
    } deriving Show

camFromCamGaze :: CameraGaze -> Camera
camFromCamGaze (CameraGaze pos dir up fovy) = Camera pos (u, v, w) fovy
    where
        w = (-1) *. (normalize dir)
        u = normalize $ up .^. w
        v = w .^. u

camLookingAt :: Pt3 -> Pt3 -> UVec3 -> Flt -> Camera
camLookingAt pos lookAt up fovy =
    camFromCamGaze $ CameraGaze pos (direction pos lookAt) up fovy

type CoordSyst = (UVec3, UVec3, UVec3)

data LightType = PointSource Pt3        -- ^ Pointsource position
        | SoftBox Pt3 Vec3 Vec3 Int Int -- ^ SoftBox origin side1 side2 numRays1 numRays2
        deriving Show

-- Distirbute the given number of rays across the surface of the softbox in 
-- each direction. The resulting number of rays is at least the given 
-- number here, but potentially more!
mkSoftBox :: Pt3 -> Vec3 -> Vec3 -> Int -> LightType
mkSoftBox origin dir1 dir2 totRays = SoftBox origin dir1 dir2 n1 n2
    where
        width  = (len dir1)
        height = (len dir2)
        averageLength = sqrt (width * height)
        n  = fromIntegral totRays
        n1 = ceiling $ (sqrt n) * width / averageLength
        n2 = ceiling $ (sqrt n) * height / averageLength

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

data Transformation = Identity
                    | Translation Vec3
                    | Rotation Vec3 Flt
                    | Scale Flt Flt Flt
                    | Transformation `After` Transformation -- Composition
                    deriving Show

-- | Returns matrices for the normal and inverse transformation.
transfoM4s :: Transformation -> (M4, M4)
transfoM4s Identity              = (m4id, m4id)
transfoM4s (Translation v)       = trans3M4s v
transfoM4s (Rotation axis angle) = rotM4s axis angle
transfoM4s (Scale x y z)         = scalexyzM4s x y z
transfoM4s (t1 `After` t2)       = (m1 .*. m2, m2inv .*. m1inv)
    where
        (m1, m1inv) = transfoM4s t1
        (m2, m2inv) = transfoM4s t2

data Scene = Scene {
        sLights :: [Light],
        sGraph  :: SceneGraph
    } deriving Show



-- TODO: Pick proper convention (0 to res-1) or (1 to res) and check if 
-- everything complies with this!
-- | ( 0,  0) is the top left corner of the image
-- | (nx, ny) is the bottom right corner of the image
newtype Pixel = Pixel (Int, Int) deriving (Show, Ord, Eq, Ix)

pixToPt :: Pixel -> Pt2
pixToPt (Pixel (x, y)) = F2 (fromIntegral x) (fromIntegral y)

newtype Resolution = Resolution (Int, Int) deriving Show
resToPix :: Resolution -> Pixel
resToPix (Resolution pair) = Pixel pair

-- | Lazy RGB triplet, components in the range [0..1].
data Color = Color {
        cRed   :: Flt,
        cGreen :: Flt,
        cBlue  :: Flt
    }

instance (NumTuple Flt) Color where
    tupleToList (Color r g b) = [r, g, b]
    tupleFromList [r, g, b] = Color r g b
instance Show Color where
    show = showTuple -- TODO: specify this somehow at the level of NumTuple
instance Mult Color Color Flt where
    (.*.) = dot
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
