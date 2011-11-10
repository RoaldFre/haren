import Math

import Data.List


-- | RGB triplet, components in the range [0..1]. Not a newtype so we can 
-- reuse our triplet math.
type Color = (Flt, Flt, Flt)
black :: Color
black = (0, 0, 0)
white :: Color
white = (1, 1, 1)

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
                rayOrigin    :: Point,
                rayDirection :: UnitVector
        } deriving Show

data Intersection = Intersection {
                intDist :: Flt,
                intNorm :: UnitVector,
                intMat  :: Material -- TODO: don't drag this along all the time?
        } deriving Show

data Camera = Camera {
        --TODO: u,v,w instead of pos, dir, up?
                camPos  :: UnitVector,
                camDir  :: UnitVector,
                camUp   :: UnitVector,
                camFovy :: Flt -- ^ in degrees
        } deriving Show

newtype Pixel = Pixel (Flt, Flt) deriving Show
newtype Resolution = Resolution (Int, Int) deriving Show

type CoordSyst = (UnitVector, UnitVector, UnitVector)


-- | Note: this ordering only really makes sense for intersections of the same ray.
instance Ord Intersection where
        i1 <= i2  =  intDist i1 <= intDist i2
-- Prerequisite for Ord...
instance Eq Intersection where
        i1 == i2  =  intDist i1 == intDist i2


-- | intersectFirstIn (min, max) ray objects. 
intersectFirstIn :: (Flt, Flt) -> Ray -> [Object] -> Maybe Intersection
intersectFirstIn (min, max) r objs =
        case validIntersections of
                [] -> Nothing
                _  -> Just (minimum validIntersections)
        where validIntersections = 
                [i | i <-  concatMap (intersectWith r) objs,
                     min <= (intDist i) && (intDist i) <= max]

intersectEpsilonRay = intersectFirstIn (epsilon, infinity)

intersectWith :: Ray -> Object -> [Intersection]
intersectWith (Ray e d) (Object (Sphere r c) mat) =
        [Intersection t (sphereNormal t) mat | t <- ts]
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
        Ray (camPos cam) dir
        where
                -- See p164 and 203-204 of Fundamentals of Computer 
                -- Graphics (Peter Shirley, 2nd ed) for drawing and info.
                -- We choose n = 1.
                nxFlt = fromIntegral ny
                nyFlt = fromIntegral nx
                (u, v, w) = cameraSystem cam
                top = tan ((camFovy cam) * pi / 360) -- theta/2
                right = top * nxFlt / nyFlt
                us = right * ((2*i + 1)/nxFlt - 1)
                vs =  top  * ((2*j + 1)/nyFlt - 1)
                dir = normalize $ us*.u .+. vs*.v .-. w -- ws = -n = -1

pixelGrid :: Resolution -> [Pixel]
pixelGrid (Resolution (nx, ny)) =
        [Pixel (i, j) | j <- reverse [0 .. (nyFlt - 1)], i <- [0 .. (nxFlt - 1)]]
        where
                nxFlt = fromIntegral nx
                nyFlt = fromIntegral ny

pixelRow :: Resolution -> Int -> [Pixel]
pixelRow (Resolution (nx, ny)) row =
        [Pixel (i, rowFlt) | i <- [0 .. (nxFlt - 1)]]
        where
                nxFlt = fromIntegral nx
                rowFlt = fromIntegral row

pixelRowRays :: Resolution -> Camera -> Int -> [Ray]
pixelRowRays res cam row =
        map (cameraRay res cam) (pixelRow res row)


-- | Calculate the Color of the Ray intersecting the given Material at the 
-- given Intersection
color :: Ray -> Intersection -> Color
color ray int =
        foldl' addWeightedPureColor black (intMat int)
        where
                addWeightedPureColor col (MaterialComponent (weight, pureMat)) =
                        col .+. weight *. (colorPure ray int pureMat)

-- | Calculate the Color of a PureMaterial from a given Ray at a given Intersection
colorPure :: Ray -> Intersection -> PureMaterial -> Color
colorPure r i (PureMaterial Diffuse matCol) =
        ((-1)*.(rayDirection r)) .*. (intNorm i) *. matCol
        --TODO: proper direction from ray to lightsource
        --now: light from camera


raytrace :: [Object] -> Ray -> Color
raytrace objs ray =
        case (intersectEpsilonRay ray objs) of
                Nothing -> black
                Just int -> color ray int

renderLine :: Resolution -> Camera -> [Object] -> Int -> [Color]
renderLine res cam objs row =
        map (raytrace objs) (pixelRowRays res cam row)

headerPPM :: Resolution -> String
headerPPM (Resolution (nx, ny)) = 
        "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255"

colorToPPM :: Color -> String
colorToPPM (r, g, b) = component r ++ " " ++ component g ++ " " ++ component b
        where component = show . round . (* 255) . min 1 . max 0

lineToPPM :: [Color] -> String
lineToPPM cols = unwords $ map colorToPPM cols

renderPPM :: Camera -> [Object] -> Resolution -> String
renderPPM cam objs res@(Resolution (_, ny)) = 
        headerPPM res ++ "\n" ++ unlines
                [lineToPPM $ renderLine res cam objs row | row <- reverse [0 .. ny]]
        
renderPPMtest =
        writeFile "out.ppm" $ renderPPM cam objs res
        where
                cam = Camera zero ((1)*.e3) e2 30
                geom1 = Sphere 1 (0,0,10)
                geom2 = Sphere 1 (0,2,20)
                mat = [MaterialComponent (1, PureMaterial Diffuse white)]
                objs = [Object geom1 mat, Object geom2 mat]
                res = Resolution (200,200)

-- vim: expandtab smarttab
