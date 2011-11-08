import Math


data Object = Sphere Flt Point
        | Triangle Point Point Point
                deriving Show

data Ray = Ray {
                rayOrigin    :: Point,
                rayDirection :: UnitVector
        } deriving Show

data Intersection = Intersection {
                intDist :: Flt,
                intNorm :: UnitVector} deriving Show

data Camera = Camera {
        --TODO: u,v,w instead of pos, dir, up?
                camPos  :: UnitVector,
                camDir  :: UnitVector,
                camUp   :: UnitVector,
                camFovy :: Flt -- ^ in degrees
        } deriving Show

newtype Pixel = Pixel (Flt, Flt)
newtype Resolution = Resolution (Int, Int)

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


intersectWith :: Ray -> Object -> [Intersection]
intersectWith (Ray e d) (Sphere r c) =
        [Intersection t (sphereNormal t) | t <- ts]
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


-- vim: expandtab smarttab
