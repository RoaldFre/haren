import Math

data Object = Sphere Flt Point3D
        | Triangle Point3D Point3D Point3D
                deriving Show

data Ray = Ray {origin::Point3D, direction::UnitVector} deriving Show

data Intersection = Intersection {distance::Flt,
        normal::UnitVector} deriving Show

-- | Note: this ordering only really makes sense for intersections of the same ray.
instance Ord Intersection where
        i1 <= i2  =  distance i1 <= distance i2
-- Prerequisite for Ord...
instance Eq Intersection where
        i1 == i2  =  distance i1 == distance i2


-- | intersectFirstIn (min, max) ray objects. 
intersectFirstIn :: (Flt, Flt) -> Ray -> [Object] -> Maybe Intersection
intersectFirstIn (min, max) r objs =
        case validIntersections of
                [] -> Nothing
                _  -> Just (minimum validIntersections)
        where validIntersections = 
                [i | i <-  concatMap (intersectWith r) objs,
                     min <= (distance i) && (distance i) <= max]


intersectWith :: Ray -> Object -> [Intersection]
intersectWith (Ray e d) (Sphere r c) =
        [Intersection t (sphereNormal t) | t <- ts]
        where
                ts = solveQuadEq
                                (d .*. d)
                                (2 *. d .*. (e .-. c))
                                ((e .-. c).^2 - r^2)
                sphereNormal t = (e .+. t*.d .-. c) ./ r




-- vim: expandtab smarttab
