module Math where

-- floating point format, easily switch between Float and Double
type Flt = Double 

type Point2D = (Flt, Flt)
type Point3D = (Flt, Flt, Flt)
type Vector = (Flt, Flt, Flt)

data Object = Sphere Flt Point3D
        | Triangle Point3D Point3D Point3D

data Ray = Ray Point3D Vector



-- TODO: penalty of having these simple functions in a separate module (= 
-- no inlining (?)). Try to merge them later, or do a whole program 
-- complisation somehow


-- works for Point3D and Vector
(.+.) :: (Flt, Flt, Flt) -> (Flt, Flt, Flt)
        -> (Flt, Flt, Flt)
(x1, y1, z1) .+. (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2) 


(.-.) :: (Flt, Flt, Flt) -> (Flt, Flt, Flt)
        -> (Flt, Flt, Flt)
(x1, y1, z1) .-. (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2) 

(.*.) :: (Flt, Flt, Flt) -> (Flt, Flt, Flt)
        -> Flt
(x1, y1, z1) .*. (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2 

(.*) :: (Flt, Flt, Flt) -> Flt
        -> (Flt, Flt, Flt)
(x, y, z) .* a = (a*x, a*y, a*z)

(*.) :: Flt -> (Flt, Flt, Flt)
        -> (Flt, Flt, Flt)
a *. v = v .* a

len :: Vector -> Flt
len v = sqrt(v .*. v)

-- vim: expandtab
