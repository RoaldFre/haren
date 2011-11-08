-- floating point format, easily switch between Float and Double
type Flt = Double

inf :: Flt
inf = 1/0 -- It is as if a million mathematicians suddenly cried out in pain

eps :: Flt
eps = 10**(-9) -- Todo: base this on actual machine epsilon

type Point2D = (Flt, Flt)
type Point3D = (Flt, Flt, Flt)
type Vector = (Flt, Flt, Flt)

zero :: (Flt, Flt, Flt)
zero = (0, 0, 0)
e1 :: (Flt, Flt, Flt)
e1 = (1, 0, 0)
e2 :: (Flt, Flt, Flt)
e2 = (0, 1, 0)
e3 :: (Flt, Flt, Flt)
e3 = (0, 0, 1)

-- works for Point3D and Vector
infixl 6 .+. 
(.+.) :: (Flt, Flt, Flt) -> (Flt, Flt, Flt)
        -> (Flt, Flt, Flt)
(x1, y1, z1) .+. (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2) 


infixl 6 .-.
(.-.) :: (Flt, Flt, Flt) -> (Flt, Flt, Flt)
        -> (Flt, Flt, Flt)
(x1, y1, z1) .-. (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2) 

infixl 7 .*.
(.*.) :: (Flt, Flt, Flt) -> (Flt, Flt, Flt)
        -> Flt
(x1, y1, z1) .*. (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2 

-- | This is a bit of a hack to get a postfix (.^2) operator. It makes 
-- subsequent code a bit more tidy and symmetric wrt to scalars.
-- TODO: does this present a performance penalty?
infixr 8 .^
(.^) :: (Flt, Flt, Flt) -> Int -> Flt
v .^ 2 = v .*. v
v .^ _ = error "Can only use .^ on a vector to get the square!"

infixl 7 .*
(.*) :: (Flt, Flt, Flt) -> Flt
        -> (Flt, Flt, Flt)
(x, y, z) .* a = (a*x, a*y, a*z)

infixl 7 *.
(*.) :: Flt -> (Flt, Flt, Flt)
        -> (Flt, Flt, Flt)
a *. v = v .* a

infixl 7 ./
(./) :: (Flt, Flt, Flt) -> Flt
        -> (Flt, Flt, Flt)
v ./ a = v .* (1/a)

len :: Vector -> Flt
len v = sqrt(v .*. v)

solveQuadEq :: Flt -> Flt -> Flt -> [Flt]
solveQuadEq a b c
        | d < 0     = []
        | d > 0     = [(-b - sqrt(d))/(2*a), (-b + sqrt(d))/(2*a)]
        | otherwise = [-b/(2*a)]
        where
                d = b^2 - 4*a*c




data Object = Sphere Flt Point3D
        | Triangle Point3D Point3D Point3D
                deriving Show

-- | Ray origin direction. The direction must be normalized to unity.
data Ray = Ray Point3D Vector deriving Show

-- | Intersection distance normal.
-- Normal will be rescaled to a unit vector.
data Intersection = Intersection Flt Vector deriving Show




intersectWith :: Ray -> Object -> [Intersection]
intersectWith (Ray e d) (Sphere r c) =
        [Intersection t (sphereNormal t) | t <- ts]
        where
                ts = solveQuadEq
                                (d .*. d)
                                (2 *. d .*. (e .-. c))
                                ((e .-. c).^2 - r^2)
                sphereNormal t = (e .+. t*.d .-. c) ./ r
                        


-- vim: expandtab
