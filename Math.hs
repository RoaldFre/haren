module Math where

-- floating point format, easily switch between Float and Double
type Flt = Double

inf :: Flt
inf = 1/0 -- It is as if a million mathematicians suddenly cried out in pain

eps :: Flt
eps = 1.0e-9 -- Todo: base this on actual machine epsilon

type Point = (Flt, Flt, Flt)
type Vector = (Flt, Flt, Flt)
type UnitVector = (Flt, Flt, Flt)

zero :: (Flt, Flt, Flt)
zero = (0, 0, 0)
e1 :: (Flt, Flt, Flt)
e1 = (1, 0, 0)
e2 :: (Flt, Flt, Flt)
e2 = (0, 1, 0)
e3 :: (Flt, Flt, Flt)
e3 = (0, 0, 1)

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

infixl 7 .^.
-- | Cross product (written as a 'wedge' product here, because .x. is not a 
-- valid infix operator)
(.^.) :: (Flt, Flt, Flt) -> (Flt, Flt, Flt)
        -> (Flt, Flt, Flt)
(x1, y1, z1) .^. (x2, y2, z2) = 
                                (y1 * z2  -  z1 * y2
                                ,z1 * x2  -  x1 * z2
                                ,x1 * y2  -  y1 * x2)

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

normalize :: Vector -> Vector
normalize v = v ./ (len v)

solveQuadEq :: Flt -> Flt -> Flt -> [Flt]
solveQuadEq a b c
        | d < 0     = []
        | d > 0     = [(-b - sqrt(d))/(2*a), (-b + sqrt(d))/(2*a)]
        | otherwise = [-b/(2*a)]
        where
                d = b^2 - 4*a*c



-- vim: expandtab smarttab
