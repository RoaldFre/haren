module Math where

-- floating point format, easily switch between Float and Double
type Flt = Double

infinity :: Flt
infinity = 1/0 -- It is as if a million mathematicians suddenly cried out in pain

epsilon :: Flt
epsilon = 1.0e-4 -- Todo: base this on actual machine epsilon

type Point = (Flt, Flt, Flt)
type Vector = (Flt, Flt, Flt)
type UnitVector = (Flt, Flt, Flt)

type Flt3 = (Flt, Flt, Flt) -- only used for generic functions below

zero :: Flt3
zero = (0, 0, 0)
e1 :: Flt3
e1 = (1, 0, 0)
e2 :: Flt3
e2 = (0, 1, 0)
e3 :: Flt3
e3 = (0, 0, 1)

infixl 6 .+. 
(.+.) :: Flt3 -> Flt3 -> Flt3
(x1, y1, z1) .+. (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2) 

infixl 6 .-.
(.-.) :: Flt3 -> Flt3 -> Flt3
(x1, y1, z1) .-. (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2) 

infixl 7 .*.
(.*.) :: Flt3 -> Flt3 -> Flt
(x1, y1, z1) .*. (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2 

infixl 7 .***.
(.***.) :: Flt3 -> Flt3 -> Flt3
(x1, y1, z1) .***. (x2, y2, z2) = (x1*x2, y1*y2, z1*z2) 

infixl 7 .^.
-- | Cross product (written as a 'wedge' product here, because .x. is not a 
-- valid infix operator)
(.^.) :: Flt3 -> Flt3 -> Flt3
(x1, y1, z1) .^. (x2, y2, z2) = 
                (y1 * z2  -  z1 * y2
                ,z1 * x2  -  x1 * z2
                ,x1 * y2  -  y1 * x2)

-- | This is a bit of a hack to get a postfix (.^2) operator. It makes 
-- subsequent code a bit more tidy and symmetric wrt to scalars.
-- TODO: does this present a performance penalty?
infixr 8 .^
(.^) :: Flt3 -> Int -> Flt
v .^ 2 = v .*. v
v .^ _ = error "Can only use .^ on a vector to get the square!"

infixl 7 .*
(.*) :: Flt3 -> Flt -> Flt3
(x, y, z) .* a = (a*x, a*y, a*z)

infixl 7 *.
(*.) :: Flt -> Flt3 -> Flt3
a *. v = v .* a

infixl 7 ./
(./) :: Flt3 -> Flt -> Flt3
v ./ a = v .* (1/a)

len :: Vector -> Flt
len v = sqrt(v .*. v)

normalize :: Vector -> UnitVector
normalize v = v ./ (len v)

solveQuadEq :: Flt -> Flt -> Flt -> [Flt]
solveQuadEq a b c
    | d < 0     = []
    | d > 0     = [(-b - sqrt(d))/(2*a), (-b + sqrt(d))/(2*a)]
    | otherwise = [-b/(2*a)]
    where
        d = b^2 - 4*a*c

direction :: Point -> Point -> UnitVector
direction p1 p2 = normalize $ p2 .-. p1

-- | Mirror the given vector along the given normal
mirror :: Vector -> UnitVector -> Vector
mirror v n = 2*(v .*. n) *. n  .-.  v

-- | Reflect the given vector along the given normal
reflect :: Vector -> UnitVector -> Vector
reflect v n = v  .-.  2*(v .*. n) *. n

-- vim: expandtab smarttab sw=4 ts=4
