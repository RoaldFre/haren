{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}

module Math where

-- floating point format, easily switch between Float and Double
type Flt = Double

infinity :: Flt
infinity = 1/0 -- It is as if a million mathematicians suddenly cried out in pain

epsilon :: Flt
epsilon = 1.0e-4 -- Todo: base this on actual machine epsilon

-- | Numerical tuples such as points and vectors in arbitrary dimensions.
-- WARNING: Compile with optimizations and increase the default 
-- unfolding-use-threshold or this will be *unusably slow*!
class (Num x, Fractional x, Show x) => NumTuple x t | t -> x where
    toList   :: t -> [x]
    fromList :: [x] -> t

    infixl 6 .+. 
    (.+.) :: t -> t -> t
    t1 .+. t2 = fromList $ zipWith (+) (toList t1) (toList t2)

    infixl 6 .-.
    (.-.) :: t -> t -> t
    t1 .-. t2 = fromList $ zipWith (-) (toList t1) (toList t2)

    infixl 7 .***.
    (.***.) :: t -> t -> t
    t1 .***. t2 = fromList $ zipWith (*) (toList t1) (toList t2)

    infixl 7 .*.
    (.*.) :: t -> t -> x
    t1 .*. t2 = sum $ toList $ t1 .***. t2
    
    infixl 7 .*
    (.*) :: t -> x -> t
    t .* x = fromList $ map (*x) $ toList t

    infixl 7 *.
    (*.) :: x -> t -> t
    x *. t = t .* x

    infixl 7 ./
    (./) :: t -> x -> t
    t ./ x = t .* (1/x)

    -- | This is a bit of a hack to get a postfix (.^2) operator. It makes 
    -- subsequent code a bit more tidy and symmetric wrt to scalars.
    infixr 8 .^
    (.^) :: t -> Int -> x
    v .^ 2 = v .*. v
    v .^ _ = error "Can only use .^ to get the square!"

    showTuple :: t -> String
    showTuple t = show $ toList t



-- | Numerical tuple with 4 components
data F4 = F4 !Flt !Flt !Flt !Flt

instance (NumTuple Flt) F4 where
    toList (F4 x y z w) = [x, y, z, w]
    fromList [x, y, z, w] = F4 x y z w
instance Show F4 where
    show = showTuple -- TODO: specify this somehow at the level of NumTuple

f4zero = F4 0 0 0 0
f4e1   = F4 1 0 0 0
f4e2   = F4 0 1 0 0
f4e3   = F4 0 0 1 0
f4e4   = F4 0 0 0 1




-- | Numerical tuple with 3 components
data F3 = F3 !Flt !Flt !Flt

instance (NumTuple Flt) F3 where
    toList (F3 x y z) = [x, y, z]
    fromList [x, y, z] = F3 x y z
instance Show F3 where
    show = showTuple -- TODO: specify this somehow at the level of NumTuple

f3zero = F3 0 0 0
f3e1   = F3 1 0 0
f3e2   = F3 0 1 0
f3e3   = F3 0 0 1

type Point = F3
type Vector = F3
type UnitVector = F3


infixl 7 .^.
-- | Cross product (written as a 'wedge' product here, because .x. is not a 
-- valid infix operator)
(.^.) :: F3 -> F3 -> F3
(F3 x1 y1 z1) .^. (F3 x2 y2 z2) = F3
                                    (y1 * z2  -  z1 * y2)
                                    (z1 * x2  -  x1 * z2)
                                    (x1 * y2  -  y1 * x2)

len :: Vector -> Flt
len v = sqrt(v .*. v)

normalize :: Vector -> UnitVector
normalize v = v ./ (len v)

direction :: Point -> Point -> UnitVector
direction p1 p2 = normalize $ p2 .-. p1

-- | Mirror the given vector along the given normal
mirror :: Vector -> UnitVector -> Vector
mirror v n = 2*(v .*. n) *. n  .-.  v

-- | Reflect the given vector along the given normal
reflect :: Vector -> UnitVector -> Vector
reflect v n = v  .-.  2*(v .*. n) *. n

solveQuadEq :: Flt -> Flt -> Flt -> [Flt]
solveQuadEq a b c
    | d < 0     = []
    | d > 0     = [(-b - sqrt(d))/(2*a), (-b + sqrt(d))/(2*a)]
    | otherwise = [-b/(2*a)]
    where
        d = b^2 - 4*a*c


-- vim: expandtab smarttab sw=4 ts=4

