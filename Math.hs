{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}

module Math where

import qualified Data.List as List

-- floating point format, easily switch between Float and Double
type Flt = Double

infinity :: Flt
infinity = 1/0 -- It is as if a million mathematicians suddenly cried out in pain

epsilon :: Flt
epsilon = radix ** (-digits / 2)
    where
        radix  = fromIntegral $ floatRadix (1 :: Flt)
        digits = fromIntegral $ floatDigits (1 :: Flt)

smallest :: Flt
smallest = radix ** (minExp / 2)
    where
        radix  = fromIntegral $ floatRadix (1 :: Flt)
        minExp = fromIntegral $ fst $ floatRange (1 :: Flt)

equalsEpsilon :: Flt -> Flt -> Bool
x1 `equalsEpsilon` x2 = 
               ((absDiff < smallest)
            || (x1 == 0  &&  abs2 < epsilon)
            || (x2 == 0  &&  abs1 < epsilon)
            || (if abs1 > abs2
                    then absDiff / abs1 < epsilon
                    else absDiff / abs2 < epsilon))
    where
        abs1 = abs x1
        abs2 = abs x2
        absDiff = abs (x2 - x1)

-- | Numerical tuples such as points and vectors in arbitrary dimensions.
-- WARNING: Compile with optimizations and increase the default 
-- unfolding-use-threshold or this will be *unusably slow*!
class (Num x, Fractional x, Show x) => NumTuple x t | t -> x where
    tupleToList   :: t -> [x]
    tupleFromList :: [x] -> t

    addt :: t -> t -> t
    t1 `addt` t2 = tupleFromList $ zipWith (+) (tupleToList t1) (tupleToList t2)

    subt :: t -> t -> t
    t1 `subt` t2 = tupleFromList $ zipWith (-) (tupleToList t1) (tupleToList t2)

    infixl 7 .***.
    (.***.) :: t -> t -> t
    t1 .***. t2 = tupleFromList $ zipWith (*) (tupleToList t1) (tupleToList t2)

    dot :: t -> t -> x
    t1 `dot` t2 = sum $ tupleToList $ t1 .***. t2
    
    infixl 7 *.
    (*.) :: x -> t -> t
    x *. t = tupleFromList $ map (*x) $ tupleToList t

    infixl 7 .*
    (.*) :: t -> x -> t
    t .* x = x *. t

    -- | This is a bit of a hack to get a postfix (.^2) operator. It makes 
    -- subsequent code a bit more tidy and symmetric wrt to scalars.
    infixr 8 .^
    (.^) :: t -> Int -> x
    v .^ 2 = v `dot` v
    v .^ _ = error "Can only use .^ to get the square!"

    equalsWith :: (x -> x -> Bool) -> t -> t -> Bool
    equalsWith f t1 t2 =
        List.foldl' (&&) True $ zipWith f (tupleToList t1) (tupleToList t2)

    showTuple :: t -> String
    showTuple t = show $ tupleToList t


-- | Numerical tuple with 4 components
data F4 = F4 !Flt !Flt !Flt !Flt

instance (NumTuple Flt) F4 where
    tupleToList (F4 x y z w) = [x, y, z, w]
    tupleFromList [x, y, z, w] = F4 x y z w
instance Show F4 where
    show = showTuple -- TODO: specify this somehow at the level of NumTuple
instance Eq F4 where
    (==) = equalsWith equalsEpsilon

f4zero = F4 0 0 0 0
f4e1   = F4 1 0 0 0
f4e2   = F4 0 1 0 0
f4e3   = F4 0 0 1 0
f4e4   = F4 0 0 0 1

type Pt4 = F4
type Vec4 = F4
type UVec4 = F4 -- ^ Unit Vector

vec4 :: Vec3 -> Vec4
vec4 (F3 x y z) = F4 x y z 0

pt4 :: Pt3 -> Pt4
pt4 (F3 x y z) = F4 x y z 1

-- | Numerical tuple with 3 components
data F3 = F3 !Flt !Flt !Flt

instance (NumTuple Flt) F3 where
    tupleToList (F3 x y z) = [x, y, z]
    tupleFromList [x, y, z] = F3 x y z
instance Show F3 where
    show = showTuple -- TODO: specify this somehow at the level of NumTuple
instance Eq F3 where
    (==) = equalsWith equalsEpsilon
    

f3zero = F3 0 0 0
f3e1   = F3 1 0 0
f3e2   = F3 0 1 0
f3e3   = F3 0 0 1

type Pt3 = F3
type Vec3 = F3
type UVec3 = F3 -- ^ Unit Vector


infixl 7 .^.
-- | Cross product (written as a 'wedge' product here, because .x. is not a 
-- valid infix operator)
(.^.) :: F3 -> F3 -> F3
(F3 x1 y1 z1) .^. (F3 x2 y2 z2) = F3
                                    (y1 * z2  -  z1 * y2)
                                    (z1 * x2  -  x1 * z2)
                                    (x1 * y2  -  y1 * x2)

len :: Vec3 -> Flt
len v = sqrt(v .*. v)

normalize :: Vec3 -> UVec3
normalize v = v ./. (len v)

direction :: Pt3 -> Pt3 -> UVec3
direction p1 p2 = normalize $ p2 .-. p1

-- | Mirror the given vector along the given normal
mirror :: Vec3 -> UVec3 -> Vec3
mirror v n = 2*(v .*. n) .*. n  .-.  v

-- | Reflect the given vector along the given normal
reflect :: Vec3 -> UVec3 -> Vec3
reflect v n = v  .-.  2*(v .*. n) .*. n



-- | Type class representing matrices composed of numerical tuples.
-- Minimum definition: matrToList and matrFromList
--
-- TODO: this can be generalised to arbitrary rank tensors, which will 
-- unify this and the tuple code above into one tidy package :P
class (Num x, Fractional x, Show x, Show t, NumTuple x t) => 
                        Matrix x t m | t -> x, m -> x, m -> t, t -> m where
    matrToList   :: m -> [t]
    matrFromList :: [t] -> m

    matrToLists   :: m -> [[x]]
    matrToLists   = (map tupleToList) . matrToList
    matrFromLists :: [[x]] -> m
    matrFromLists = matrFromList . (map tupleFromList)

    addm :: m -> m -> m
    m1 `addm` m2 = matrFromList $ zipWith addt (matrToList m1) (matrToList m2)

    subm :: m -> m -> m
    m1 `subm` m2 = matrFromList $ zipWith subt (matrToList m1) (matrToList m2)

    transpose :: m -> m
    transpose m = matrFromLists $ List.transpose $ matrToLists m

    mult :: m -> m -> m
    m1 `mult` m2 = matrFromList [
                    tupleFromList [row `dot` col | col <- m2l]  |  row <- m1l]
        where
            m1l = matrToList m1
            m2l = matrToList $ transpose m2

    tupleRight :: m -> t -> t
    m `tupleRight` t = tupleFromList [row `dot` t | row <- matrToList m]

    tupleLeft :: t -> m -> t
    t `tupleLeft` m = (transpose m) `tupleRight` t
    
    scalemRight :: m -> x -> m
    m `scalemRight` x = matrFromList $ map (x *.) $ matrToList m

    scalemLeft :: x -> m -> m
    x `scalemLeft` m = m `scalemRight` x

    showMatrix :: m -> String
    showMatrix m = show $ matrToList m


-- | 3x3 Matrix. M3 row1 row2 row3
data M3 = M3 !F3 !F3 !F3

instance (Matrix Flt F3) M3 where
    matrToList (M3 r1 r2 r3) = [r1, r2, r3]
    matrFromList [r1, r2, r3] = M3 r1 r2 r3
instance Show M3 where
    show = showMatrix 
instance Eq M3 where
    m1 == m2 = (matrToList m1) == (matrToList m2)

m3id = matrFromList [f3e1, f3e2, f3e3]


-- | 4x4 Matrix. M4 row1 row2 row3 row4
data M4 = M4 !F4 !F4 !F4 !F4

instance (Matrix Flt F4) M4 where
    matrToList (M4 r1 r2 r3 r4) = [r1, r2, r3, r4]
    matrFromList [r1, r2, r3, r4] = M4 r1 r2 r3 r4
instance Show M4 where
    show = showMatrix 
instance Eq M4 where
    m1 == m2 = (matrToList m1) == (matrToList m2)

m4id = matrFromList [f4e1, f4e2, f4e3, f4e4]

-- | Make a (4x4)-matrix in homogeneous coordinates from the given 
-- (3x3)-matrix
mat4 :: M3 -> M4
mat4 (M3 r1 r2 r3) = matrFromList [vec4 r1, vec4 r2, vec4 r3, f4e4]



-- | Used to fully overload the .*. operator to behave as a correct product 
-- in all situations.
class Mult a b c | a b -> c where
    infixl 7 .*.
    (.*.) :: a -> b -> c

instance Mult F3 F3 Flt where (.*.) = dot
instance Mult F3 Flt F3 where (.*.) = (.*) -- note : .* and *. are often more 
instance Mult Flt F3 F3 where (.*.) = (*.) -- usefull (verbose) than .*.
    
instance Mult F4 F4 Flt where (.*.) = dot
instance Mult F4 Flt F4 where (.*.) = (.*) -- note : .* and *. are often more 
instance Mult Flt F4 F4 where (.*.) = (*.) -- usefull (verbose) than .*.

instance Mult M3 M3 M3 where (.*.) = mult
instance Mult M3 F3 F3 where (.*.) = tupleRight
instance Mult F3 M3 F3 where (.*.) = tupleLeft
instance Mult M3 Flt M3 where (.*.) = scalemRight
instance Mult Flt M3 M3 where (.*.) = scalemLeft

instance Mult M4 M4 M4 where (.*.) = mult
instance Mult M4 F4 F4 where (.*.) = tupleRight
instance Mult F4 M4 F4 where (.*.) = tupleLeft
instance Mult M4 Flt M4 where (.*.) = scalemRight
instance Mult Flt M4 M4 where (.*.) = scalemLeft


-- | Used to fully overload the ./. operator to behave as a correct 
-- scaling.
class (Num l, Fractional l, Mult x l x) => Div x l where
    infixl 7 ./.
    (./.) :: x -> l -> x
    x ./. l = x .*. (1/l)
instance Div F3 Flt
instance Div F4 Flt
instance Div M3 Flt
instance Div M4 Flt

-- | Used to fully overload the .+. operator to behave as a correct sum 
-- in all situations.
class Add a where
    infixl 6 .+.
    (.+.) :: a -> a -> a

instance Add F3 where (.+.) = addt
instance Add F4 where (.+.) = addt
instance Add M4 where (.+.) = addm
instance Add M3 where (.+.) = addm

-- | Used to fully overload the .-. operator to behave as a correct 
-- difference in all situations.
class Sub a where
    infixl 6 .-.
    (.-.) :: a -> a -> a

instance Sub F3 where (.-.) = subt
instance Sub F4 where (.-.) = subt
instance Sub M4 where (.-.) = subm
instance Sub M3 where (.-.) = subm


-- | Homogeneous coordinates for a 3D point.
homP :: Pt3 -> Pt4
homP (F3 x y z) = F4 x y z 1

-- | Homogeneous coordinates for a 3D vector.
homV :: Vec3 -> Vec4
homV (F3 x y z) = F4 x y z 0

-- | Translation matrix for the given 3D vector.
trans3M4 :: Vec3 -> M4
trans3M4 = transM4 . homV

-- | Translation matrix for the given vector in homogeneous coordinates.
transM4 :: Vec4 -> M4
transM4 v@(F4 x y z 0) = transpose $ matrFromList [f4e1, f4e2, f4e3, v .+. f4e4]

-- | Translation matrix (M, M^-1) for the given vector.
transM4s :: Vec4 -> (M4, M4)
transM4s v = (transM4 v, transM4 $ (-1) *. v)

-- | Matrix for rotation along the given (non-zero) axis vector and the 
-- given angle in degrees.
rotM4 :: Vec3 -> Flt -> M4
rotM4 axis angle =
    mat4 $ (transpose changeOfBasis) .*. rotz .*. changeOfBasis
    where
        w = normalize axis
        -- Make sure we use sufficiently orthogonal axes!
        u = if (w .*. f3e1) < 0.8
                then normalize $ w .^. f3e1
                else normalize $ w .^. f3e2
        v = w .^. u
        changeOfBasis = matrFromList [u, v, w]
        rotz = matrFromLists [[c, -s,  0]
                             ,[s,  c,  0]
                             ,[0,  0,  1]] :: M3
        s = sin $ angle * pi / 180
        c = cos $ angle * pi / 180

-- | Matrices (M, M^-1) for rotation along the given (non-zero) axis vector 
-- and the given angle in degrees.
rotM4s :: Vec3 -> Flt -> (M4, M4)
rotM4s axis angle = (rot, transpose rot)
    where rot = rotM4 axis angle

-- | Translation matrix for the given factor.
scaleM4 :: Flt -> M4
scaleM4 = (.*.) m4id

-- | Translation matrices (M, M^-1) for the given factor.
scaleM4s :: Flt -> (M4, M4)
scaleM4s x = (scaleM4 x, scaleM4 (1/x))

-- vim: expandtab smarttab sw=4 ts=4

