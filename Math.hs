{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}

module Math where

import qualified Data.List as List

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

    showTuple :: t -> String
    showTuple t = show $ tupleToList t


-- | Numerical tuple with 4 components
data F4 = F4 !Flt !Flt !Flt !Flt

instance (NumTuple Flt) F4 where
    tupleToList (F4 x y z w) = [x, y, z, w]
    tupleFromList [x, y, z, w] = F4 x y z w
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
    tupleToList (F3 x y z) = [x, y, z]
    tupleFromList [x, y, z] = F3 x y z
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
normalize v = v ./. (len v)

direction :: Point -> Point -> UnitVector
direction p1 p2 = normalize $ p2 .-. p1

-- | Mirror the given vector along the given normal
mirror :: Vector -> UnitVector -> Vector
mirror v n = 2*(v .*. n) .*. n  .-.  v

-- | Reflect the given vector along the given normal
reflect :: Vector -> UnitVector -> Vector
reflect v n = v  .-.  2*(v .*. n) .*. n



-- | TODO: this can be generalised to arbitrary dimension tensors, which 
-- will fold this and the tuple code above into one tidy package :P
class (Num x, Fractional x, Show x, Show t, NumTuple x t) => 
                        Matrix x t m | t -> x, m -> x, m -> t, t -> m where
    matrToList   :: m -> [t]
    matrFromList :: [t] -> m

    addm :: m -> m -> m
    m1 `addm` m2 = matrFromList $ zipWith addt (matrToList m1) (matrToList m2)

    subm :: m -> m -> m
    m1 `subm` m2 = matrFromList $ zipWith subt (matrToList m1) (matrToList m2)

    transpose :: m -> m
    transpose m = matrFromList $ map tupleFromList $ List.transpose $
                                                map tupleToList (matrToList m)

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


-- | 4x4 Matrix. M4 row1 row2 row3 row4
data M4 = M4 !F4 !F4 !F4 !F4

instance (Matrix Flt F4) M4 where
    matrToList (M4 r1 r2 r3 r4) = [r1, r2, r3, r4]
    matrFromList [r1, r2, r3, r4] = M4 r1 r2 r3 r4
instance Show M4 where
    show = showMatrix 




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
instance Div M4 Flt

-- | Used to fully overload the .+. operator to behave as a correct sum 
-- in all situations.
class Add a where
    infixl 6 .+.
    (.+.) :: a -> a -> a

instance Add F3 where (.+.) = addt
instance Add F4 where (.+.) = addt
instance Add M4 where (.+.) = addm

-- | Used to fully overload the .-. operator to behave as a correct 
-- difference in all situations.
class Sub a where
    infixl 6 .-.
    (.-.) :: a -> a -> a

instance Sub F3 where (.-.) = subt
instance Sub F4 where (.-.) = subt
instance Sub M4 where (.-.) = subm

-- vim: expandtab smarttab sw=4 ts=4

