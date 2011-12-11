{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveFunctor, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification #-}

--module Haray where
module Haray (RayTraceConfig (..), RayTracer, rayTrace, optimizeTriangleMesh) where

import Types
import Math
import Renderer

import Data.List hiding (transpose, intersect)
import Data.Maybe
--import System.Random.Mersenne.Pure64
import System.Random

import Control.Monad.State
import Control.Applicative
import GHC.Exts

import Prelude hiding (concatMap)
import Data.Foldable hiding (minimum, maximum, concat, concatMap, foldl')

data RayTraceConfig = RayTraceConfig {
        confDepth     :: Int,
        confAAsamples :: Int,
        confSeed      :: Int,
        confRes       :: Resolution, -- TODO, this and cam are shared with rasterizer, not part of raytracer per se
        confCam       :: Camera,
        confAmbient   :: Color
    } deriving Show

-- TODO: make (some of) these strict?
data RayTraceState = RayTraceState {
        stateScene     :: AnyIntersectable,
        stateLights    :: [Light],
        stateAmbient   :: Color,
        stateDepth     :: Int,
        stateAAsamples :: Int,
        stateRndGen    :: StdGen, -- ^ Random number generator
        stateRes       :: Resolution,
        stateCam       :: Camera,
        stateMaxDepth  :: Int
    } deriving Show


-- | Transformed thingamajings, represented by transformations and the 
-- untransformed thingamajing.
data Transformed a = Transformed {
        tTrans    :: M4,
        tInvTrans :: M4,
        tOriginal :: a
    } deriving (Show, Functor)

instance (Ord a) => Ord (Transformed a) where
    t1 <= t2  =  (tOriginal t1) <= (tOriginal t2)
-- Prerequisite for Ord...
instance (Eq a) => Eq (Transformed a) where
    t1 == t2  =  (tOriginal t1) == (tOriginal t2)

-- | tTrans is here the transformation that should be applied to the object
type TransformedObj = Transformed Object


transformRay :: M4 -> Ray -> Ray
transformRay trans (Ray origin direction near far dist) =
    Ray (trans `multPt` origin) (trans `multVec` direction) near far dist

transformInt :: Pt3 -> (M4, M4) -> Intersection -> Intersection
transformInt originalOrigin (trans, invTrans) int =
    int {intDir = newDir, intPos = newPos, intNorm = newNorm}
    where 
        newDir = trans `multVec` (intDir int)
        --newPos = trans `multPt` (intPos int),
        newPos = originalOrigin .+. (newDir .* intDist int) 
        newNorm = normalize $ (transpose invTrans) `multVec` (intNorm int)





instance Boxable TransformedObj where
    box to = transformBox (tTrans to) $ box $ tOriginal to

transformBox :: M4 -> Box -> Box
transformBox trans b = box $ map (trans `multPt`) $ getBoxVertices b


getBoxVertices :: Box -> [Pt3]
getBoxVertices (Box p1 p2) = [p1 .+. (F3 x y z) | x <- [0, f3x (p2 .-. p1)], 
                                                  y <- [0, f3y (p2 .-. p1)],
                                                  z <- [0, f3z (p2 .-. p1)]]

flattenSceneGraph :: SceneGraph -> [TransformedObj]
flattenSceneGraph sceneGraph = 
    map mkTransObj $ flattenObjectGraph multTuples (m4id, m4id) matricesGraph
    where
        mkTransObj ((m,mInv), obj) = Transformed m mInv obj
        multTuples (a, b) (x, y) = (a .*. x, b .*. y)
        matricesGraph = transfoM4s `fmap` sceneGraph

-- | Store the precomputed bounding box -> O(1) retrieval time
-- TODO THIS NEEDS TO BE/HAVE A CLASS (and immediately put hitsBoxed in here) [?]
data Boxed a = Boxed {
        thebox :: !Box,
        unbox  :: a
    } deriving Functor
instance (Show a) => Show (Boxed a) where
    show b = "Box <" ++ show p1 ++ "#" ++ show p2 ++ "> containing " ++ show (unbox b)
        where Box p1 p2 = thebox b
instance Boxable (Boxed a) where
    box = thebox

-- | Put the argument in a box. -- TODO  *unless* it already is boxed.!!!!!
mkBoxed :: (Boxable a) => a -> Boxed a
mkBoxed x = Boxed (box x) x

-- | Bounding Volume Hierarchy. (Contains at least one object.)
data BVH a = 
        BVHleaf [Boxed a]
      | BVHnode (Boxed (BVH a)) (Boxed (BVH a))
instance (Show a) => Show (BVH a) where
    show bvh = show' "" bvh
     where
        show' t (BVHleaf []) = t ++ "BVHleaf []"
        show' t (BVHleaf xs) = t ++ "BVHleaf [\n" ++ showlist (t ++ "        ") xs ++ "\n" ++ t ++ "       ]"
        show' t (BVHnode l r) = t ++ "BVHnode (\n" ++ show' newt (unbox l) ++ "\n)(\n"
                                                   ++ show' newt (unbox r) where newt = t ++ "        "
        showlist t [] = ""
        showlist t [x] = t ++ show x
        showlist t (x:xs) = t ++ show x ++ ",\n" ++ showlist t xs
        --tab = "  "

instance (Boxable a) => Boxable (BVH a) where
    box (BVHleaf b) = box b
    box (BVHnode b1 b2) = box (b1, b2)


-- for optimizing a single geometry
instance (Geometry a) => Geometry (BVH a) where
    boundingBox (BVHleaf b) = box b
    boundingBox (BVHnode b1 b2) = box (b1, b2)

    intersectGeom bvh ray = concatMap (\g -> intersectGeom g ray) $ bvhPotentialHits ray bvh


-- | Returns a list of all elements whose bounding box got hit by the given 
-- ray.
-- TODO: I'm not testing if I won't hit with the *entire* bvh alltogether 
-- (== potentially slight performance drop for small, 'dense' trees)
bvhPotentialHits :: Ray -> BVH a -> [a]
bvhPotentialHits ray (BVHleaf b) = map unbox $ filter (ray `hitsBoxed`) b
bvhPotentialHits ray (BVHnode b1 b2) =
    concatMap (bvhPotentialHits ray . unbox) $ filter (ray `hitsBoxed`) [b1, b2]

hitsBoxed :: Ray -> Boxed a -> Bool
ray `hitsBoxed` boxed = ray `hitsBox` (box boxed)
--ray `hitsBoxed` boxed = True -- DEBUG

-- TODO: make this beautiful, loose the ugly imperative feel! ;P
hitsBox :: Ray -> Box -> Bool
ray `hitsBox` (Box p1 p2)
    | tfar1 < tnear1   ||  tfar1 < 0  =  False
    | tfar2 < tnear2   ||  tfar2 < 0  =  False
    | tfar3 < tnear3   ||  tfar3 < 0  =  False
    | otherwise                       =  True
    where
        distFromSlabs dir bound1 bound2 = if t1 < t2 then (t1, t2) else (t2, t1)
            where
                t1 = bound1 / dir
                t2 = bound2 / dir
        [dists1, dists2, dists3] = zipWith3 distFromSlabs 
                                       (tupleToList (rayDir ray)) 
                                       (tupleToList (p1 .-. rayOrigin ray))
                                       (tupleToList (p2 .-. rayOrigin ray))
        shrink (x,y) (a,b) = (maximum [x,a], minimum [y,b])
        (tnear1, tfar1) = shrink (rayNear ray, rayFar ray) dists1
        (tnear2, tfar2) = shrink (tnear1, tfar1) dists2
        (tnear3, tfar3) = shrink (tnear2, tfar2) dists3




-- | Top level class of all possible things that are intersectable. This 
-- can be individual objects, but also entire scenes.
class Intersectable a where
    intersect :: Ray -> a -> [Intersection]

-- Existential intersectable (i11e cfr i18n: intersectable is too long to 
-- type and fucks up formatting :P)
data AnyIntersectable = forall a . (Intersectable a, Show a) => MkAnyI11e a
instance Intersectable AnyIntersectable where
    intersect ray (MkAnyI11e intersectable) = intersect ray intersectable
instance Show AnyIntersectable where
    show (MkAnyI11e intersectable) = "AnyI11e " ++ show intersectable

instance Intersectable Object where
    intersect ray (Object geom mat) = map (\x -> x mat) $ intersectGeom geom ray

instance Intersectable TransformedObj where
    intersect ray (Transformed trans invTrans obj) = globalInts
     where
        localInts = intersect (transformRay invTrans ray) obj
        globalInts = map (transformInt (rayOrigin ray) (trans, invTrans)) localInts

-- for optimizing an entire scene
instance (Intersectable a) => Intersectable (BVH a) where
    intersect ray bvh = concatMap (intersect ray) $ bvhPotentialHits ray bvh




{-
sceneGraphToInternalStructure :: SceneGraph -> SceneStructure
sceneGraphToInternalStructure = (buildBVH 1) . (fmap mkBoxed) . flattenSceneGraph
-}

-- | Optimize the given scenegraph for raytracing.
optimizeSceneGraph :: SceneGraph -> AnyIntersectable
optimizeSceneGraph = MkAnyI11e . (buildBVH 1) . (fmap mkBoxed) . flattenSceneGraph

-- | Optimize the given trianglemesh for raytracing. Triangles will be 
-- placed in a BVH, with each leaf containing at most the given number of 
-- triangles.
optimizeTriangleMesh:: Int -> TriangleMesh -> AnyGeom
optimizeTriangleMesh n (TriangleMesh triangles) =
    MkAnyGeom $! buildBVH n $ (mkBoxed . MkAnyGeom) `fmap` triangles


-- | Build a Bounding Volume Hierarchy (top-down) from the list of boxables.
-- This will keep partitioning until there are at most 'n' elements in each 
-- leaf. In the pathological case where it is impossible to partition a 
-- list further (eg when it holds more than 'n' items with the exact same 
-- box), then that list will be put in a leaf in its entirety.
buildBVH :: (Boxable a) => Int -> [Boxed a] -> BVH a
buildBVH n xs = buildBVH' $ WL (length xs) xs
    where
        buildBVH' :: (Boxable a) => WithLength [Boxed a] -> BVH a
        buildBVH' withLength@(WL l xs)
           | l <= n    = BVHleaf xs
           -- | otherwise = case bestPartition $ partitionBoxeds withLength of
           | otherwise = case bestPartition $ partitionBoxedsFast withLength of
                Nothing       -> BVHleaf xs
                Just (p1, p2) -> BVHnode (buildBVH' <$> swapBoxedAndWithLength p1)
                                         (buildBVH' <$> swapBoxedAndWithLength p2)
        swapBoxedAndWithLength (WL n b) = Boxed (thebox b) (WL n (unbox b))


-- | A thingamajing with an associated length of said thingamajing stored 
-- for fast access.
data WithLength a = WL {
    wlLength :: Int,
    fromWL   :: a 
} deriving Functor
toWL :: [a] -> WithLength [a]
toWL xs = WL (length xs) xs

-- | A partition of a bunch of boxeds, each part wrapped in a boxed itself. 
-- For each 'bunch of boxeds', the number of elements inside is stored as 
-- well.
type BoxedPartition a = (WithLength (Boxed [Boxed a]), WithLength (Boxed [Boxed a]))

-- | Generate a list of possible Partitions from a list of boxeds. 
partitionBoxeds :: (Boxable a) => WithLength [Boxed a] -> [BoxedPartition a]
partitionBoxeds boxedsWL = map (mapPair (fmap mkBoxed)) $
    partitionList sortedx ++ partitionList sortedy ++ partitionList sortedz
    where
        sortedx = fmap (sortWith (\b -> f3x (min b))) boxedsWL
        sortedy = fmap (sortWith (\b -> f3y (min b))) boxedsWL
        sortedz = fmap (sortWith (\b -> f3z (min b))) boxedsWL
        min boxed = boxmin where (Box boxmin boxmax) = box boxed


-- Only partitions dividing the three axes in the center are generated (so 
-- this will run in O(n) instead of O(n log n) when sorting).  Partitions 
-- where one of the parts is empty are suppressed.
partitionBoxedsFast :: (Boxable a) => WithLength [Boxed a] -> [BoxedPartition a]
partitionBoxedsFast (WL 0 [])  = []
partitionBoxedsFast (WL 1 [_]) = []
partitionBoxedsFast (WL _ xs)  = map (mapPair (fmap mkBoxed . toWL)) $
                     filter (\(a, b) -> not (null a)  &&  not (null b))
                        [partition (\b -> f3x (min b) < thresholdx) xs
                        ,partition (\b -> f3y (min b) < thresholdy) xs
                        ,partition (\b -> f3z (min b) < thresholdz) xs]
    where
        min boxed = boxmin where (Box boxmin boxmax) = box boxed
        (Box totalmin totalmax) = box xs
        (F3 thresholdx thresholdy thresholdz) = totalmin .+. (totalmax .* 0.5)



mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

-- | No trivial partitions "((0,[]), (n,xs))" are created.
partitionList :: WithLength [a] -> [(WithLength [a], WithLength [a])]
partitionList (WL 0 []) = []
partitionList (WL n (x:xs)) = partitionList' (WL 1 [x]) (WL (n - 1) xs)
    where
        partitionList' (WL n1 xs) (WL 0  [])   = []
        partitionList' (WL n1 xs) (WL n2 (y:ys)) = (WL n1 xs, WL n2 (y:ys)) :
                            partitionList' (WL (n1 + 1) (y:xs)) (WL (n2 - 1) ys)


costOfPartition :: BoxedPartition a -> Flt
costOfPartition ((WL n1 b1), (WL n2 b2)) = 
    (halfSurfaceArea $ box b1) * (fromIntegral n1)
    + (halfSurfaceArea $ box b2) * (fromIntegral n2)
    where 
        halfSurfaceArea (Box p1 p2) = x*y + y*z + z*x
            where (F3 x y z) = p2 .-. p1

bestPartition :: [BoxedPartition a] -> Maybe (BoxedPartition a)
bestPartition [] = Nothing
bestPartition [p] = Just p
bestPartition (p1:p2:ps) = if costOfPartition p1 < costOfPartition p2
    then bestPartition (p1:ps)
    else bestPartition (p2:ps)


newtype RayTracer a = RT (State RayTraceState a)
    deriving (Monad, Functor)
getRes       = RT $ gets stateRes
getLights    = RT $ gets stateLights
getAmbient   = RT $ gets stateAmbient
getDepth     = RT $ gets stateDepth
decDepth     = getDepth >>= (\d -> RT $ modify (\s -> s {stateDepth = d - 1}))
getAAsamples = RT $ gets stateAAsamples
getScene     = RT $ gets stateScene
getCam       = RT $ gets stateCam
resetDepth   = RT $ gets stateMaxDepth >>= \d -> modify (\s -> s {stateDepth = d})
getRndGen    = RT $ gets stateRndGen
setRndGen new = RT $ modify (\s -> s {stateRndGen = new})

instance Renderer RayTraceConfig RayTracer where
    colorPixel = rayTrace
    getResolution = getRes
    run scene conf (RT s) = evalState s (mkInitialState scene conf)

mkInitialState :: Scene -> RayTraceConfig -> RayTraceState
mkInitialState scene conf =
    RayTraceState internalScene lights ambient depth aa rndGen res cam depth
    where
        depth = confDepth conf
        aa = confAAsamples conf
        rndGen = mkStdGen $ confSeed conf
        res = confRes conf
        cam = confCam conf
        ambient = confAmbient conf
        internalScene = optimizeSceneGraph $ sGraph scene
        lights = sLights scene


-- | Return the default value if getDepth is not zero, or use the given 
-- 'recursion' and apply that after decrementing depth.
orRecurseOn :: a -> RayTracer a -> RayTracer a
defaultValue `orRecurseOn` recursion = do
    depth <- getDepth
    if depth <= 0
        then return defaultValue
        else decDepth >> recursion

getRandomR :: Random a => (a, a) -> RayTracer a
getRandomR range = do
    gen <- getRndGen
    let (rand, newGen) = randomR range gen
    setRndGen newGen
    return rand

getRandom :: Random a => RayTracer a
getRandom = do
    gen <- getRndGen
    let (rand, newGen) = random gen
    setRndGen newGen
    return rand

getRandomRs :: Random a => Int -> (a, a) -> RayTracer [a]
getRandomRs n range = do
    gen <- getRndGen
    let (newGen, gen') = split gen
    setRndGen newGen
    return $ take n $ randomRs range gen'

-- Normal distributed variable with given standard deviation and zero mean. 
-- Via Box-Muller transform.
getStdNorm :: Flt -> RayTracer Flt
getStdNorm stdev = do
    [u1, u2] <- getRandomRs 2 (0, 1)
    return $ stdev * (sqrt (-2 * (log u1)) * (cos (2*pi*u2)))

getStdNorms :: Int -> Flt -> RayTracer [Flt]
getStdNorms num stdev = replicateM num $ getStdNorm stdev


-- | Find the closest intersection of the ray with an intersectable. Only 
-- intersections where the ray enters the objects from the outside (as 
-- determined by the normal) are considered.
intersectFirst :: AnyIntersectable -> Ray -> Maybe Intersection
intersectFirst intersectable ray =
    case filter correctSide $ intersect ray intersectable of
        []   -> Nothing
        ints -> Just $ minimum ints
        where
            correctSide int = (rayDir ray) .*. (intNorm int) < 0
            

cameraRays :: Pixel -> RayTracer [Ray]
cameraRays pixel = do
    res <- getRes -- TODO: getResolution from Renderer?
    cam <- getCam
    num <- getAAsamples
    let pixPt = pixToPt pixel
    if num <= 1
        then return $ [cameraRay res cam pixPt]
        else do
            dis <- getRandomRs num (-0.5, 0.5)
            djs <- getRandomRs num (-0.5, 0.5)
            let pts = zipWith (\di dj -> pixPt .+. (F2 di dj)) dis djs
            return $ map (cameraRay res cam) pts

cameraRay :: Resolution -> Camera -> Pt2 -> Ray
cameraRay (Resolution (nx, ny)) cam (F2 i j) =
    Ray (camPos cam) dir 0 infinity 0
    where
        -- See p164 and 203-204 of Fundamentals of Computer 
        -- Graphics (Peter Shirley, 2nd ed) for drawing and info.
        -- We choose n = 1.
        nxFlt = fromIntegral nx
        nyFlt = fromIntegral ny
        (u, v, w) = camUVW cam
        halfHeight = tan ((camFovy cam) * pi / 360) -- tan(theta/2)
        halfWidth  = halfHeight * nxFlt / nyFlt
        us = halfWidth  * ((2*i + 1)/nxFlt - 1) -- i == 0 => left
        vs = halfHeight * (1 - (2*j + 1)/nyFlt) -- j == 0 => top
        dir = normalize $ us*.u .+. vs*.v .-. w -- ws = -n = -1


-- | Calculate the Color of the given Intersection
color :: Intersection -> RayTracer Color
color int = do
    incidentLight <- incidentDirectLight int
    foldlM (addWeightedPureColor int incidentLight) black (intMat int)
--TODO STRICT//seq?

addWeightedPureColor :: Intersection -> [IncidentLight] -> Color -> MaterialComponent -> RayTracer Color
--TODO, make this strict in its argument, and/or return strict?
addWeightedPureColor int incidentLight col (MaterialComponent (weight, pureMat)) = do
    pureCol <- colorPure int incidentLight pureMat
    return $ col  .+.  weight *. pureCol

-- | Calculate the Color of a PureMaterial under the given light at a given 
-- Intersection
colorPure :: Intersection -> [IncidentLight] -> PureMaterial -> RayTracer Color
colorPure int incidentLights pureMat@(PureMaterial matType matCol) = do
    contributions <- mapM (colorMaterialType int matType) incidentLights
    let total = foldl' (.+.) black contributions
    return $ matCol .***. total 

colorMaterialType :: Intersection -> MaterialType -> IncidentLight -> RayTracer Color
colorMaterialType int (Texture f) (ilDir, ilCol) = case intTexUV int of
    Nothing -> error "Trying to map texture on something without texture coordinates!"
    Just uv -> return $ (ilCol .***. f uv) .* (ilDir .*. (intNorm int))
colorMaterialType int Diffuse (ilDir, ilCol) =
    return $ ilCol .* (ilDir .*. (intNorm int))
colorMaterialType int (Phong p) (ilDir, ilCol) =
    return $ ilCol .* ((h .*. n) ** p)
    where
        n = intNorm int
        h = normalize $ ilDir .-. (intDir int)
colorMaterialType int Reflecting (ilDir, _) =
    black `orRecurseOn` (colorRay ray)
    where
        ray = Ray (intPos int) reflectedDir epsilon infinity (intTotDist int)
        reflectedDir = reflect (intDir int) (intNorm int)
colorMaterialType int (Glossy stdev n) (ilDir, _) =
    black `orRecurseOn` do
        dus <- getStdNorms n stdev
        dvs <- getStdNorms n stdev
        let perturbs = perturb (zip dus dvs) reflectedDir
        let directions = filter (\d -> (intNorm int) .*. d > 0) perturbs -- Only reflect *away*!
        let rays = map makeReflRay directions
        colorRays n rays
    where
        makeReflRay dir = Ray (intPos int) dir epsilon infinity (intTotDist int)
        reflectedDir = reflect (intDir int) (intNorm int)

perturb :: [(Flt, Flt)] -> UVec3 -> [UVec3]
perturb deltas vec = map perturb1 deltas
    where
        perturb1 (du, dv) = normalize $ vec .+. u.*du .+. v.*dv
        u = if (vec .*. f3e1) < 0.8
                then normalize $ vec .^. f3e1
                else normalize $ vec .^. f3e2
        v = vec .^. u

-- | Return the combined color from the rays, divided by the given factor.
-- Function is strict.
colorRays :: Int -> [Ray] -> RayTracer Color
colorRays n rays = do
    combined <- foldl' (.+.) black `fmap` mapM colorRay rays
    return $! combined ./. ((fromIntegral n)::Flt)

colorRay :: Ray -> RayTracer Color
colorRay ray = do
    scene <- getScene
    ambient <- getAmbient
    case (intersectFirst scene ray) of
        Nothing  -> return black
        Just int -> fmap (\c -> ambient .+. (attenuate (intTotDist int) c)) $ color int
        --Nothing -> return red
        --Just int -> return white

--TODO clean this up, check if correct here, merge with the attenuation for 
--propogating shadow rays -- lose the hack rescale factor
attenuate :: Flt -> Color -> Color
attenuate dist col = col ./. dist^2


-- | (direction pointing *to* the lightsource, color of incident light)
-- The direction is normalised to unity.
type IncidentLight = (UVec3, Color)

-- | Returns the incident light from the scene that's hitting the given 
-- intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight :: Intersection -> RayTracer [IncidentLight]
incidentDirectLight int = do
    lights <- getLights
    concat <$> mapM (incidentDirectLight1 int) lights

-- | Returns the incident light from the given Light that's hitting the 
-- given intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight1 :: Intersection -> Light -> RayTracer [IncidentLight]
incidentDirectLight1 int light = do
    scene <- getScene
    allRays <- spawnShadowRays light (intPos int)
    let shadowRays = filter correctSide allRays
    return $ mapMaybe (propagateShadowRay scene light) shadowRays
    where
        correctSide ray = (rayDir ray) .*. (intNorm int) > 0
    
-- | Spawn Rays from the given point to the given light.
spawnShadowRays :: Light -> Pt3 -> RayTracer [Ray]
spawnShadowRays (Light lightType _) point = spawnShadowRaysFromType lightType point

spawnShadowRaysFromType :: LightType -> Pt3 -> RayTracer [Ray]
spawnShadowRaysFromType (PointSource lightPos) point = return [ray]
    where
        diff = lightPos .-. point
        distance = len diff
        direction = diff ./. distance
        ray = Ray point direction epsilon distance 0
spawnShadowRaysFromType (Softbox origin dir1 dir2 n) point = do
    rand1s <- getRandomRs n (0, 1)
    rand2s <- getRandomRs n (0, 1)
    let step1s = map (dir1 .*) rand1s
    let step2s = map (dir2 .*) rand2s
    let positions = map (origin .+.) $ zipWith (.+.) step1s step2s
    concat <$> sequence [spawnShadowRaysFromType (PointSource pos) point | pos <- positions]
        
-- | Propagate the given ray from the given light through the scene. Return 
-- the resulting incident light of the lightray, or Nothing if it is 
-- blocked.
propagateShadowRay :: AnyIntersectable -> Light -> Ray -> Maybe IncidentLight
propagateShadowRay scene light ray =
    case (intersect ray scene) of
        [] -> Just (normalize (rayDir ray), color)
        _  -> Nothing
    where
        color = scaledLightColor light

-- scale to account for multiple lightrays -- TODO: make this nicer
scaledLightColor :: Light -> Color
scaledLightColor (Light (Softbox _ _ _ n) col) = col .* (1 / fromIntegral n)
scaledLightColor (Light _ col) = col

rayTrace :: Pixel -> RayTracer Color
rayTrace pixel = do
    resetDepth
    num <- getAAsamples
    cameraRays pixel >>= colorRays num

-- vim: expandtab smarttab sw=4 ts=4
