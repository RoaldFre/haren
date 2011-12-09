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

import qualified Data.Sequence as S

data RayTraceConfig = RayTraceConfig {
        confDepth   :: Int,
        confSeed    :: Int,
        confRes     :: Resolution, -- TODO, this and cam are shared with rasterizer, not part of raytracer per se
        confCam     :: Camera,
        confAmbient :: Color
    } deriving Show

-- TODO: make (some of) these strict?
data RayTraceState = RayTraceState {
        stateScene    :: AnyIntersectable,
        stateLights   :: [Light],
        stateAmbient  :: Color,
        stateDepth    :: Int,
        stateRndGen   :: StdGen, -- ^ Random number generator
        stateRes      :: Resolution,
        stateCam      :: Camera,
        stateMaxDepth :: Int
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
transformRay trans (Ray origin direction near far) =
    Ray (trans `multPt` origin) (trans `multVec` direction) near far

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
buildBVH n bs = buildBVH' $ S.fromList bs
    where
        buildBVH' :: (Boxable a) => S.Seq (Boxed a) -> BVH a
        buildBVH' bs
           | S.length bs <= n = BVHleaf $ toList bs
           | otherwise        = case bestPartition $ partitionBoxeds bs of
                Nothing         -> BVHleaf $ toList bs
                Just (p1, p2) -> BVHnode (buildBVH' <$> p1) (buildBVH' <$> p2)

-- | A partition of a sequence of boxeds, each part wrapped in a boxed 
-- itself. 
type BoxedPartition a = (Boxed (S.Seq (Boxed a)), Boxed (S.Seq (Boxed a)))

-- | Generate a list of possible Partitions from a sequence of boxeds. This 
-- runs in O(n log n) time with n the number of Boxeds.
partitionBoxeds :: (Boxable a) => S.Seq (Boxed a) -> [BoxedPartition a]
partitionBoxeds boxeds = map (mapPair $ mkBoxed) $
    partitionSeq sortedx ++ partitionSeq sortedy ++ partitionSeq sortedz
    where
        sortedx = S.unstableSortBy (orderingFor f3x) boxeds
        sortedy = S.unstableSortBy (orderingFor f3y) boxeds
        sortedz = S.unstableSortBy (orderingFor f3z) boxeds
        orderingFor fcoord b1 b2 = compare (fcoord $ min b1) (fcoord $ min b2)
        min boxed = boxmin where (Box boxmin boxmax) = box boxed

-- | Generate (all) partitions of the given sequence. No trivial partitions 
-- "([], xs)" are created.
-- This runs in O(n log n) time with n the length of the sequence.
partitionSeq :: S.Seq a -> [(S.Seq a, S.Seq a)]
partitionSeq xs
    | n < 2     = []
    | otherwise = map (\i -> S.splitAt i xs) [1 .. n-1]
    where n = S.length xs

{-
-- | Generate (all) partitions of the given sequence. No trivial partitions 
-- "([], xs)" are created.
-- This runs in O(n) time with n the length of the sequence, potentially 
-- with a large constant factor and GC action.
partitionSeq :: S.Seq a -> [(S.Seq a, S.Seq a)]
partitionSeq = (map $ mapPair S.fromList) . partitionList . toList

-- | No trivial partitions "([], xs)" are created.
partitionList :: [a] -> [([a], [a])]
partitionList [] = []
partitionList (x:xs) = partitionList' [x] xs
    where
        partitionList' xs []     = []
        partitionList' xs (y:ys) = (xs,(y:ys)) : partitionList' (y:xs) ys
        -}


mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

costOfPartition :: BoxedPartition a -> Flt
costOfPartition (bp1, bp2) = 
    (halfSurfaceArea $ box bp1) * (fromIntegral $ S.length $ unbox bp1)
    + (halfSurfaceArea $ box bp2) * (fromIntegral $ S.length $ unbox bp2)
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
getRes     = RT $ gets stateRes
getLights  = RT $ gets stateLights
getAmbient = RT $ gets stateAmbient
getDepth   = RT $ gets stateDepth
decDepth   = getDepth >>= (\d -> RT $ modify (\s -> s {stateDepth = d - 1}))
getScene   = RT $ gets stateScene
getCam     = RT $ gets stateCam
resetDepth = RT $ gets stateMaxDepth >>= \d -> modify (\s -> s {stateDepth = d})
getRndGen  = RT $ gets stateRndGen
setRndGen new = RT $ modify (\s -> s {stateRndGen = new})

instance Renderer RayTraceConfig RayTracer where
    colorPixel = rayTrace
    getResolution = getRes
    run scene conf (RT s) = evalState s (mkInitialState scene conf)

mkInitialState :: Scene -> RayTraceConfig -> RayTraceState
mkInitialState scene conf =
    RayTraceState internalScene lights ambient depth rndGen res cam depth
    where
        depth = confDepth conf
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
            

cameraRay :: Resolution -> Camera -> Pixel -> Ray
cameraRay (Resolution (nx, ny)) cam (Pixel (i, j)) =
    Ray (camPos cam) dir 0 infinity
    where
        -- See p164 and 203-204 of Fundamentals of Computer 
        -- Graphics (Peter Shirley, 2nd ed) for drawing and info.
        -- We choose n = 1.
        nxFlt = fromIntegral ny
        nyFlt = fromIntegral nx
        iFlt = fromIntegral i
        jFlt = fromIntegral j
        (u, v, w) = camUVW cam
        halfHeight = tan ((camFovy cam) * pi / 360) -- tan(theta/2)
        halfWidth  = halfHeight * nxFlt / nyFlt
        us = halfWidth  * ((2*iFlt + 1)/nxFlt - 1) -- i == 0 => left
        vs = halfHeight * (1 - (2*jFlt + 1)/nyFlt) -- j == 0 => top
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
colorMaterialType int Diffuse (ilDir, ilCol) =
    return $ ilCol .* (ilDir .*. (intNorm int))
colorMaterialType int (Phong p) (ilDir, ilCol) =
    return $ ilCol .* ((h .*. n) ** p)
    where
        n = intNorm int
        h = normalize $ ilDir .-. (intDir int)
colorMaterialType int Reflecting (ilDir, ilCol) =
    black `orRecurseOn` ((ilCol .***.) <$> (colorRay ray))
    where
        ray = Ray (intPos int) reflectedDir epsilon infinity
        reflectedDir = reflect (intDir int) (intNorm int)
    --TODO attenuation?


colorRay :: Ray -> RayTracer Color
colorRay ray = do
    scene <- getScene
    ambient <- getAmbient
    case (intersectFirst scene ray) of
        Nothing -> return black
        Just intersection -> fmap (ambient .+.) $ color intersection
        --Nothing -> return red
        --Just int -> return white


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
        ray = Ray point direction epsilon distance
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
        color = (scaledLightColor light) .* (attenuation (lightType light) ray)
 
attenuation :: LightType -> Ray -> Flt
attenuation (Directional _) _ = 1
attenuation _             ray = 1 / (rayFar ray)^2 -- 1/(((rayFar ray) * (rayFar ray)) faster?

-- scale to account for multiple lightrays -- TODO: make this nicer
scaledLightColor :: Light -> Color
scaledLightColor (Light (Softbox _ _ _ n) col) = col .* (1 / fromIntegral n)
scaledLightColor (Light _ col) = col

rayTrace :: Pixel -> RayTracer Color
rayTrace pixel = do
    res <- getRes -- TODO: getResolution from Renderer?
    cam <- getCam
    resetDepth
    (colorRay . (cameraRay res cam)) pixel

-- vim: expandtab smarttab sw=4 ts=4
