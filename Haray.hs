{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveFunctor, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification #-}

module Haray where
--module Haray (RayTraceConfig (..), RayTracer, rayTrace, optimizeTriangleMesh, optimizeTriangleMeshFast) where

-- TODO clean this mess, export more from these modules so I have to import 
-- less here
import Scene
import Intersection
import Camera
import Object
import Light
import Math
import Renderer
import BVH


import Data.List hiding (transpose, intersect)
import Data.Maybe
import System.Random

import Control.Monad.State hiding (state)
import Control.Applicative
import GHC.Exts
import Control.Parallel.Strategies

import Prelude hiding (concatMap)


-- TODO read up on build/foldr/fusion and use it!


data RayTraceConfig = RayTraceConfig {
        confDepth     :: Int,
        confAAsamples :: Int,
        confStdGen    :: StdGen,
        confRes       :: Resolution,
        confCam       :: Camera
    } deriving Show


type ObjIntersectable = AnyIntersectable Object

data RayTraceState = RayTraceState {
        stateScene     :: ObjIntersectable,
        stateLights    :: [Light],
        stateDepth     :: Int,
        stateAAsamples :: Int,
        stateRndGen    :: StdGen,
        stateRes       :: Resolution,
        stateCam       :: Camera,
        stateMaxDepth  :: Int
    } deriving Show

mkInitialState :: Scene -> RayTraceConfig -> RayTraceState
mkInitialState scene conf =
    RayTraceState internalScene lights depth aa rndGen res cam depth
    where
        depth = confDepth conf
        aa = confAAsamples conf
        rndGen = confStdGen conf
        res = confRes conf
        cam = confCam conf
        internalScene = optimizeSceneGraph $ sGraph scene
        lights = sLights scene

newtype RayTracer a = RT (State RayTraceState a)
    deriving (Monad, Functor)

instance Renderer RayTraceConfig RayTracer where
    colorPixel = rayTrace
    getResolution = getRes
    run scene conf action = evaluate action $ mkInitialState scene conf
    renderPar = parallizeList

evaluate :: RayTracer a -> RayTraceState -> a
evaluate (RT action) state = evalState action state

-- | Fully evaluates the list in parallel.
parallizeList :: (NFData a) => [RayTracer a] -> RayTracer [a]
parallizeList xs = do
    states <- forkStates
    return $ parMap rdeepseq (\(x, s) -> evaluate x s) $ zip xs states


getState     = RT $ get
getRes       = RT $ gets stateRes
getLights    = RT $ gets stateLights
getDepth     = RT $ gets stateDepth
decDepth     = getDepth >>= (\d -> RT $ modify (\s -> s {stateDepth = d - 1}))
getAAsamples = RT $ gets stateAAsamples
getScene     = RT $ gets stateScene
getCam       = RT $ gets stateCam
resetDepth   = RT $ gets stateMaxDepth >>= \d -> modify (\s -> s {stateDepth = d})
getRndGen    = RT $ gets stateRndGen
setRndGen new = RT $ modify (\s -> s {stateRndGen = new})



-- | Return an infinite list of raytracer states that are a copy of the 
-- current one, but each one has a new random generator seed
forkStates :: RayTracer [RayTraceState]
forkStates = do
    currentState <- getState
    let generators = splitGenerators $ stateRndGen currentState
    setRndGen $ head generators
    return [currentState {stateRndGen = g} | g <- tail generators]

-- | Return an infinite list of random generators that were split off from 
-- the given one.
splitGenerators :: RandomGen g => g -> [g]
splitGenerators g = g1 : splitGenerators g2
    where (g1, g2) = split g


-- | Optimize the given scenegraph for raytracing.
optimizeSceneGraph :: SceneGraph -> ObjIntersectable
optimizeSceneGraph = MkAnyI11e . (buildBVH 1) . flattenSceneGraph


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


-- | Find the closest intersection of the ray with an intersectable. 
intersectFirst :: ObjIntersectable -> Ray -> Maybe ObjIntersection
intersectFirst intersectable ray =
    case intersect ray intersectable of
        []   -> Nothing
        ints -> Just $ minimum ints

-- | Stratified sampling
sampleStratified :: Int -> Int -> RayTracer [(Flt, Flt)]
sampleStratified ni nj =
    mapM jitter grid
    where
        stepi = 1 / (fromIntegral ni)
        stepj = 1 / (fromIntegral nj)
        grid = concat [[(i,j) | i <- steppedSequence ni stepi]
                              | j <- steppedSequence nj stepj]
        jitter (i,j) = do
            di <- getRandomR (0, stepi)
            dj <- getRandomR (0, stepj)
            return (i+di, j+dj)

-- | Returns step * [n-1 .. 0]
steppedSequence :: Int -> Flt -> [Flt]
steppedSequence 0 _      = []
steppedSequence num step = steppedSequence' (num-1) [0]
    where
        steppedSequence' 0 xs     = xs
        steppedSequence' n (x:xs) = steppedSequence' (n-1) (x+step:x:xs)
        steppedSequence' _ []     = error "Pigs can fly."

cameraRays :: Pixel -> RayTracer [Ray]
cameraRays pixel = do
    res <- getRes
    cam <- getCam
    num <- getAAsamples
    let pixPt = pixToPt pixel
    if num <= 1
        then return $ [cameraRay res cam pixPt]
        else do
            rs <- sampleStratified num num
            let pts = map (\(r1,r2) -> pixPt .+. (F2 (r1-0.5) (r2-0.5))) rs
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





--colorRays = colorRaysSerial
colorRays = colorRaysPar

-- | Return the combined color from the rays, divided by the given factor. 
-- Contributions from individiual rays are calculated in parallel when 
-- possible.
colorRaysPar :: Int -> [Ray] -> RayTracer Color
colorRaysPar n rays = do
    states <- forkStates
    --let contributions = parMap rdeepseq (\(r, s) -> evaluate (colorRay r) s) $ zip rays states
    let contributions = parMap rdeepseq (\(r, s) -> evaluate (colorRay r) s) $ zip rays states
    {-
    --equivalently:
    let colorActions = map colorray rays
    let contributions = parallizeList colorActions
    -}
    let combined = foldl (.+.) black contributions -- not foldl': would kill sparks!
    return $ combined ./. ((fromIntegral n)::Flt)


colorRaysSerial :: Int -> [Ray] -> RayTracer Color
colorRaysSerial n rays = do
    colors <- mapM colorRay rays
    let combined = foldl' (.+.) black colors
    return $ combined ./. ((fromIntegral n)::Flt)


-- | Compute the color of the given ray.
colorRay :: Ray -> RayTracer Color
colorRay ray = do
    scene <- getScene
    case (intersectFirst scene ray) of
        Nothing  -> return black
        Just int -> color int

-- | Compute the color of the given ray, after transforming its normal color with the given function.
transformColorRay :: (ObjIntersection -> Color -> Color) -> Ray -> RayTracer Color
transformColorRay weightFunction ray = do
    scene <- getScene
    case (intersectFirst scene ray) of
        Nothing  -> return black
        Just int -> weightFunction int <$> color int

-- | Calculate the Color of the given ObjIntersection
color :: ObjIntersection -> RayTracer Color
color int = do
    incidentLight <- incidentDirectLight int -- TODO is this sufficiently lazy (at all??)
    colorMaterial int (intMat int) incidentLight


-- | Returns the incident light from the scene that's hitting the given 
-- intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight :: ObjIntersection -> RayTracer [IncidentLight]
incidentDirectLight int = do
    lights <- getLights
    concat <$> mapM (incidentDirectLight1 int) lights

-- | Returns the incident light from the given Light that's hitting the 
-- given intersection point from the 'correct' side (as determined by the 
-- normal).
incidentDirectLight1 :: ObjIntersection -> Light -> RayTracer [IncidentLight]
incidentDirectLight1 int light = do
    scene <- getScene
    allRays <- spawnShadowRays light (intPos int)
    let shadowRays = filter correctSide allRays
    return $ mapMaybe (propagateShadowRay scene light) shadowRays
    where
        correctSide (_, ray) = (rayDir ray) .*. (intNorm int) > 0

{-
 - TODO 
 - This needs to be tidied up *completely*! (haven't bothered with making 
 - light a seperate module/class yet, as I'll redesign it anyway -- just 
 - lacking the time right now...)
 -
 - I'll define a material 'emitting' that completely unifies lightsources 
 - with objects.
 -
 - This means that a LIGHT SOURCE CAN HAVE ANY ARBITRARY GEOMETRY (a teapot 
 - shaped light!).
 - That will need a function defined for geometries that spaws rays in 
 - their direction (this can easily be done if they have a bounding sphere 
 - -> then it's just shooting random rays at a disk, no matter what the 
 - relative positions are!).
 -
 - This will, in return, make it very easy to get INTRICATE GLOBAL 
 - LIGHTING! CAUSTICS/FULL PROJECTIONS(!) can simply be done by letting the 
 - object on which we want to project spawn 'shadow' rays to the dielectric 
 - object (using the framework to spawn lightrays to a geometry/object that 
 - represents a lightsource, but this time use it to spawn lightrays to a 
 - geometry/object that represents a lens -- remember that the only 
 - distinction between a 'lightsource' and any other object will be that 
 - the lightsource has an 'emitting' material (component)!).
 -
 - This will, in turn, automatically allow fancy effects 'out of the box' 
 - such as a CAMERA OBSCURA, and true rendering of DEPTH OF FIELD through 
 - actual lenses made out of dielectric material!
 - Even SPHERICAL ABBERATIONS etc will be correctly rendered through a 
 - (spherical) lens -- of course!
 -}

-- | Spawn Rays from the given point to the given light. Returns a tuple of 
-- the shadow ray and its associated weight.
spawnShadowRays :: Light -> Pt3 -> RayTracer [(Flt, Ray)]
spawnShadowRays (Light lType _) point = spawnShadowRaysFromType lType point

spawnShadowRaysFromType :: LightType -> Pt3 -> RayTracer [(Flt, Ray)]
spawnShadowRaysFromType (PointSource lightPos) point =
    return [(1 / distance^2, ray)]
    where
        diff = lightPos .-. point
        distance = len diff
        dir = diff ./. distance
        ray = Ray point dir epsilon distance 0
spawnShadowRaysFromType (SoftBox origin dir1 dir2 normal n1 n2) point = do
    rands <- sampleStratified n1 n2
    let positions = map (\(r1, r2) -> origin .+. dir1.*r1 .+. dir2.*r2) rands
    concat <$> sequence [shadowRaysFrom pos | pos <- positions]
    where
        shadowRaysFrom pt = mapMaybe rescaleRay <$> spawnShadowRaysFromType (PointSource pt) point
        rescaleRay (weight, ray)
            | cosTheta < 0 = Nothing
            | otherwise    = Just (weight / (fromIntegral $ n1 * n2) * cosTheta, ray)
            where
                cosTheta = -(normal .*. normalize (rayDir ray))

-- | Propagate the given ray from the given light through the scene. Return 
-- the resulting incident light of the lightray, or Nothing if it is 
-- blocked.
propagateShadowRay :: ObjIntersectable -> Light -> (Flt, Ray) -> Maybe IncidentLight
propagateShadowRay scene light (weight, ray) =
    case (intersect ray scene)::[Intersection Object] of
        [] -> Just (normalize (rayDir ray), weight *. (lightColor light))
        _  -> Nothing


--rayTrace :: Pixel -> RayTracer Color
rayTrace pixel = do
    resetDepth
    num <- getAAsamples
    cameraRays pixel >>= colorRays (num*num)


-- vim: expandtab smarttab sw=4 ts=4
