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
import Transform
import BVH
import Color
import Boxes

import Material


-- TODO which can be removed?
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


-- TODO name && ...something like AnyIntersectable "class"?
type ObjIntersectable = AnyIntersectable Object

-- TODO: make (some of) these strict?
data RayTraceState = RayTraceState {
        stateScene     :: ObjIntersectable,
        stateLights    :: [Light],
        stateAmbient   :: Color,
        stateDepth     :: Int,
        stateAAsamples :: Int,
        stateRndGen    :: StdGen, -- ^ Random number generator
        stateRes       :: Resolution,
        stateCam       :: Camera,
        stateMaxDepth  :: Int
    } deriving Show

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

-- TODO declared as newtype here, as data in bootfile ... ok?
newtype RayTracer a = RT (State RayTraceState a)
    deriving (Monad, Functor)

instance Renderer RayTraceConfig RayTracer where
    colorPixel = rayTrace
    getResolution = getRes
    run scene conf (RT s) = evalState s (mkInitialState scene conf)

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












-- TODO best to define this here, or somethere else?
type TransformedObj = Transformed Object

flattenSceneGraph :: SceneGraph -> [TransformedObj]
flattenSceneGraph sceneGraph = 
    map mkTransObj $ flattenObjectGraph multTuples (m4id, m4id) matricesGraph
    where
        mkTransObj ((m,mInv), obj) = Transformed m mInv obj
        multTuples (a, b) (x, y) = (a .*. x, b .*. y)
        matricesGraph = transfoM4s `fmap` sceneGraph










{-
sceneGraphToInternalStructure :: SceneGraph -> SceneStructure
sceneGraphToInternalStructure = (buildBVH 1) . flattenSceneGraph
-}

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
--
-- TODO this isn't true any more:
-- Only intersections where the ray enters the objects from the outside (as 
-- determined by the normal) are considered.
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

cameraRays :: Pixel -> RayTracer [Ray]
cameraRays pixel = do
    res <- getRes -- TODO: getResolution from Renderer?
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


-- | Calculate the Color of the given ObjIntersection
color :: ObjIntersection -> RayTracer Color
color int = do
    incidentLight <- incidentDirectLight int
    colorMaterial int (intMat int) incidentLight


-- | Return the combined color from the rays, divided by the given factor.
colorRays :: Int -> [Ray] -> RayTracer Color
colorRays n rays = do
    combined <- foldl' (.+.) black `fmap` mapM colorRay rays
    return $ combined ./. ((fromIntegral n)::Flt)

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
--propagating shadow rays -- lose the hack rescale factor
attenuate :: Flt -> Color -> Color
attenuate dist col = col ./. dist^2


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
spawnShadowRaysFromType (SoftBox origin dir1 dir2 n1 n2) point = do
    rands <- sampleStratified n1 n2
    let positions = map (\(r1, r2) -> origin .+. dir1.*r1 .+. dir2.*r2) rands
    concat <$> sequence [spawnShadowRaysFromType (PointSource pos) point | pos <- positions]
        
-- | Propagate the given ray from the given light through the scene. Return 
-- the resulting incident light of the lightray, or Nothing if it is 
-- blocked.
propagateShadowRay :: ObjIntersectable -> Light -> Ray -> Maybe IncidentLight
propagateShadowRay scene light ray =
    case (intersect ray scene)::[Intersection Object] of -- TODO better way?
        [] -> Just (normalize (rayDir ray), color)
        _  -> Nothing
    where
        color = scaledLightColor light

-- scale to account for multiple lightrays -- TODO: make this nicer
scaledLightColor :: Light -> Color
scaledLightColor (Light (SoftBox _ _ _ n1 n2) col) = col .* (1 / fromIntegral (n1 * n2))
scaledLightColor (Light _ col) = col

--rayTrace :: Pixel -> RayTracer Color
rayTrace pixel = do
    resetDepth
    num <- getAAsamples
    cameraRays pixel >>= colorRays (num*num)



-- vim: expandtab smarttab sw=4 ts=4
