{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, Rank2Types, UndecidableInstances #-}
-- TODO: UndecidableInstances was needed for the s in
-- instance (Renderer RasterizerConfig) (Rasterizer s) where

-- | Haras: a HAskell RASterizer.
module Haras where

import Math hiding (transpose)
import Types
import Renderer

import Data.Array.ST
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Maybe
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Data.List
import Control.Applicative

data RasterizerConfig = RasterizerConfig {
        confRes     :: Resolution, -- TODO, this and cam are shared with rasterizer, not part of raytracer per se
        --confCam     :: Camera,
        confAmbient :: Color
    } deriving Show

data ColorChannel = Red | Green | Blue
    deriving (Show, Ord, Eq, Ix)

type Raster s  = STUArray s (Pixel, ColorChannel) Flt
type Zbuffer s = STUArray s Pixel Flt

data RasterizerState s = RasterizerState {
        stateRes     :: Resolution,
        stateAmbient :: Color,
        stateLights  :: [Light],
        stateRaster  :: (Raster s),
        stateZbuf    :: (Zbuffer s)
}
getRes     = RZ $ gets stateRes
getAmbient = RZ $ gets stateAmbient
getLights  = RZ $ gets stateLights
getRaster  = RZ $ gets stateRaster
getZbuf    = RZ $ gets stateZbuf


--type ObjParser s a = ParsecT BS.ByteString (ObjParserState s) (ST s) a
newtype Rasterizer s a = RZ (StateT (RasterizerState s) (ST s) a)
    deriving (Monad, Functor)
--getRes     = RT $ gets stateRes

--instance Renderer RayTraceConfig RayTracer where

{-
instance RendererST RasterizerConfig (Rasterizer s) s where
    --colorPixel = rasterize
    colorPixel_ = undefined
    getResolution_ = getRes
    run_ = runRasterizer

-- SOOOOOOO CLOSE!!!
runRasterizer scene conf (RZ computation) = runST $ do
    image   <- newArray ((Pixel (0, 0), Red), (Pixel (nx-1, ny-1), Blue)) 0 -- TODO 'Red' & 'Blue' .....
    zbuffer <- newArray (Pixel (0,0), Pixel (nx-1, ny-1)) 0
    let initialState = RasterizerState res ambient lights image zbuffer
    evalStateT computation initialState
    where
        res@(Resolution (nx, ny)) = confRes conf
        ambient = confAmbient conf
        lights  = sLights scene
-}

type Image = UArray (Pixel, ColorChannel) Flt

rasterizeToImage :: Triangle -> RasterizerConfig -> Image
rasterizeToImage scene conf = runSTUArray $ do
    raster <- newArray ((Pixel (0, 0), Red), (Pixel (nx-1, ny-1), Blue)) 0 -- TODO 'Red' & 'Blue' .....
    zbuffer <- newArray (Pixel (0,0), Pixel (nx-1, ny-1)) 0
    let initialState = RasterizerState res ambient lights raster zbuffer
    evalStateT (fromRZ $ rasterize scene) initialState
    return raster
    where
        res@(Resolution (nx, ny)) = confRes conf
        ambient = confAmbient conf
        --lights  = sLights scene
        lights  = [Light (PointSource (F3 1 1 (-1))) white] -- TODO
        fromRZ (RZ computation) = computation

-- Rasterize all the triangles in the scene to the raster
rasterize :: Triangle -> Rasterizer s ()
rasterize scene = do
    rasterizeTriangle [] scene


-- | (direction pointing *to* the lightsource, color of incident light)
-- The direction is normalised to unity.
type IncidentLight = (UVec3, Color)

data RasterVertex = RasterVertex {
        rvPos   :: Pt2,            -- ^ Position in screen space
        rvDepth :: Flt,            -- ^ Z coordinate
        rvNorm  :: UVec3,          -- ^ Normal (in original world space)
        rvIL    :: [IncidentLight] -- ^ Incident light (world space)
    }

data RasterTriangle = RasterTriangle RasterVertex RasterVertex RasterVertex




rasterizeTriangle :: Material -> Triangle -> Rasterizer s ()
rasterizeTriangle mat triangle@(Triangle v1 v2 v3) = do
    [rv1, rv2, rv3] <- mapM vertexShader [v1, v2, v3]
    amb <- getAmbient
    forM_ pixAndBaryCoords (\(p, c) -> do
        writeColor p $ pixelShader amb (RasterTriangle rv1 rv2 rv3) mat c)
    where
        [p1, p2, p3] = map (from3Dto2D . vPos) [v1, v2, v3]
        pixels = possiblePixels p1 p2 p3
        pixAndBaryCoords = mapMaybe pixAndBaryCoord pixels
        dAlpha = distToLine (p2, p3) p1
        dBeta  = distToLine (p3, p1) p2
        dGamma = distToLine (p1, p2) p3
        pixAndBaryCoord pixel
            | alpha < 0  ||  beta < 0  ||  gamma < 0 = Nothing
            | alpha < epsilon  &&  dAlpha * (distToLine (p2, p3) osp) < 0 = Nothing
            | beta  < epsilon  &&  dBeta  * (distToLine (p3, p1) osp) < 0 = Nothing
            | gamma < epsilon  &&  dGamma * (distToLine (p1, p2) osp) < 0 = Nothing
            | otherwise = Just (pixel, (alpha, beta, gamma))
         where
            alpha = (distToLine (p2, p3) (pixToPt pixel)) / dAlpha
            beta  = (distToLine (p3, p1) (pixToPt pixel)) / dBeta
            gamma = (distToLine (p1, p2) (pixToPt pixel)) / dGamma
            osp = F2 (-1) (-1) -- off screen point

{-
writeColor :: Pixel -> Color -> Rasterizer s ()
writeColor pixel color = do
    raster <- getRaster
    RZ $ lift $ writeArray image (pixel, Red)   $ cRed   color
    RZ $ lift $ writeArray image (pixel, Green) $ cGreen color
    RZ $ lift $ writeArray image (pixel, Blue)  $ cBlue  color
-}

writeColor :: Pixel -> Color -> Rasterizer s ()
writeColor pixel color = do
    raster <- getRaster
    RZ $ lift $ writeColorST pixel color raster

writeColorST :: Pixel -> Color -> Raster s -> ST s ()
writeColorST pixel color raster = do
    writeArray raster (pixel, Red)   $ cRed   color
    writeArray raster (pixel, Green) $ cGreen color
    writeArray raster (pixel, Blue)  $ cBlue  color
        

-- | Returns the pixels that are within the bounding rectangle of the 
-- triangle with the given vertex points.
possiblePixels :: Pt2 -> Pt2 -> Pt2 -> [Pixel]
possiblePixels p1 p2 p3 =
    map Pixel $ range ((xfloor, yfloor), (xceil, yceil))
    where
        [xs, ys] = transpose $ map tupleToList [p1, p2, p3]
        [xfloor, yfloor] = map (floor . minimum)   [xs, ys]
        [xceil,  yceil]  = map (ceiling . maximum) [xs, ys]

-- | A measure of the distance of given point to the line formed by given 
-- pair of points (NOTE: not normalized to 'proper' distance).
distToLine :: (Pt2, Pt2) -> Pt2 -> Flt
distToLine ((F2 x1 y1), (F2 x2 y2)) (F2 x y) =
    x*(y1 - y2) - y*(x1 - x2) + x1*y2 - x2*y1
 




-- | Take vertex from world space and transform it to a RasterVertex.
vertexShader :: Vertex -> Rasterizer s RasterVertex
vertexShader vertex = do
    lights <- incidentLights pos
    return $ RasterVertex (F2 x y) z (vNorm vertex) lights
    where pos@(F3 x y z) = vPos vertex

incidentLights :: Pt3 -> Rasterizer s [IncidentLight]
incidentLights point = do
    lights <- getLights
    let posCols = map getPosCol lights
    return $ map (\(p,c) -> (direction point p, c)) posCols
    where
        getPosCol (Light (PointSource p) c) = (p,c)
        getPosCol _ = error "Rasterizer only supports point light sources!"


-- | Calculate the color for a pixel with the given barycentric coordinates 
-- within the given triangle composed of the given material, under the 
-- given ambient color.
pixelShader :: Color -> RasterTriangle -> Material -> (Flt, Flt, Flt) -> Color
pixelShader ambient (RasterTriangle v1 v2 v3) mat (a, b, c) =
--    white
    shading ambient mat norm incidentLight
    where
        norm = interpolate (rvNorm v1) (rvNorm v2) (rvNorm v3)
        incidentLight = zipWith3 (\(d1,c1) (d2,c2) (d3,c3) -> 
                            (interpolate d1 d2 d3, interpolate c1 c2 c3)) 
                            (rvIL v1) (rvIL v2) (rvIL v3)
        interpolate t1 t2 t3 = t1 .* a  .+.  t2 .* b  .+.  t3 .* c

shading :: Color -> Material -> UVec3 -> [IncidentLight] -> Color
shading ambient material normal incidentLight = 
    foldl' addWeightedPureColor ambient material
    where 
        addWeightedPureColor col (MaterialComponent (weight, pureMat)) =
            col  .+.  weight *. (colorPure normal incidentLight pureMat)

-- | Calculate the Color of a PureMaterial under the given light for the 
-- given normal.
colorPure :: UVec3 -> [IncidentLight] -> PureMaterial -> Color
colorPure norm incidentLights pureMat@(PureMaterial matType matCol) = 
    matCol .***. total 
    where
        total = foldl' (.+.) black contributions
        contributions = map (colorMaterialType norm matType) incidentLights

colorMaterialType :: UVec3 -> MaterialType -> IncidentLight -> Color
colorMaterialType norm Diffuse (ilDir, ilCol) = ilCol .* (ilDir .*. norm)
colorMaterialType _ _ _ = error "Rasterizer only supports diffuse shading!"






-- vim: expandtab smarttab sw=4 ts=4
