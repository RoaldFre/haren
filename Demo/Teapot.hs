import Haray

import Math
import Scene
import Object
import ObjParser 
import Material.Diffuse
import Material.Phong
import Material.Glossy
import Material.Texture
import Geometry.Triangles
import Geometry.Plane
import Transform
import Camera
import Light
import Renderer

import OutputSDL
import OutputPPM

import System.Directory
import System.Random

dir = "/home/roald/source/haren/Demo"
outfile = "teapot.ppm"

main = do
    setCurrentDirectory dir
    mesh <- parseObjFile "../obj/teapot.obj"
    let optimMesh = optimizeTriangleMesh 5 mesh
    gen <- getStdGen
    renderPPM outfile (mkScene optimMesh) $ mkConf gen
    --renderSDL PerLine (mkScene optimMesh) $ mkConf gen

mkScene anyGeom = scene
    where
        matDiff   = mkDiffuse
        matPhong  = mkPhong 35
        matGlossyTp    = scaleMat 0.5 $ mkGlossy 0.07 nGlossyTp
        matGlossyPlane = scaleMat 0.4 $ mkGlossy 0.05 nGlossyPlane
        matTexture = mkTexture mkDiffuse $ checkers (0.2 *. white) (1.5 *. white) 30 1500

        tpmat = combineMats [matDiff, matPhong, matGlossyTp]
        planeMat = combineMats [matTexture]

        planeGeom = MkAnyGeom $ mkPlane (F3 (-10) 0 (2)) (F3 20 0 0) (F3 0 0 (-1000)) f3e2
        plane = Object planeGeom planeMat

        objs = Fork [Node (Scale 0.5) (Leaf (Object anyGeom tpmat))
                    ,Node Identity (Leaf plane)]


        key   = Light (mkSoftBox (F3 (12) 0.2 8) (F3 (3) 0 (0)) (F3 0 7 0) f3zero n) (11 *. white)
        fillbg = Light (mkSoftBox (F3 (-10) 5 (-2)) (F3 (20) 0 (0)) (F3 0 6 (-1)) (F3 0 0 (-100)) n) (30 *. white)
        fillfg = Light (mkSoftBox (F3 (-10) 0.2 (10)) (F3 (20) 0 (0)) (F3 0 1 (-0.1)) (F3 0 0 (-100)) n) (4 *. white)
        strip = Light (mkSoftBox (F3 (-2.5) 0.2 5) (F3 (0.1) 0 (0.1)) (F3 0 2 0) f3zero n) (0.35 *. white)
        riml  = Light (mkSoftBox (F3 (-21) 0.4 (-16)) (F3 0 1 0) (F3 4 0 (0)) f3zero n) (100 *. white)
        rimr  = Light (mkSoftBox (F3 (20) 0.5 (-12)) (F3 0 1 0) (F3 (-3) 0 (-2)) f3zero n) (60 *. white)
        top   = Light (mkSoftBox (F3 (4) 7 (-7)) (F3 1 0 0) (F3 0 1 0) f3zero n) (17 *. white)
        top2  = Light (mkSoftBox (F3 (-4) 7 (-7)) (F3 1 0 0) (F3 0 1 0) f3zero n) (35 *. white)
        --lights = [key, strip, fillbg, fillfg, rimr, riml, top, top2]
        lights = [key, strip, fillfg, rimr, riml, top, top2]
        scene = Scene lights objs

--{-
n              = 3
nGlossyTp      = 15
nGlossyPlane   = 10
recursionDepth = 2
aaSamples      = 3
---}

{-
n              = 1
nGlossyTp      = 1
nGlossyPlane   = 1
recursionDepth = 2
aaSamples      = 1
-}


res = Resolution (400, 300)
--res = Resolution (200, 150)
cam = camLookingAt (F3 0 2.1 (5)) (F3 0 0.47 0) f3e2 35

mkConf stdgen = RayTraceConfig recursionDepth aaSamples stdgen res cam
-- vim: expandtab smarttab sw=4 ts=4
