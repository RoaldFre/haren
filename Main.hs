import Haray

import Math
import Scene
import Object
import ObjParser 
import Materials
import Geometry.Triangles
import Geometry.Plane
import Transform
import Camera
import Light
import Renderer

import OutputSDL
--import OutputPPM

main = do
    mesh <- parseObjFile "teapot.obj"
    --let optimMesh = optimizeTriangleMesh 10 mesh
    let optimMesh = optimizeTriangleMeshFast 10 mesh
    --renderPPM "./out.ppm" (testScene optimMesh) testConf
    --renderSDL PerLine (testScene optimMesh) testConf
    renderSDL PerPixel (testScene optimMesh) testConf

testScene anyGeom = scene
    where
        matDiff   = mkDiffuse
        matPhong  = mkPhong 25
        matRefl   = mkReflecting
        matGlossyTp = mkGlossy 0.10 nGlossyTp
        matGlossyPlane = mkGlossy 0.08 nGlossyPlane
        matTexture = mkTexture $ checkers (0.2 *. white) white 200 1000
        mat = combineMats [matDiff, matPhong]

        planeGeom = MkAnyGeom $ mkPlane (F3 (-100) 0 (100)) (F3 200 0 0) (F3 0 0 (-1000))
        planeMat = combineMats [matTexture]

        plane = Object planeGeom planeMat

        objs = Fork [Node Identity (Leaf (Object anyGeom mat))
                    ,Node Identity (Leaf plane)]


        key   = Light (mkSoftBox (F3 5 0 3) (F3 (1) 0 (0)) (F3 0 6 0) n) (40 *. white)
        strip = Light (mkSoftBox (F3 (-1.8) 0 3) (F3 (0.1) 0 (0)) (F3 0 6 0) n) (10 *. white)
        rim   = Light (mkSoftBox (F3 (-11) 0 (-5)) (F3 0 4 0) (F3 3 0 (-8)) n) (100 *. white)
        top   = Light (mkSoftBox (F3 (4) 5 (-7)) (F3 (1) 0 (1)) (F3 (-0.8) 1 0.8) n) (15 *. white)
        top2  = Light (mkSoftBox (F3 (-4) 5 (-7)) (F3 (-1) 0 (1)) (F3 (-0.8) 1 0.8) n) (40 *. white)
        --bounce = Light (Softbox (F3 (-2) (0.1) (2)) (F3 4 0 0) (F3 0 0 (-2)) n) (0.2 *. white)
        lights = [key, strip, rim, top, top2]
        scene = Scene lights objs

{-
n              = 4
nGlossyTp      = 40
nGlossyPlane   = 30
recursionDepth = 2
aaSamples      = 3
seed           = 1
-}

--{-
n              = 1
nGlossyTp      = 1
nGlossyPlane   = 1
recursionDepth = 1
aaSamples      = 1
seed           = 0
---}


testConf = RayTraceConfig recursionDepth aaSamples seed res cam (0.0*.white)
    where
        res = Resolution (200, 150)
        cam = camLookingAt (F3 0 4.4 (10)) (F3 0 0.95 0) f3e2 35

---}

-- vim: expandtab smarttab sw=4 ts=4
