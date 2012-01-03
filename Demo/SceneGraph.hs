import Haray

import Math
import Scene
import Object
import ObjParser 
import Material.Diffuse
import Material.Phong
import Material.Texture
import Geometry.Triangles
import Geometry.Plane
import Geometry.Sphere
import Transform
import Camera
import Light
import Renderer

import OutputSDL
import OutputPPM

import System.Directory
import System.Random

dir = "/home/roald/source/haren/Demo"
outfile = "scenegraph.ppm"

main = do
    setCurrentDirectory dir
    mesh <- parseObjFile "../obj/teapot.obj"
    let optimMesh = optimizeTriangleMesh 10 mesh
    gen <- getStdGen
    --renderPPM outfile (testScene optimMesh) $ mkConf gen
    renderSDL PerLine (testScene optimMesh) $ mkConf gen
    --renderSDL PerPixel (testScene optimMesh) $ mkConf gen

testScene anyGeom = scene
 where
    matDiff   = mkDiffuse
    matPhong  = mkPhong 25
    matTexture = mkTexture $ checkers (0.2 *. white) white 200 1000

    mat = combineMats [matDiff, matPhong]
    planeMat = combineMats [matTexture]

    planeGeom = MkAnyGeom $ mkPlane (F3 (-100) 0 (100)) (F3 200 0 0) (F3 0 0 (-1000)) f3e2

    plane = Object planeGeom planeMat
    obj = Object mkSphere mat

    threeSpheres = Fork [Node (Translation (F3 (-1) 0 0)) (Leaf obj)
                        ,Node (Translation (F3   1  0 0)) (Leaf obj)
                        ,Node (Translation (F3   0  0 2)) (Leaf obj)]

    objs = Fork [Node Identity threeSpheres
                ,Node (Translation (F3   4   0  0) `After` ScaleXYZ 0.5 1.0 1.5) threeSpheres
                ,Node (Translation (F3 (-4)  1  0) `After` ScaleXYZ 1.0 2.0 0.5) threeSpheres
                ,Node (Translation (F3   0 (-1) 0)) (Leaf plane)]


    {-
    key   = Light (mkSoftBox (F3 (8) 0 5) (F3 (2) 0 (0)) (F3 0 6 0) f3zero n) (5 *. white)
    fillbg = Light (mkSoftBox (F3 (-10) 5 (-2)) (F3 (20) 0 (0)) (F3 0 6 (-1)) (F3 0 0 (-100)) n) (30 *. white)
    fillfg = Light (mkSoftBox (F3 (-10) 0 (10)) (F3 (20) 0 (0)) (F3 0 1 (-0.1)) (F3 0 0 (-100)) n) (3 *. white)
    strip = Light (mkSoftBox (F3 (-2.5) 0.2 5) (F3 (0.1) 0 (0.1)) (F3 0 2 0) f3zero n) (0.35 *. white)
    riml  = Light (mkSoftBox (F3 (-21) 0.1 (-16)) (F3 0 1 0) (F3 4 0 (0)) f3zero n) (80 *. white)
    rimr  = Light (mkSoftBox (F3 (20) 0.2 (-12)) (F3 0 1 0) (F3 (-3) 0 (-2)) f3zero n) (40 *. white)
    top   = Light (mkSoftBox (F3 (4) 7 (-7)) (F3 1 0 0) (F3 0 1 0) f3zero n) (15 *. white)
    top2  = Light (mkSoftBox (F3 (-4) 7 (-7)) (F3 1 0 0) (F3 0 1 0) f3zero n) (30 *. white)
    lights = [key, strip, fillbg, fillfg, rimr, riml, top, top2]
    -}
    
    lights = [Light (PointSource (F3 2 5 10)) (30 *. white)
             ,Light (PointSource (F3 (-2) 10 5)) ( 20 *. white)]
    scene = Scene lights objs

{-
n              = 4
nGlossyTp      = 40
nGlossyPlane   = 30
recursionDepth = 2
aaSamples      = 3
-}

--{-
n              = 1
nGlossyTp      = 1
nGlossyPlane   = 1
recursionDepth = 1
aaSamples      = 1
---}

res = Resolution (400, 200)
cam = camLookingAt (F3 0 (15) (15)) (F3 0 0 0) f3e2 22

mkConf stdgen = RayTraceConfig recursionDepth aaSamples stdgen res cam

-- vim: expandtab smarttab sw=4 ts=4
