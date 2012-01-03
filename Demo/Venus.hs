import Haray

import Math
import Scene
import Object
import ObjParser 
import Material.Diffuse
import Material.Phong
import Material.Glossy
import Material.Texture
import Material.Ambient
import Geometry.Triangles
import Geometry.Plane
import Geometry.Box
import Transform
import Camera
import Light
import Renderer

import OutputSDL
import OutputPPM

import System.Directory
import System.Random

dir = "/home/roald/source/haren/Demo"
outfile = "venus.ppm"

main = do
    setCurrentDirectory dir
    venusMesh <- parseObjFile "../obj/venus.obj"
    let venusOptim = optimizeTriangleMesh 5 venusMesh
    gen <- getStdGen
    renderPPM 100 outfile (mkScene venusOptim) $ mkConf gen
    --renderSDL PerLine (mkScene venusOptim) $ mkConf gen

mkScene venusGeom = scene
    where
        matDiff   = mkDiffuse
        matPhong  = mkPhong 20
        matGlossy = mkGlossy 0.05 nGlossy

        groundMat = colorMat (Color 0.01 0.2 0.01) matDiff
        groundGeom = MkAnyGeom $ mkPlane (F3 (-100) 0 (100)) (F3 200 0 0) (F3 0 0 (-1000)) f3e2
        ground = Object groundGeom groundMat
        
        venusMat = combineMats [matDiff, matPhong]
        venus = Object venusGeom venusMat

        pedestalMat = combineMats [scaleMat 0.5 matGlossy, matDiff]
        pedestal = Object (mkBox (F3 (-1.9) (-100) (-2.2)) (F3 1.9 (-2.7) 2.5)) pedestalMat

        objs = Fork [Node (Scale 0.5) (Leaf venus)
                    --,Leaf ground
                    ,Leaf pedestal
                    ]


        --key   = Light (mkSoftBox (F3 (8) 0 5) (F3 (2) 0 (0)) (F3 0 6 0) f3zero n) (5 *. white)
        key   = Light (mkSoftBox (F3 (8) (-1) 5) (F3 (2) 0 (0)) (F3 0 6 0) f3zero n) (9 *. white)
        --fillfg = Light (mkSoftBox (F3 (-4) (-3) (5)) (F3 (6) 0 (0)) (F3 0 4 0.1) (F3 0 0 (-100)) n) (2 *. white)
        fillfg = Light (mkSoftBox (F3 (-4) (-0) (6)) (F3 (2) 0 (0)) (F3 0 5 0.1) (F3 0 0 (-100)) n) (0.6 *. white)
        fillped = Light (mkSoftBox (F3 (-3) (-6) (5)) (F3 (5) 0 (0)) (F3 0 2 0.1) (F3 0 0 (-100)) n) (0.4 *. white)
        fillbg = Light (mkSoftBox (F3 (-10) 5 (-2)) (F3 (20) 0 (0)) (F3 0 6 (-1)) (F3 0 0 (-100)) n) (30 *. white)
        strip = Light (mkSoftBox (F3 (-2.5) 0.2 5) (F3 (0.1) 0 (0.1)) (F3 0 2 0) f3zero n) (0.35 *. white)
        riml  = Light (mkSoftBox (F3 (-21) 0.1 (-13)) (F3 0 9 0) (F3 8 0 (0)) f3zero n) (50 *. white)
        rimr  = Light (mkSoftBox (F3 (20) 0.2 (-12)) (F3 0 8 0) (F3 (-4) 0 (-3)) f3zero n) (40 *. white)
        top   = Light (mkSoftBox (F3 (4) 7 (-7)) (F3 1 0 0) (F3 0 1 0) f3zero n) (15 *. white)
        top2  = Light (mkSoftBox (F3 (-4) 7 (-7)) (F3 1 0 0) (F3 0 1 0) f3zero n) (30 *. white)
        --lights = [key, strip, fillbg, fillfg, rimr, riml, top, top2]
        lights = [key, rimr, riml, fillped, fillfg]
        scene = Scene lights objs

--{-
n              = 2
nGlossy        = 20
recursionDepth = 2
aaSamples      = 3
---}

{-
n              = 1
nGlossy        = 1
recursionDepth = 2
aaSamples      = 1
-}


res = Resolution (400, 800)
--res = Resolution (200, 150)
cam = camLookingAt (F3 0 1.2 (15)) (F3 0 (-0.3) 0) f3e2 38

mkConf stdgen = RayTraceConfig recursionDepth aaSamples stdgen res cam
-- vim: expandtab smarttab sw=4 ts=4
