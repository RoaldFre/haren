import Haray

import Types
import Math
import ObjParser 

import OutputSDL
import OutputPPM

main = do
    mesh <- parseObjFile "teapot.obj"
    let optimMesh = optimizeTriangleMesh 1 mesh
    renderPPM "out.ppm" (testScene optimMesh) testConf
    --renderSDL PerLine (testScene optimMesh) testConf

testScene anyGeom = scene
    where
        pmDiff  = PureMaterial Diffuse white
        pmPhong = PureMaterial (Phong 50) white
        pmRefl  =  PureMaterial Reflecting white
        mat = [MaterialComponent (  1, pmDiff),
               MaterialComponent (  1, pmPhong)]
               --MaterialComponent (0.1, pmRefl)]

        planeGeom = MkAnyGeom $ mkPlane (F3 (-10) 0 (-10)) (F3 20 0 0) (F3 0 0 20)
        planeMat =  [MaterialComponent (0.8, pmDiff)
                    ,MaterialComponent (0.1, pmPhong)
                    ]
                    --MaterialComponent (0.5, pmRefl)]
        plane = Object planeGeom planeMat

        objs = Fork [Node Identity (Leaf (Object anyGeom mat))
                    ,Node Identity (Leaf plane)]


        n = 30
        lights = [Light (Softbox (F3 12 6 5) (F3 (-4) 0 (4)) (F3 0 2 0) n) (15.5 *. white)
                 ,Light (Softbox (F3 (-10) 0 (-5)) (F3 (2) 4 (-2)) (F3 5 0 2) n) (100.0 *. white)
                 ,Light (Softbox (F3 (4) 10 (-5)) (F3 (1) 0 (1)) (F3 (-0.5) 1 0.5) n) (10.3 *. white)]
        scene = Scene lights objs

testConf = RayTraceConfig 5 0 res cam (0.1*.white)
    where
        res = Resolution (400, 400)
        cam = camLookingAt (F3 0 2.2 (15)) (F3 0 1 0) f3e2 30


-- vim: expandtab smarttab sw=4 ts=4
