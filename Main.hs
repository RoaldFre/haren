import Haray

import Types
import Math
import ObjParser 

import OutputSDL
import OutputPPM

--{-
import Haras
import OutputHaras

main = renderImage "ras.ppm" image
    where
        image = rasterizeToImage triangle conf
        triangle = Triangle (Vertex (F3   2   2 0) (F3 0 1 0))
                            (Vertex (F3   2  95 0) (F3 0 0 1))
                            (Vertex (F3  95  95 0) (F3 1 0 0))
        conf = RasterizerConfig (Resolution (100, 100)) blue

---}

{-
main = do
    mesh <- parseObjFile "teapot.obj"
    let optimMesh = optimizeTriangleMesh 10 mesh
    renderPPM "out.ppm" (testScene optimMesh) testConf
    --renderSDL PerLine (testScene optimMesh) testConf

testScene anyGeom = scene
    where
        pmDiff  = PureMaterial Diffuse white
        pmPhong = PureMaterial (Phong 50) white
        pmRefl  =  PureMaterial Reflecting white
        mat = [MaterialComponent (0.5, pmDiff),
               MaterialComponent (  2, pmPhong)]
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
        --key = Light (Softbox (F3 12 6 5) (F3 (-4) 0 (4)) (F3 0 2 0) n) (40.5 *. white)
        key = Light (Softbox (F3 5 0 3) (F3 (1) 0 (0)) (F3 0 6 0) n) (10 *. white)
        rim = Light (Softbox (F3 (-10) 0 (-5)) (F3 (2) 4 (-2)) (F3 5 0 2) n) (200.0 *. white)
        top = Light (Softbox (F3 (4) 10 (-5)) (F3 (1) 0 (1)) (F3 (-0.8) 1 0.8) n) (20.3 *. white)
        lights = [key, rim, top]
        scene = Scene lights objs

testConf = RayTraceConfig 5 0 res cam (0.1*.white)
    where
        res = Resolution (400, 400)
        cam = camLookingAt (F3 0 2.2 (15)) (F3 0 1 0) f3e2 30

-}

-- vim: expandtab smarttab sw=4 ts=4
