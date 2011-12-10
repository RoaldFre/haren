import Haray

import Types
import Math
import ObjParser 

import OutputSDL
import OutputPPM

{--
import Haras
import OutputHaras

main = renderImage "ras.ppm" image
    where
        image = rasterizeToImage triangle conf
        triangle = Triangle (Vertex (F3   2   2 0) (F3 0 1 0))
                            (Vertex (F3   2  95 0) (F3 0 0 1))
                            (Vertex (F3  95  95 0) (F3 1 0 0))
        conf = RasterizerConfig (Resolution (100, 100)) black

-}

--{-
main = do
    mesh <- parseObjFile "teapot.obj"
    --let optimMesh = optimizeTriangleMesh 10 mesh
    let optimMesh = optimizeTriangleMesh 10 mesh
    --renderPPM "./out.ppm" (testScene optimMesh) testConf
    renderSDL PerLine (testScene optimMesh) testConf

testScene anyGeom = scene
    where
        pmDiff  = PureMaterial Diffuse white
        pmPhong = PureMaterial (Phong 25) white
        pmRefl  =  PureMaterial Reflecting white
        mat = [MaterialComponent (0.5, pmDiff),
               MaterialComponent (  2, pmPhong),
               MaterialComponent (0.4, pmRefl)]

        planeGeom = MkAnyGeom $ mkPlane (F3 (-100) 0 (100)) (F3 200 0 0) (F3 0 0 (-1000))
        planeMat =  [MaterialComponent (0.8, pmDiff)
                    ,MaterialComponent (0.1, pmPhong)
                    ]
                    --MaterialComponent (0.5, pmRefl)]
        plane = Object planeGeom planeMat

        objs = Fork [Node Identity (Leaf (Object anyGeom mat))
                    ,Node Identity (Leaf plane)]


        n = 50
        key   = Light (Softbox (F3 5 0 3) (F3 (1) 0 (0)) (F3 0 6 0) n) (40 *. white)
        strip = Light (Softbox (F3 (-1.8) 0 3) (F3 (0.1) 0 (0)) (F3 0 6 0) n) (8 *. white)
        rim   = Light (Softbox (F3 (-11) 0 (-5)) (F3 0 4 0) (F3 3 0 (-8)) n) (100 *. white)
        top   = Light (Softbox (F3 (4) 5 (-7)) (F3 (1) 0 (1)) (F3 (-0.8) 1 0.8) n) (15 *. white)
        top2  = Light (Softbox (F3 (-4) 5 (-7)) (F3 (-1) 0 (1)) (F3 (-0.8) 1 0.8) n) (40 *. white)
        --bounce = Light (Softbox (F3 (-2) (0.1) (2)) (F3 4 0 0) (F3 0 0 (-2)) n) (0.2 *. white)
        lights = [key, strip, rim, top, top2]
        scene = Scene lights objs

testConf = RayTraceConfig 5 0 res cam (0.02*.white)
    where
        res = Resolution (900, 600)
        cam = camLookingAt (F3 0 2.7 (10)) (F3 0 1.2 0) f3e2 31

---}

-- vim: expandtab smarttab sw=4 ts=4
