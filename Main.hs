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
        mc1 = MaterialComponent (0.1, PureMaterial Diffuse white)
        mc2 =  MaterialComponent (1, PureMaterial (Phong 50) white)
        mc3 =  MaterialComponent (0.1, PureMaterial Reflecting white)
        mat = [mc1, mc2, mc3]
        {-
        objs = Fork
                [Node (Scale 1 2 1.5) ( 
                    Node (Translation (F3   0    0 8))
                        (Leaf (Object (MkAnyGeom Sphere) mat))
                    )
                ,Node (Translation (F3 (-1.1) 0 12)) (Leaf (Object anyGeom mat))
                --,Node (Translation (F3 (-1.1) 0 12)) (Leaf (Object (MkAnyGeom Sphere) mat))
                ,Node (Translation (F3 ( 1.1) 0 12)) (Leaf (Object (MkAnyGeom Sphere) mat))]
                -}

        triangle = Triangle 
                        (Vertex (F3 (-1) (-1) 0) ((1)*.f3e3))
                        (Vertex (F3   2  (-1) 0) ((1)*.f3e3))
                        (Vertex (F3   0    2  0) ((1)*.f3e3))
        --objs = Node Identity (Leaf (Object (MkAnyGeom triangle) mat))
        --objs = Node (Translation (F3 3 2 0)) (Leaf (Object (MkAnyGeom Sphere) mat))
        objs = Node (Translation (F3 0 (-1.5) 0)) (Leaf (Object anyGeom mat))


        lights = [Light (PointSource (F3   10  10 10)) (white)
                 ,Light (PointSource (F3 (-10) 10 10)) (0.5 *. white)]
        scene = Scene lights objs

testConf = RayTraceConfig 5 0 res cam (0.1*.white)
    where
        res = Resolution (200, 200)
        cam = camLookingAt (F3 0 0 (10)) (F3 0 0 0) f3e2 40
        --cam = Camera (F3 0 0 (10)) ((1)*.f3e1, (1)*.f3e2, (1)*.f3e3) 40



-- vim: expandtab smarttab sw=4 ts=4
