import Haray

import Types
import Math

import OutputSDL
import OutputPPM

main = do
    renderPPM "out.ppm" testScene testConf
    --renderSDL PerLine testScene testConf

testScene = scene
    where
        mc1 = MaterialComponent (0.1, PureMaterial Diffuse red)
        mc2 =  MaterialComponent (1, PureMaterial (Phong 50) blue)
        mc3 =  MaterialComponent (1, PureMaterial Reflecting $ 0.8 *. white)
        mat = [mc1, mc2, mc3]
        objs = Fork
                [Node (Scale 1 2 1.5) ( 
                    Node (Translation (F3   0    0 8))
                        (Leaf (Object (MkAnyGeom Sphere) mat))
                    )
                ,Node (Translation (F3 (-1.1) 0 12)) (Leaf (Object (MkAnyGeom Sphere) mat))
                ,Node (Translation (F3 ( 1.1) 0 12)) (Leaf (Object (MkAnyGeom Sphere) mat))]
        lights = [Light (PointSource (F3   10  10 10)) (white)
                 ,Light (PointSource (F3 (-10) 10 10)) (0.5 *. white)]
        scene = Scene lights objs

testConf = RayTraceConfig 5 0 res cam
    where
        res = Resolution (600, 600)
        cam = camLookingAt (F3 0 0 0) (F3 0 0 10) f3e2 20

-- vim: expandtab smarttab sw=4 ts=4
