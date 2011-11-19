import Haray

import Types
import Math

--import OutputSDL
import OutputPPM

main = do
    renderPPM "out.ppm" testScene testConf

testScene = scene
    where
        geom1 = Sphere 1.0 (   0, 0, 10)
        geom2 = Sphere 1.0 (-1.1, 0, 12)
        geom3 = Sphere 1.0 ( 1.1, 0, 12)
        mc1 = MaterialComponent (0.1, PureMaterial Diffuse red)
        mc2 =  MaterialComponent (1, PureMaterial (Phong 50) blue)
        mc3 =  MaterialComponent (1, PureMaterial Reflecting $ 0.8 *. white)
        mat = [mc1, mc2, mc3]
        objs = [Object geom1 mat, Object geom2 mat, Object geom3 mat]
        lights = [Light (PointSource (10,10,10)) (white)
                 ,Light (PointSource (-10,10,10)) (0.5 *. white)]
        scene = Scene lights objs

testConf = RayTraceConfig 5 0 res cam
    where
        res = Resolution (600, 600)
        cam = camLookingAt (0,15,0) (0,0,10) e2 20

-- vim: expandtab smarttab sw=4 ts=4
