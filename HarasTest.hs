import Math
import ObjParser 
import Color
import Light
import Camera

import Haras
import OutputHaras

main = do
    mesh <- parseObjFile "teapot.obj"
    renderImage "test.ppm" (makeImg mesh)

makeImg mesh = image
    where
        image = rasterizeToImage mesh color lights conf

    	color = white
        lights  = [Light (PointSource (F3 50 300 (100))) (0.5 *. white)
                  ,Light (PointSource (F3 50 0   (100))) (0.5 *. white)]
        conf = RasterizerConfig (Resolution (800, 800)) cam (0.1 *. white)
        cam = camLookingAt (F3 0 (10) (15)) (F3 0 1 0) ((1) *. f3e2) 15


