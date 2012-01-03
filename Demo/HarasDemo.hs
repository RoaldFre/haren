import Math
import ObjParser 
import Color
import Light
import Camera

import Haras
import OutputHaras
import System.Directory

dir = "/home/roald/source/haren/Demo"
--outfile = "haras-venus.ppm"
outfile = "haras-teapot.ppm"

main = do
    setCurrentDirectory dir
    --mesh <- parseObjFile "../obj/venus.obj"
    mesh <- parseObjFile "../obj/teapot.obj"
    renderImage outfile $ makeImg mesh

makeImg mesh = image
    where
        image = rasterizeToImage mesh color lights conf

    	color = white
        lights  = [Light (PointSource (F3   50  50  10)) (0.8 *. white)
                  ,Light (PointSource (F3 (-50) 50  10)) (0.2 *. white)
                  --for venus:
                  --,Light (PointSource (F3 (-90) 80 (-100))) (1.0 *. white)
                  ]
        conf = RasterizerConfig (Resolution (800, 800)) cam (0.1 *. white)
        -- for teapot:
        cam = camLookingAt (F3 0 (10) (15)) (F3 0 1 0) ((1) *. f3e2) 15
        -- for venus:
        --cam = camLookingAt (F3 0 (6) (15)) (F3 0 1 0) ((1) *. f3e2) 26


-- vim: expandtab smarttab sw=4 ts=4
