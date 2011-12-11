import Types
import Math
import ObjParser 

import Haras
import OutputHaras

main = do
    mesh <- parseObjFile "teapot.obj"
    renderImage "test.ppm" (makeImg mesh)

makeImg mesh = image
    where
        image = rasterizeToImage mesh mat lights conf

        triangle = Triangle (Vertex (F3  0  0 0) (F3 0 1 0))
                            (Vertex (F3  150  10 0) (F3 0 0 1))
                            (Vertex (F3  10  300 0) (F3 1 0 0))
	mesh_ = TriangleMesh [triangle]

        pmDiff  = PureMaterial Diffuse white
	mat = [MaterialComponent (1, pmDiff)]
        lights  = [Light (PointSource (F3 50 300 (100))) (0.5 *. white)
                  ,Light (PointSource (F3 50 0   (100))) (0.5 *. white)]
        conf = RasterizerConfig (Resolution (800, 800)) cam (0.1 *. white)
        cam = camLookingAt (F3 0 (10) (15)) (F3 0 1 0) ((1) *. f3e2) 15


