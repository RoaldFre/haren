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
import Geometry.Sphere
import Transform
import Camera
import Light
import Renderer

import OutputSDL
import OutputPPM

import System.Directory
import System.Random

dir = "/home/roald/source/haren/Demo"
outfile = "tree.ppm"

main = do
    setCurrentDirectory dir
    treeMesh <- parseObjFile "../obj/treebranches.obj"
    let treeOptim = optimizeTriangleMesh 15 treeMesh
    gen <- getStdGen
    renderPPM outfile (mkScene treeOptim) $ mkConf gen
    --renderSDL PerLine (mkScene treeOptim) $ mkConf gen


sinTex :: Flt -> Tex
sinTex k (F2 u v) = Color r g b where
    r = 0
    b = 0
    g = 0.3 + 0.06*sin(k*u)

mkScene treeGeom = scene
    where
    	sunColor = Color 1 1 0.8
    	sunPos = F3 44000 40000 (-100000)
    	sun = Object mkSphere $ mkAmbient sunColor

        baseMat = colorMat green mkDiffuse
        --groundMat = mkTexture baseMat $ gridLines 0.2 (0.1 *. white) (0.2 *. white) 1000 10
        groundMat = mkTexture baseMat $ sinTex 500
        groundGeom = MkAnyGeom $ mkPlane (F3 (-100) 0 (200)) (F3 200 0 0) (F3 0 0 (-4000)) f3e2
        ground = Object groundGeom groundMat
        
        brown = Color 0.6 0.4 0.2
        treeMat = combineMats [colorMat (0.7 *. brown) mkDiffuse, mkAmbient (0.3 *. brown)]
        --treeMat = mkAmbient (0.3 *. brown)
        tree = Object treeGeom treeMat

        objs = Fork [Leaf tree
                    ,Leaf ground
                    ,Node (Translation (1.1 *. sunPos) `After` Scale 1000) (Leaf sun)
                    ]

        lights = [Light (PointSource sunPos) (20000000000 *. sunColor)]
        scene = Scene lights objs



recursionDepth = 2
aaSamples      = 3


res = Resolution (2000, 1000)
--res = Resolution (500, 250)
cam = camLookingAt (F3 7 5 (10)) (F3 0 4 0) f3e2 91

mkConf stdgen = RayTraceConfig recursionDepth aaSamples stdgen res cam
-- vim: expandtab smarttab sw=4 ts=4
