import Haray

import Math
import Scene
import Object
import ObjParser 
import Material.Diffuse
import Material.Ambient
import Material.Phong
import Material.Dielectric
import Material.Reflecting
import Material.Texture
import Geometry.IFS
import Geometry.Plane
import Geometry.Sphere
import Geometry.Box
import Transform
import Camera
import Light
import Renderer


import OutputSDL
import OutputPPM

import System.Directory
import System.Random

dir = "/home/roald/source/haren/Demo"
outfile = "IFS1-d-menger.ppm"

main = do
    setCurrentDirectory dir
    gen <- getStdGen
    renderPPM outfile mkScene $ mkConf gen
    --renderSDL PerLine mkScene $ mkConf gen

mkScene = scene
 where
    matDiff   = mkDiffuse
    matPhong  = mkPhong 25

    matMenger = combineMats [mkDielectric 1.4 (2.0, 0.7, 2.0)
                            --,colorMat (0.5 *. white) mkDiffuse
                            ]

    matSierpinski = combineMats [colorMat (Color 0.5 0.5 0.1) matDiff, matPhong]
    sierpinski = Object (sierpinskiPiramidStd 6) $ matSierpinski
    --mengerBase = MkAnyGeom $ mkTransformed (Scale 0.5 `After` Translation f3e2) mkSphere
    --mengerSponge = Object (menger mengerBase 2) $ matMenger
    mengerSponge = Object (mengerStd 1) $ matMenger
    
    front = 10
    back = -5
    left = -2
    right = 3
    top = 5
    height = top
    width = right - left
    depth = back - front

    lineWidth = 0.1
    lineDens = 5

    baseMat = combineMats [mkAmbient (0.2 *. white), mkDiffuse]
    backMat = mkTexture baseMat $ 
                gridLines lineWidth black (Color 1 0.5 0.5) (width*lineDens) (height*lineDens)
    backGeom = MkAnyGeom $ mkPlane (F3 left 0 back) (F3 width 0 0) (F3 0 top 0) f3e3
    backObj = Object backGeom backMat

    sideMat = mkTexture baseMat $ 
                gridLines lineWidth black (Color 0.5 0.5 1) (depth*lineDens) (height*lineDens)
    leftGeom = MkAnyGeom $ mkPlane (F3 left 0 front) (F3 0 0 depth) (F3 0 top 0) f3e1
    leftObj = Object leftGeom sideMat
    rightGeom = MkAnyGeom $ mkPlane (F3 right 0 front) (F3 0 0 depth) (F3 0 top 0) (inv f3e1)
    rightObj = Object rightGeom sideMat

    floorMat = mkTexture baseMat $ checkers (0.2 *. white) white 400 2000
    floorGeom = MkAnyGeom $ mkPlane (F3 (-100) 0 (100)) (F3 200 0 0) (F3 0 0 (-1000)) f3e2
    floorObj = Object floorGeom floorMat

    box = Fork [Leaf floorObj, Leaf backObj, Leaf leftObj, Leaf rightObj]

    objs = Fork [Node (Translation (F3  0  0 0)) (Leaf mengerSponge)
                ,Node (Scale 1.4 `After` Translation (F3  0  0 (-1.2))) (Leaf sierpinski)
                ,Node (Translation (F3  0  (-2*epsilon)  0)) box]


    lightRight  = Light (mkSoftBox (F3 (1.9) 0.9 0.8) (F3 0 1 0) (F3 1 0 (-1.5)) (F3 0 0 0) n) (7.5 *. white)
    lightLeft   = Light (mkSoftBox (F3 (-1.5) 0.9 0.8) (F3 0 1 0) (F3 (-1) 0 (-1.5)) (F3 0 0 0) n) (7.5 *. white)
    lights = [lightLeft, lightRight]
    --lights = []
   
    {-
    lights = [Light (PointSource (F3 2 5 10)) (50 *. white)
             ,Light (PointSource (F3 0 0.5 (-3.5))) ( 20 *. white) -- in the center of the sponge! :-) SHADOW PATTERN!
             ,Light (PointSource (F3 (-2) 10 5)) ( 50 *. white)
             ,Light (PointSource (F3 (-2) 10 5)) ( 50 *. white)]
    -}
    scene = Scene lights objs

--{-
n              = 1
recursionDepth = 5
aaSamples      = 2
---}

{-
n              = 1
recursionDepth = 0--9
aaSamples      = 1
-}

res = Resolution (400, 400)
--res = Resolution (250, 250)
--cam = camLookingAt (F3 0.0 0.5 0) (F3 0.1 0.5 0) f3e2 80
cam = camLookingAt (F3 0.95 1.55 3) (F3 0.15 0.65 0) f3e2 32

mkConf stdgen = RayTraceConfig recursionDepth aaSamples stdgen res cam

-- vim: expandtab smarttab sw=4 ts=4
