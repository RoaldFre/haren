import Haray

import Math
import Scene
import Object
import ObjParser 
import Material.Diffuse
import Material.Ambient
import Material.Phong
import Material.Dielectric
import Material.Glossy
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
outfile = "IFS2.ppm"

main = do
    setCurrentDirectory dir
    gen <- getStdGen
    renderPPM outfile mkScene $ mkConf gen
    --renderSDL PerLine mkScene $ mkConf gen

mkScene = scene
 where
    matMenger = colorMat (Color 1 1 0.8) $
                    combineMats [scaleMat 0.5 $ mkGlossy 0.007 nGlossy
                                ,colorMat (Color 0.9 0.9 0.7) mkDiffuse]

    mengerSponge = Object (mengerStd 4) $ matMenger
    
    front = 10
    back = -5
    left = -3
    right = 3
    top = 8
    height = top
    width = right - left
    depth = back - front

    lineWidth = 0.2
    lineDens = 2

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

    floorMat = mkTexture baseMat $ checkers (0.2 *. white) white 800 4000
    floorGeom = MkAnyGeom $ mkPlane (F3 (-100) 0 (100)) (F3 200 0 0) (F3 0 0 (-1000)) f3e2
    floorObj = Object floorGeom floorMat

    box = Fork [Leaf floorObj, Leaf backObj, Leaf leftObj, Leaf rightObj]

    objs = Fork [Node (Translation (F3  0  0 0)) (Leaf mengerSponge)
                ,Node (Translation (F3  0  (-2*epsilon)  0)) box]


    lightRight  = Light (mkSoftBox (F3 (2.2) 0.4 0.4) (F3 0 1 0) (F3 1 0 (-1)) (F3 0 0 0) n) (4 *. white)
    lightLeft   = Light (mkSoftBox (F3 (-2.2) 0.4 0.4) (F3 0 1 0) (F3 (-1) 0 (-1)) (F3 0 0 0) n) (4 *. white)
    lightFront  = Light (mkSoftBox (F3 (-0.1) 1.4 1.5) (F3 0 0.1 (-0.1)) (F3 0.2 0 0) (F3 0 0 (-100)) n) (5 *. white)
    lightInterior = Light (PointSource (F3 0 0.5 0)) (20 *. (Color 1 1 0.8))
    lights = [lightLeft, lightRight, lightFront]
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
nGlossy        = 1
recursionDepth = 20
aaSamples      = 4
---}

{-
n              = 1
nGlossy        = 1
recursionDepth = 4
aaSamples      = 1
-}

{-
res = Resolution (600, 600)
cam = camLookingAt (F3 0.0 0.5 (1.2)) (F3 0.0 0.3 (-1)) f3e2 90
-}

res = Resolution (800, 800)
cam = camLookingAt (F3 0.70 0.62 (1.7)) (F3 (-0.1) 0.42 0) f3e2 50

mkConf stdgen = RayTraceConfig recursionDepth aaSamples stdgen res cam

-- vim: expandtab smarttab sw=4 ts=4
