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
outfile = "dielectric.ppm"

main = do
    setCurrentDirectory dir
    gen <- getStdGen
    renderPPM outfile scene $ mkConf gen
    --renderSDL PerLine scene $ mkConf gen

greenGlass = mkDielectric 1.5 (1, 0.2, 1)
blueGlass = mkDielectric 1.5 (1, 1, 0.2)
redGlass = mkDielectric 1.5 (0.2, 1.5, 1.5)

objGreen = Object (mkBox (F3 0 0 0) (F3 1.5 1 0.3)) $ greenGlass
objBlue  = Object (mkBox (F3 (-0.8) 0 0) (F3 2.4 0.5 0.5)) $ blueGlass
objRed   = Object (mkBox (F3 (0.5) 0 (-0.4)) (F3 2.0 0.4 (-0.8))) $ redGlass

objs = Fork [Node (Translation (F3 0 (-epsilon) 0)) box
            ,Fork [Node (Translation (F3 0 0 0) `After` Rotation f3e2 (-20)) (Leaf objGreen)
                  ,Node (Translation (F3 0.1 1.1 0)
                            `After` Rotation f3e2 67 
                            `After` Rotation f3e3 (-25)
                            `After` Rotation f3e1 (-10)) (Leaf objBlue)
                  ,Node (Rotation f3e2 12) (Leaf objRed)]
            ]



-- Box:
front = 10
back = -8
left = -5
right = 5
top = 4
height = top
width = right - left
depth = back - front

lineWidth = 0.15
lineDens = 3

baseMat = combineMats [mkAmbient (0.1 *. white), mkDiffuse]
makeTex color nu nv = mkTexture baseMat $ gridLines lineWidth black color nu nv

backMat = makeTex (Color 1 0.9 0.2) (width*lineDens) (height*lineDens)
backGeom = MkAnyGeom $ mkPlane (F3 left 0 back) (F3 width 0 0) (F3 0 top 0) f3e3
backObj = Object backGeom backMat

sideMat = makeTex (Color 1 0.4 0.4) (width*lineDens) (height*lineDens)
--sideMat = makeTex (Color 0.4 0.4 1) (depth*lineDens) (height*lineDens)
leftGeom = MkAnyGeom $ mkPlane (F3 left 0 front) (F3 0 0 depth) (F3 0 top 0) f3e1
leftObj = Object leftGeom sideMat
rightGeom = MkAnyGeom $ mkPlane (F3 right 0 front) (F3 0 0 depth) (F3 0 top 0) (inv f3e1)
rightObj = Object rightGeom sideMat

floorMat = makeTex white (width*lineDens) (depth*lineDens)
floorGeom = MkAnyGeom $ mkPlane (F3 left 0 front) (F3 width 0 0) (F3 0 0 depth) f3e2
floorObj = Object floorGeom floorMat

box = Fork [Leaf floorObj, Leaf backObj, Leaf leftObj, Leaf rightObj]




lightKey  = Light (mkSoftBox (F3 (-5) 3 3) (F3 1 0 1) (F3 0 1 0) f3zero n) (30 *. white)
lightBack = Light (mkSoftBox (F3 3 3 (-2))  (F3 0 0 (-1)) (F3 0 1 0) f3zero n) (20 *. white)
lights = [lightKey, lightBack]
scene = Scene lights objs

--{-
n              = 1
recursionDepth = 15
aaSamples      = 3
---}

{-
n              = 1
recursionDepth = 10
aaSamples      = 1
-}

res = Resolution (800, 800)
cam = camLookingAt (F3 1.2 2.6 7) (F3 0.75 (0.8) 0) f3e2 20

mkConf stdgen = RayTraceConfig recursionDepth aaSamples stdgen res cam


-- vim: expandtab smarttab sw=4 ts=4
