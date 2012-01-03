import Haray

import Math
import Scene
import Object
import Geometry.Plane
import Geometry.Sphere
import Transform
import Camera
import Light
import Renderer

import Material.Ambient
import Material.Diffuse
import Material.Phong
import Material.Reflecting
import Material.Glossy
import Material.Dielectric
import Material.Texture

import OutputSDL
import OutputPPM

import System.Random

outfile = "materials.ppm"

main = do
    gen <- getStdGen
    --renderPPM outfile scene $ mkConf gen
    renderSDL PerLine scene $ mkConf gen

matAmbient  = mkAmbient (0.8 *. white)
matDiffuse  = mkDiffuse
matPhong    = mkPhong 35
matRefl     = mkReflecting
matGlossy   = mkGlossy 0.07 nGlossy
matDielectr = mkDielectric 1.5 (0.1, 0.1, 0.1)

geom = mkSphere

s = 1.2
obj = Object geom
objs = Fork [Node (Translation (F3 0 (-1) 0)) box
            ,Node (Translation (F3 (-2.5*s) 0 (-2.5))) $
                    Fork [Node (Translation (F3 (0*s) 0 0)) (Leaf $ obj matAmbient)
                         ,Node (Translation (F3 (1*s) 0 1)) (Leaf $ obj matDiffuse)
                         ,Node (Translation (F3 (2*s) 0 2)) (Leaf $ obj matPhong)
                         ,Node (Translation (F3 (3*s) 0 3)) (Leaf $ obj matRefl)
                         ,Node (Translation (F3 (4*s) 0 4)) (Leaf $ obj matGlossy)
                         ,Node (Translation (F3 (5*s) 0 5)) (Leaf $ obj matDielectr)]
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

lineWidth = 0.1
lineDens = 3

baseMat = combineMats [mkAmbient (0.2 *. white), mkDiffuse]
makeTex color nu nv = mkTexture baseMat $ gridLines lineWidth black color nu nv

backMat = makeTex (Color 1 0.5 0.5) (width*lineDens) (height*lineDens)
backGeom = MkAnyGeom $ mkPlane (F3 left 0 back) (F3 width 0 0) (F3 0 top 0) f3e3
backObj = Object backGeom backMat

sideMat = makeTex (Color 0.5 0.5 1) (depth*lineDens) (height*lineDens)
leftGeom = MkAnyGeom $ mkPlane (F3 left 0 front) (F3 0 0 depth) (F3 0 top 0) f3e1
leftObj = Object leftGeom sideMat
rightGeom = MkAnyGeom $ mkPlane (F3 right 0 front) (F3 0 0 depth) (F3 0 top 0) (inv f3e1)
rightObj = Object rightGeom sideMat

floorMat = mkTexture baseMat $ checkers (0.25 *. blue) white 200 1000
floorGeom = MkAnyGeom $ mkPlane (F3 (-100) 0 (100)) (F3 200 0 0) (F3 0 0 (-1000)) f3e2
floorObj = Object floorGeom floorMat

box = Fork [Leaf floorObj, Leaf backObj, Leaf leftObj, Leaf rightObj]




{-
--original without box:
lightKey  = Light (mkSoftBox (F3 (-15) 10 8) (F3 2 0 2) (F3 0 2 0) f3zero n) (200 *. white)
lightBack = Light (mkSoftBox (F3 10 9 (-5))  (F3 0 0 (-2)) (F3 0 2 0) f3zero n) (130 *. white)
-}

lightKey  = Light (mkSoftBox (F3 (-5) 3 3) (F3 1 0 1) (F3 0 1 0) f3zero n) (30 *. white)
lightBack = Light (mkSoftBox (F3 3 3 (-2))  (F3 0 0 (-1)) (F3 0 1 0) f3zero n) (20 *. white)
lights = [lightKey, lightBack]
scene = Scene lights objs

{-
n              = 3
nGlossy        = 25
recursionDepth = 4
aaSamples      = 4
-}

--{-
n              = 1
nGlossy        = 1
recursionDepth = 4
aaSamples      = 1
---}

--res = Resolution (800, 500)
res = Resolution (400, 250)
cam = camLookingAt (F3 2.2 3.5 15) (F3 0.5 (-0.5) 0) f3e2 20

mkConf stdgen = RayTraceConfig recursionDepth aaSamples stdgen res cam


-- vim: expandtab smarttab sw=4 ts=4
