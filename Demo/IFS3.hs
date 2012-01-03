import Haray

import Math
import Scene
import Object
import ObjParser 
import Material.Diffuse
import Material.Phong
import Material.Dielectric
import Material.Reflecting
import Material.Texture
import Geometry.IFS
import Geometry.Plane
import Geometry.Triangles
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
outfile = "IFS3.ppm"

main = do
    setCurrentDirectory dir
    mesh <- parseObjFile "../obj/teapot.obj"
    let meshOptim = optimizeTriangleMesh 15 mesh
    gen <- getStdGen
    renderPPM 100 outfile (mkScene meshOptim) $ mkConf gen
    --renderSDL PerLine (mkScene meshOptim) $ mkConf gen

mkScene geom = scene
 where
    matDiff   = mkDiffuse
    matPhong  = mkPhong 25

    matFractal = scaleMat 0.6 $ combineMats [matDiff, matPhong]

    --mengerSponge = Object (mengerStd 1) $ matMenger
    mengerBase = MkAnyGeom $ mkTransformed (Scale 0.32 `After` ScaleXYZ 0.75 1 1
                                            `After` Rotation f3e2 (-45)) geom
    mengerSponge = menger mengerBase 2
    geomFractal = sierpinskiPiramid mengerSponge 3
    fractal = Object geomFractal matFractal


    
    front = 10
    back = -2
    left = -3
    right = 2
    top = 5
    height = top
    width = right - left
    depth = back - front

    lineWidth = 0.1
    lineDens = 5

    baseMat = mkDiffuse
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

    objs = Fork [Leaf fractal
                ,Node (Translation (F3  0  (-2*epsilon)  0)) box]


    --lightRight  = Light (mkSoftBox (F3 (2.9) 0.6 0.1) (F3 0 0 (-0.3)) (F3 0 0.2 0) (F3 0 0 0) n) (100 *. white)
    lightLeft   = Light (mkSoftBox (F3 (-2.3) 0.2 0.1) (F3 0 0 (-0.4)) (F3 0 0.2 0) (F3 0 0 0) n) (10 *. white)
    lightFront  = Light (mkSoftBox (F3 (-0.4) 0.6 4.5) (F3 1.4 0 0) (F3 0 1.0 0) (F3 0 0 0) n) (30 *. white)
    --lights = [lightLeft, lightRight, lightFront]
    lights = [lightLeft, lightFront]
    --lights = []
   
    {-
    lights = [Light (PointSource (F3 2 5 10)) (50 *. white)
             ,Light (PointSource (F3 0 0.5 (-3.5))) ( 20 *. white) -- in the center of the sponge! :-) SHADOW PATTERN!
             ,Light (PointSource (F3 (-2) 10 5)) ( 50 *. white)
             ,Light (PointSource (F3 (-2) 10 5)) ( 50 *. white)]
             -}
    scene = Scene lights objs

{-
n              = 2
recursionDepth = 25
aaSamples      = 2
-}

--{-
n              = 1--2
recursionDepth = 9
aaSamples      = 3--2
---}

res = Resolution (1666, 1000)
--res = Resolution (300, 200)
cam = camLookingAt (F3 (-0.9) 0.25 1.3) (F3 0.1 0.37 0) f3e2 55

mkConf stdgen = RayTraceConfig recursionDepth aaSamples stdgen res cam

-- vim: expandtab smarttab sw=4 ts=4
