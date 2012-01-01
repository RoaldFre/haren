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
--import OutputPPM

main = do
    --renderPPM outfile scene config
    renderSDL PerLine scene config

config = RayTraceConfig recursionDepth aaSamples seed res cam (0.0*.white)
    where
        res = Resolution (400, 250)
        cam = camLookingAt (F3 2.2 3.5 15) (F3 0.5 (-0.4) 0) f3e2 20

scene = theScene
    where
        matAmbient  = mkAmbient (0.8 *. white)
        matDiffuse  = mkDiffuse
        matPhong    = mkPhong 35
        matRefl     = mkReflecting
        matGlossy   = mkGlossy 0.08 nGlossy
        matDielectr = mkDielectric 1.5 (0.1, 0.1, 0.1)
        matTexture  = mkTexture $ checkers (0.2 *. blue) (1.0 *. white) 200 1000

        planeMat = matTexture
        planeGeom = MkAnyGeom $ mkPlane (F3 (-100) 0 (100)) (F3 200 0 0) (F3 0 0 (-1000))
        plane = Object planeGeom planeMat

        geom = mkSphere
        
        s = 1.2
        objs = Fork [Node (Translation (F3 0 (-1) 0)) (Leaf plane)
                    ,Node (Translation (F3 (-2.5*s) 0 (-2.5))) $
                            Fork [Node (Translation (F3 (0*s) 0 0)) (Leaf $ Object geom matAmbient)
                                 ,Node (Translation (F3 (1*s) 0 1)) (Leaf $ Object geom matDiffuse)
                                 ,Node (Translation (F3 (2*s) 0 2)) (Leaf $ Object geom matPhong)
                                 ,Node (Translation (F3 (3*s) 0 3)) (Leaf $ Object geom matRefl)
                                 ,Node (Translation (F3 (4*s) 0 4)) (Leaf $ Object geom matGlossy)
                                 ,Node (Translation (F3 (5*s) 0 5)) (Leaf $ Object geom matDielectr)]
                     ]
                         

        key  = Light (mkSoftBox (F3 (-15) 10 8) (F3 2 0 2) (F3 0 2 0) f3zero n) (200 *. white)
        back = Light (mkSoftBox (F3 10 9 (-5))  (F3 0 0 (-2)) (F3 0 2 0) f3zero n) (130 *. white)
        lights = [key, back]
        theScene = Scene lights objs

{-
n              = 4
nGlossy        = 10
recursionDepth = 4
aaSamples      = 3
seed           = 0
-}

--{-
n              = 1
nGlossy        = 1
recursionDepth = 4
aaSamples      = 1
seed           = 0
---}


-- vim: expandtab smarttab sw=4 ts=4
