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
outfile = "forrest-summer.ppm"

main = do
    setCurrentDirectory dir
    branchMesh <- parseObjFile "../obj/treebranches.obj"
    leavesMesh <- parseObjFile "../obj/treeleaves.obj"
    let branchOptim = optimizeTriangleMesh 10 branchMesh
    let leavesOptim = optimizeTriangleMesh 10 leavesMesh
    gen <- getStdGen
    renderPPM 100 outfile (mkScene branchOptim leavesOptim) $ mkConf gen
    --renderSDL PerLine (mkScene branchOptim leavesOptim) $ mkConf gen


mkScene branchGeom leavesGeom = scene
    where
        brown = Color 0.6 0.4 0.2
        groundCol = 0.5 *. brown
        branchCol = brown
        leavesCol = Color 0.2 0.9 0.2

        groundMat = colorMat groundCol mkDiffuse
        groundGeom = MkAnyGeom $ mkPlane (F3 (-100) 0 (200)) (F3 200 0 0) (F3 0 0 (-4000)) f3e2
        ground = Object groundGeom groundMat
        
        branchMat = combineMats [colorMat (0.7 *. branchCol) mkDiffuse
				,mkAmbient (0.05 *. branchCol)]
        branch = Object branchGeom branchMat


        leavesMat = combineMats [colorMat (0.7 *. leavesCol) mkDiffuse
				,mkAmbient (0.05 *. leavesCol)
				,colorMat (0.9 *. leavesCol) $ mkPhong 35]
        leaves = Object leavesGeom leavesMat
        
        tree = Node (Scale 0.8) $ Fork [Leaf branch, Leaf leaves]
        --tree = Node (Scale 0.8) $ Fork [Leaf branch]

        objs = Fork
            [Node (Translation (F3 (-9) 0 5) `After` Rotation f3e2 23 `After` Scale 1.1) tree
            ,Node (Translation (F3 (-8) 0 9) `After` Rotation f3e2 13 `After` Scale 0.8) tree
            ,Node (Translation (F3 (-8) 0 2) `After` Rotation f3e2 34 `After` Scale 0.9) tree
            ,Node (Translation (F3 (-7) 0 4) `After` Rotation f3e2 73 `After` Scale 1.2) tree
            ,Node (Translation (F3 (-6) 0 8) `After` Rotation f3e2 63 `After` Scale 1.0) tree
            ,Node (Translation (F3 (-4) 0 2) `After` Rotation f3e2 96 `After` Scale 1.2) tree
            ,Node (Translation (F3 (-3) 0 9) `After` Rotation f3e2 83 `After` Scale 1.1) tree
            ,Node (Translation (F3 (-1) 0 6) `After` Rotation f3e2  3 `After` Scale 0.8) tree
            ,Node (Translation (F3   0  0 9) `After` Rotation f3e2 25 `After` Scale 0.9) tree
            ,Node (Translation (F3   0  0 4) `After` Rotation f3e2 83 `After` Scale 1.1) tree
            ,Node (Translation (F3   1  0 7) `After` Rotation f3e2 93 `After` Scale 0.9) tree
            ,Node (Translation (F3   3  0 9) `After` Rotation f3e2 73 `After` Scale 1.3) tree
            ,Node (Translation (F3   4  0 7) `After` Rotation f3e2 33 `After` Scale 1.2) tree
            ,Node (Translation (F3   5  0 4) `After` Rotation f3e2 13 `After` Scale 1.0) tree
            ,Node (Translation (F3   5  0 9) `After` Rotation f3e2 53 `After` Scale 0.7) tree
            ,Node (Translation (F3   7  0 6) `After` Rotation f3e2 43 `After` Scale 0.9) tree

            ,Node (Translation (F3   9  0 11) `After` Rotation f3e2 23 `After` Scale 1.1) tree
            ,Node (Translation (F3   8  0 15) `After` Rotation f3e2 93 `After` Scale 0.9) tree
            ,Node (Translation (F3   8  0 10) `After` Rotation f3e2 43 `After` Scale 1.2) tree
            ,Node (Translation (F3   7  0 12) `After` Rotation f3e2 73 `After` Scale 1.0) tree
            ,Node (Translation (F3   6  0 16) `After` Rotation f3e2 33 `After` Scale 1.2) tree
            ,Node (Translation (F3   4  0 10) `After` Rotation f3e2 13 `After` Scale 0.7) tree
            ,Node (Translation (F3   3  0 18) `After` Rotation f3e2 23 `After` Scale 0.9) tree
            ,Node (Translation (F3   1  0 14) `After` Rotation f3e2 73 `After` Scale 1.0) tree
            ,Node (Translation (F3 (-0) 0 18) `After` Rotation f3e2 43 `After` Scale 1.1) tree
            ,Node (Translation (F3 (-0) 0 11) `After` Rotation f3e2 33 `After` Scale 1.3) tree
            ,Node (Translation (F3 (-1) 0 15) `After` Rotation f3e2 93 `After` Scale 0.7) tree
            ,Node (Translation (F3 (-3) 0 19) `After` Rotation f3e2 73 `After` Scale 0.9) tree
            ,Node (Translation (F3 (-4) 0 15) `After` Rotation f3e2 13 `After` Scale 1.2) tree
            ,Node (Translation (F3 (-6) 0 12) `After` Rotation f3e2 33 `After` Scale 1.0) tree
            ,Node (Translation (F3 (-7) 0 17) `After` Rotation f3e2 43 `After` Scale 1.1) tree
            ,Node (Translation (F3 (-8) 0 14) `After` Rotation f3e2 83 `After` Scale 0.9) tree
            ,Leaf ground
            ]

        lights = [Light (PointSource (F3   10  10 30)) (250 *. white)
                 ,Light (PointSource (F3 (-10)  5 30)) (100 *. white)]
        scene = Scene lights objs



recursionDepth = 2
aaSamples      = 2


--res = Resolution (600, 300)
res = Resolution (2000, 1000)
cam = camLookingAt (F3 0 1.8 (30)) (F3 0 4 0) f3e2 32

mkConf stdgen = RayTraceConfig recursionDepth aaSamples stdgen res cam
-- vim: expandtab smarttab sw=4 ts=4
