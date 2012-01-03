import Haray

import Math
import Scene
import Object
import ObjParser 
import Geometry.Plane
import Geometry.Sphere
import Geometry.Triangles
import Geometry.Box
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
import System.Directory

dir = "/home/roald/source/haren/Demo"
outfile = "chess-n3-glossy3-depth5-AA3-1k.ppm"

main = do
    setCurrentDirectory dir
    kingMesh   <- parseObjFile "../obj/treebranches.obj"
    queenMesh  <- parseObjFile "../obj/venus.obj"
    bishopMesh <- parseObjFile "../obj/teapot.obj"
    horseMesh  <- parseObjFile "../obj/bunny.obj"
    rookMesh   <- parseObjFile "../obj/triceratops.obj"
    let kingTransfo   = Scale 0.28
    let queenTransfo  = Scale 0.11 `After` Translation (6 *. f3e2)
    let bishopTransfo = Scale 0.18 `After` Rotation f3e2 (-90)
    let horseTransfo  = Scale 0.15 `After` Translation (F3 0 0 (-1)) `After` Rotation f3e2 35
    let rookTransfo   = Scale 0.10 `After` Rotation f3e2 (-90) `After` Translation (3 *. f3e2)
    let pawnTransfo   = Scale 0.22 `After` Translation f3e2
    let kingGeom   = MkAnyGeom $ mkTransformed kingTransfo   $ optimizeTriangleMesh 10 kingMesh
    let queenGeom  = MkAnyGeom $ mkTransformed queenTransfo  $ optimizeTriangleMesh 10 queenMesh
    let bishopGeom = MkAnyGeom $ mkTransformed bishopTransfo $ optimizeTriangleMesh 10 bishopMesh
    let horseGeom  = MkAnyGeom $ mkTransformed horseTransfo  $ optimizeTriangleMesh 10 horseMesh
    let rookGeom   = MkAnyGeom $ mkTransformed rookTransfo   $ optimizeTriangleMesh 10 rookMesh
    let pawnGeom   = MkAnyGeom $ mkTransformed pawnTransfo   $ mkSphere

    gen <- getStdGen
    let scene = mkScene kingGeom queenGeom bishopGeom horseGeom rookGeom pawnGeom
    renderPPM 100 outfile scene $ mkConf gen
    --renderSDL PerLine scene $ mkConf gen



mkScene kingGeom queenGeom bishopGeom horseGeom rookGeom pawnGeom = scene
 where
    objs = Fork [chessSet, room]

    matGlossy = scaleMat 0.6 $ mkGlossy 0.03 nGlossy

    blackKingMat = combineMats [mkPhong 25, mkDielectric 1.5 (5, 5, 5)]
    blackPawnMat = combineMats [mkPhong 25, mkDielectric 1.5 (2.8, 2.8, 2.8)]
    blackMat = combineMats [mkPhong 25, mkDielectric 1.5 (1.5, 1.5, 1.5)]

    whiteKingMat = combineMats [mkPhong 25, mkDielectric 1.5 (0.06, 0.06, 0.06)]
    whitePawnMat = combineMats [mkPhong 25, mkDielectric 1.5 (0.04, 0.04, 0.04)]
    whiteMat = combineMats [mkPhong 25, mkDielectric 1.5 (0.03, 0.03, 0.03)]

    plateMat = combineMats [mkPhong 25, mkDielectric 1.5 (0.03, 0.03, 0.03)]


    boardGeom = MkAnyGeom $ mkPlane (F3 (-4) 0 (-4)) (F3 8 0 0) (F3 0 0 8) f3e2
    brown = Color 0.6 0.4 0.2
    boardMat = combineMats [matGlossy
                           ,mkTexture mkDiffuse $ checkers black (1.5 *. brown) 8 8]
    board = Leaf $ Object boardGeom boardMat

    -- Base directly under board
    baseHeight = 0.1
    baseSize = 4.8
    baseGeom = mkBox (F3 (-baseSize) 0 (-baseSize)) (F3 baseSize baseHeight baseSize)
    baseMat = combineMats [matGlossy, scaleMat 0.2 mkDiffuse]
    base = Leaf $ Object baseGeom baseMat

    -- Glass plate under base
    plateHeight = 0.35
    plateSize = 5.75
    plateGeom = mkBox (F3 (-plateSize) 0 (-plateSize)) (F3 plateSize plateHeight plateSize)
    plate = Leaf $ Object plateGeom plateMat

    king   = Object kingGeom
    queen  = Object queenGeom
    bishop = Object bishopGeom
    horse  = Object horseGeom
    rook   = Object rookGeom
    pawn   = Object pawnGeom

    team mat kingMat pawnMat = Node (Translation (F3 (-3.5) 0 (-3.5))) $
                   Fork $ [Node (Translation (F3 x 0 1)) (Leaf $ pawn pawnMat) | x <- [0..7]]
                       ++ [Node (Translation (F3 0 0 0)) (Leaf $ rook mat)
                          ,Node (Translation (F3 1 0 0)) (Leaf $ horse mat)
                          ,Node (Translation (F3 2 0 0)) (Leaf $ bishop mat)
                          ,Node (Translation (F3 3 0 0)) (Leaf $ queen mat)
                          ,Node (Translation (F3 4 0 0)) (Leaf $ king kingMat)
                          ,Node (Translation (F3 5 0 0)) (Leaf $ bishop mat)
                          ,Node (Translation (F3 6 0 0)) (Leaf $ horse mat)
                          ,Node (Translation (F3 7 0 0)) (Leaf $ rook mat)]

    blackPieces = team blackMat blackKingMat blackPawnMat
    whitePieces = Node (Rotation f3e2 180) $ team whiteMat whiteKingMat whitePawnMat
   
    chessSet = Fork [Node (Translation ((plateHeight + epsilon)*.f3e2)) $
                        Fork [Node (Translation ((baseHeight + epsilon)*.f3e2)) $
                                Fork [board, whitePieces, blackPieces]
                        ,base]
                    ,plate]


    lightRight  = Light (mkSoftBox (F3   10  4   2) (F3 0 0 (-8)) (F3 0 2 0) (F3 0 0 0) n) (35 *. white)
    lightLeft   = Light (mkSoftBox (F3 (-10) 3   2) (F3 0 0 (-8)) (F3 0 2 0) (F3 0 0 0) n) (35 *. white)
    --lightFront  = Light (mkSoftBox (F3 ( -4) 3  20) (F3 8 0 0) (F3 0 2 0) (F3 0 0 0) n) (35 *. white)
    lightBack   = Light (mkSoftBox (F3 ( -4) 5(-10)) (F3 8 0 0) (F3 0 2 0) (F3 0 0 0) n) (35 *. white)
    lights = [lightLeft, lightBack, lightRight]

    scene = Scene lights objs





-- Room
sideCol = white
backCol = white
lineWidth = 0.15
lineDens  = 2

front  = 20
back   = -10 - epsilon
left   = -10 - epsilon
right  = 10 + epsilon
top    = 10
height = top
width  = right - left
depth  = back - front

baseMat = mkDiffuse
makeTex color nu nv = mkTexture baseMat $ gridLines lineWidth black color nu nv

backMat = makeTex backCol (width*lineDens) (height*lineDens)
backGeom = MkAnyGeom $ mkPlane (F3 left 0 back) (F3 width 0 0) (F3 0 top 0) f3e3
backObj = Object backGeom backMat

sideMat = makeTex sideCol (width*lineDens) (height*lineDens)
leftGeom = MkAnyGeom $ mkPlane (F3 left 0 front) (F3 0 0 depth) (F3 0 top 0) f3e1
leftObj = Object leftGeom sideMat
rightGeom = MkAnyGeom $ mkPlane (F3 right 0 front) (F3 0 0 depth) (F3 0 top 0) (inv f3e1)
rightObj = Object rightGeom sideMat

floorMat = makeTex white (width*lineDens) (depth*lineDens)
floorGeom = MkAnyGeom $ mkPlane (F3 left 0 front) (F3 width 0 0) (F3 0 0 depth) f3e2
floorObj = Object floorGeom floorMat

room = Node (Translation $ (-epsilon) *. f3e2) $ Fork [Leaf floorObj, Leaf backObj, Leaf leftObj, Leaf rightObj]



{-
n              = 1
nGlossy        = 1
recursionDepth = 1
aaSamples      = 2
-}

--{-
n              = 3
nGlossy        = 3--5
recursionDepth = 5--10
aaSamples      = 3
---}

--res = Resolution (450, 200)
--res = Resolution (675, 300)
--res = Resolution (1125, 500)
res = Resolution (2250, 1000)
cam = camLookingAt (F3 (-9.0) 9.5 30) (F3 0.9 0.2 0) f3e2 13 --13 XXX TODO DON'T FORGET THIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 13

mkConf stdgen = RayTraceConfig recursionDepth aaSamples stdgen res cam

-- vim: expandtab smarttab sw=4 ts=4
