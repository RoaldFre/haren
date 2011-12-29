module Scene (
    Scene(..),
    ObjectGraph,
    SceneGraph,
    TransformedObj,
    flattenSceneGraph,

    module Graph
) where

import Graph
import Object
import Light
import Transform
import Math

data Scene = Scene {
        sLights :: [Light],
        sGraph  :: SceneGraph
    } deriving Show

type ObjectGraph = Graph Object
type SceneGraph = ObjectGraph Transformation

type TransformedObj = Transformed Object

flattenSceneGraph :: SceneGraph -> [TransformedObj]
flattenSceneGraph sceneGraph = 
    map mkTransObj $ flattenGraph multTuples (m4id, m4id) matricesGraph
    where
        mkTransObj ((m,mInv), obj) = Transformed m mInv obj
        multTuples (a, b) (x, y) = (a .*. x, b .*. y)
        matricesGraph = transfoM4s `fmap` sceneGraph

-- vim: expandtab smarttab sw=4 ts=4
