module Scene (
    Scene(..),
    SceneGraph,
    flattenSceneGraph,

    module Graph
) where

import Graph
import Object
import Light
import Transform

data Scene = Scene {
        sLights :: [Light],
        sGraph  :: SceneGraph
    } deriving Show

type SceneGraph = TransformationGraph Object
flattenSceneGraph = flattenTransfoGraph

-- vim: expandtab smarttab sw=4 ts=4
