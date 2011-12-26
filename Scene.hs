{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor #-}

module Scene where

import Object
import Light
import Transform

data Scene = Scene {
        sLights :: [Light],
        sGraph  :: SceneGraph
    } deriving Show

type SceneGraph = ObjectGraph Transformation

data ObjectGraph a = Node a (ObjectGraph a)
                   | Fork [ObjectGraph a]
                   | Leaf Object
                   deriving (Show, Functor)

-- | Flatten or 'fold' the object graph starting with the given initial 
-- value and proceeding with the given folding function. If the graph 
-- contains loops, the resulting list will be infinite.
-- The first argument given to the supplied function will be the object 
-- that sits closest to the root in the tree, the second argument will be 
-- its child.
flattenObjectGraph :: (a -> a -> a) -> a -> ObjectGraph a -> [(a, Object)]
flattenObjectGraph _ initial (Leaf obj) = [(initial, obj)]
flattenObjectGraph f initial (Fork graphs) =
        concatMap (flattenObjectGraph f initial) graphs
flattenObjectGraph f initial (Node x subGraph) =
        flattenObjectGraph f (f initial x) subGraph


-- vim: expandtab smarttab sw=4 ts=4
