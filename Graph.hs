module Graph where

-- | A graph with leaves of type l. Nodes have a payload of type n.
data Graph l n = Node n (Graph l n)
               | Fork [Graph l n]
               | Leaf l
               deriving (Show)
instance Functor (Graph l) where
    fmap f (Node n g) = Node (f n) (fmap f g)
    fmap f (Fork gs)  = Fork $ map (fmap f) gs
    fmap _ (Leaf l)   = Leaf l

-- | Flatten or 'fold' the object graph starting with the given initial 
-- value and proceeding with the given folding function. If the graph 
-- contains loops, the resulting list will be infinite.
-- The first argument given to the supplied function will be the payload of 
-- the node that sits closest to the root in the tree, the second argument 
-- will be the payload of its child.
flattenGraph :: (n -> n -> n) -> n -> Graph l n -> [(n, l)]
flattenGraph _ initial (Leaf l) = [(initial, l)]
flattenGraph f initial (Fork graphs) =
        concatMap (flattenGraph f initial) graphs
flattenGraph f initial (Node x subGraph) =
        flattenGraph f (f initial x) subGraph

-- vim: expandtab smarttab sw=4 ts=4
