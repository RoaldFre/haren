module Haray (RayTracer) where

data RayTracer a
instance Monad RayTracer
instance Functor RayTracer
