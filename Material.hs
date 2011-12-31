{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances #-}
module Material (
    IncidentLight,
    --Material(..),
    Material(colorMaterial, colorMaterialSingleLight),
    AnyMat(..),

    UVec3, -- from module Math
    module Intersection,
    module Color
) where

import {-# SOURCE #-} Haray
import {-# SOURCE #-} Object
import Math
import Color
import Intersection
import Data.List

-- | (direction pointing *to* the lightsource, color of incident light)
-- The direction is normalised to unity.
type IncidentLight = (UVec3, Color)

-- | Minimum implementation: either colorMaterial or 
-- colorMaterialSingleLight.
class Material a where
    colorMaterial :: Intersection Object -> a -> [IncidentLight] -> RayTracer Color
    colorMaterial int mat incidentLights = do
        contributions <- mapM (colorMaterialSingleLight int mat) incidentLights
        return $ foldl' (.+.) black contributions

    colorMaterialSingleLight :: Intersection Object -> a -> IncidentLight -> RayTracer Color
    colorMaterialSingleLight int mat incidentLight = 
        colorMaterial int mat [incidentLight]

-- Existential material
data AnyMat = forall a . (Material a, Show a) => MkAnyMat a
instance Show AnyMat where
    show (MkAnyMat mat) = "AnyMat " ++ show mat
instance Material AnyMat where
    colorMaterial int (MkAnyMat mat) = colorMaterial int mat


-- vim: expandtab smarttab sw=4 ts=4
