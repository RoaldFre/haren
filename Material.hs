{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances #-}
module Material (
    IncidentLight,
    Material(..),
    AnyMat(..),

    colorMat,
    scaleMat,
    combineMats,

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
import Control.Applicative

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


-- Combinators

newtype ColoredMat = ColoredMat (Color, AnyMat) deriving Show
-- | Modulate the material with the given color
colorMat :: Color -> AnyMat -> AnyMat
colorMat col mat = MkAnyMat $ ColoredMat (col, mat)

newtype ScaledMat = ScaledMat (Flt, AnyMat) deriving Show
-- | Scale the material with the given weight
scaleMat :: Flt -> AnyMat -> AnyMat
scaleMat weight mat = MkAnyMat $ ScaledMat (weight, mat)

newtype CombinedMat = CombinedMat [AnyMat] deriving Show
combineMats :: [AnyMat] -> AnyMat
combineMats mats = MkAnyMat $ CombinedMat mats

instance Material ColoredMat where
    colorMaterial int (ColoredMat (col, mat)) incidentLights =
        (col .***.) <$> colorMaterial int mat incidentLights

instance Material ScaledMat where
    colorMaterial int (ScaledMat (weight, mat)) incidentLights =
        (weight *.) <$> colorMaterial int mat incidentLights

instance Material CombinedMat where
    colorMaterial int (CombinedMat materials) incidentLights = do
        contributions <- mapM (\m -> colorMaterial int m incidentLights) materials
        return $ foldl' (.+.) black contributions

-- vim: expandtab smarttab sw=4 ts=4
