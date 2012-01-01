{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Material.Texture (
    mkTexture,
    Tex,
    checkers,
    gridLines,
) where

import Material
import Math
import Control.Applicative

data Texture = Texture Tex AnyMat deriving Show
mkTexture :: AnyMat -> Tex -> AnyMat
mkTexture baseMaterial texture = MkAnyMat $ Texture texture baseMaterial

type Tex = (Pt2 -> Color) -- ^ function of (u,v) to colors, u and v in [0,1]
instance Show Tex where
    show _ = "<<Texture>>"

checkers :: Color -> Color -> Int -> Int -> Tex
checkers c1 c2 nu nv (F2 u v)
    | uParity + vParity == 1  =  c1
    | otherwise               =  c2
    where
        uParity = (floor $ (fromIntegral nu) * u) `mod` 2 :: Int
        vParity = (floor $ (fromIntegral nv) * v) `mod` 2 :: Int

-- TODO make a combinater "tile" that tiles textures in a tile/grid

gridLines :: Flt -> Color -> Color -> Flt -> Flt -> Tex
gridLines width c1 c2 nu nv (F2 u v)
    | uHits && vHits  =  c2
    | otherwise       =  c1
    where
        uBox = nu * u - (fromIntegral $ floor $ nu * u)
        vBox = nv * v - (fromIntegral $ floor $ nv * v)
        uHits = uBox > width
        vHits = vBox > width

instance Material Texture where
    colorMaterial int (Texture f baseMaterial) ilights = case intTexUV int of
        Nothing -> error "Trying to map texture on something without texture coordinates!"
        Just uv -> (f uv .***.) <$> colorMaterial int baseMaterial ilights

-- vim: expandtab smarttab sw=4 ts=4
