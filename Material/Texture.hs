{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Material.Texture (
    mkTexture,
    Tex,
    checkers,
) where

import Material
import Math

data Texture = Texture Tex deriving Show
mkTexture :: Tex -> AnyMat
mkTexture texture = MkAnyMat $ Texture texture

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


instance Material Texture where
    colorMaterialSingleLight int (Texture f) (ilDir, ilCol) = case intTexUV int of
        Nothing -> error "Trying to map texture on something without texture coordinates!"
        Just uv -> return $ (ilCol .***. f uv) .* (ilDir .*. (intNorm int))

-- vim: expandtab smarttab sw=4 ts=4
