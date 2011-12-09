{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Renderer where

import Types

class (Monad m, Functor m) => Renderer c m | m -> c, c -> m where
    colorPixel :: Pixel -> m Color
    getResolution :: m Resolution
    run :: Scene -> c -> m a -> a -- ^ scene -> config -> computation -> result


class (Monad m, Functor m) => RendererST c m s | m -> c, c -> m where
    colorPixel_ :: Pixel -> m Color 
    getResolution_ :: m Resolution
    run_ :: Scene -> c -> m a -> a -- ^ scene -> config -> computation -> result

-- vim: expandtab smarttab sw=4 ts=4
