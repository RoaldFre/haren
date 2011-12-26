module Camera where

import Math

data CameraGaze = CameraGaze {
        cgPos  :: Pt3,
        cgDir  :: Vec3,
        cgUp   :: Vec3,
        cgFovy :: Flt -- ^ in degrees
    } deriving Show

data Camera = Camera {
        camPos  :: Pt3,
        camUVW  :: CoordSyst, -- ^ w is the reverse of the looking direction
        camFovy :: Flt -- ^ in degrees
    } deriving Show

-- TODO use this elsewhere? (eg when setting up orthonormal coords some 
-- place else?)
type CoordSyst = (UVec3, UVec3, UVec3)

camFromCamGaze :: CameraGaze -> Camera
camFromCamGaze (CameraGaze pos dir up fovy) = Camera pos (u, v, w) fovy
    where
        w = (-1) *. (normalize dir)
        u = normalize $ up .^. w
        v = w .^. u

camLookingAt :: Pt3 -> Pt3 -> UVec3 -> Flt -> Camera
camLookingAt pos lookAt up fovy =
    camFromCamGaze $ CameraGaze pos (direction pos lookAt) up fovy

-- vim: expandtab smarttab sw=4 ts=4
