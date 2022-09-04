{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- |
-- Operations for defining cameras and compiling them to matrices.
module Camera where

import Data.Kind (Type)
import Graphics.Rendering.OpenGL qualified as GL
import Maths qualified
import SDL qualified

-- | At the moment, a camera is defined by its position along with its rotation
-- as a function of pitch and yaw. Note that we don't allow for roll rotation!
-- This avoids gimbal lock, and makes the whole thing a lot easier.
type Camera :: Type
data Camera
  = Camera
      { position :: SDL.V3 Float
      , pitch    :: Float
      , yaw      :: Float
      }
  deriving (Eq, Ord, Show)

-- | Convert a 'Camera' into a 'GL.GLmatrix', most likely to be passed as a
-- uniform variable into a 'Shader.Shader'.
view :: Camera -> IO (GL.GLmatrix GL.GLfloat)
view Camera{..} = Maths.transform (negate position) rotation 1
  where
    rotation :: SDL.Quaternion GL.GLfloat
    rotation
      = SDL.axisAngle (SDL.V3 1 0 0) pitch
      * SDL.axisAngle (SDL.V3 0 1 0) yaw
