{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Entities are models that have been placed in the world.
module Entity where

import Data.Kind (Type)
import Graphics.Rendering.OpenGL qualified as GL
import Linear.Matrix qualified as Matrix
import Model (Model)

-- | A container for a model and a position.
type Entity :: Type
data Entity
  = Entity
      { entityModel     :: Model
      , entityTransform :: Matrix.M44 GL.GLfloat
      }
