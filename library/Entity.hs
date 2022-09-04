{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Entities are models that have been placed in the world.
module Entity where

import Data.Kind (Type)
import Graphics.Rendering.OpenGL qualified as GL
import Model (Model)
import SDL qualified

-- | A container for a model, a texture, and the components of a transformation
-- matrix. This is the data required to render a model to the screen.
type Entity :: Type
data Entity
  = Entity
      { model     :: Model
      , texture   :: GL.TextureObject
      , translate :: SDL.V3 Float
      , rotate    :: SDL.Quaternion Float
      , scale     :: Float
      }
  deriving (Eq, Ord, Show)
