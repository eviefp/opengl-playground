{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- General-purpose functions for doing maths.
module Maths
  ( V3 (..)

  , identity
  , ortho

  , rotate
  , scale
  , translate

  , toGLmatrix
  ) where

import Control.Lens ((&), (+~))
import Data.Foldable (toList)
import Graphics.Rendering.OpenGL qualified as GL
import Linear.Quaternion (Quaternion)
import Linear.V3 (V3 (..))
import SDL (identity, ortho)
import SDL qualified

-- | Once we have a matrix, we can prepare it for OpenGL by calculating a
-- position in memory for it.
toGLmatrix :: GL.MatrixComponent x => SDL.M44 x -> IO (GL.GLmatrix x)
toGLmatrix = GL.newMatrix GL.RowMajor . foldMap toList

-- | Create a translation matrix.
translate :: Num x => V3 x -> SDL.M44 x
translate vector = identity & SDL.translation +~ vector

-- | Create a rotation matrix from a quaternion.
rotate :: Num x => Quaternion x -> SDL.M44 x
rotate quaternion = SDL.mkTransformation quaternion (V3 0 0 0)

-- | Create a uniform scaling matrix.
scale :: Num x => x -> SDL.M44 x
scale factor = factor SDL.*!! identity
