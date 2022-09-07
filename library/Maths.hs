{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- General-purpose functions for doing maths.
module Maths where

import Foreign.Ptr (castPtr)
import Foreign.Storable (poke)
import Graphics.Rendering.OpenGL qualified as GL
import SDL (Quaternion, V3 (..))
import SDL qualified

-- | Create a transformation matrix from a translation, rotation, and uniform
-- scale factor. Rotations will be performed about the origin before
-- translation.
transform :: (GL.MatrixComponent x, Num x) => V3 x -> Quaternion x -> x -> IO (GL.GLmatrix x)
transform translation rotation scaling = toGLmatrix do
  scaling SDL.*!! SDL.mkTransformation rotation translation

-- | Once we have a matrix, we can prepare it for OpenGL by calculating a
-- position in memory for it.
toGLmatrix :: GL.MatrixComponent x => SDL.M44 x -> IO (GL.GLmatrix x)
toGLmatrix matrix = GL.withNewMatrix GL.RowMajor \p -> poke (castPtr p) matrix
