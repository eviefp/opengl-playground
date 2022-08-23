{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Operations related to rendering objects.
module Renderer where

import Entity (Entity (..))
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL qualified as GL
import Maths qualified
import Model (Model (..))
import Shader.Static (StaticShader, transformationMatrix)

-- | Clear the colour buffer before drawing.
prepare :: IO ()
prepare = do
  GL.clearColor GL.$= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]

-- | Render the given model to the display buffers.
render :: GL.TextureObject -> Entity -> StaticShader -> IO ()
render texture entity shader = do
  let model :: Model
      model = entityModel entity

  GL.bindVertexArrayObject GL.$= Just (modelIdentifier model)

  GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Enabled
  GL.vertexAttribArray (GL.AttribLocation 1) GL.$= GL.Enabled

  matrix <- Maths.toGLmatrix (entityTransform entity)
  transformationMatrix shader GL.$= matrix

  GL.activeTexture GL.$= GL.TextureUnit 0
  GL.texture GL.Texture2D GL.$= GL.Enabled
  GL.textureBinding GL.Texture2D GL.$= Just texture

  GL.drawElements GL.Triangles (modelVertices model) GL.UnsignedInt nullPtr

  GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Disabled
  GL.vertexAttribArray (GL.AttribLocation 1) GL.$= GL.Disabled

  GL.bindVertexArrayObject GL.$= Nothing
