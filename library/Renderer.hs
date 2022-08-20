{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Operations related to rendering objects.
module Renderer where

import Graphics.Rendering.OpenGL qualified as GL
import Foreign.Ptr (nullPtr)
import Model (Model (..))

-- | Clear the colour buffer before drawing.
prepare :: IO ()
prepare = do
  GL.clearColor GL.$= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]

-- | Render the given model to the display buffers.
render :: GL.TextureObject -> Model -> IO ()
render texture model = do
  GL.bindVertexArrayObject GL.$= Just (modelIdentifier model)

  GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Enabled
  GL.vertexAttribArray (GL.AttribLocation 1) GL.$= GL.Enabled
  
  GL.activeTexture GL.$= GL.TextureUnit 0
  GL.texture GL.Texture2D GL.$= GL.Enabled
  GL.textureBinding GL.Texture2D GL.$= Just texture

  GL.drawElements GL.Triangles (modelVertices model) GL.UnsignedInt nullPtr

  GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Disabled
  GL.vertexAttribArray (GL.AttribLocation 1) GL.$= GL.Disabled

  GL.bindVertexArrayObject GL.$= Nothing
