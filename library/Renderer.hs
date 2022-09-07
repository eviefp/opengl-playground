{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Operations related to rendering objects.
module Renderer where

import Entity (Entity (..))
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL qualified as GL
import Maths qualified
import Model (Model (..))
import Model qualified as Model
import Shader.Program (Shader (interface))
import Shader.Static (Static, transformationMatrix)

-- | Clear the colour buffer before drawing. Also, because we use OpenGL's
-- inbuilt mechanism for computing the order in which triangles should be
-- rendered (i.e. descending order of distance from the camera, we clear the
-- depth buffer as well.
prepare :: IO ()
prepare = do
  GL.clearColor GL.$= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.depthFunc GL.$= Just GL.Lequal

-- | Render the given model to the display buffers. The model is rendered by
-- the shaders according to the entity's given transformation matrix.
render :: Entity -> Shader Static -> IO ()
render Entity{ model, rotate, scale, texture, translate } shader = do
  Model.withVertexArrayObject (modelIdentifier model) do
    GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Enabled
    GL.vertexAttribArray (GL.AttribLocation 1) GL.$= GL.Enabled

    transformation <- Maths.transform translate rotate scale
    GL.uniform (transformationMatrix (interface shader)) GL.$= transformation

    GL.activeTexture GL.$= GL.TextureUnit 0
    GL.texture GL.Texture2D GL.$= GL.Enabled
    GL.textureBinding GL.Texture2D GL.$= Just texture

    GL.drawElements GL.Triangles (modelVertices model) GL.UnsignedInt nullPtr

    GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Disabled
    GL.vertexAttribArray (GL.AttribLocation 1) GL.$= GL.Disabled
