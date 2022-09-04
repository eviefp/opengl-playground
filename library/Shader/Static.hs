{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- A shader for rendering plain, static objects.
module Shader.Static where

import Data.Kind (Type)
import Graphics.Rendering.OpenGL qualified as GL
import Maths qualified
import Shader.Program (Shader)
import Shader.Program qualified as Shader
import SDL qualified

-- | A static shader's interface. At the moment, we can control the position of
-- its vertices, the associated texture coordinates, and the matrices that
-- govern its global, camera and local coordinate transformations.
type Static :: Type
data Static
  = Static
      { position      :: GL.AttribLocation
      , textureCoords :: GL.AttribLocation

      , projectionMatrix     :: GL.UniformLocation
      , transformationMatrix :: GL.UniformLocation
      , viewMatrix           :: GL.UniformLocation
      }

-- | Create a 'Static' shader.
static :: IO (Shader Static)
static = do
  shader <- Shader.create do
    Shader.shader GL.VertexShader   "shaders/static/shader.vert"
    Shader.shader GL.FragmentShader "shaders/static/shader.frag"

    position      <- Shader.attribute 0 "position"
    textureCoords <- Shader.attribute 1 "textureCoords"

    projectionMatrix     <- Shader.uniform "projectionMatrix"
    transformationMatrix <- Shader.uniform "transformationMatrix"
    viewMatrix           <- Shader.uniform "viewMatrix"

    pure Static{..}

  Shader.withProgram shader do
    projection <- Maths.toGLmatrix @GL.GLfloat (SDL.perspective 0.8 1 0.1 1000)
    GL.uniform (projectionMatrix (Shader.interface shader)) GL.$= projection

  pure shader
