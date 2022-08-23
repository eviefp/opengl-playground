{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- |
-- A static shader represents objects that are rendered entirely statically.
-- They can be transformed, but are otherwise stationary.
module Shader.Static where

import Data.Kind (Type)
import Graphics.Rendering.OpenGL qualified as GL
import Shader.Core qualified as Shader

-- | A shader for rendering static elements.
type StaticShader :: Type
data StaticShader
  = StaticShader
      { staticShaderProgram  :: GL.Program
      , transformationMatrix :: GL.StateVar (GL.GLmatrix GL.GLfloat)
      }

instance Shader.HasProgram StaticShader where
  getProgram = staticShaderProgram

-- | Create a static element shader.
create :: IO StaticShader
create = do
  staticShaderProgram <- Shader.create
    Shader.Config
      { configAttributes =
          [ ( "position", GL.AttribLocation 0 ),
            ( "textureCoords", GL.AttribLocation 1 )
          ]

      , configShaders =
          [ (GL.VertexShader, "shaders/static/shader.vert")
          , (GL.FragmentShader, "shaders/static/shader.frag")
          ]
      }

  transformationMatrixLocation <- GL.get do
    GL.uniformLocation staticShaderProgram "transformationMatrix"

  pure StaticShader
    { staticShaderProgram  = staticShaderProgram
    , transformationMatrix = GL.uniform (transformationMatrixLocation)
    }
