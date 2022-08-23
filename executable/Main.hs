{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Control qualified
import Display (withOpenGLWindow)
import Entity (Entity (..))
import Graphics.Rendering.OpenGL qualified as GL
import Input qualified
import Maths qualified
import Model qualified
import Renderer (prepare, render)
import SDL qualified
import Shader.Core qualified
import Shader.Static qualified
import Texture qualified

main :: IO ()
main = withOpenGLWindow \window _ -> do
  program <- Shader.Static.create

  model <- Model.create Model.Config
    { Model.configIndices =
        [ GL.Vertex3 0 1 3
        , GL.Vertex3 3 1 2
        ]

    , Model.configPositions =
        [ GL.Vertex3 (-0.5) ( 0.5) 0
        , GL.Vertex3 (-0.5) (-0.5) 0
        , GL.Vertex3 ( 0.5) (-0.5) 0
        , GL.Vertex3 ( 0.5) ( 0.5) 0
        ]

    , Model.configTextureCoords =
        [ GL.Vertex2 0 0
        , GL.Vertex2 0 1
        , GL.Vertex2 1 1
        , GL.Vertex2 1 0
        ]
    }

  texture <- Texture.loadTexture("textures/beef.png")

  let update :: SDL.Event -> Float -> Float
      update event current
        | Input.isPressed SDL.KeycodeLeft  event = current - 0.1
        | Input.isPressed SDL.KeycodeRight event = current + 0.1
        | otherwise = current

  Control.loop 0 update \state -> do
    prepare

    let entity :: Entity
        entity =
          Entity
            { entityModel     = model
            , entityTransform = Maths.translate (Maths.V3 state 0 0)
            }

    Shader.Core.with program do
      render texture entity program

    SDL.glSwapWindow window
