{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Camera (Camera (Camera))
import Camera qualified
import Control qualified
import Display (withOpenGLWindow)
import Entity (Entity)
import Entity qualified
import Graphics.Rendering.OpenGL qualified as GL
import Model qualified
import Renderer (prepare, render)
import SDL qualified
import Shader.Program qualified as Shader (Shader (..), withProgram)
import Shader.Static qualified as Shader (static, viewMatrix)
import Texture qualified

-- | Spin the cube regardless of events.
update :: [SDL.Event] -> Entity -> Entity
update _ e = e { Entity.rotate = Entity.rotate e * delta }
  where delta = SDL.axisAngle (SDL.V3 1 1 1) 0.001 

main :: IO ()
main = withOpenGLWindow \window -> do
  -- Create a static shader program, build the cat cube, and load its texture.
  program <- Shader.static
  model   <- beef
  texture <- Texture.loadTexture("textures/beef.png")

  let
    -- An entity (specifically, a cube of pictures of my cat, Beef) to be
    -- rendered on the screen.
    initial :: Entity
    initial =
      Entity.Entity
        { model     = model
        , texture   = texture
        , translate = SDL.V3 0 0 0
        , rotate    = SDL.axisAngle (SDL.V3 1 1 1) 0
        , scale     = 1
        }

  viewMatrix <- Camera.view Camera
    { position = SDL.V3 0 0 3
    , pitch = 0
    , yaw = 0
    }

  Control.loop initial update \state -> do
    prepare

    -- We could move everything other than 'render' here out of the control
    -- loop as, at the moment, the camera can't move once it has been created.
    Shader.withProgram program do
      let location :: GL.UniformLocation
          location = Shader.viewMatrix (Shader.interface program)

      GL.uniform location GL.$= viewMatrix
      render state program

    SDL.glSwapWindow window

beef :: IO Model.Model
beef = Model.create Model.Config
  { Model.configIndices = -- Which positions make up each triangle?
      [ SDL.V3 0  1  3
      , SDL.V3 3  1  2
      , SDL.V3 4  5  7
      , SDL.V3 7  5  6
      , SDL.V3 8  9  11
      , SDL.V3 11 9  10
      , SDL.V3 12 13 15
      , SDL.V3 15 13 14
      , SDL.V3 16 17 19
      , SDL.V3 19 17 18
      , SDL.V3 20 21 23
      , SDL.V3 23 21 22
      ]

  , Model.configPositions = -- A list of vertex positions.
      [ SDL.V3 (-0.5) ( 0.5) (-0.5)
      , SDL.V3 (-0.5) (-0.5) (-0.5)
      , SDL.V3 ( 0.5) (-0.5) (-0.5)
      , SDL.V3 ( 0.5) ( 0.5) (-0.5)

      , SDL.V3 (-0.5) 0.5 0.5
      , SDL.V3 (-0.5) (-0.5) 0.5
      , SDL.V3 0.5 (-0.5) 0.5
      , SDL.V3 0.5 0.5 0.5

      , SDL.V3 0.5 0.5 (-0.5)
      , SDL.V3 0.5 (-0.5) (-0.5)
      , SDL.V3 0.5 (-0.5) 0.5
      , SDL.V3 0.5 0.5 (0.5)


      , SDL.V3 (-0.5) 0.5 (-0.5)
      , SDL.V3 (-0.5) (-0.5) (-0.5)
      , SDL.V3 (-0.5) (-0.5) 0.5
      , SDL.V3 (-0.5) 0.5 0.5

      , SDL.V3 (-0.5) 0.5 0.5
      , SDL.V3 (-0.5) 0.5 (-0.5)
      , SDL.V3 0.5 0.5 (-0.5)
      , SDL.V3 0.5 0.5 0.5

      , SDL.V3 (-0.5) (-0.5) 0.5
      , SDL.V3 (-0.5) (-0.5) (-0.5)
      , SDL.V3 0.5 (-0.5) (-0.5)
      , SDL.V3 0.5 (-0.5) 0.5
      ]

  , Model.configTextureCoords = -- Where does each vertex land on the texture?
      [ SDL.V2 0 0
      , SDL.V2 0 0.25
      , SDL.V2 0.25 0.25
      , SDL.V2 0.25 0

      , SDL.V2 0.25 0
      , SDL.V2 0.5 0
      , SDL.V2 0.5 0.25
      , SDL.V2 0.25 0.25

      , SDL.V2 0.5 0
      , SDL.V2 0.75 0
      , SDL.V2 0.75 0.25
      , SDL.V2 0.5 0.25

      , SDL.V2 0.75 0
      , SDL.V2 1 0
      , SDL.V2 1 0.25
      , SDL.V2 0.75 0.25

      , SDL.V2 0.25 0.25
      , SDL.V2 0.25 0.5
      , SDL.V2 0.5 0.5
      , SDL.V2 0.5 0.25

      , SDL.V2 0.25 0.25
      , SDL.V2 0.25 0.5
      , SDL.V2 0.5 0.5
      , SDL.V2 0.5 0.25
      ]
  }
