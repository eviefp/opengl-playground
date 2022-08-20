{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.Monad (unless)
import Display (withOpenGL, withWindow)
import Graphics.Rendering.OpenGL qualified as GL
import Model qualified
import Renderer (prepare, render)
import Shader qualified
import Texture qualified
import SDL qualified

main :: IO ()
main = withWindow \window ->
  withOpenGL window \_ -> do
    program <- Shader.createWith
      [ ( "position", GL.AttribLocation 0 ),
        ( "textureCoords", GL.AttribLocation 1 )
      ]
      [ (GL.VertexShader, "shaders/static/shader.vert")
      , (GL.FragmentShader, "shaders/static/shader.frag")
      ]

    model <- Model.create [ 0, 1, 3, 3, 1, 2 ]
        [ GL.Vertex3 (-0.5) ( 0.5) 0
        , GL.Vertex3 (-0.5) (-0.5) 0
        , GL.Vertex3 ( 0.5) (-0.5) 0
        , GL.Vertex3 ( 0.5) ( 0.5) 0
        ]
        [ GL.Vertex2 0 0
        , GL.Vertex2 0 1
        , GL.Vertex2 1 1
        , GL.Vertex2 1 0
        ]

    texture <- Texture.loadTexture("textures/beef.png")

    loopUntilUserQuits \_events -> do
      prepare

      Shader.withProgram program do
        render texture model

      SDL.glSwapWindow window

loopUntilUserQuits :: ([SDL.Event] -> IO ()) -> IO ()
loopUntilUserQuits callback = do
  events <- SDL.pollEvents

  unless (SDL.QuitEvent `elem` map SDL.eventPayload events) do
    callback events
    loopUntilUserQuits callback
