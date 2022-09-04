{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Operations for controlling the windows and their contexts.
module Display where

import Control.Exception (bracket)
import Control.Monad (unless)
import SDL qualified

-- | Run an operation for the lifetime of a window.
--
-- This operation is bracketed: if an exception occurs within the lifetime of
-- the window, it will be destroyed correctly.
withWindow :: (SDL.Window -> IO x) -> IO x
withWindow callback = do
  let setup :: IO SDL.Window
      setup = do
        -- TODO: this and @quit@ probably need moving once we have multiple
        -- windows?
        SDL.initialize [SDL.InitVideo]

        SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
        renderQuality <- SDL.get SDL.HintRenderScaleQuality

        unless (renderQuality == SDL.ScaleLinear) do
          putStrLn "Warning: Linear texture filtering not enabled!"

        SDL.createWindow "Visualiser"
          SDL.defaultWindow
            { SDL.windowGraphicsContext = SDL.OpenGLContext
                SDL.defaultOpenGL
                  { SDL.glProfile = SDL.Core SDL.Debug 4 1
                  }

            , SDL.windowInitialSize = SDL.V2 800 800
            }

      teardown :: SDL.Window -> IO ()
      teardown window = do
        SDL.destroyWindow window
        SDL.quit

  bracket setup teardown \window -> do
    SDL.showWindow window
    callback window

-- | Run an operation within an OpenGL context attached to the given
-- 'SDL.Window'.
withOpenGL :: SDL.Window -> (SDL.GLContext -> IO x) -> IO x
withOpenGL window = bracket setup SDL.glDeleteContext
  where
    setup :: IO SDL.GLContext
    setup = do
      context <- SDL.glCreateContext window
      SDL.glMakeCurrent window context

      pure context

-- | A combination of 'withWindow' and 'withOpenGL' for what will probably be
-- the most common use case.
withOpenGLWindow :: (SDL.Window -> IO x) -> IO x
withOpenGLWindow action = withWindow \window ->
  withOpenGL window \_ -> action window
