{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Texture operations.
module Texture where

import Graphics.GLUtil qualified as Utilities
import Graphics.Rendering.OpenGL qualified as GL
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

-- | Load a texture from disk.
loadTexture :: FilePath -> IO GL.TextureObject
loadTexture path = Utilities.readTexture path >>= \case
  Right texture -> do
    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
    Utilities.texture2DWrap GL.$= (GL.Repeated, GL.ClampToEdge)

    pure texture

  Left message -> do
    hPutStrLn stderr ("Loading texture " <> path)
    hPutStrLn stderr message

    exitFailure
