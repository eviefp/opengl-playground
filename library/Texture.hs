{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Operations for handling textures.
module Texture where

import Graphics.GLUtil (readTexture, texture2DWrap)
import Graphics.Rendering.OpenGL qualified as GL
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Text.Printf (hPrintf)

-- | Load a texture from disk.
loadTexture :: FilePath -> IO GL.TextureObject
loadTexture path = readTexture path >>= \case
  Right texture -> do
    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
    texture2DWrap GL.$= (GL.Repeated, GL.ClampToEdge)

    hPrintf stderr "Loaded texture '%s'\n" path
    pure texture

  Left message -> do
    hPrintf stderr "Error while loading texture '%s'\n" path
    hPutStrLn stderr message

    exitFailure
