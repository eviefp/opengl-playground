{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- |
-- Generic for producing shader programs.
module Shader.Core where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.ByteString (readFile)
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Graphics.Rendering.OpenGL qualified as GL
import Model qualified
import Prelude hiding (readFile)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | Some of our shader setups get more elaborate, particularly if uniform
-- variables are involved. To keep the interface tidy, we can write rendering
-- functions in terms of this class.
type HasProgram :: Type -> Constraint
class HasProgram object where
  getProgram :: object -> GL.Program

-- | The configuration required to build any sort of model. We deal with
-- uniform locations /after/ compilation.
type Config :: Type
data Config
  = Config
      { configAttributes :: Map String GL.AttribLocation
      , configShaders :: Map GL.ShaderType FilePath
      }

-- | Create a new shader program using the given variables and shader files.
-- This is a convenience function for other shader constructors.
create :: Config -> IO GL.Program
create (Config attributes shaders) = do
  program <- GL.createProgram

  -- Compile each shader individually, and fail with errors if any of them
  -- can't be compiled.
  _ <- flip Map.traverseWithKey shaders \shaderType shaderPath -> do
    shader <- GL.createShader shaderType

    source <- readFile shaderPath
    GL.shaderSourceBS shader GL.$= source

    GL.compileShader shader
    compileStatus <- GL.get (GL.compileStatus shader)

    unless compileStatus do
      infoLog <- GL.shaderInfoLog shader

      hPutStrLn stderr (show shaderType)
      hPutStrLn stderr infoLog
      exitFailure

    GL.attachShader program shader

  -- Register each of the attributes mentioned.
  _ <- flip Map.traverseWithKey attributes \attribute location ->
    GL.attribLocation program attribute GL.$= location

  -- With later versions of OpenGL, we need a VAO bound when we link and
  -- validate a shader. We can then immediately throw it away.
  GL.genObjectName >>= flip Model.withVAO do
    GL.linkProgram program
    linkOK <- GL.linkStatus program

    GL.validateProgram program
    validateOK <- GL.get (GL.validateStatus program)

    unless (linkOK && validateOK) do
      infoLog <- GL.get (GL.programInfoLog program)

      hPutStrLn stderr "Linking or validation error"
      hPutStrLn stderr infoLog

      exitFailure

  pure program

-- | Run an operation while the given shader program is registered as the
-- current program in the OpenGL context.
with :: HasProgram object => object -> IO x -> IO x
with object = bracket setup (const teardown) . const
  where
    setup :: IO ()
    setup = GL.currentProgram GL.$= Just (getProgram object)

    teardown :: IO ()
    teardown = GL.currentProgram GL.$= Nothing
