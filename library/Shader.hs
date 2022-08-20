{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Shader where

import Control.Monad (unless)
import Data.ByteString (readFile)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Graphics.Rendering.OpenGL qualified as GL
import Prelude hiding (readFile)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

-- | Load a shader of the given type from the given file path.
load :: FilePath -> GL.ShaderType -> IO GL.Shader
load path shaderType = do
  shader <- GL.createShader shaderType

  source <- readFile path
  GL.shaderSourceBS shader GL.$= source

  GL.compileShader shader
  compileStatus <- GL.get (GL.compileStatus shader)

  unless compileStatus do
    infoLog <- GL.shaderInfoLog shader

    hPutStrLn stderr (show shaderType)
    hPutStrLn stderr infoLog
    exitFailure

  pure shader

-- | Create a shader program with the given parameters and shaders.
createWith :: Map String GL.AttribLocation -> Map GL.ShaderType FilePath -> IO GL.Program
createWith attributes shaders = do
  program <- GL.createProgram

  _ <- flip Map.traverseWithKey shaders \shaderType shaderPath -> do
    shader <- load shaderPath shaderType
    GL.attachShader program shader

  _ <- flip Map.traverseWithKey attributes \attribute location ->
    GL.attribLocation program attribute GL.$= location

  sandbox <- GL.genObjectName
  GL.bindVertexArrayObject GL.$= Just sandbox

  GL.linkProgram program
  linkOK <- GL.linkStatus program

  GL.validateProgram program
  validateOK <- GL.get (GL.validateStatus program)

  GL.bindVertexArrayObject GL.$= Nothing

  unless (linkOK && validateOK) do
    infoLog <- GL.get (GL.programInfoLog program)

    hPutStrLn stderr "Linking or validation error"
    hPutStrLn stderr infoLog

    exitFailure

  pure program

withProgram :: GL.Program -> IO x -> IO x
withProgram program action = do
  GL.currentProgram GL.$= Just program
  result <- action

  GL.currentProgram GL.$= Nothing
  pure result
