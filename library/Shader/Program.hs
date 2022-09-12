{-# LANGUAGE ViewPatterns #-}

-- |
-- A "declarative" API for describing shader programs.
module Shader.Program (
 -- * Using shaders
 Shader (..),
 withProgram,

 -- * Compiling shaders
 Program,
 create,

 -- ** DSL
 attribute,
 shader,
 uniform,
) where

import Control.Applicative.Free (Ap)
import Control.Applicative.Free qualified as Ap
import Control.Exception (bracket)
import Control.Lens (ifor_, (&))
import Control.Monad (unless)
import Data.ByteString (readFile)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)
import Graphics.Rendering.OpenGL qualified as GL
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (hPrintf)
import Prelude hiding (readFile)

-- | The result of a 'Program' is a 'Shader'. Another very overloaded term.
-- This datatype stores the 'GL.Program' along with any uniforms or attributes
-- collected in the 'Program'.
type Shader :: Type -> Type
data Shader x = Shader {program :: GL.Program, interface :: x}
 deriving stock (Eq, Foldable, Functor, Ord, Show, Traversable)

-- | Run an action while the given 'Shader' is enabled. This action most likely
-- renders something. Under the hood, this is just a bracketed call to
-- @OpenGL@'s 'GL.currentProgram' call.
withProgram :: Shader x -> IO y -> IO y
withProgram current action = bracket setup (const teardown) (const action)
 where
  setup :: IO ()
  setup = GL.currentProgram GL.$= Just (program current)

  teardown :: IO ()
  teardown = GL.currentProgram GL.$= Nothing

-- | An algebra for defining 'Shader' values.
type ProgramF :: Type -> Type
data ProgramF x where
 AttributeF :: Word32 -> String -> ProgramF GL.AttribLocation
 UniformF :: String -> ProgramF GL.UniformLocation
 ShaderF :: GL.ShaderType -> FilePath -> ProgramF ()

-- | An 'Applicative' for defining a 'Shader' 'Program' specification. This is
-- compiled using 'create'.
type Program :: Type -> Type
type Program = Ap ProgramF

-- | Interpret a 'Program' to compile a 'GL.Program' while also creating a
-- 'Shader' whose interface is the result of the 'Applicative' computation.
create :: Program x -> IO (Shader x)
create action = do
 program <- GL.createProgram

 let -- Before compiling, we need to collect all the mentioned shaders and
     -- attributes to bind to the program.
     _setup :: (Map String Word32, Map GL.ShaderType FilePath)
     _setup@(attributes, shaders) =
      action & Ap.runAp_ \case
       AttributeF i k -> (Map.singleton k i, Map.empty)
       UniformF _ -> (Map.empty, Map.empty)
       ShaderF k v -> (Map.empty, Map.singleton k v)

 -- Compile and attach each shader.
 ifor_ shaders \shaderType shaderPath -> do
  result <- GL.createShader shaderType
  source <- readFile shaderPath

  GL.shaderSourceBS result GL.$= source

  GL.compileShader result
  compileStatus <- GL.get (GL.compileStatus result)

  unless compileStatus do
   infoLog <- GL.shaderInfoLog result

   hPutStrLn stderr (show shaderType)
   hPutStrLn stderr infoLog
   exitFailure

  GL.attachShader program result

 -- Bind each attribute to the specified location.
 ifor_ attributes \name (GL.AttribLocation -> location) ->
  GL.attribLocation program name GL.$= location

 sandbox <- GL.genObjectName
 GL.bindVertexArrayObject GL.$= Just sandbox

 GL.linkProgram program
 linkOK <- GL.linkStatus program

 GL.validateProgram program
 validateOK <- GL.get (GL.validateStatus program)

 unless (linkOK && validateOK) do
  infoLog <- GL.get (GL.programInfoLog program)

  hPutStrLn stderr "Linking or validation error"
  hPutStrLn stderr infoLog

  exitFailure

 GL.bindVertexArrayObject GL.$= Nothing

 -- For debugging's sake, log every found attribute.
 GL.activeAttribs program >>= mapM_ \(_, _, name) ->
  hPrintf stderr "Found attribute '%s'\n" name

 -- ... and do the same with the uniforms.
 GL.activeUniforms program >>= mapM_ \(_, _, name) ->
  hPrintf stderr "Found uniform '%s'\n" name

 -- Now we have a compiled program, we can actually interpret the 'Shader' we
 -- wrote. Specifically, we can just look up the locations of the attributes
 -- and uniforms mentioned, and entirely ignore the shaders.
 --
 -- In theory, the 'GL.attribLocation' call here is pointless, but I suppose
 -- it doesn't hurt.
 interface <-
  action & Ap.runAp \case
   AttributeF _ k -> GL.get (GL.attribLocation program k)
   UniformF k -> GL.get (GL.uniformLocation program k)
   ShaderF _ _ -> pure ()

 pure Shader {program, interface}

-- | Define an attribute (as well as its vertex attribute array index). Returns
-- the given index wrapped in 'GL.AttribLocation'. These are values we feed
-- into the top of the shader pipeline.
attribute :: Word32 -> String -> Program GL.AttribLocation
attribute index = Ap.liftAp . AttributeF index

-- | Define a type of shader to use along with the path to its source file.
-- Typically, most programs involve a vertex shader and a fragment shader.
shader :: GL.ShaderType -> FilePath -> Program ()
shader shaderType = Ap.liftAp . ShaderF shaderType

-- | Define a uniform within the program. These are values we feed into every
-- step of the shader pipeline.
uniform :: String -> Program GL.UniformLocation
uniform = Ap.liftAp . UniformF
