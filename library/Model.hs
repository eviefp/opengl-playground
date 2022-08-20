{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Information for identifying OpenGL models and meshes.
module Model where

import Data.Kind (Type)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable, sizeOf)
import Graphics.Rendering.OpenGL qualified as GL

-- | Once we've bound all the information to the VAO, the information we have
-- to store boils down to the VAO name and the number of vertices in the mesh.
type Model :: Type
data Model =
  Model
    { modelIdentifier :: GL.VertexArrayObject
    , modelVertices   :: GL.NumArrayIndices
    }
  deriving Show

-- | Create a model from a list of indices and positions.
create :: [GL.GLint] -> [GL.Vertex3 GL.GLfloat] -> [GL.Vertex2 GL.GLfloat] -> IO Model
create indices positions textureCoordinates = do
  vertexArrayObjectName <- GL.genObjectName
  GL.bindVertexArrayObject GL.$= Just vertexArrayObjectName

  indexBuffer <- GL.genObjectName

  GL.bindBuffer GL.ElementArrayBuffer GL.$= Just indexBuffer
  bindToBuffer GL.ElementArrayBuffer GL.StaticDraw indices

  storeInVAO (GL.AttribLocation 0) 3 positions
  storeInVAO (GL.AttribLocation 1) 2 textureCoordinates

  GL.bindVertexArrayObject GL.$= Nothing
  GL.bindBuffer GL.ElementArrayBuffer GL.$= Nothing
  GL.bindBuffer GL.ArrayBuffer GL.$= Nothing

  pure Model
    { modelIdentifier = vertexArrayObjectName
    , modelVertices   = fromIntegral (length indices)
    }

-- | Bind an array of data to the given buffer for the given purpose.
bindToBuffer :: Storable x => GL.BufferTarget -> GL.BufferUsage -> [x] -> IO ()
bindToBuffer target usage (xs :: [x]) = withArray xs \pointer ->
  GL.bufferData target GL.$= (fromIntegral size, pointer, usage)
  where
    size :: Int
    size = sizeOf @x undefined * length xs

storeInVAO :: Storable x => GL.AttribLocation -> GL.NumComponents -> [x] -> IO ()
storeInVAO location coordinateSize xs = do
  buffer <- GL.genObjectName

  GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer
  bindToBuffer GL.ArrayBuffer GL.StaticDraw xs

  GL.vertexAttribPointer location GL.$=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor coordinateSize GL.Float 0 nullPtr
    )

  GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
