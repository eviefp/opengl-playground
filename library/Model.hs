{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Information for identifying OpenGL models and meshes.
module Model where

import Control.Exception (bracket)
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

-- | Configuration for creating models. Models are defined by the vertex
-- positions, texture coordinates for each of those positions, and triples of
-- indices into those positions (determining how the triangles are made).
type Config :: Type
data Config =
  Config
    { configIndices       :: [GL.Vertex3 GL.GLint  ]
    , configPositions     :: [GL.Vertex3 GL.GLfloat]
    , configTextureCoords :: [GL.Vertex2 GL.GLfloat]
    }

-- | Create a model from a list of position vertices and the contents of an
-- index buffer. If texture coordinates are present, also bind the texture.
create :: Config -> IO Model
create Config{..} = do
  vertexArrayObjectName <- GL.genObjectName

  withVAO vertexArrayObjectName do
    indexBuffer <- GL.genObjectName

    GL.bindBuffer GL.ElementArrayBuffer GL.$= Just indexBuffer
    bindToBuffer GL.ElementArrayBuffer GL.StaticDraw configIndices

    storeInVAO (GL.AttribLocation 0) 3 configPositions
    storeInVAO (GL.AttribLocation 1) 2 configTextureCoords

  GL.bindBuffer GL.ElementArrayBuffer GL.$= Nothing
  GL.bindBuffer GL.ArrayBuffer GL.$= Nothing

  pure Model
    { modelIdentifier = vertexArrayObjectName
    , modelVertices   = fromIntegral (length configIndices * 3)
    }

-- | Bind an array of data to the given buffer for the given purpose.
bindToBuffer :: Storable x => GL.BufferTarget -> GL.BufferUsage -> [x] -> IO ()
bindToBuffer target usage (xs :: [x]) = withArray xs \pointer ->
  GL.bufferData target GL.$= (fromIntegral size, pointer, usage)
  where
    size :: Int
    size = sizeOf @x undefined * length xs

-- | Store the given information in the current VAO.
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

-- | Perform an action while the given VAO is bound.
withVAO :: GL.VertexArrayObject -> IO x -> IO x
withVAO vertexArrayObjectName = bracket setup (const teardown) . const
  where
    setup :: IO ()
    setup = GL.bindVertexArrayObject GL.$= Just vertexArrayObjectName

    teardown :: IO ()
    teardown = GL.bindVertexArrayObject GL.$= Nothing
