{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- A parser for OBJ files.
module Object.Parser where

import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as Parser
import Data.ByteString (ByteString, readFile)
import Prelude hiding (readFile)
import SDL qualified
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (hPrintf)

-- | Parse an @OBJ@ file into a list of commands. We also handle commands we
-- don't understand by preserving the line's 'ByteString'.
parse :: FilePath -> IO [Command]
parse path = do
  source <- readFile path

  let parser :: Parser [Command]
      parser = Parser.many' do
        Parser.choice
          [ comment
          , group
          , vertex
          , texture
          , normal
          , face
          , unknown
          ]

  case Parser.parseOnly parser source of
    Right commands -> do
      hPrintf stderr "Loaded model '%s'\n" path
      pure commands

    Left message -> do
      hPrintf stderr "Error loading model '%s'\n" path
      hPutStrLn stderr message

      exitFailure

-- | All commands that we understand how to parse within an @OBJ@ file. We
-- might not do anything with some of these (e.g. 'Comment'), but this gives us
-- a "complete" AST.
data Command
  = Unknown  ByteString
  | Comment  ByteString
  | Group    ByteString
  | Vertex  (SDL.V3  Float)
  | Texture (SDL.V2  Float)
  | Normal  (SDL.V3  Float)
  | Face    (SDL.M33   Int)
  deriving stock Show

-- | A comment from the source. We don't care about these commands, so we'll
-- throw them away in the next phase.
comment :: Parser Command
comment = do
  _ <- Parser.char '#'
  Parser.skipMany Parser.space

  message <- Parser.takeTill (== '\n')
  Parser.endOfLine

  pure $ Comment message

-- | The name of a group of points in the source. We also don't care about
-- these for now.
group :: Parser Command
group = do
  _ <- Parser.char 'o'
  Parser.skipMany1 Parser.space

  title <- Parser.takeTill (== '\n')
  Parser.endOfLine

  pure $ Group title

-- | A vertex triple (x, y, z).
vertex :: Parser Command
vertex = do
  _ <- Parser.char 'v'
  Parser.skipMany1 Parser.space

  x <- float <* Parser.skipMany1 Parser.space
  y <- float <* Parser.skipMany1 Parser.space
  z <- float <* Parser.takeTill (== '\n')

  Parser.endOfLine
  pure $ Vertex (SDL.V3 x y z)

-- | A texture coordinate (u, v).
texture :: Parser Command
texture = do
  _ <- Parser.string "vt"
  Parser.skipMany1 Parser.space

  u <- float <* Parser.skipMany1 Parser.space
  v <- float <* Parser.takeTill (== '\n')

  Parser.endOfLine
  pure $ Texture (SDL.V2 u v)

-- | A normal vector (x, y, z).
normal :: Parser Command
normal = do
  _ <- Parser.string "vn"
  Parser.skipMany1 Parser.space

  x <- float <* Parser.skipMany1 Parser.space
  y <- float <* Parser.skipMany1 Parser.space
  z <- float <* Parser.takeTill (== '\n')

  Parser.endOfLine
  pure $ Normal (SDL.V3 x y z)

-- | A face describes the three points of a triangle. It consists of three
-- triples, each representing the vertex, texture, and normal indices for that
-- point.
face :: Parser Command
face = do
  _ <- Parser.char 'f'
  Parser.skipMany1 Parser.space

  let triple :: Parser (SDL.V3 Int)
      triple = do
        x <- Parser.decimal <* Parser.char '/'
        y <- Parser.decimal <* Parser.char '/'
        z <- Parser.decimal

        pure (SDL.V3 x y z)

  x <- triple <* Parser.skipMany1 Parser.space
  y <- triple <* Parser.skipMany1 Parser.space
  z <- triple <* Parser.takeTill (== '\n')

  Parser.endOfLine
  pure $ Face (SDL.V3 x y z)

-- | Fallback parser for commands we don't recognise.
unknown :: Parser Command
unknown = do
  command <- Parser.takeTill (== '\n')
  Parser.endOfLine

  pure (Unknown command)

-- | Parse a floating point number. Because we work with 'GL.GLfloat', we need
-- to convert all doubles to floats.
float :: Parser Float
float = fmap realToFrac (Parser.signed Parser.double)
