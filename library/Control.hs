{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Functions for controlling the game loop.
module Control where

import Control.Monad (unless)
import SDL qualified

-- | Loop the game with the given state-modifying function.
loop :: state -> (SDL.Event -> state -> state) -> (state -> IO ()) -> IO ()
loop initial update render = do
  events <- SDL.pollEvents

  unless (SDL.QuitEvent `elem` map SDL.eventPayload events) do
    let updated = foldr update initial events
    render updated *> loop updated update render
