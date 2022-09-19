{-# LANGUAGE NumericUnderscores #-}

-- | Functions for controlling the game loop.
module Control where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import SDL qualified

-- | Loop the game with the given state-modifying function. This function also
-- caps the frame rate at 60 FPS.
loop :: state -> ([SDL.Event] -> state -> state) -> (state -> IO ()) -> IO ()
loop (initial :: state) update render = SDL.ticks >>= go initial . fromIntegral
 where
  go :: state -> Int -> IO ()
  go state previous = do
   current <- fmap fromIntegral SDL.ticks

   let difference :: Int
       difference = 1_000 * (current - previous)

       period :: Int
       period = 1_000_000 `div` 60

   threadDelay (period - difference)
   events <- SDL.pollEvents

   unless (SDL.QuitEvent `elem` map SDL.eventPayload events) do
    let updated :: state
        updated = update events state

    render updated
    go updated current
