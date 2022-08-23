{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

-- | Functions for responding to user input.
module Input where

import SDL qualified

-- | Check whether the given key has been pressed.
isPressed :: SDL.Keycode -> SDL.Event -> Bool
isPressed key event = case SDL.eventPayload event of
  SDL.KeyboardEvent SDL.KeyboardEventData{..} ->
    SDL.keysymKeycode keyboardEventKeysym == key
      && keyboardEventKeyMotion == SDL.Pressed
  _ -> False
