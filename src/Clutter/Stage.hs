module Clutter.Stage
  ( getDefault
  ) where

import Clutter.Actor(Actor)


foreign import ccall "clutter_stage_get_default" getDefault :: IO Actor
