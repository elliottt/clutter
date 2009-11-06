module Clutter.Behavior.Opacity where

import Clutter.Private

import Data.Word

foreign import ccall "clutter_behaviour_opacity_new"
  clutter_behavior_opacity_new :: Ptr () -> Word8 -> Word8 -> IO (Ptr ())


