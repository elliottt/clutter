module Clutter.Stage
  ( Stage
  , stageGetDefault
  , stageSetColor
  ) where

import Foreign.Ptr(Ptr)
import Foreign.Marshal.Utils(with)

import Clutter.Private(Actor(..),Container(..))
import Clutter.Color

newtype Stage = Stage { unStage :: Ptr () }

instance Actor Stage where
  withActor x k = k (unStage x)

instance Container Stage where
  fromContainer = unStage

foreign import ccall "clutter_stage_get_default"
  stageGetDefault :: IO Stage

stageSetColor :: Stage -> Color -> IO ()
stageSetColor s c  = with c (c_clutter_stage_set_color s)

foreign import ccall "clutter_stage_set_color"
  c_clutter_stage_set_color :: Stage -> Ptr Color -> IO ()

