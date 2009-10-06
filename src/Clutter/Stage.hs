module Clutter.Stage
  ( Stage
  , stageGetDefault
  , stageSetColor
  ) where

import Foreign.Ptr(Ptr)
import Foreign.ForeignPtr(ForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Marshal.Utils(with)

import Clutter.Private(Actor(..),Container(..))
import Clutter.Color

newtype Stage = Stage (ForeignPtr ())

instance Actor Stage     where withActor = withPtr
instance Container Stage where withContainer = withPtr

withPtr :: Stage -> (Ptr () -> IO a) -> IO a
withPtr (Stage p) k = withForeignPtr p k


stageGetDefault :: IO Stage
stageGetDefault = Stage `fmap` (newForeignPtr_ =<< clutter_stage_get_default)

foreign import ccall "clutter_stage_get_default"
  clutter_stage_get_default :: IO (Ptr ())

stageSetColor :: Stage -> Color -> IO ()
stageSetColor s c  = withActor s $ \p -> with c (clutter_stage_set_color p)

foreign import ccall "clutter_stage_set_color"
  clutter_stage_set_color :: Ptr () -> Ptr Color -> IO ()

