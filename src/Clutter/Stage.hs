module Clutter.Stage
  ( Stage
  , stageGetDefault
  , stageSetColor
  ) where

import Foreign.Ptr(Ptr)
import Foreign.ForeignPtr(ForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Marshal.Utils(with)

import Clutter.Private
import Clutter.Color
import Clutter.GLib

newtype Stage = Stage (ForeignPtr ())

instance ForeignObject Stage where rawPtr (Stage p) = p
instance Actor Stage
instance Container Stage


stageGetDefault :: IO Stage
stageGetDefault = Stage `fmap` (newForeignPtr_ =<< clutter_stage_get_default)

foreign import ccall "clutter_stage_get_default"
  clutter_stage_get_default :: IO (Ptr ())

stageSetColor :: Stage -> Color -> IO ()
stageSetColor s c  = withPtr s $ \p -> with c (clutter_stage_set_color p)

foreign import ccall "clutter_stage_set_color"
  clutter_stage_set_color :: Ptr () -> Ptr Color -> IO ()


toStage :: SomeActor -> IO (Maybe Stage)
toStage p = do t <- withActor p getType
               ts <- clutter_stage_get_type
               return (if t == ts then Just (Stage (rawPtr p))
                                  else Nothing)

foreign import ccall "clutter_stage_get_type"
  clutter_stage_get_type :: IO GType

