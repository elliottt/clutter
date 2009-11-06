module Clutter.Alpha
  ( Alpha
  , AnimationMode(..)
  ) where

import Clutter.Timeline
import Clutter.Private
import Clutter.GLib
import Clutter.AnimationMode

import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types


--------------------------------------------------------------------------------

foreign import ccall "clutter_alpha_new_full"
  clutter_alpha_new_full :: Ptr () -> CULong -> IO (Ptr ())


