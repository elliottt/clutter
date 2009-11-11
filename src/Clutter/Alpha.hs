module Clutter.Alpha
  ( Alpha
  , newAlpha
  , AnimationMode(..)
  ) where

import Clutter.Private
import Clutter.AnimationMode

import Foreign.Ptr
import Foreign.C.Types


newAlpha :: Timeline -> AnimationMode -> IO Alpha
newAlpha t (AnimationMode x) = withTimeline t $ \pt ->
  ptrAlpha =<< clutter_alpha_new_full pt x

--------------------------------------------------------------------------------

foreign import ccall "clutter_alpha_new_full"
  clutter_alpha_new_full :: Ptr () -> CULong -> IO (Ptr ())


