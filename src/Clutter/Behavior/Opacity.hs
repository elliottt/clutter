module Clutter.Behavior.Opacity
  ( Opacity
  , newOpacity
  ) where

import Clutter.Private
import Clutter.GLib

import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr

newtype Opacity = Opacity (ForeignPtr ())
instance ForeignObject Opacity where rawPtr (Opacity p) = p
instance Behavior Opacity

newOpacity :: Alpha -> Word8 -> Word8 -> IO Opacity
newOpacity a x y = withAlpha a $ \ap ->
  do p  <- clutter_behaviour_opacity_new ap x y
     fp <- newGObject p
     return (Opacity fp)

--------------------------------------------------------------------------------

foreign import ccall "clutter_behaviour_opacity_new"
  clutter_behaviour_opacity_new :: Ptr () -> Word8 -> Word8 -> IO (Ptr ())


