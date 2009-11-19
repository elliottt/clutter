module Clutter.Behavior.Opacity
  ( OpacityBehavior
  , newOpacityBehavior
  ) where

import Clutter.Private

import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr

newtype OpacityBehavior = OpacityBehavior (ForeignPtr ())
instance ForeignObject OpacityBehavior where rawPtr (OpacityBehavior p) = p
instance Behavior OpacityBehavior

newOpacityBehavior :: Alpha -> Word8 -> Word8 -> IO OpacityBehavior
newOpacityBehavior a x y = withAlpha a $ \ap ->
  do p  <- clutter_behaviour_opacity_new ap x y
     fp <- newGObject p
     return (OpacityBehavior fp)

--------------------------------------------------------------------------------

foreign import ccall "clutter_behaviour_opacity_new"
  clutter_behaviour_opacity_new :: Ptr () -> Word8 -> Word8 -> IO (Ptr ())


