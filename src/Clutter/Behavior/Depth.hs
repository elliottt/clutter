module Clutter.Behavior.Depth
  ( DepthBehavior
  , newDepthBehavior
  ) where

import Clutter.Private

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

newtype DepthBehavior = DepthBehavior (ForeignPtr ())
instance ForeignObject DepthBehavior where rawPtr (DepthBehavior p) = p
instance Behavior DepthBehavior

newDepthBehavior :: Alpha -> Int -> Int -> IO DepthBehavior
newDepthBehavior a x y = withAlpha a $ \ap ->
  do p  <- clutter_behaviour_depth_new ap (fromIntegral x) (fromIntegral y)
     fp <- newGObject p
     return (DepthBehavior fp)

--------------------------------------------------------------------------------

foreign import ccall "clutter_behaviour_depth_new"
  clutter_behaviour_depth_new :: Ptr () -> CInt -> CInt -> IO (Ptr ())


