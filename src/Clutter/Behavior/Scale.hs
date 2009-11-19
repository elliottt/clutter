module Clutter.Behavior.Scale
  ( ScaleBehavior
  , newScaleBehavior
  ) where

import Clutter.Private

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

newtype ScaleBehavior = ScaleBehavior (ForeignPtr ())
instance ForeignObject ScaleBehavior where rawPtr (ScaleBehavior p) = p
instance Behavior ScaleBehavior

newScaleBehavior :: Alpha -> Double -> Double
                          -> Double -> Double -> IO ScaleBehavior
newScaleBehavior a x1 y1 x2 y2 = withAlpha a $ \ap ->
  do p  <- clutter_behaviour_scale_new ap (realToFrac x1) (realToFrac y1)
                                          (realToFrac x2) (realToFrac y2)
     fp <- newGObject p
     return (ScaleBehavior fp)

--------------------------------------------------------------------------------

foreign import ccall "clutter_behaviour_scale_new"
  clutter_behaviour_scale_new :: Ptr () -> CDouble -> CDouble
                                        -> CDouble -> CDouble
                                        -> IO (Ptr ())


