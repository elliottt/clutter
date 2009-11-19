module Clutter.Behavior.Rotate
  ( RotateDirection
  , clockwise
  , counterClockwise

  , RotateAxis
  , xAxis
  , yAxis
  , zAxis

  , RotateBehavior
  , newRotateBehavior
  ) where

import Clutter.Private
import Clutter.GLib
import Clutter.Private.Types

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types


newtype RotateBehavior = RotateBehavior (ForeignPtr ())
instance ForeignObject RotateBehavior where rawPtr (RotateBehavior p) = p
instance Behavior RotateBehavior

newRotateBehavior :: Alpha -> RotateAxis -> RotateDirection
                  -> Double -> Double -> IO RotateBehavior
newRotateBehavior alpha axis direction start end = withAlpha alpha $ \ap ->
  do p  <- clutter_behaviour_rotate_new ap axis direction
                                    (realToFrac start) (realToFrac end)
     fp <- newGObject p
     return (RotateBehavior fp)



--------------------------------------------------------------------------------

foreign import ccall "clutter_behaviour_rotate_new"
  clutter_behaviour_rotate_new :: Ptr () -> RotateAxis -> RotateDirection
                                         -> CDouble    -> CDouble
                                         -> IO (Ptr ())

{-
foreign import ccall "clutter_behaviour_rotate_set_center"
  clutter_behaviour_rotate_set_center
    :: Ptr () -> CDouble -> CDouble -> CDouble -> IO ()
-} 
                    


