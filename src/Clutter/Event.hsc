module Clutter.Event where

import Foreign.Ptr(Ptr)
import Foreign.Storable(peekByteOff)

#include <clutter/clutter.h>

simpleButtonHandler :: (Float -> Float -> IO Bool) -> (Ptr () -> IO Bool)
simpleButtonHandler g = f
  where f p = do x <- (#peek ClutterButtonEvent, x) p
                 y <- (#peek ClutterButtonEvent, x) p
                 g x y





