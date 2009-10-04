{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

module Clutter.Actor.Rectangle (
    Rectangle
  , newRectangle
  , newRectangleWithColor
  , getRectangleColor
  , setRectangleColor
  , getRectangleBorderColor
  , setRectangleBorderColor
  , getRectangleBorderWidth
  , setRectangleBorderWidth
  ) where

import Clutter.Color
import Clutter.Private

import Foreign
import Foreign.C

newtype Rectangle = R (Ptr ())

instance Actor Rectangle where
  fromActor (R p) = p

-- | Creates a new 'Rectangle' shape.
foreign import ccall "clutter_rectangle_new"
  newRectangle :: IO Rectangle

foreign import ccall "clutter_rectangle_new_with_color"
  clutter_rectangle_new_with_color :: Ptr Color -> IO Rectangle

-- | Creates a new 'Rectangle' shape of the given 'Color'.
newRectangleWithColor :: Color -> IO Rectangle
newRectangleWithColor c = with c clutter_rectangle_new_with_color

foreign import ccall "clutter_rectangle_get_color"
  clutter_rectangle_get_color :: Rectangle -> Ptr Color -> IO ()

-- | Retrieve the 'Color' of the 'Rectangle'.
getRectangleColor :: Rectangle -> IO Color
getRectangleColor r = with (Color 0 0 0 0) $ \c -> do
  clutter_rectangle_get_color r c
  peek c

foreign import ccall "clutter_rectangle_set_color"
  clutter_rectangle_set_color :: Rectangle -> Ptr Color -> IO ()

-- | Set the color of the 'Rectangle' to the given 'Color'
setRectangleColor :: Rectangle -> Color -> IO ()
setRectangleColor r c = with c (clutter_rectangle_set_color r)

foreign import ccall "clutter_rectangle_get_border_color"
  clutter_rectangle_get_border_color :: Rectangle -> Ptr Color -> IO ()

-- | Gets the 'Color' of the border used by the 'Rectangle'.
getRectangleBorderColor :: Rectangle -> IO Color
getRectangleBorderColor r = with (Color 0 0 0 0) $ \c -> do
  clutter_rectangle_get_border_color r c
  peek c

foreign import ccall "clutter_rectangle_set_border_color"
  clutter_rectangle_set_border_color :: Rectangle -> Ptr Color -> IO ()

-- | Sets the 'Color' of the border used by the 'Rectangle'.
setRectangleBorderColor :: Rectangle -> Color -> IO ()
setRectangleBorderColor r c = with c (clutter_rectangle_set_border_color r)

foreign import ccall "clutter_rectangle_get_border_width"
  clutter_rectangle_get_border_width :: Rectangle -> IO CInt

-- | Get the width of the border used by 'Rectangle'.
getRectangleBorderWidth :: Rectangle -> IO Int
getRectangleBorderWidth r =
  fromIntegral `fmap` clutter_rectangle_get_border_width r

foreign import ccall "clutter_rectangle_set_border_width"
  clutter_rectangle_set_border_width :: Rectangle -> CInt -> IO ()

-- | Set the width of the border used by 'Rectangle'.
setRectangleBorderWidth :: Rectangle -> Int -> IO ()
setRectangleBorderWidth r w =
  clutter_rectangle_set_border_width r (fromIntegral w)
