{-# LANGUAGE ForeignFunctionInterface #-}

#include <clutter/clutter.h>

module Clutter.Actor.Rectangle (
    Rectangle
  , newRectangle
  , newRectangleWithColor
  , getRectangleColor
  , setRectangleColor
  ) where

import Clutter.Color

import Foreign

newtype Rectangle = R (Ptr ())

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
