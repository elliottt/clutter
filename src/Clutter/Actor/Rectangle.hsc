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
import Clutter.GLib

import Foreign
import Foreign.C

newtype Rectangle = R (ForeignPtr ())

instance Actor Rectangle where
  withActor (R p) k = withForeignPtr p k

-- | Creates a new 'Rectangle' shape.
foreign import ccall "clutter_rectangle_new"
  clutter_rectangle_new :: IO (Ptr ())

newRectangle :: IO Rectangle
newRectangle = R `fmap` (newGObject =<< clutter_rectangle_new)

foreign import ccall "clutter_rectangle_new_with_color"
  clutter_rectangle_new_with_color :: Ptr Color -> IO (Ptr ())

-- | Creates a new 'Rectangle' shape of the given 'Color'.
newRectangleWithColor :: Color -> IO Rectangle
newRectangleWithColor c =
  R `fmap` (newGObject =<<  with c clutter_rectangle_new_with_color)

foreign import ccall "clutter_rectangle_get_color"
  clutter_rectangle_get_color :: Ptr () -> Ptr Color -> IO ()

-- | Retrieve the 'Color' of the 'Rectangle'.
getRectangleColor :: Rectangle -> IO Color
getRectangleColor r = with (Color 0 0 0 0) $ \c ->
                      withActor r $ \p ->
  do clutter_rectangle_get_color p c
     peek c

foreign import ccall "clutter_rectangle_set_color"
  clutter_rectangle_set_color :: Ptr () -> Ptr Color -> IO ()

-- | Set the color of the 'Rectangle' to the given 'Color'
setRectangleColor :: Rectangle -> Color -> IO ()
setRectangleColor r c = withActor r $ \p ->
                        with c (clutter_rectangle_set_color p)

foreign import ccall "clutter_rectangle_get_border_color"
  clutter_rectangle_get_border_color :: Ptr () -> Ptr Color -> IO ()

-- | Gets the 'Color' of the border used by the 'Rectangle'.
getRectangleBorderColor :: Rectangle -> IO Color
getRectangleBorderColor r = withActor r $ \p ->
                            with (Color 0 0 0 0) $ \c -> do
  clutter_rectangle_get_border_color p c
  peek c

foreign import ccall "clutter_rectangle_set_border_color"
  clutter_rectangle_set_border_color :: Ptr () -> Ptr Color -> IO ()

-- | Sets the 'Color' of the border used by the 'Rectangle'.
setRectangleBorderColor :: Rectangle -> Color -> IO ()
setRectangleBorderColor r c = withActor r $ \p ->
                              with c (clutter_rectangle_set_border_color p)

foreign import ccall "clutter_rectangle_get_border_width"
  clutter_rectangle_get_border_width :: Ptr () -> IO CInt

-- | Get the width of the border used by 'Rectangle'.
getRectangleBorderWidth :: Rectangle -> IO Int
getRectangleBorderWidth r =
  withActor r $ \p ->
  fromIntegral `fmap` clutter_rectangle_get_border_width p

foreign import ccall "clutter_rectangle_set_border_width"
  clutter_rectangle_set_border_width :: Ptr () -> CInt -> IO ()

-- | Set the width of the border used by 'Rectangle'.
setRectangleBorderWidth :: Rectangle -> Int -> IO ()
setRectangleBorderWidth r w =
  withActor r $ \p ->
  clutter_rectangle_set_border_width p (fromIntegral w)


