{-# LANGUAGE ForeignFunctionInterface #-}

module Clutter.Actor.Rectangle (
    Rectangle
  , newRectangle
  , newRectangleWithColor
  , getBorderColor
  , setBorderColor
  , getBorderWidth
  , setBorderWidth
  , toRectangle
  ) where

import Clutter.Color
import Clutter.Private

import Foreign
import Foreign.C

newtype Rectangle = R (ForeignPtr ())

instance ForeignObject Rectangle where rawPtr (R x) = x
instance Actor Rectangle

toRectangle :: SomeActor -> IO (Maybe Rectangle)
toRectangle p = do t  <- withActor p getType
                   ts <- clutter_rectangle_get_type
                   return (if t == ts then Just (R (rawPtr p))
                                      else Nothing)

foreign import ccall "clutter_rectangle_get_type"
  clutter_rectangle_get_type :: IO GType



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

instance HasColor Rectangle where
  setColor r c = withPtr r $ \p -> with c (clutter_rectangle_set_color p)
  getColor r   = with defaultColor $ \c ->
                 withPtr r $ \p ->
                   do clutter_rectangle_get_color p c
                      peek c

foreign import ccall "clutter_rectangle_get_color"
  clutter_rectangle_get_color :: Ptr () -> Ptr Color -> IO ()

foreign import ccall "clutter_rectangle_set_color"
  clutter_rectangle_set_color :: Ptr () -> Ptr Color -> IO ()



foreign import ccall "clutter_rectangle_get_border_color"
  clutter_rectangle_get_border_color :: Ptr () -> Ptr Color -> IO ()

-- | Gets the 'Color' of the border used by the 'Rectangle'.
getBorderColor :: Rectangle -> IO Color
getBorderColor r = withPtr r $ \p ->
                            with (Color 0 0 0 0) $ \c -> do
  clutter_rectangle_get_border_color p c
  peek c

foreign import ccall "clutter_rectangle_set_border_color"
  clutter_rectangle_set_border_color :: Ptr () -> Ptr Color -> IO ()

-- | Sets the 'Color' of the border used by the 'Rectangle'.
setBorderColor :: Rectangle -> Color -> IO ()
setBorderColor r c = withPtr r $ \p ->
                              with c (clutter_rectangle_set_border_color p)

foreign import ccall "clutter_rectangle_get_border_width"
  clutter_rectangle_get_border_width :: Ptr () -> IO CInt

-- | Get the width of the border used by 'Rectangle'.
getBorderWidth :: Rectangle -> IO Int
getBorderWidth r =
  withPtr r $ \p ->
  fromIntegral `fmap` clutter_rectangle_get_border_width p

foreign import ccall "clutter_rectangle_set_border_width"
  clutter_rectangle_set_border_width :: Ptr () -> CInt -> IO ()

-- | Set the width of the border used by 'Rectangle'.
setBorderWidth :: Rectangle -> Int -> IO ()
setBorderWidth r w =
  withPtr r $ \p ->
  clutter_rectangle_set_border_width p (fromIntegral w)


