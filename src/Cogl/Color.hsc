{-# LANGUAGE ForeignFunctionInterface #-}

#include <cogl/cogl.h>

module Cogl.Color (
    CoglColor

  , newCoglColor
  , freeCoglColor
  , copyCoglColor

  , ColorComponent(..)
  , newCoglColorWith

  ) where

import Foreign

newtype CoglColor = CC (Ptr ())

foreign import ccall "cogl_color_new"
  newCoglColor :: IO CoglColor

foreign import ccall "cogl_color_free"
  freeCoglColor :: CoglColor -> IO ()

foreign import ccall "cogl_color_copy"
  copyCoglColor :: CoglColor -> IO CoglColor

-- | Create a new color, and initialize it with the provided rgba values.
newCoglColorWith :: ColorComponent a => a -> a -> a -> a -> IO CoglColor
newCoglColorWith r g b a = do
  c <- newCoglColor
  setCoglColor c r g b a
  return c

class ColorComponent a where
  setCoglColor :: CoglColor -> a -> a -> a -> a -> IO ()
  getRed   :: CoglColor -> IO a
  getGreen :: CoglColor -> IO a
  getBlue  :: CoglColor -> IO a
  getAlpha :: CoglColor -> IO a

instance ColorComponent Float where
  setCoglColor = cogl_color_set_from_4f
  getRed   = cogl_color_get_red_float
  getGreen = cogl_color_get_green_float
  getBlue  = cogl_color_get_blue_float
  getAlpha = cogl_color_get_alpha_float

foreign import ccall "cogl_color_set_from_4f"
  cogl_color_set_from_4f :: CoglColor -> Float -> Float -> Float -> Float
                         -> IO ()

foreign import ccall "cogl_color_get_red_float"
  cogl_color_get_red_float :: CoglColor -> IO Float

foreign import ccall "cogl_color_get_green_float"
  cogl_color_get_green_float :: CoglColor -> IO Float

foreign import ccall "cogl_color_get_blue_float"
  cogl_color_get_blue_float :: CoglColor -> IO Float

foreign import ccall "cogl_color_get_alpha_float"
  cogl_color_get_alpha_float :: CoglColor -> IO Float

instance ColorComponent Word8 where
  setCoglColor = cogl_color_set_from_4ub
  getRed   = cogl_color_get_red_byte
  getGreen = cogl_color_get_green_byte
  getBlue  = cogl_color_get_blue_byte
  getAlpha = cogl_color_get_alpha_byte

foreign import ccall "cogl_color_set_from_4ub"
  cogl_color_set_from_4ub :: CoglColor -> Word8 -> Word8 -> Word8 -> Word8
                          -> IO ()

foreign import ccall "cogl_color_get_red_byte"
  cogl_color_get_red_byte :: CoglColor -> IO Word8

foreign import ccall "cogl_color_get_green_byte"
  cogl_color_get_green_byte :: CoglColor -> IO Word8

foreign import ccall "cogl_color_get_blue_byte"
  cogl_color_get_blue_byte :: CoglColor -> IO Word8

foreign import ccall "cogl_color_get_alpha_byte"
  cogl_color_get_alpha_byte :: CoglColor -> IO Word8
