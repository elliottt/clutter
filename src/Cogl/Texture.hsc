{-# LANGUAGE ForeignFunctionInterface #-}

#include <cogl/cogl.h>

module Cogl.Texture (
    textureFromFile

  , CoglTextureFlags
  , textureNone
  , textureNoAutoMipmap
  , textureNoSlicing

  , CoglPixelFormat
  , pixelFormatAny
  , pixelFormatA8
  , pixelFormatRgb565
  , pixelFormatRgba4444
  , pixelFormatRgba5551
  , pixelFormatYuv
  , pixelFormatG8
  , pixelFormatRgb888
  , pixelFormatBgr888
  , pixelFormatRgba8888
  , pixelFormatBgra8888
  , pixelFormatArgb8888
  , pixelFormatAbgr8888
  , pixelFormatRgba8888Pre
  , pixelFormatBgra8888Pre
  , pixelFormatArgb8888Pre
  , pixelFormatAbgr8888Pre
  , pixelFormatRgba4444Pre
  , pixelFormatRgba5551Pre
  ) where

import Cogl.Material (CoglHandle)

import Foreign
import Foreign.C.String
import Foreign.C.Types

newtype CoglTextureFlags = CTF CULong

#enum CoglTextureFlags, CTF\
  , textureNone         = COGL_TEXTURE_NONE\
  , textureNoAutoMipmap = COGL_TEXTURE_NO_AUTO_MIPMAP\
  , textureNoSlicing    = COGL_TEXTURE_NO_SLICING

newtype CoglPixelFormat = CPF CULong

#enum CoglPixelFormat, CPF\
  , pixelFormatAny         = COGL_PIXEL_FORMAT_ANY\
  , pixelFormatA8          = COGL_PIXEL_FORMAT_A_8\
  , pixelFormatRgb565      = COGL_PIXEL_FORMAT_RGB_565\
  , pixelFormatRgba4444    = COGL_PIXEL_FORMAT_RGBA_4444\
  , pixelFormatRgba5551    = COGL_PIXEL_FORMAT_RGBA_5551\
  , pixelFormatYuv         = COGL_PIXEL_FORMAT_YUV\
  , pixelFormatG8          = COGL_PIXEL_FORMAT_G_8\
  , pixelFormatRgb888      = COGL_PIXEL_FORMAT_RGB_888\
  , pixelFormatBgr888      = COGL_PIXEL_FORMAT_BGR_888\
  , pixelFormatRgba8888    = COGL_PIXEL_FORMAT_RGBA_8888\
  , pixelFormatBgra8888    = COGL_PIXEL_FORMAT_BGRA_8888\
  , pixelFormatArgb8888    = COGL_PIXEL_FORMAT_ARGB_8888\
  , pixelFormatAbgr8888    = COGL_PIXEL_FORMAT_ABGR_8888\
  , pixelFormatRgba8888Pre = COGL_PIXEL_FORMAT_RGBA_8888_PRE\
  , pixelFormatBgra8888Pre = COGL_PIXEL_FORMAT_BGRA_8888_PRE\
  , pixelFormatArgb8888Pre = COGL_PIXEL_FORMAT_ARGB_8888_PRE\
  , pixelFormatAbgr8888Pre = COGL_PIXEL_FORMAT_ABGR_8888_PRE\
  , pixelFormatRgba4444Pre = COGL_PIXEL_FORMAT_RGBA_4444_PRE\
  , pixelFormatRgba5551Pre = COGL_PIXEL_FORMAT_RGBA_5551_PRE

mkFlags :: [CoglTextureFlags] -> CULong
mkFlags  = foldl (\z (CTF a) -> z .|. a) 0

foreign import ccall "cogl_texture_new_from_file"
  cogl_texture_new_from_file :: CString -> CULong -> CoglPixelFormat
                             -> IO CoglHandle

-- | Creates a @CoglTexture@ from an image file.
textureFromFile :: FilePath -> [CoglTextureFlags] -> CoglPixelFormat
                -> IO CoglHandle
textureFromFile path flags fmt =
  withCString path (\f -> cogl_texture_new_from_file f (mkFlags flags) fmt)
