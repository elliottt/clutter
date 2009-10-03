{-# LANGUAGE ForeignFunctionInterface #-}

#include <cogl/cogl.h>

module Cogl.Color where

import Foreign

newtype CoglColor = CC (Ptr ())

getCoglColorPtr :: CoglColor -> Ptr ()
getCoglColorPtr (CC ptr) = ptr

foreign import ccall "cogl_color_new"
  cogl_color_new :: IO (Ptr ())

newCoglColor :: IO CoglColor
newCoglColor  = CC `fmap` cogl_color_new
