{-# LANGUAGE ForeignFunctionInterface #-}

#include <cogl/cogl.h>

module Cogl.Material where

import Foreign

newtype CoglHandle = CH (Ptr ())

foreign import ccall "cogl_material_new"
  newCoglHandle :: IO CoglHandle

foreign import ccall "cogl_handle_ref"
  refCoglHandle :: CoglHandle -> IO CoglHandle

foreign import ccall "cogl_handle_unref"
  unrefCoglHandle :: CoglHandle -> IO ()
