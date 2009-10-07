{-# LANGUAGE ForeignFunctionInterface #-}
module Clutter.Actor.Text
  ( Text
  , toText
  , Font(..)
  , newText
  , newTextWithColor
  ) where

#include <clutter/clutter.h>
import Clutter.Color(Color)
import Clutter.Private
import Clutter.GLib

import Foreign
import Foreign.C

--------------------------------------------------------------------------------
newtype Text = T (ForeignPtr ())

instance ForeignObject Text where
  rawPtr (T p) = p

instance Actor Text

toText :: SomeActor -> IO (Maybe Text)
toText p = do t  <- withActor p getType
              ts <- clutter_text_get_type
              return (if t == ts then Just (T (rawPtr p)) else Nothing)

foreign import ccall "clutter_text_get_type"
  clutter_text_get_type :: IO GType
--------------------------------------------------------------------------------

newtype Font = Font String

foreign import ccall "clutter_text_new_full"
  clutter_text_new_full :: CString -> CString -> Ptr Color -> IO (Ptr ())

newTextWithColor :: Font -> String -> Color -> IO Text
newTextWithColor (Font f) s c =
  withCString f $ \fp ->
  withCString s $ \sp ->
  with c        $ \cp ->
    T `fmap` (newGObject =<< clutter_text_new_full fp sp cp)

foreign import ccall "clutter_text_new_with_text"
  clutter_text_new_with_text :: CString -> CString -> IO (Ptr ())

newText :: Font -> String -> IO Text
newText (Font f) s =
  withCString f $ \fp ->
  withCString s $ \sp ->
    T `fmap` (newGObject =<< clutter_text_new_with_text fp sp)


