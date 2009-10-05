module Clutter.GLib where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Data.Word

#include <glib-object.h>

type GType   = #type GType

getType :: Ptr () -> IO GType
getType x = (#peek GTypeClass, g_type) =<< (#peek GTypeInstance, g_class) x

-- | Given a freshliny allocated \"floaitng\" pointer,
-- we make a foreign pointer.
newGObject :: Ptr () -> IO (ForeignPtr ())
newGObject p = newForeignPtr g_object_unref_ptr =<< g_object_ref_sink p

foreign import ccall "g_object_ref_sink"
  g_object_ref_sink :: Ptr () -> IO (Ptr ())

foreign import ccall "&g_object_unref"
  g_object_unref_ptr :: FunPtr (Ptr () -> IO ())



