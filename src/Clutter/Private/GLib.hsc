module Clutter.Private.GLib where

import Foreign.C.String
import Foreign.C.Types
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


-- Signals ---------------------------------------------------------------------

data HandlerId  = HID !(Ptr ()) !(FunPtr ()) !CULong

signalConnect :: Ptr ()         -- ^ An object to receive signal
              -> String         -- ^ Name of signal
              -> IO (FunPtr ()) -- ^ Callback factory
              -> IO HandlerId   -- ^ An id for disconnecting
signalConnect = signalConnectHow 0

signalConnectAfter :: Ptr ()         -- ^ An object to receive signal
                   -> String         -- ^ Name of signal
                   -> IO (FunPtr ()) -- ^ Callback factory
                   -> IO HandlerId   -- ^ An id for disconnecting
signalConnectAfter = signalConnectHow (#const G_CONNECT_AFTER)

signalConnectHow :: CInt
                 -> Ptr ()         -- ^ An object to receive signal
                 -> String         -- ^ Name of signal
                 -> IO (FunPtr ()) -- ^ Callback factory
                 -> IO HandlerId   -- ^ An id for disconnecting
signalConnectHow how obj sig new = withCString sig $ \str  -> do
  cbk <- new
  HID obj cbk `fmap` g_signal_connect_data obj str cbk nullPtr nullPtr how

signalDisconnect :: HandlerId -> IO ()
signalDisconnect (HID obj cbk i) = do
  g_signal_handler_disconnect obj i
  freeHaskellFunPtr cbk

type Factory a = a -> IO (FunPtr a)

foreign import ccall "wrapper"
  new_void :: Factory (Ptr () -> Ptr () -> IO ())

foreign import ccall "wrapper"
  new_ptr_void :: Factory (Ptr () -> Ptr () -> Ptr () -> IO ())

foreign import ccall "wrapper"
  new_ptr_bool :: Factory (Ptr () -> Ptr () -> Ptr () -> IO Bool)

foreign import ccall "wrapper"
  new_int_void :: Factory (Ptr () -> CInt -> Ptr () -> IO ())

foreign import ccall "wrapper"
  new_ptr_int_void :: Factory (Ptr () -> Ptr () -> CInt -> Ptr () -> IO ())

type Callback a = a -> IO (FunPtr ())

void :: Callback (IO ())
void x = castFunPtr `fmap` new_void (\_ _ -> x)

ptr_void :: Callback (Ptr () -> IO ())
ptr_void x = castFunPtr `fmap` new_ptr_void (\_ p _ -> x p)

ptr_bool :: Callback (Ptr () -> IO Bool)
ptr_bool x = castFunPtr `fmap` new_ptr_bool (\_ p _ -> x p)

int_void :: Callback (Int -> IO ())
int_void x = castFunPtr `fmap` new_int_void (\_ i _ -> x (fromIntegral i))

string_int_void :: Callback (String -> Int -> IO ())
string_int_void x = castFunPtr `fmap` new_ptr_int_void (\_ p i _ -> do
  str <- peekCString (castPtr p)
  x str (fromIntegral i))


foreign import ccall "g_signal_connect_data"
  g_signal_connect_data :: Ptr ()               -- object
                        -> CString              -- event
                        -> FunPtr a             -- callback
                        -> Ptr ()               -- callback data
                        -> Ptr ()               -- ??
                        -> CInt                 -- ??
                        -> IO CULong

foreign import ccall "g_signal_handler_disconnect"
  g_signal_handler_disconnect :: Ptr () -- object
                              -> CULong -- handler id
                              -> IO ()

