{-# LANGUAGE ForeignFunctionInterface #-}
module Clutter.Utils
  ( HandlerId
  , signalConnect
  , signalDisconnect
  , void0, void1, bool1
  ) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

data HandlerId  = HID !(Ptr ()) !(FunPtr ()) !CULong

signalConnect :: Ptr ()         -- ^ An object to receive signal
              -> String         -- ^ Name of signal
              -> IO (FunPtr ()) -- ^ Callback factory
              -> IO HandlerId   -- ^ An id for disconnecting
signalConnect obj sig new = withCString sig $ \str  -> do
  cbk <- new
  HID obj cbk `fmap` g_signal_connect_data obj str cbk nullPtr nullPtr 0

signalDisconnect :: HandlerId -> IO ()
signalDisconnect (HID obj cbk i) = do
  g_signal_handler_disconnect obj i
  freeHaskellFunPtr cbk

type Factory a = a -> IO (FunPtr a)

foreign import ccall "wrapper"
  newVoid0 :: Factory (Ptr () -> Ptr () -> IO ())

foreign import ccall "wrapper"
  newVoid1 :: Factory (Ptr () -> Ptr () -> Ptr () -> IO ())

foreign import ccall "wrapper"
  newBool1 :: Factory (Ptr () -> Ptr () -> Ptr () -> IO Bool)

void0 :: IO () -> IO (FunPtr ())
void0 x = castFunPtr `fmap` newVoid0 (\_ _ -> x)

void1 :: (Ptr () -> IO ()) -> IO (FunPtr ())
void1 x = castFunPtr `fmap` newVoid1 (\_ p _ -> x p)

bool1 :: (Ptr () -> IO Bool) -> IO (FunPtr ())
bool1 x = castFunPtr `fmap` newBool1 (\_ p _ -> x p)


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

